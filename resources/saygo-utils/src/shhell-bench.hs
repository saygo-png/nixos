{-# LANGUAGE QuasiQuotes #-}

-- | Benchmarks shhell the way you use it: in a terminal, typing a line and
-- waiting for the next prompt. tasty-bench's options apply: @-p@ to pick
-- benchmarks, @--csv@ to save results and @--baseline@ to compare against them.
--
-- It runs in a throwaway sandbox: a copy of your shhell and direnv config, with
-- empty caches and its own direnv allow list. Set SHHELL_BENCH_DIRENV_DIR to a
-- directory with an .envrc (e.g. `use flake`) to load that instead of a
-- generated one.
--
-- The prompt's parts are timed in this process, where starting direnv is
-- cheaper than from GHCi: compare them across changes, and use the prompt
-- round trip for the real cost.
module Main (main) where

import Control.Concurrent (forkIO, threadWaitRead)
import Control.Exception (IOException, bracket_, throwIO, try)
import Data.ByteString qualified as BS
import Relude
import Shh (exe, ignoreFailure)
import Shhell.Direnv (direnvPrompt, direnvSync)
import Shhell.Prompt (shhellPrompt)
import System.Directory.OsPath
import System.Environment (getExecutablePath)
import System.File.OsPath qualified as File
import System.IO.Error (userError)
import System.OsPath (OsPath, decodeFS, encodeFS, osp, takeDirectory, (</>))
import System.OsString.Internal.Types (OsString (OsString, getOsString))
import System.OsString.Posix (pstr)
import System.OsString.Posix qualified as PS
import System.Posix.Directory.PosixPath (changeWorkingDirectory)
import System.Posix.Env.PosixString qualified as Env
import System.Posix.IO.PosixString
import System.Posix.Process.PosixString (createSession, executeFile, forkProcess, getProcessStatus)
import System.Posix.Resource (Resource (ResourceOpenFiles), ResourceLimit (ResourceLimit), ResourceLimits (softLimit), getResourceLimit, setResourceLimit)
import System.Posix.Signals (killProcess, signalProcess)
import System.Posix.Temp.PosixString (mkdtemp)
import System.Posix.Terminal.PosixString (getSlaveTerminalName, openPseudoTerminal)
import System.Posix.Types (Fd, ProcessID)
import System.Timeout (timeout)
import Test.Tasty (Timeout (NoTimeout), adjustOption, localOption, mkTimeout)
import Test.Tasty.Bench
import Test.Tasty.Providers (IsTest (..), singleTest, testPassed)
import Test.Tasty.Runners (NumThreads (NumThreads))

-------------------------------------------------------------------------------
-- The sandbox

data Sandbox = Sandbox {root, program, loaded :: OsPath}

instance NFData Sandbox where
  rnf (Sandbox a b c) = rnf (a, b, c)

plain, blocked, cache :: Sandbox -> OsPath
plain = (</> [osp|plain|]) . (.root)
blocked = (</> [osp|blocked|]) . (.root)
cache = (</> [osp|cache|]) . (.root)

-- | Copies your config into a new sandbox and points this process's
-- environment, which everything it starts inherits, at it.
sandbox :: IO Sandbox
sandbox = do
  program <- (</> [osp|shhell|]) . takeDirectory <$> (encodeFS =<< getExecutablePath)
  userEnvrcDir <- traverse (makeAbsolute . OsString) =<< Env.getEnv [pstr|SHHELL_BENCH_DIRENV_DIR|]
  config <- getXdgDirectory XdgConfig mempty
  whenNothingM_ (findExecutable [osp|direnv|]) $ throwIO (userError "direnv is not on PATH")

  root <- fmap OsString . mkdtemp . (<> [pstr|/shhell-bench-|]) . fromMaybe [pstr|/tmp|] =<< Env.getEnv [pstr|TMPDIR|]
  let sb = Sandbox{root, program, loaded = fromMaybe (root </> [osp|loaded|]) userEnvrcDir}
  createDirectoryIfMissing True (root </> [osp|config|])
  -- -L: home-manager links config files into the Nix store.
  for_ [[osp|shhell|], [osp|direnv|]] $ \d -> whenM (doesDirectoryExist (config </> d)) $ do
    args <- traverse decodeFS [config </> d, root </> [osp|config|]]
    void (try @IOException (ignoreFailure (exe ("cp" :: ByteString) ("-rL" :: ByteString) args)))

  for_ [([pstr|XDG_CONFIG_HOME|], [osp|config|]), ([pstr|XDG_DATA_HOME|], [osp|data|]), ([pstr|XDG_STATE_HOME|], [osp|state|])]
    $ \(k, d) -> Env.setEnv k (getOsString (root </> d)) True
  -- GHC's temporary files too: benchmarks kill shells that would remove them.
  Env.setEnv [pstr|TMPDIR|] (getOsString root) True
  Env.setEnv [pstr|TERM|] [pstr|xterm-256color|] True
  -- nix run raises the soft limit on open files to the hard one (often 524288),
  -- and every program shh starts first closes each descriptor up to that limit,
  -- taking about 100 ms. Use a login session's default instead.
  limit <- getResourceLimit ResourceOpenFiles
  setResourceLimit ResourceOpenFiles limit{softLimit = ResourceLimit 1024}
  -- If this runs inside a loaded .envrc, start outside of it.
  Env.getEnvironment >>= traverse_ (\(k, _) -> when ([pstr|DIRENV_|] `PS.isPrefixOf` k) (Env.unsetEnv k))

  for_ [plain sb, root </> [osp|loaded|], blocked sb] (createDirectoryIfMissing True)
  let envrc = "PATH_add bin\n" <> foldMap (\i -> "export SHHELL_BENCH_" <> show i <> "=" <> BS.replicate 120 0x78 <> "\n") [1 .. 100 :: Int]
  for_ [root </> [osp|loaded|], blocked sb] $ \d -> File.writeFile' (d </> [osp|.envrc|]) envrc
  exe ("direnv" :: ByteString) ("allow" :: ByteString) =<< decodeFS sb.loaded

  -- Fill the cache the warm benchmarks use: Shell.o and its linked library.
  launch sb (cache sb) (plain sb) >>= \s -> typeLine s "true" >> kill s
  pure sb

-------------------------------------------------------------------------------
-- A shhell in a pseudo-terminal

data Session = Session {master :: Fd, pid :: ProcessID, pending :: IORef ByteString}

instance NFData Session where
  rnf s = s `seq` ()

-- | Starts shhell in a new terminal and waits for its first prompt.
launch :: Sandbox -> OsPath -> OsPath -> IO Session
launch sb cacheDir dir = do
  Env.setEnv [pstr|XDG_CACHE_HOME|] (getOsString cacheDir) True
  (m, slave) <- openPseudoTerminal
  slaveName <- getSlaveTerminalName m
  p <- forkProcess $ do
    -- A new session, whose controlling terminal is the first one it opens.
    void createSession
    closeFd m >> closeFd slave
    tty <- openFd slaveName ReadWrite defaultFileFlags
    for_ [stdInput, stdOutput, stdError] (dupTo tty)
    when (tty > stdError) (closeFd tty)
    changeWorkingDirectory (getOsString dir)
    executeFile (getOsString sb.program) False [] Nothing
  closeFd slave
  s <- Session m p <$> newIORef mempty
  s <$ waitPrompt 900 s

-- | Waits for the next prompt and returns what was printed before it. Haskeline
-- switches the terminal to keypad mode (smkx, for the TERM set above) whenever
-- it starts reading a line, just before the prompt, so that marks a prompt,
-- whatever it looks like.
waitPrompt :: Int -> Session -> IO ByteString
waitPrompt seconds s = maybe (throwIO (userError "no prompt")) pure =<< timeout (seconds * 1000000) loop
  where
    marker :: ByteString = "\ESC[?1h\ESC="
    loop = do
      (before, rest) <- BS.breakSubstring marker <$> readIORef s.pending
      if BS.null rest
        then threadWaitRead s.master >> fdRead s.master 65536 >>= \chunk -> modifyIORef' s.pending (<> chunk) >> loop
        else before <$ writeIORef s.pending (BS.drop (BS.length marker) rest)

-- | Types a line and waits for the next prompt; returns what it printed.
typeLine :: Session -> Text -> IO ByteString
typeLine s line = fdWrite s.master (encodeUtf8 line <> "\n") >> waitPrompt 120 s

-- | Ends shhell without waiting, so quitting isn't timed.
kill :: Session -> IO ()
kill s = do
  signalProcess killProcess s.pid
  closeFd s.master
  void (forkIO (void (getProcessStatus True False s.pid)))

-- | Resident memory of a shhell, from /proc, reported as a passing test: the
-- session to measure, and what to do with it after.
data Memory = Memory (IO Session) (Session -> IO ())

instance IsTest Memory where
  testOptions = pure []
  run _ (Memory acquire done) _ = do
    s <- acquire
    status <- decodeUtf8 <$> (File.readFile' =<< encodeFS ("/proc/" <> show s.pid <> "/status"))
    done s
    -- Lines like "VmRSS:     123456 kB".
    let mb :: Text -> Text
        mb key = maybe "?" (\kB -> show (kB `div` 1024 :: Int) <> " MB") $ do
          _ : n : _ <- find ((== [key <> ":"]) . take 1) (map words (lines status))
          readMaybe (toString n)
    pure
      . testPassed
      . toString
      $ unwords [mb "VmRSS", "resident:", mb "RssAnon", "heap and other anonymous,", mb "RssFile", "mapped files (libraries)"]

-------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    . pure
    -- The work happens in other processes, so it's wall time that counts.
    . localOption WallTime
    . localOption (NumThreads 1)
    -- Without a timeout, a noisy benchmark keeps doubling its runs.
    . adjustOption (\case NoTimeout -> mkTimeout 20000000; t -> t)
    . envWithCleanup sandbox (removeDirectoryRecursive . (.root))
    $ \sb ->
      bgroup
        "shhell"
        [ bgroup
            "startup"
            [ -- One run each: compiling takes seconds.
              localOption (RelStDev (1 / 0))
                . bgroup "cold, compiling Shell.hs"
                $ [ bench "to the first prompt" . whnfIO $ cold sb >>= kill
                  , bench "and the first `true` (links)" . whnfIO $ cold sb >>= \s -> binding s >> kill s
                  ]
            , bench "warm: to the first prompt" . whnfIO $ warm sb >>= kill
            , bcompare "$NF == \"warm: to the first prompt\""
                . bench "warm: and the first `true` (cached link)"
                . whnfIO
                $ warm sb
                >>= \s -> binding s >> kill s
            , singleTest "memory at the first prompt" (Memory (warm sb) kill)
            ]
        , inSession sb (plain sb) "commands" $ \s ->
            [ bench "empty statement (the prompt round trip)" . whnfIO $ void (typeLine s noop)
            , bench "Shh.exe \"true\"" . whnfIO $ void (typeLine s "Shh.exe (\"true\" :: Prelude.String)")
            , bench "`true`" . whnfIO $ binding s
            , singleTest "memory after these" (Memory (pure s) (const pass))
            ]
        , bgroup
            "direnv"
            [ inSession sb (plain sb) "cd" $ \s ->
                [ bench "cd between two plain directories" . whnfIO $ cd s (blocked sb) >> cd s (plain sb)
                , bcompare "$NF == \"cd between two plain directories\""
                    . bench "cd into a loaded .envrc and back"
                    . whnfIO
                    $ cd s sb.loaded
                    >> cd s (plain sb)
                ]
            , inSession sb sb.loaded "inside a loaded .envrc" $ \s -> [bench "empty statement" . whnfIO $ void (typeLine s noop)]
            , inSession sb (blocked sb) "inside a blocked .envrc" $ \s -> [bench "empty statement" . whnfIO $ void (typeLine s noop)]
            ]
        , bgroup "prompt parts, in this process" [promptParts "outside any .envrc" (plain sb), promptParts "inside a loaded .envrc" sb.loaded]
        ]
  where
    warm sb = launch sb (cache sb) (plain sb)
    -- Each run compiles Shell.hs into a new, empty cache.
    cold sb = mkdtemp (getOsString sb.root <> [pstr|/cold-|]) >>= \c -> launch sb (OsString c) (plain sb)

    -- GHCi resolves qualified names from any exposed package without an
    -- import, so these work whatever your init.ghci imports.
    noop :: Text = "Prelude.pure ()"
    cd s d = decodeFS d >>= \p -> void (typeLine s ("Shh.cd " <> show p))
    binding s = typeLine s "true" >>= \out -> when ("not in scope" `BS.isInfixOf` out) $ throwIO (userError "Shell.hs has no `true`")

    -- A group sharing one shhell, started in the given directory.
    inSession sb dir name = envWithCleanup (launch sb (cache sb) dir) kill . (bgroup name .)

    -- The functions shhellPrompt is made of, with $PWD in the given directory.
    -- The first direnvSync loads the .envrc into this process; direnv says so
    -- on stderr, which goes to /dev/null meanwhile.
    promptParts name dir =
      envWithCleanup (Env.setEnv [pstr|PWD|] (getOsString dir) True >> quietly direnvSync) pure $ \_ ->
        bgroup
          name
          [ bench "direnvSync" (whnfIO direnvSync)
          , bench "direnvPrompt" (whnfIO (void direnvPrompt))
          , bench "shhellPrompt" (whnfIO (void (shhellPrompt [] 0)))
          ]
    quietly action = do
      (saved, devNull) <- (,) <$> dup stdError <*> openFd [pstr|/dev/null|] WriteOnly defaultFileFlags
      bracket_ (dupTo devNull stdError) (dupTo saved stdError >> closeFd saved >> closeFd devNull) action

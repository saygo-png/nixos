{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Packages in the running shell without starting a new one, like a
-- @nix-shell -p@ that doesn't nest. GHCi commands, which init.ghci defines
-- with @:def use Shhell.Use.use@ and so on:
--
--   * @:use typst ripgrep@ adds packages from nixpkgs. An installable like
--     @nixpkgs#python3Packages.numpy@ works too.
--   * @:dev@ picks named environments from dev-envs.nix with fzf and adds
--     them. Anything after it is fzf's starting query.
--   * @:unuse@ takes off everything that was added.
--
-- Nix builds them, and their bin directories go at the front of PATH in GHCi's
-- own environment, which the commands it runs inherit. What was added is kept
-- in USE_ENV_PATH and USE_ENV_NAMES. The named environments are in the JSON
-- file in SHHELL_DEV_ENVS, which shhell.nix sets.
--
-- Each program they add gets a function, like the ones shhell writes to
-- Programs.hs for the programs on PATH, so @:use hello@ lets you run @hello@.
-- Only the programs in the directories just added are declared, so Programs.hs
-- isn't touched. A name that's already taken, like Prelude's zip, keeps its
-- meaning; run that program with @exe "zip"@. They're GHCi commands because
-- only GHCi can add names to the session: each returns GHCi input that
-- declares them.
module Shhell.Use (use, dev, unuse, declarePrograms, programCmd, useEnvFix, useEnvPrompt) where

import Data.ByteString.Char8 qualified as BC
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Language.Haskell.TH (Exp, Q, appE, lookupValueName, nameModule, runIO)
import Language.Haskell.TH.Syntax qualified as TH
import Relude
import Shh (Cmd, Proc, captureLines, encodeIdentifier, exe, tryFailure, writeOutput, (|>))
import System.Directory (doesDirectoryExist, executable, getPermissions, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.Posix.Env.ByteString (getEnv, setEnv, unsetEnv)
import Text.JSON.Yocto qualified as JSON

-- | @:use typst ripgrep@. Without packages, it declares the added programs
-- again, which GHCi forgets on :reload.
use :: String -> IO String
use arguments = case words (toText arguments) of
  [] -> do
    added <- getList ':' pathVar
    if null added
      then "" <$ hPutStrLn stderr "usage: :use PACKAGE..."
      else declareAdded []
  names -> add [(name, installable name) | name <- names]
  where
    installable name
      | any (`elem` ['#', ':', '/']) (toString name) = name
      | otherwise = "nixpkgs#" <> name

-- | @:dev@, or @:dev query@ to start fzf with that query.
dev :: String -> IO String
dev query = do
  environments <- namedEnvironments
  if null environments
    then "" <$ hPutStrLn stderr "dev: there are no named environments, add some to dev-envs.nix"
    else do
      let menu = unlines [name <> "\t" <> unwords packages | (name, (_, packages)) <- Map.toList environments]
      -- fzf fails when nothing is picked, and reports its own errors.
      picked <- tryFailure (writeOutput (encodeUtf8 @Text @ByteString menu) |> fzf |> captureLines)
      case picked of
        Left _ -> pure ""
        Right names -> add [(name, path) | name <- map decodeUtf8 names, Just (path, _) <- [Map.lookup name environments]]
  where
    fzf :: Proc ()
    fzf = exe ("fzf" :: ByteString) ["--multi", "--delimiter", "\t", "--accept-nth", "1", "--prompt", "dev> ", "--query", toString query]

-- | @:unuse@ takes everything @:use@ and @:dev@ added off PATH again, and
-- their functions out of scope with :forget, from the patched GHCi.
unuse :: String -> IO String
unuse _ = do
  added <- getList ':' pathVar
  programs <- concat <$> traverse (programsIn . decodeUtf8) added
  modifyPath (filter (`notElem` added))
  unsetEnv pathVar
  unsetEnv namesVar
  pure $ ":forget " <> List.unwords (map encodeIdentifier programs)

-- | Puts directories @:use@ added back on PATH if something took them off.
-- direnv does when it unloads an .envrc that was loaded before them: it
-- restores PATH as it was then. For the prompt, after
-- 'Shhell.Direnv.direnvSync'.
useEnvFix :: IO ()
useEnvFix = do
  added <- getList ':' pathVar
  unless (null added) . modifyPath $ \path -> filter (`notElem` path) added <> path

-- | A prompt element naming what @:use@ added.
useEnvPrompt :: IO String
useEnvPrompt =
  getList ' ' namesVar <&> \case
    [] -> ""
    names -> " (use: " <> decodeUtf8 (BC.unwords names) <> ")"

-- | Builds these installables, puts their bin directories on PATH, and returns
-- the GHCi input that declares functions for their programs. The names are for
-- the prompt.
add :: [(Text, Text)] -> IO String
add named = do
  let (names, installables) = unzip named
  -- Nix reports its own errors.
  built <- tryFailure (exe ("nix" :: ByteString) ["build", "--no-link", "--print-out-paths" :: ByteString] (map (encodeUtf8 @Text @ByteString) installables) |> captureLines)
  case built of
    Left _ -> pure ""
    Right paths -> do
      -- A package's other outputs, like man, have no bin directory.
      dirs <- filterM doesDirectoryExist [decodeUtf8 path </> "bin" | path <- paths]
      if null dirs
        then "" <$ hPutStrLn stderr "use: none of them have programs (a bin directory)"
        else do
          added <- getList ':' pathVar
          addedNames <- getList ' ' namesVar
          let dirBytes = map encodeUtf8 dirs
              -- The latest first, so they win over what was added before.
              allAdded = dirBytes <> filter (`notElem` dirBytes) added
          setList ':' pathVar allAdded
          setList ' ' namesVar (ordNub (addedNames <> map encodeUtf8 names))
          modifyPath $ \path -> allAdded <> filter (`notElem` allAdded) path
          declareAdded (filter (`notElem` added) dirBytes)

-- | GHCi input that declares the programs in the added directories, the latest
-- first. 'declarePrograms' leaves out those that are already declared, so this
-- only declares new ones, and the ones a :reload made GHCi forget. Only the
-- programs in these directories, the ones just added, get a note when their
-- name is taken.
declareAdded :: [ByteString] -> IO String
declareAdded new = do
  added <- getList ':' pathVar
  programs <- ordNub . concat <$> traverse (programsIn . decodeUtf8) added
  newPrograms <- concat <$> traverse (programsIn . decodeUtf8) new
  pure $ if null programs then "" else declare programs newPrograms

-- | The executables in a directory, without hidden ones, like the
-- .name-wrapped programs that wrappers run.
programsIn :: FilePath -> IO [String]
programsIn dir = do
  files <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory dir
  filterM (fmap executable . getPermissions . (dir </>)) files

-- | GHCi input that declares the programs that 'declarePrograms' finds free.
-- It checks what's in scope in GHCi's session, which only a splice there can
-- do, and that needs TemplateHaskell, which :cmd only takes from :set.
-- Looking up names like head warns as if they were used, so those warnings
-- are off meanwhile. The flags are put back as they are by default, which
-- also keeps GHCi from recompiling Shell.hs over changed flags.
declare :: [String] -> [String] -> String
declare programs noted =
  List.unlines
    [ ":set -XTemplateHaskell -Wno-x-partial -Wno-deprecations"
    , ":cmd $(Shhell.Use.declarePrograms " <> show programs <> " " <> show noted <> ")"
    , ":set -XNoTemplateHaskell -XNoTemplateHaskellQuotes -Wx-partial -Wdeprecations"
    ]

-- | For a splice in GHCi's session: an @IO String@ of GHCi input that gives
-- each program a function named like 'Shh.loadEnv' names them, unless that
-- name is already taken. The programs in the second list get a note when their
-- name is taken by something else than Shell.hs, Programs.hs or GHCi's
-- session, where programs are declared.
declarePrograms :: [String] -> [String] -> Q Exp
declarePrograms programs noted = do
  checked <- forM programs $ \program -> do
    let name = encodeIdentifier program
    (program,name,) <$> lookupValueName name
  let free = [(program, name) | (program, name, Nothing) <- checked]
      taken = [(program, from) | (program, _, Just existing) <- checked, program `elem` noted, Just from <- [nameModule existing], from `notElem` ["Shell", "Programs"], not ("Ghci" `isPrefixOf` from)]
  for_ taken $ \(program, from) ->
    runIO . hPutStrLn stderr $ "use: " <> program <> " is taken by " <> from <> ", run it with exe " <> show program
  let declarations =
        concat
          [ [name <> " :: Shh.Cmd", name <> " = Shhell.Use.programCmd " <> show program]
          | (program, name) <- free
          ]
      input = if null declarations then "" else List.unlines (":{" : declarations <> [":}"])
  [|pure|] `appE` TH.lift input

programCmd :: String -> Cmd
programCmd program = withFrozenCallStack (exe program)

-- | The named environments from dev-envs.nix: the installable to build for each
-- one, and the names of its packages.
namedEnvironments :: IO (Map Text (Text, [Text]))
namedEnvironments = do
  file <- lookupEnv "SHHELL_DEV_ENVS"
  json <- maybe (pure Nothing) (fmap (Just . decodeUtf8) . readFileBS) file
  pure . fromMaybe mempty $ do
    JSON.Object environments <- JSON.decode <$> json
    pure . Map.fromList $ do
      (name, JSON.Object fields) <- Map.toList environments
      JSON.String path <- maybeToList (Map.lookup "installable" fields)
      JSON.Array packages <- maybeToList (Map.lookup "packages" fields)
      pure (toText name, (toText path, [toText package | JSON.String package <- packages]))

pathVar, namesVar :: ByteString
pathVar = "USE_ENV_PATH"
namesVar = "USE_ENV_NAMES"

getList :: Char -> ByteString -> IO [ByteString]
getList sep name = maybe [] (filter (not . BC.null) . BC.split sep) <$> getEnv name

setList :: Char -> ByteString -> [ByteString] -> IO ()
setList sep name values = setEnv name (BC.intercalate (BC.singleton sep) values) True

-- | Changes PATH, if that changes anything.
modifyPath :: ([ByteString] -> [ByteString]) -> IO ()
modifyPath f = do
  path <- getList ':' "PATH"
  let new = f path
  when (new /= path) $ setList ':' "PATH" new

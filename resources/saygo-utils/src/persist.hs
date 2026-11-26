-- persist lost [--all]  what the next boot deletes
-- persist unused        what doesnt get mounted but is in /persist and /cache
--
-- Every boot rolls the root filesystem back to empty (see
-- modules/impermanence.nix), so all of it is lost but what's mounted from
-- elsewhere: the persisted directories and files, and other filesystems. Run
-- with sudo to see into directories only root can read.

import Control.Exception (IOException, try)
import Data.Set qualified as Set
import Data.Text qualified as T
import Options.Applicative
import Relude
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.Process (readProcess)
import System.Posix.Files (deviceID, fileSize, getSymbolicLinkStatus, isDirectory, isSymbolicLink, readSymbolicLink)
import System.Posix.Types (DeviceID)

data Command = Lost Bool | Unused

-- | What's at a path, as far as the next boot is concerned.
data Node
  = File Integer
  | Link FilePath
  | Dir [(FilePath, Node)]
  | -- | On another filesystem for lost, persisted for unused.
    Kept

main :: IO ()
main =
  execParser parserInfo >>= \case
    Lost showAll -> do
      root <- deviceID <$> getSymbolicLinkStatus "/"
      tree <- scan root mempty "/"
      mapM_ putTextLn . report . lost "/" $ if showAll then tree else withoutGenerated tree
    Unused -> do
      host <- toString . T.strip . decodeUtf8 <$> readFileBS "/proc/sys/kernel/hostname"
      hPutStrLn stderr "persist: evaluating /etc/flake-src..."
      out <- readProcess "nix" ["eval", "--raw", "/etc/flake-src#nixosConfigurations." <> host <> ".config", "--apply", inUse] ""
      let (roots, kept) = break null (map toString (lines (toText out)))
      found <- forM roots $ \root -> do
        device <- deviceID <$> getSymbolicLinkStatus root
        lost root <$> scan device (Set.fromList kept) root
      mapM_ putTextLn (report (concat found))

-- | Applied to the config in /etc/flake-src, what the running system was built
-- from: the storage roots, an empty line, and the paths in use in them.
inUse :: String
inUse =
  "config: let\n\
  \  storages = builtins.filter (s: s.enable) (builtins.attrValues config.environment.persistence);\n\
  \  entries = builtins.concatMap (s: [s] ++ builtins.attrValues s.users) storages;\n\
  \  paths = e: map (d: d.persistentStoragePath + d.dirPath) e.directories ++ map (f: f.persistentStoragePath + f.filePath) e.files;\n\
  \  # Nothing mounts password files kept there.\n\
  \  passwordFiles = builtins.filter (f: f != null) (map (u: u.hashedPasswordFile) (builtins.attrValues config.users.users));\n\
  \in builtins.concatStringsSep \"\\n\" (map (s: s.persistentStoragePath) storages ++ [\"\"] ++ builtins.concatMap paths entries ++ passwordFiles)"

-- | The tree at a path, down to where other filesystems or kept paths start.
scan :: DeviceID -> Set FilePath -> FilePath -> IO Node
scan root kept path = do
  status <- getSymbolicLinkStatus path
  if
    | deviceID status /= root || path `Set.member` kept -> pure Kept
    | isSymbolicLink status -> Link <$> readSymbolicLink path
    | isDirectory status -> Dir <$> (traverse (\name -> (name,) <$> scan root kept (path </> name)) . sort =<< entries)
    | otherwise -> pure (File (fromIntegral (fileSize status)))
  where
    -- None, if it can't be read.
    entries =
      try @IOException (listDirectory path) >>= \case
        Right names -> pure names
        Left err -> [] <$ hPutStrLn stderr ("persist: " <> displayException err)

-- | What's lost, with its size: a path that has nothing kept below it is one
-- entry, and empty directories don't count.
lost :: FilePath -> Node -> [(FilePath, Integer)]
lost path = \case
  Kept -> []
  Dir entries | any (keepsSomething . snd) entries -> concatMap (\(name, node) -> lost (path </> name) node) entries
  node -> [(path, sum sizes) | let sizes = leaves node, not (null sizes)]
  where
    keepsSomething = \case
      Kept -> True
      Dir entries -> any (keepsSomething . snd) entries
      _ -> False
    leaves = \case
      File size -> [size]
      Link target -> [genericLength target]
      Dir entries -> concatMap (leaves . snd) entries
      Kept -> []

-- | Without the links into the Nix store, like /bin/sh and those in /etc, that
-- NixOS makes anew on every boot.
withoutGenerated :: Node -> Node
withoutGenerated = \case
  Dir entries -> Dir [(name, withoutGenerated node) | (name, node) <- entries, not (generated node)]
  node -> node
  where
    generated = \case
      Link target -> any (`isPrefixOf` target) ["/nix/store/", "/etc/static"]
      _ -> False

report :: [(FilePath, Integer)] -> [Text]
report found =
  [T.justifyRight 7 ' ' (human size) <> "  " <> toText path | (path, size) <- found]
    <> ["Total " <> human (sum (map snd found)) <> " in " <> show (length found) <> " paths"]

human :: Integer -> Text
human bytes
  | bytes < 1024 = show bytes <> "B"
  | otherwise = go (fromIntegral bytes :: Double) ["B", "K", "M", "G", "T"]
  where
    go size (unit : units)
      | size < 1024 || null units = show (fromIntegral (round (size * 10) :: Integer) / 10 :: Double) <> unit
      | otherwise = go (size / 1024) units
    go _ [] = ""

parserInfo :: ParserInfo Command
parserInfo =
  info
    (helper <*> hsubparser (lostCommand <> unusedCommand))
    (progDesc "Look at what impermanence deletes")
  where
    lostCommand =
      command "lost"
        . info (Lost <$> switch (long "all" <> help "Also show links into the Nix store, which NixOS makes on every boot"))
        $ progDesc "Show what the next boot deletes: everything on / that isn't persisted"
    unusedCommand = command "unused" . info (pure Unused) $ progDesc "Show what in /persist and /cache nothing uses anymore"

-- persist lost [--all]  what the next boot deletes
--
-- Every boot rolls the root filesystem back to empty (see
-- modules/impermanence.nix), so all of it is lost but what's mounted from
-- elsewhere: the persisted directories and files, and other filesystems. Run
-- with sudo to see into directories only root can read.

import Control.Exception (IOException, try)
import Data.Text qualified as T
import Options.Applicative
import Relude
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.Posix.Files (deviceID, fileSize, getSymbolicLinkStatus, isDirectory, isSymbolicLink, readSymbolicLink)
import System.Posix.Types (DeviceID)

newtype Command = Lost Bool

-- | What's at a path, as far as the next boot is concerned.
data Node
  = File Integer
  | Link FilePath
  | Dir [(FilePath, Node)]
  | -- | On another filesystem, so the boot keeps it.
    Elsewhere

main :: IO ()
main =
  execParser parserInfo >>= \case
    Lost showAll -> do
      root <- deviceID <$> getSymbolicLinkStatus "/"
      tree <- scan root "/"
      mapM_ putTextLn . report . lost "/" $ if showAll then tree else withoutGenerated tree

-- | The tree at a path, down to where other filesystems start.
scan :: DeviceID -> FilePath -> IO Node
scan root path = do
  status <- getSymbolicLinkStatus path
  if
    | deviceID status /= root -> pure Elsewhere
    | isSymbolicLink status -> Link <$> readSymbolicLink path
    | isDirectory status -> Dir <$> (traverse (\name -> (name,) <$> scan root (path </> name)) . sort =<< entries)
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
  Elsewhere -> []
  Dir entries | any (keepsSomething . snd) entries -> concatMap (\(name, node) -> lost (path </> name) node) entries
  node -> [(path, sum sizes) | let sizes = leaves node, not (null sizes)]
  where
    keepsSomething = \case
      Elsewhere -> True
      Dir entries -> any (keepsSomething . snd) entries
      _ -> False
    leaves = \case
      File size -> [size]
      Link target -> [genericLength target]
      Dir entries -> concatMap (leaves . snd) entries
      Elsewhere -> []

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
    (helper <*> hsubparser lostCommand)
    (progDesc "Look at what impermanence deletes")
  where
    lostCommand =
      command "lost"
        . info (Lost <$> switch (long "all" <> help "Also show links into the Nix store, which NixOS makes on every boot"))
        $ progDesc "Show what the next boot deletes: everything on / that isn't persisted"

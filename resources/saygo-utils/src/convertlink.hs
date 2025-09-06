import Distribution.Simple.Utils qualified as U
import Distribution.Verbosity (normal)
import Options.Applicative
import System.Directory (pathIsSymbolicLink)
import System.Directory qualified as D
import System.FilePath
import System.Posix.Files
import Text.Printf (printf)
import Universum

data Options = Options {path :: FilePath, write :: Bool, recurse :: Bool}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (help "Symlink that will get converted" <> metavar "SYMLINK" <> action "file")
    <*> switch (long "write" <> short 'w' <> help "Give owner write permissions to the resulting file")
    <*> switch (long "recursive" <> short 'r' <> help "Convert symlinks in a directory recursively")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) (progDesc "Convert a symlink to the file its pointing to")

main :: IO ()
main = convertlink =<< execParser parserInfo

convertlink :: Options -> IO ()
convertlink opts = do
  filestatus <- getFileStatus opts.path
  isSymlink <- pathIsSymbolicLink opts.path
  let pointsToDir = isDirectory filestatus

  -- pointsToDir becomes isDir when isSymlink is false
  -- due to how getFileStatus works
  case (isSymlink, pointsToDir, opts.recurse) of
    (True, _, False) -> convertLinkAndWrite opts.path opts.write
    (True, False, _) -> convertLinkAndWrite opts.path opts.write
    (False, False, _) -> printf "convertlink: %s is not a symlink or a directory\n" opts.path
    (False, True, False) -> printf "convertlink: -r not specified; omitting directory %s\n" opts.path
    (_, True, True) -> recursiveConvert [opts.path] opts.write >> pass

convertLinkAndWrite :: FilePath -> Bool -> IO ()
convertLinkAndWrite filepath write = do
  convertSymlink filepath
  printf "Converted symlink %s\n" filepath
  when write $ do
    currentMode <- fileMode <$> getFileStatus filepath
    currentMode `unionFileModes` ownerWriteMode & setFileMode filepath
  where
    convertSymlink :: FilePath -> IO ()
    convertSymlink symlink = do
      fileFromSymlink <- readSymbolicLink symlink
      filestatus <- getFileStatus symlink
      let pointsToDir = isDirectory filestatus
      removeLink symlink
      if pointsToDir
        then
          U.copyDirectoryRecursive normal fileFromSymlink symlink
        else
          D.copyFile fileFromSymlink symlink

recursiveConvert :: [FilePath] -> Bool -> IO [FilePath]
recursiveConvert fileList giveWrite = do
  symlinkList <- concatMapM findSymlinks fileList
  if null symlinkList
    then
      pure fileList
    else do
      mapM_ (`convertLinkAndWrite` giveWrite) symlinkList
      recursiveConvert symlinkList giveWrite

findSymlinks :: FilePath -> IO [FilePath]
findSymlinks dir = do
  result <- try $ D.listDirectory dir
  case result of
    Left (_ :: SomeException) -> pure []
    Right entries -> do
      let fullPaths = map (dir </>) entries
      print fullPaths
      filterM check fullPaths
  where
    check :: FilePath -> IO Bool
    check path = do
      pathIsSymbolicLink path

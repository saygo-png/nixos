{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (handleJust, try)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Options.Applicative
import Relude
import System.Directory.OsPath qualified as D
import System.IO.Error (isDoesNotExistError)
import System.OsPath (OsPath, encodeFS, takeDirectory, (</>))
import System.OsString.Internal.Types (OsString (OsString, getOsString), PosixString (getPosixString))
import System.Posix.Files (fileMode, isDirectory, ownerWriteMode, unionFileModes)
import System.Posix.Files.PosixString (getFileStatus, readSymbolicLink, removeLink, setFileMode)

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
main = do
  opts <- execParser parserInfo
  path <- encodeFS opts.path
  convertlink path opts.write opts.recurse

convertlink :: OsPath -> Bool -> Bool -> IO ()
convertlink path write recurse = do
  filestatus <- getFileStatus (getOsString path)
  isSymlink <- D.pathIsSymbolicLink path
  let pointsToDir = isDirectory filestatus

  -- pointsToDir becomes isDir when isSymlink is false
  -- due to how getFileStatus works
  case (isSymlink, pointsToDir, recurse) of
    (True, _, False) -> convertLinkAndWrite path write
    (True, False, _) -> convertLinkAndWrite path write
    (False, False, _) -> putBS $ "convertlink: " <> toBytes path <> " is not a symlink or a directory\n"
    (False, True, False) -> putBS $ "convertlink: -r not specified; omitting directory " <> toBytes path <> "\n"
    (True, True, True) -> convertLinkAndWrite path write >> recursiveConvert path write
    (False, True, True) -> recursiveConvert path write

convertLinkAndWrite :: OsPath -> Bool -> IO ()
convertLinkAndWrite symlink write =
  handleJust (guard . isDoesNotExistError) pure $ do
    -- A relative target is relative to the symlink's directory.
    fileFromSymlink <- (takeDirectory symlink </>) . OsString <$> readSymbolicLink (getOsString symlink)
    fileStatus <- getFileStatus (getOsString symlink)
    let pointsToDir = isDirectory fileStatus
    removeLink (getOsString symlink)
    if pointsToDir
      then copyDirectory fileFromSymlink symlink
      else D.copyFile fileFromSymlink symlink
    putBS $ "Converted symlink " <> toBytes symlink <> "\n"
    when write (fileMode fileStatus `unionFileModes` ownerWriteMode & setFileMode (getOsString symlink))

-- | Copies a directory, following symlinks in it.
copyDirectory :: OsPath -> OsPath -> IO ()
copyDirectory from to = do
  D.createDirectory to
  entries <- D.listDirectory from
  for_ entries $ \entry -> do
    isDir <- D.doesDirectoryExist (from </> entry)
    bool D.copyFile copyDirectory isDir (from </> entry) (to </> entry)

recursiveConvert :: OsPath -> Bool -> IO ()
recursiveConvert root giveWrite = go $ V.singleton root
  where
    go :: Vector OsPath -> IO ()
    go fileList =
      if null fileList
        then pass
        else do
          (symlinks, directories) <- bimap join join . V.unzip <$> V.mapM findSymlinks fileList
          mapM_ (`convertLinkAndWrite` giveWrite) symlinks
          go (symlinks <> directories)

findSymlinks :: OsPath -> IO (Vector OsPath, Vector OsPath)
findSymlinks dir = do
  putBS $ "searching in: " <> toBytes dir <> "\n"
  result <- try $ V.fromList <$> D.listDirectory dir
  case result of
    Left (_ :: SomeException) -> pure mempty
    Right entries -> do
      let fullPaths = V.map (dir </>) entries
      (symlinks, otherFiles) <- partitionM D.pathIsSymbolicLink fullPaths
      directories <- V.filterM (fmap isDirectory . getFileStatus . getOsString) otherFiles
      pure (symlinks, directories)

partitionM :: (Monad m) => (a -> m Bool) -> Vector a -> m (Vector a, Vector a)
partitionM f = fmap (V.partitionWith id) . V.mapM tag
  where
    tag x = bool (Right x) (Left x) <$> f x

-- | A path's bytes, to print.
toBytes :: OsPath -> ByteString
toBytes = fromShort . getPosixString . getOsString

{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (handleJust, try)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Distribution.Simple.Utils qualified as U
import Distribution.Verbosity (normal)
import Options.Applicative
import Relude
import System.Directory qualified as D
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
import Text.Printf (printf)

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
  isSymlink <- D.pathIsSymbolicLink opts.path
  let pointsToDir = isDirectory filestatus

  -- pointsToDir becomes isDir when isSymlink is false
  -- due to how getFileStatus works
  case (isSymlink, pointsToDir, opts.recurse) of
    (True, _, False) -> convertLinkAndWrite opts.path opts.write
    (True, False, _) -> convertLinkAndWrite opts.path opts.write
    (False, False, _) -> printf "convertlink: %s is not a symlink or a directory\n" opts.path
    (False, True, False) -> printf "convertlink: -r not specified; omitting directory %s\n" opts.path
    (True, True, True) -> convertLinkAndWrite opts.path opts.write >> recursiveConvert opts.path opts.write >> pass
    (False, True, True) -> recursiveConvert opts.path opts.write >> pass

convertLinkAndWrite :: FilePath -> Bool -> IO ()
convertLinkAndWrite symlink write =
  handleJust (guard . isDoesNotExistError) pure $ do
    fileFromSymlink <- readSymbolicLink symlink
    fileStatus <- getFileStatus symlink
    let pointsToDir = isDirectory fileStatus
    removeLink symlink
    if pointsToDir
      then U.copyDirectoryRecursive normal fileFromSymlink symlink
      else D.copyFile fileFromSymlink symlink
    printf "Converted symlink %s\n" symlink
    when write (fileMode fileStatus `unionFileModes` ownerWriteMode & setFileMode symlink)

recursiveConvert :: FilePath -> Bool -> IO ()
recursiveConvert root giveWrite = go $ V.singleton root
  where
    go :: Vector FilePath -> IO ()
    go fileList =
      if null fileList
        then pass
        else do
          (symlinks, directories) <- bimap join join . V.unzip <$> V.mapM findSymlinks fileList
          mapM_ (`convertLinkAndWrite` giveWrite) symlinks
          go (symlinks <> directories)

findSymlinks :: FilePath -> IO (Vector FilePath, Vector FilePath)
findSymlinks dir = do
  putTextLn $ "searching in: " <> fromString dir
  result <- try $ V.fromList <$> D.listDirectory dir
  case result of
    Left (_ :: SomeException) -> pure mempty
    Right entries -> do
      let fullPaths = V.map (dir </>) entries
      (symlinks, otherFiles) <- partitionM D.pathIsSymbolicLink fullPaths
      directories <- V.filterM (fmap isDirectory . getFileStatus) otherFiles
      pure (symlinks, directories)

partitionM :: (Monad m) => (a -> m Bool) -> Vector a -> m (Vector a, Vector a)
partitionM f = fmap (V.partitionWith id) . V.mapM tag
  where
    tag x = bool (Right x) (Left x) <$> f x

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BinaryLiterals, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DoAndIfThenElse, EmptyDataDecls, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures, LambdaCase, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures, PatternGuards, PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeSynonymInstances, ViewPatterns #-}
{- ORMOLU_ENABLE -}

import Flow
import Options.Applicative
import RIO
import System.Directory
import System.FilePath
import System.Posix.Files
import Text.Printf (printf)

data Options = Options {path :: FilePath, write :: Bool, recurse :: Bool}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument
      str
      ( help "Target for the greeting"
          <> metavar "SYMLINK"
          <> help "Symlink that will be converted"
      )
    <*> switch
      ( long "write"
          <> short 'w'
          <> help "Give owner write permissions to the resulting file"
      )
    <*> switch
      ( long "recursive"
          <> short 'r'
          <> help "Convert symlinks in a directory recursively"
      )

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) (progDesc "Convert a symlink to the file its pointing to")

main :: IO ()
main = convertlinksAndWrite =<< execParser parserInfo

convertlinksAndWrite :: Options -> IO ()
convertlinksAndWrite opts = do
  let filepath = path opts
      giveWrite = write opts
      recursive = recurse opts
  filestatus <- getFileStatus filepath

  case isDirectory filestatus of
    True
      | not recursive -> printf "convertlink: -r not specified; omitting directory %s\n" filepath
      | otherwise -> do
          symlinks <- findSymlinksRecursively filepath
          if symlinks /= []
            then mapM_ (`convertLinkAndWrite` giveWrite) symlinks
            else printf "convertlink: directory doesn't contain symlinks %s\n" filepath
    False -> do
      isSymlink <- pathIsSymbolicLink filepath
      if isSymlink
        then convertLinkAndWrite filepath giveWrite
        else printf "convertlink: cannot convert %s: Not a symlink\n" filepath

convertLinkAndWrite :: FilePath -> Bool -> IO ()
convertLinkAndWrite filepath write = do
  convertSymlinkPreservePerms filepath
  printf "Converted symlink %s\n" filepath
  when write <| do
    addOwnerWritePermission filepath

addOwnerWritePermission :: FilePath -> IO ()
addOwnerWritePermission filepath = do
  currentMode <- fileMode <$> getFileStatus filepath
  currentMode `unionFileModes` ownerWriteMode |> setFileMode filepath

convertSymlinkPreservePerms :: FilePath -> IO ()
convertSymlinkPreservePerms symlink = do
  fileFromSymlink <- readSymbolicLink symlink
  targetMode <- fileMode <$> getFileStatus fileFromSymlink

  removeLink symlink
  copyFile fileFromSymlink symlink
  setFileMode symlink targetMode

findSymlinksRecursively :: FilePath -> IO [FilePath]
findSymlinksRecursively dir = do
  result <- try <| listDirectory dir
  case result of
    Left (_ :: SomeException) -> return []
    Right entries -> do
      let fullPaths = map (dir </>) entries
      symlinkResults <- mapM checkAndRecurse fullPaths
      return <| concat symlinkResults
  where
    checkAndRecurse :: FilePath -> IO [FilePath]
    checkAndRecurse path = do
      isSymlink <- pathIsSymbolicLink path
      if isSymlink
        then return [path]
        else do
          isDir <- doesDirectoryExist path
          if isDir
            then findSymlinksRecursively path
            else return []

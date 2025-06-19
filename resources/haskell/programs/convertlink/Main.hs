{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BinaryLiterals, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DoAndIfThenElse, EmptyDataDecls, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures, LambdaCase, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures, PatternGuards, PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeSynonymInstances, ViewPatterns #-}
{- ORMOLU_ENABLE -}

import Options.Applicative
import RIO
import System.Directory
import System.Posix.Files
import Text.Printf (printf)

data Options = Options {path :: FilePath, write :: Bool}

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

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) (progDesc "Convert a symlink to the file its pointing to")

main :: IO ()
main = convertlinksAndWrite =<< execParser parserInfo

convertlinksAndWrite :: Options -> IO ()
convertlinksAndWrite opts = do
  let filepath = path opts
  let giveWrite = write opts
  filestatus <- getFileStatus filepath

  if isDirectory filestatus
    then printf "Directories are not supported\n"
    else
      pathIsSymbolicLink filepath >>= \case
        True -> convertLinkAndWrite filepath giveWrite
        False -> printf "convertlink: cannot convert %s: Not a symlink\n" filepath

convertLinkAndWrite :: FilePath -> Bool -> IO ()
convertLinkAndWrite filepath write = do
  convertSymlinkPreservePerms filepath
  printf "Converted symlink %s\n" filepath
  when write $ do
    addOwnerWritePermission filepath

addOwnerWritePermission :: FilePath -> IO ()
addOwnerWritePermission filepath = do
  currentMode <- fileMode <$> getFileStatus filepath
  setFileMode filepath (currentMode `unionFileModes` ownerWriteMode)

convertSymlinkPreservePerms :: FilePath -> IO ()
convertSymlinkPreservePerms symlink = do
  fileFromSymlink <- readSymbolicLink symlink
  targetMode <- fileMode <$> getFileStatus fileFromSymlink

  removeLink symlink
  copyFile fileFromSymlink symlink
  setFileMode symlink targetMode

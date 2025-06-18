{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BinaryLiterals, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DoAndIfThenElse, EmptyDataDecls, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures, LambdaCase, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures, PatternGuards, PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeSynonymInstances, ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{- ORMOLU_ENABLE -}
import Options.Applicative
import RIO
import RIO.ByteString.Lazy (putStrLn)
import RIO.Directory (XdgDirectory (XdgData))
import System.Environment (getEnv)
import Text.Printf (printf)

outputDir :: XdgDirectory
outputDir = XdgData

-- Parsing {{{
data Command = CmdList | CmdTake

newtype Options = Options
  { optCommand :: Command
  }

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions) (progDesc "Reminds you to take a drug")

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "take" (info (pure CmdTake) (progDesc "Take drug"))
        <> command "list" (info (pure CmdList) (progDesc "List previously taken drug"))
    )

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

main :: IO ()
main = do
  options <- execParser parserInfo
  case optCommand options of
    CmdTake -> putStrLn "taking"
    CmdList -> putStrLn "listing"

-- }}}

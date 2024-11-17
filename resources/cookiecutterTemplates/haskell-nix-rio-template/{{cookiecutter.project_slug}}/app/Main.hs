{-# LANGUAGE AutoDeriveTypeable, BangPatterns, BinaryLiterals, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DoAndIfThenElse, EmptyDataDecls, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, KindSignatures, LambdaCase, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures, PatternGuards, PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeFamilies, TypeSynonymInstances, ViewPatterns #-}
{-# LANGUAGE MonadFailDesugaring #-}

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Vector (toList)
import GHC.Generics
import System.Process
import System.Process.ByteString.Lazy qualified as BS

data Window = Window
  { pinned :: Bool,
    focusHistoryID :: Int,
    tags :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Window

instance ToJSON Window

filterJson :: Value -> Maybe Window
filterJson = parseMaybe $ withArray "windows" $ \arr ->
  case filter (\v -> parseMaybe (withObject "Window" (.: "focusHistoryID")) v == Just (0 :: Int)) (toList arr) of
    [x] -> parseJSON x
    _ -> mzero

main :: IO ()
main = do
  let stdin' = ""
  (_, stdout', _) <- BS.readProcessWithExitCode "hyprctl" ["clients", "-j"] stdin'
  let jsonData = stdout'
  let decoded = decode jsonData :: Maybe Value
  case decoded of
    Just val -> case filterJson val of
      Just window ->
        if pinned window
          then do
            putStrLn "pin full pin"
            _ <- callProcess "hyprctl" ["dispatch", "pin"]
            _ <- callProcess "hyprctl" ["dispatch", "fullscreen"]
            pure ()
          else do
            if "69PINNED69" `elem` tags window
              then do
                _ <- callProcess "hyprctl" ["dispatch", "fullscreen"]
                _ <- callProcess "hyprctl" ["dispatch", "pin"]
                pure ()
              else do
                _ <- callProcess "hyprctl" ["dispatch", "fullscreen"]
                pure ()
      Nothing -> putStrLn "No matching window found"
    Nothing -> putStrLn "Failed to parse JSON"

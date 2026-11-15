{-# LANGUAGE TemplateHaskell #-}

-- | Bakes in the tools a program runs, like writeShellApplication's
-- runtimeInputs. A program can only run tools from the packages in its
-- x-runtime-inputs in saygo-utils.cabal, whose paths default.nix gives in
-- RUNTIME_INPUTS, one "<package> <path>" line each.
module Bin (bin) where

import Data.List (groupBy, lookup)
import Data.Text qualified as T
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Relude
import System.Directory (doesFileExist)
import System.FilePath (normalise, (</>))

-- | @$(bin "fd") :: String@ is the path of fd when the program was built.
bin :: String -> TH.Q TH.Exp
bin name = do
  file <- normalise . TH.loc_filename <$> TH.location
  packages <- runtimeInputs file
  paths <- storePaths
  found <- TH.runIO $ filterM doesFileExist [path </> "bin" </> name | Just path <- (`lookup` paths) <$> packages]
  case found of
    path : _ -> TH.lift path
    [] -> fail $ name <> " isn't in " <> file <> "'s x-runtime-inputs: " <> toString (T.intercalate ", " packages)

-- | The x-runtime-inputs of the executable whose main-is is this file.
runtimeInputs :: FilePath -> TH.Q [Text]
runtimeInputs file = do
  TH.addDependentFile cabalFile
  -- A stanza starts at a line that isn't indented.
  stanzas <- groupBy (\_ line -> T.null line || " " `T.isPrefixOf` line) . lines . decodeUtf8 <$> TH.runIO (readFileBS cabalFile)
  case find ((== Just file) . fmap (normalise . toString) . field "main-is") stanzas of
    Nothing -> fail $ "No executable in " <> cabalFile <> " has main-is: " <> file
    Just stanza -> pure $ maybe [] (map T.strip . T.splitOn ",") (field "x-runtime-inputs" stanza)
  where
    field name stanza = viaNonEmpty head [T.strip value | Just value <- T.stripPrefix (name <> ":") . T.strip <$> stanza]

cabalFile :: FilePath
cabalFile = "saygo-utils.cabal"

storePaths :: TH.Q [(Text, FilePath)]
storePaths = do
  inputs <- maybe (fail "RUNTIME_INPUTS is not set") (pure . toText) =<< TH.runIO (lookupEnv "RUNTIME_INPUTS")
  pure [(package, toString (T.drop 1 path)) | (package, path) <- T.breakOn " " <$> lines inputs]

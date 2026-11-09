{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- comma-bg                 pick a lazy package with fzf
-- comma-bg <name> [args]   launch a lazy package, or else a program from comma
--
-- Lazy packages come from the JSON file in COMMA_BG_LAZY_PACKAGES, which
-- saygo-utils' Nix package sets.

import Data.Map.Strict qualified as Map
import Relude
import Shh
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (UseHandle), createProcess, proc)
import Text.JSON.Yocto qualified as JSON

default (ByteString)

-- A package from custom.lazyPackages, kept as a derivation until launched.
data Lazy = Lazy
  { name :: Text
  , installable :: Text
  , program :: Text
  }

-- A command printing a store path, and where the program is relative to that path.
data Locate = Locate (Proc ()) (FilePath -> FilePath)

main :: IO ()
main = do
  lazies <- maybe (pure []) (fmap (parseLazies . decodeUtf8) . readFileBS) =<< lookupEnv "COMMA_BG_LAZY_PACKAGES"
  (name, args) <-
    getArgs >>= \case
      [] -> (,[]) <$> output (pick lazies)
      name : args -> pure (toText name, args)
  let Locate command toProgram = locate lazies name
  path <- toProgram . toString <$> output command
  withFile "/dev/null" ReadWriteMode $ void . createProcess . detached path args
  where
    -- fzf, Nix and comma report their own errors.
    output :: Proc () -> IO Text
    output p = tryFailure (p |> captureTrim) >>= either (const exitFailure) (pure . decodeUtf8)

parseLazies :: String -> [Lazy]
parseLazies json = fromMaybe [] $ do
  JSON.Array packages <- pure (JSON.decode json)
  pure (mapMaybe lazy packages)
  where
    lazy value = do
      JSON.Object fields <- pure value

      let getField key = do
            JSON.String s <- Map.lookup key fields
            pure (toText s)

      name <- getField "name"
      installable <- getField "installable"
      program <- getField "program"
      pure Lazy{name, installable, program}

pick :: [Lazy] -> Proc ()
pick lazies = writeOutput (toString . unlines $ map (.name) lazies) |> exe "fzf"

locate :: [Lazy] -> Text -> Locate
locate lazies name = case find (\lazy -> lazy.name == name) lazies of
  Just lazy ->
    Locate
      (exe "nix" "build" "--no-link" "--print-out-paths" (toString lazy.installable))
      (\out -> out </> "bin" </> toString lazy.program)
  Nothing -> Locate (exe "," "--print-path" (toString name)) id

detached :: FilePath -> [String] -> Handle -> CreateProcess
detached path args nullHandle =
  (proc path args)
    { std_in = UseHandle nullHandle
    , std_out = UseHandle nullHandle
    , std_err = UseHandle nullHandle
    , new_session = True
    , close_fds = True
    }

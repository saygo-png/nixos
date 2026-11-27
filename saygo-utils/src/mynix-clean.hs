{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Bin
import Options.Applicative
import Relude
import Shh

default (ByteString)

nh :: Cmd
nh = exe $(bin "nh")

-- The setuid one from PATH.
sudo :: Cmd
sudo = exe "sudo"

main :: IO ()
main = do
  execParser parserInfo
  -- Optimise the store even if cleaning fails, nh reports its own errors.
  ignoreFailure $ nh "clean" "all" "--keep" "1"
  sudo "nix" "store" "optimise"

parserInfo :: ParserInfo ()
parserInfo = info (helper <*> pass) (progDesc "Remove old generations and optimise the Nix store")

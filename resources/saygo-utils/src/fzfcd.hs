{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Bin
import Options.Applicative
import Relude
import Shh
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory)

default (ByteString)

fd, fzf :: Cmd
fd = exe $(bin "fd")
fzf = exe $(bin "fzf")

eza :: String
eza = $(bin "eza")

main :: IO ()
main = do
  execParser parserInfo
  -- fzf reports its own errors, and fails when nothing is picked.
  path <- either (const exitFailure) (pure . decodeUtf8) =<< tryFailure (pick |> captureTrim)
  isDirectory <- doesDirectoryExist path
  isFile <- doesFileExist path
  if
    | isDirectory -> putStrLn path
    | isFile -> putStrLn (takeDirectory path)
    | otherwise -> exitFailure

pick :: Proc ()
pick =
  fd "--hidden" "--type" "directory" "--type" "file" "--maxdepth" "15" "."
    |> fzf "--preview" (eza <> " --tree --color=always {}")

parserInfo :: ParserInfo ()
parserInfo = info (helper <*> pass) (progDesc "Pick a file or directory with fzf and print the directory to cd into")

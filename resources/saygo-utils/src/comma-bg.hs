{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Relude
import Shh
import System.Directory (doesFileExist, executable, getPermissions)
import System.Process (CreateProcess (..), StdStream (UseHandle), createProcess, proc)

default (ByteString)

main :: IO ()
main =
  getArgs >>= \case
    [] -> die "Usage: comma-bg <command> [args...]"
    command : args -> do
      path <- either (const "") decodeUtf8 <$> tryFailure (comma "--print-path" command |> captureTrim)
      unlessM (isExecutable path) $ die ("comma-bg: could not find " <> command)
      launchDetached path args
      putStrLn ("Launched " <> path)

comma :: Cmd
comma = exe ","

isExecutable :: FilePath -> IO Bool
isExecutable path = doesFileExist path &&^ (executable <$> getPermissions path)

launchDetached :: FilePath -> [String] -> IO ()
launchDetached path args = withFile "/dev/null" ReadWriteMode $ \nullHandle ->
  void
    $ createProcess
      (proc path args)
        { std_in = UseHandle nullHandle,
          std_out = UseHandle nullHandle,
          std_err = UseHandle nullHandle,
          new_session = True,
          close_fds = True
        }

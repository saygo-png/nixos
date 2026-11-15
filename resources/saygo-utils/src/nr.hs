-- nr <package> [args]    run a program from nixpkgs
-- snr <package> [args]   the same, as root

import Relude
import System.Environment (getProgName)
import System.Posix.Process (executeFile)

main :: IO ()
main =
  getArgs >>= \case
    [] -> die "Usage: nr <package> [args]"
    package : args -> do
      let nixRun = ["run", "nixpkgs#" <> package, "--"] <> args
      getProgName >>= \case
        "snr" -> executeFile "sudo" True ("nix" : nixRun) Nothing
        _ -> executeFile "nix" True nixRun Nothing

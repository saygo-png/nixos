import Options.Applicative
import Relude
import System.Environment (getProgName)
import System.Posix.Process (executeFile)

data Options = Options {package :: String, args :: [String]}

-- Called as snr, it runs the program as root.
main :: IO ()
main = do
  asRoot <- (== "snr") <$> getProgName
  options <- execParser (parserInfo asRoot)
  let nixRun = ["run", "nixpkgs#" <> options.package, "--"] <> options.args
  if asRoot
    then executeFile "sudo" True ("nix" : nixRun) Nothing
    else executeFile "nix" True nixRun Nothing

parserInfo :: Bool -> ParserInfo Options
parserInfo asRoot =
  -- Options after the package, --help included, are the program's.
  info (helper <*> optionsParser) (noIntersperse <> progDesc ("Run a program from nixpkgs" <> bool "" " as root" asRoot))

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument (metavar "PACKAGE" <> help "Package in nixpkgs")
    <*> many (strArgument (metavar "ARGS..." <> help "Arguments for the program"))

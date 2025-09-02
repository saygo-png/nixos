import Flow
import Options.Applicative
import RIO
import System.Directory
import System.Posix.Files
import Text.Printf (printf)

data Options = Options {path :: FilePath, write :: Bool, recurse :: Bool}

averageWakeupTime :: Int
averageWakeupTime = 7

averageBedTime :: Int
averageBedTime = 0

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
    <*> switch
      ( long "recursive"
          <> short 'r'
          <> help "Convert symlinks in a directory recursively"
      )

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) (progDesc "Convert a symlink to the file its pointing to")

main :: IO ()
main = convertlinksAndWrite =<< execParser parserInfo

convertlinksAndWrite :: Options -> IO ()
convertlinksAndWrite opts = pure ()

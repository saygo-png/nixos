import Options.Applicative
import Relude

main :: IO ()
main = do
  execParser parserInfo
  putText
    $ unlines
      [ "GPU"
      , "nr amdgpu_top --gui"
      , "General Tree"
      , "snr lshw -json | nvim -c 'set filetype=json'"
      ]

parserInfo :: ParserInfo ()
parserInfo = info (helper <*> pass) (progDesc "List commands that show hardware information")

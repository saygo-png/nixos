{-# LANGUAGE LambdaCase #-}

import Control.Monad (when)
import Options.Applicative
import System.Directory
import System.Posix.Files
import System.Process (callProcess)
import Text.Printf (printf)

data Sample = Sample
  { path :: FilePath,
    write :: Bool
  }

sample :: Parser Sample
sample =
  Sample
    <$> argument
      str
      ( help "Target for the greeting"
          <> metavar "SYMLINK"
          <> help "Symlink that will be converted"
      )
    <*> switch
      ( long "write"
          <> short 'w'
          <> help "Give write permissions to the resulting file"
      )

main :: IO ()
main = convertlinkAndWrite =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Convert a symlink to the file its pointing to"
        )

convertlinkAndWrite :: Sample -> IO ()
convertlinkAndWrite (Sample filepath giveWrite) = do
  pathIsSymbolicLink filepath >>= \case
    True -> do
      fileFromSymlink <- readSymbolicLink filepath
      _ <- callProcess "cp" ["--remove-destination", fileFromSymlink, filepath]
      printf "Converted symlink %s\n" filepath
      when giveWrite $ do
        _ <- callProcess "chmod" ["+w", filepath]
        pure ()
    False -> printf "convertlink: cannot convert %s: Not a symlink\n" filepath

-- Commands that show hardware information.

import Relude

main :: IO ()
main =
  putText
    $ unlines
      [ "GPU"
      , "nr amdgpu_top --gui"
      , "General Tree"
      , "snr lshw -json | nvim -c 'set filetype=json'"
      ]

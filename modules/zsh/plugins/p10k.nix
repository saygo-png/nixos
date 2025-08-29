{
  lib,
  zsh,
  gitstatus,
  replaceVars,
  stdenv,
  writeScriptBin,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  pname = "compiled-powerlevel10k";
  version = "1.20.0";

  src = fetchFromGitHub {
    owner = "romkatv";
    repo = "powerlevel10k";
    rev = "36f3045d69d1ba402db09d09eb12b42eebe0fa3b";
    hash = "sha256-BRJyGn+gTGUWifpJ1ziBKVHACcWw+R5N/HdUi8HzSvY=";
  };

  nativeBuildInputs = [zsh];
  strictDeps = true;

  # https://github.com/NixOS/nixpkgs/blob/88983d4b665fb491861005137ce2b11a9f89f203/pkgs/by-name/zs/zsh-powerlevel10k/package.nix
  patches = let
    patch = builtins.toFile "gitstatusd.patch" ''
      diff --git a/gitstatus/gitstatus.plugin.zsh b/gitstatus/gitstatus.plugin.zsh
      index b469072..eb1e3be 100644
      --- a/gitstatus/gitstatus.plugin.zsh
      +++ b/gitstatus/gitstatus.plugin.zsh
      @@ -44,6 +44,8 @@

       [[ -o 'interactive' ]] || 'return'

      +GITSTATUS_DAEMON=@gitstatusdPath@
      +
       # Temporarily change options.
       'builtin' 'local' '-a' '_gitstatus_opts'
       [[ ! -o 'aliases'         ]] || _gitstatus_opts+=('aliases')
    '';
  in [
    (replaceVars patch {
      gitstatusdPath = "${gitstatus}/bin/gitstatusd";
    })
  ];

  installPhase = let
    zcomp = writeScriptBin "zcomp" ''
      #!${lib.getExe zsh}
      function zcompile-many() {
        local f
        for f; do zcompile -R -- "$f".zwc "$f"; done
      }
      zcompile-many {powerlevel10k.zsh-theme,internal/**/*.zsh,config/**/*.zsh,gitstatus/**/*.zsh}
    '';
  in ''
    ${zcomp}/bin/zcomp

    plugindir="$out/powerlevel10k"
    mkdir -p "$plugindir"
    mv ./* $plugindir
  '';

  meta = with lib; {
    homepage = "https://github.com/romkatv/powerlevel10k";
    description = "Fast ZSH prompt";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}

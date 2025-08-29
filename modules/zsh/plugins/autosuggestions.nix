{
  lib,
  zsh,
  stdenvNoCC,
  writeScriptBin,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "compiled-zsh-autosuggestions";
  version = "0.7.1";

  src = fetchFromGitHub {
    owner = "zsh-users";
    repo = "zsh-autosuggestions";
    rev = "85919cd1ffa7d2d5412f6d3fe437ebdbeeec4fc5";
    hash = "sha256-KmkXgK1J6iAyb1FtF/gOa0adUnh1pgFsgQOUnNngBaE=";
  };

  nativeBuildInputs = [zsh];
  strictDeps = true;

  installPhase = let
    zcomp = writeScriptBin "zcomp" ''
      #!${lib.getExe zsh}
      function zcompile-many() {
        local f
        for f; do zcompile -R -- "$f".zwc "$f"; done
      }
      zcompile-many {zsh-autosuggestions.zsh,src/**/*.zsh}
    '';
  in ''
    ${zcomp}/bin/zcomp

    plugindir="$out/zsh-autosuggestions"
    mkdir -p "$plugindir"
    mv ./* $plugindir
  '';

  meta = with lib; {
    homepage = "https://github.com/zsh-users/zsh-autosuggestions";
    description = "Fish-like autosuggestions for zsh";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}

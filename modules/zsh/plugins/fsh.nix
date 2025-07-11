{
  lib,
  zsh,
  stdenvNoCC,
  writeScriptBin,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "compiled-fast-syntax-highlighting";
  version = "1.55";

  src = fetchFromGitHub {
    owner = "zdharma-continuum";
    repo = "fast-syntax-highlighting";
    rev = "dcee72bb99b422bb8e4510f5087af9c1721392e4";
    hash = "sha256-9itq8Pq/+1Yflo7b31eHEVOFrbO9b1CAMr988xYyNLI=";
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
      zcompile-many {fast*,.fast*,**/*.ch,**/*.zsh}
    '';
  in ''
    mv "→chroma" "tmp"
    ${zcomp}/bin/zcomp
    mv "tmp" "→chroma"

    plugindir="$out/fast-syntax-highlighting"
    mkdir -p "$plugindir"
    mv ./* $plugindir
  '';

  meta = with lib; {
    homepage = "https://github.com/zdharma-continuum/fast-syntax-highlighting";
    description = "Zcompiled fast syntax highlighting for ZSH";
    license = licenses.bsd3;
    platforms = platforms.unix;
  };
}

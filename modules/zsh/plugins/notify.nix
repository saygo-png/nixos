{
  lib,
  zsh,
  stdenvNoCC,
  writeScriptBin,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "compiled-zsh-auto-notify";
  version = "0.11.1";

  src = fetchFromGitHub {
    owner = "MichaelAquilina";
    repo = "zsh-auto-notify";
    rev = "b51c934d88868e56c1d55d0a2a36d559f21cb2ee";
    hash = "sha256-s3TBAsXOpmiXMAQkbaS5de0t0hNC1EzUUb0ZG+p9keE=";
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
      zcompile-many auto-notify.plugin.zsh
    '';
  in ''
    ${zcomp}/bin/zcomp

    plugindir="$out/zsh-auto-notify"
    mkdir -p "$plugindir"
    mv ./* $plugindir
  '';

  meta = with lib; {
    homepage = "https://github.com/marzocchi/zsh-notify";
    description = "ZSH plugin that automatically sends out a notification when a long running task has completed.";
    license = licenses.gpl3Plus;
    platforms = platforms.unix;
  };
}

{
  lib,
  zsh,
  stdenvNoCC,
  writeScriptBin,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "compiled-zsh-system-clipboard";
  version = "0.8.0-git";

  src = fetchFromGitHub {
    owner = "kutsan";
    repo = "zsh-system-clipboard";
    rev = "8b4229000d31e801e6458a3dfa2d60720c110d11";
    hash = "sha256-phsIdvuqcwtAVE1dtQyXcMgNdRMNY03/mIzsvHWPS1Y=";
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
      zcompile-many zsh-system-clipboard.zsh
    '';
  in ''
    ${zcomp}/bin/zcomp

    plugindir="$out/zsh-system-clipboard"
    mkdir -p "$plugindir"
    mv ./zsh-system-clipboard* $plugindir
  '';

  meta = with lib; {
    homepage = "https://github.com/kutsan/zsh-system-clipboard";
    description = "System clipboard key bindings for Zsh Line Editor with vi mode. It is similar to what `set clipboard=unnamed` does for vim. ";
    license = licenses.gpl3Plus;
    platforms = platforms.unix;
  };
}

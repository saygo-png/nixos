{pkgs, ...}: {
  cachix.enable = false;
  packages = with pkgs; [
    xcb-util-cursor
    xorg.xcbutilwm
    xorg.xcbutilrenderutil
    xorg.xcbutilkeysyms
    xorg.xcbutilerrors
    xorg.libxcb
    xorg.xcbutilimage
    libxkbcommon

    xorg.libX11
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
  ];

  languages.haskell.enable = true;
}

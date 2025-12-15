{
  pkgs ?
    import (
      fetchTree {
        type = "github";
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "f4b140d5b253f5e2a1ff4e5506edbf8267724bde";
      }
    ) {},
}:
pkgs.mkShell {
  packages = let
    ghcPackages = pkgs.haskell.packages.ghc984;
  in [
    pkgs.glib
    pkgs.pcre2
    pkgs.libsysprof-capture
    pkgs.pkg-config
    pkgs.util-linux
    pkgs.gobject-introspection
    pkgs.libnotify
    pkgs.libselinux
    pkgs.libsepol
    pkgs.libdeflate
    pkgs.lerc
    pkgs.xz
    pkgs.zstd
    pkgs.libwebp
    pkgs.wl-screenrec

    (ghcPackages.ghcWithPackages (p: [
      # p.gi-gdkpixbuf
      # p.gi-glib
      # p.gi-gobject
      # p.gi-notify
      # p.haskell-gi
      # p.haskell-gi-base
      # p.haskell-gi-overloading
      # p.gi-girepository
    ]))

    ghcPackages.haskell-language-server
    ghcPackages.ghc
    ghcPackages.cabal-install
  ];
}

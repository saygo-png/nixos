{
  niceHaskell,
  pkgs,
  myLib,
  ...
}: let
  ghcPackages = pkgs.haskell.packages.ghc984;
in
  niceHaskell.mkPackage {
    flags = niceHaskell.mkFlags {doCheck = false;};
    packageRoot = ./.;
    cabalName = "drug2";
    compiler = "ghc984";
    developPackageArgs.overrides = _: _: {
      gi-notify =
        ghcPackages.callPackage (
          {
            mkDerivation,
            base,
            bytestring,
            Cabal,
            containers,
            gi-gdkpixbuf,
            gi-glib,
            gi-gio,
            gi-gobject,
            haskell-gi,
            haskell-gi-base,
            haskell-gi-overloading,
            libnotify,
            lib,
            text,
            transformers,
          }:
            mkDerivation {
              pname = "gi-notify";
              version = "0.7.28";
              sha256 = "1sph16xhvyyfp81b2njz99crzwqas8njn6h0ma7hbi068jmnj7nq";
              patches = [(myLib.relativeToRoot "resources/saygo-utils/gi-notify.patch")];
              setupHaskellDepends = [
                base
                Cabal
                gi-gdkpixbuf
                gi-glib
                gi-gio
                gi-gobject
                haskell-gi
              ];
              libraryHaskellDepends = [
                base
                bytestring
                containers
                gi-gdkpixbuf
                gi-gio
                gi-glib
                gi-gobject
                haskell-gi
                haskell-gi-base
                haskell-gi-overloading
                text
                transformers
              ];
              libraryPkgconfigDepends = [libnotify];
              homepage = "https://github.com/haskell-gi/haskell-gi";
              description = "Libnotify bindings";
              license = lib.licenses.lgpl21Only;
            }
        )
        {inherit (pkgs) libnotify;};
    };
  }

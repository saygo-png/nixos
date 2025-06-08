{
  lib,
  pkgs,
  config,
  ...
}: {
  # I don't like how comma uses ","

  nixpkgs.overlays = [
    (_final: prev: {
      comma = prev.comma.overrideAttrs (oldAttrs: {
        preFixupPhases = (oldAttrs.preFixupPhases or []) ++ ["myPrefixup"];
        myPrefixup = ''
          rm $out/bin/,
          ln -s $out/bin/comma $out/bin/com
        '';
      });
    })
  ];

  environment.systemPackages = [pkgs.comma];
}

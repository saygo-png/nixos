# Packages that comma-bg evaluates and builds only when launched.
{
  lib,
  pkgs,
  ...
}: {
  options.custom.lazyPackages = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [];
  };

  config.custom.lazyPackages = [
    pkgs.blender
    pkgs.audacity
    pkgs.pitivi
    pkgs.localsend
    pkgs.inkscape
  ];
}

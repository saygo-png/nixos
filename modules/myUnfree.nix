{
  config,
  lib,
  ...
}: {
  options.custom = {
    allowedUnfreePkgs = lib.mkOption {
      default = [];
      description = "List of unfree packages that are allowed to be installed";
      type = lib.types.listOf lib.types.package;
    };
  };

  config = {
    nixpkgs.config.allowUnfreePredicate = let
      allowedUnfreePkgsList = map lib.getName config.custom.allowedUnfreePkgs;
    in
      pkg:
        builtins.elem (lib.getName pkg) allowedUnfreePkgsList;
  };
}

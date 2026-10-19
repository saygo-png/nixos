# This module is here to allow me to define constants that are "global"
# and dont need to be imported in each file like specialArgs.
# Every constant can only be defined once.
{lib, ...}: {
  options.const = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.raw;
    default = {};
    description = "Constants, like extraSpecialArgs but as an option";
  };
}

# This module is here to allow me to define constants that are "global"
# and dont need to be imported in each file like specialArgs
{lib, ...}: let
  # Create dummy option for easy referencing in the config
  mkConstWithType = name: value: {
    ${name} = lib.mkOption {
      default = value;
      readOnly = true;
      type = lib.types.anything;
      description = "Constant option meant only for referencing and not setting";
    };
  };

  # Take attribute set of values, apply function (a -> b -> attrSet) to each value in the set
  setMap = f: attrSet:
    lib.foldlAttrs
    (acc: name: value: acc // (f name value))
    {}
    attrSet;

  mkConst = name: value: mkConstWithType name value;

  # Take attribute set of values, apply mkConst to each value in the set
  mkConstsFromSet = setMap mkConst;

  # Take attribute set of values, apply mkConstInsanity to each value in the set
  mkConstsFromSetInsanity = setMap mkConst;
in {
  options = {
    constLib = mkConstsFromSetInsanity {
      inherit mkConstsFromSetInsanity;
      inherit mkConst;
      inherit mkConstsFromSet;
    };
    const = {};
  };
}

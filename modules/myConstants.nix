# This module is here to allow me to define constants that are "global"
# and dont need to be imported in each file like specialArgs
{lib, ...}: let
  # Create dummy option for easy referencing in the config using the type
  mkConstWithType = _type: name: value: {
    ${name} = lib.mkOption {
      default = value;
      readOnly = true;
      type = _type;
      description = "Constant, meant only for referencing, not setting";
    };
  };

  # Take attribute set of values, apply function (a -> b -> attrSet) to each value in the set
  setMap = f: attrSet:
    lib.foldlAttrs
    (acc: name: value: acc // (f name value))
    {}
    attrSet;

  # Create dummy option for easy referencing in the config with the type of the value
  mkConst = name: value: mkConstWithType (lib.types.${builtins.typeOf value}) name value;

  # Take attribute set of values, apply mkConst to each value in the set
  mkConstsFromSet = setMap mkConst;

  # Create dummy option for easy referencing in the config with the type "anything'
  mkConstInsanity = mkConstWithType lib.types.anything;

  # Take attribute set of values, apply mkConstInsanity to each value in the set
  mkConstsFromSetInsanity = setMap mkConstInsanity;
in {
  options = {
    constLib = mkConstsFromSetInsanity {
      mkConst = mkConst;
      mkConstsFromSet = mkConstsFromSet;
    };
    const = mkConstsFromSet {
      accelSpeed = -0.9;
      accentColor = "7d8618";
    };
  };
}

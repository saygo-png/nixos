{lib, ...}: let
  # Create dummy option for easy referencing in the config
  mkConst = name: value: {
    ${name} = lib.mkOption {
      default = value;
      readOnly = true;
      type = lib.types.${builtins.typeOf value};
      description = "Constant, meant only for referencing, not setting";
    };
  };

  # Take attribute set of values, apply mkConst to each value in the set
  mkConstsFromSet = attrSet:
    lib.foldlAttrs
    (acc: name: value: acc // (mkConst name value))
    {}
    attrSet;
in {
  options = {
    const = mkConstsFromSet {
      accelSpeed = -0.9;
      accentColor = "7d8618";
    };
  };
}

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

  libTypeOf = value: let
    stringToStr = x:
      if x == "string"
      then "str"
      else x;

    # We can unwrap here, because these values should be already merged
    typeName =
      if (value._type or "") == "override"
      then stringToStr (builtins.typeOf value.content)
      else stringToStr (builtins.typeOf value);
  in
    lib.types.${typeName};

  # Create dummy option for easy referencing in the config with the type of the value
  mkConst = name: value:
    mkConstWithType (libTypeOf value) name value;

  # Take attribute set of values, apply mkConst to each value in the set
  mkConstsFromSet = setMap mkConst;

  # mkConstsFromSetManuallyTyped = let
  #   takeSetOf2AndUseType = name: valueAndType:
  #     mkConstWithType valueAndType.type name valueAndType.value;
  # in
  #   setMap takeSetOf2AndUseType;

  # Create dummy option for easy referencing in the config with the type "anything'
  mkConstInsanity = mkConstWithType lib.types.anything;

  # Take attribute set of values, apply mkConstInsanity to each value in the set
  mkConstsFromSetAny = setMap mkConstInsanity;

  mkPreConstsFromSet = set: wrapListInSet (lib.mapAttrsToList (name: value: {${name} = value;}) set);
  wrapListInSet = lst: {listWrapper = lst;};
  # unwrapListInSet = set: set.list;
  safeUnwrapListInSet = set:
    if builtins.typeOf (set.listWrapper or "") == "list"
    then set.listWrapper
    else set;
  mkConstsFromSetAnyFull = wrappedAttrsList: mkConstsFromSetAny (lib.mergeAttrsList (safeUnwrapListInSet wrappedAttrsList));
in {
  options = {
    constLib = mkConstsFromSetAny {
      # inherit mkConst;
      # inherit mkConstWithType;

      inherit mkConstsFromSet;
      # inherit mkConstsFromSetManuallyTyped;
      inherit mkConstsFromSetAnyFull;
      inherit mkPreConstsFromSet;
      inherit mkConstsFromSetAny;
    };
    # preConst = mkPreConstsFromSet {
    #   accentColor = "7d8618";
    #   accentColorOv = lib.mkForce "stronger";
    # };
    # preConst = [];
    stupid = {};
    # const = mkConstsFromSetAny (lib.mergeAttrsList preConst);
    const = mkConstsFromSetAnyFull [{}];
  };
}

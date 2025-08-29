lib: pkgs: let
  inherit (lib) filterAttrs;
in rec {
  jsonFormat = pkgs.formats.json {};

  # Removes nulls because Waybar ignores them.
  # This is not recursive.
  removeTopLevelNulls = filterAttrs (_: v: v != null);

  # The clean list of configurations
  finalConfiguration = settings: map removeTopLevelNulls (lib.attrValues settings);

  wrapWaybarWithConfig = config: wrapperName: let
    configFile = jsonFormat.generate "waybar-${wrapperName}-config.json" (finalConfiguration config);
  in
    pkgs.writeShellApplication {
      name = "waybar-${wrapperName}";
      runtimeInputs = [pkgs.waybar];
      text = ''
        waybar -c ${configFile}
      '';
    };
}

{
  lib,
  pkgs,
  config,
  ...
}: {
  # Used for wrapping something like "awesome" with its own xinitrc, so
  # it can be launched like wayland compositors
  wrapWithXinitrc = xinitrc: wmExe: let
    xinitrcFile = pkgs.writeTextFile {
      name = ".xinitrc";
      executable = true;
      text = xinitrc;
    };
  in
    if (!config.services.xserver.displayManager.startx.enable && !config.services.xserver.displayManager.sx.enable)
    then throw "You need either startx or sx enabled"
    else let
      startx-wrapper-if = lib.mkIf config.services.xserver.displayManager.startx.enable [
        (pkgs.writeShellApplication {
          name = "startx-${wmExe}";
          runtimeInputs = [pkgs.xinit];
          text = ''
            XINITRC=${xinitrcFile} startx
          '';
        })
      ];
      sx-wrapper-if = lib.mkIf config.services.xserver.displayManager.sx.enable [
        (pkgs.writeShellScriptBin "sx-${wmExe}"
          ''
            sx ${xinitrcFile}
          '')
      ];
      unpackBasedIfListOnCon = ifAttrset:
        if ifAttrset.condition
        then ifAttrset.content
        else [];
    in
      (unpackBasedIfListOnCon startx-wrapper-if) ++ (unpackBasedIfListOnCon sx-wrapper-if);
}

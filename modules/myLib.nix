{
  lib,
  pkgs,
  ...
}: {
  # Use path relative to the root of the project
  # Yes this can be point free
  relativeToRoot = stringRelativePath: lib.path.append ../. stringRelativePath;

  # Used for wrapping something like "awesome" with its own xinitrc, so
  # it can be launched like wayland compositors
  wrapWithXinitrc = xinitrc: wmExe: let
    xinitrcFile = builtins.toFile ".xinitrc" xinitrc;
  in
    pkgs.writeShellApplication {
      name = "start-${wmExe}";
      runtimeInputs = [pkgs.xorg.xinit];
      text = ''
        XINITRC=${xinitrcFile} startx
      '';
    };
}

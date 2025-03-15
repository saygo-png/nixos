{
  pkgs,
  lib,
  ...
}: {
  projectRootFile = "flake.nix";

  programs.alejandra.enable = true;

  programs.deadnix.enable = true;

  programs.statix.enable = true;
  settings.global.excludes = ["resources/cookiecutterTemplates/*"];
  settings.formatter = {
    trim-newlines = {
      command = "${lib.getExe pkgs.gnused}";
      options = lib.strings.splitString " " ''-i -e :a -e /^\n*$/{$d;N;ba -e }'';
      includes = ["*"];
    };
  };
}

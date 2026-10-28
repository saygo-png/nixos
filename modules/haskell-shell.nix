{
  lib,
  self,
  system,
  conUsername,
  ...
}: let
  shellPkg = lib.getExe "my-shh-shell" self.packages.${system}.saygo-utils;
in {
  custom.persist.home.cache.directories = [".config/shh"];

  home-manager.users.${conUsername} = {
    programs.alacritty.settings.terminal.shell = lib.mkForce shellPkg;
  };
}

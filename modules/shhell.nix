{
  lib,
  self,
  system,
  conUsername,
  ...
}: let
  shellPkg = lib.getExe' self.packages.${system}.saygo-utils "shhell" ;
in {
  custom.persist.home.cache.directories = [".config/shhell" ".cache/shhell"];

  home-manager.users.${conUsername} = {
    programs.alacritty.settings.terminal.shell = lib.mkForce shellPkg;
  };
}

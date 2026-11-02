{
  lib,
  self,
  system,
  conUsername,
  ...
}: let
  shellPkg = lib.getExe' self.packages.${system}.saygo-utils "shhell" ;
in {
  custom.persist.home.cache.directories = [".config/shhell" ".cache/shhell" ".cache/ghc-link-cache"];

  home-manager.users.${conUsername} = {
    programs.alacritty.settings.terminal.shell = lib.mkForce shellPkg;
    # GHCi that reads ~/.config/haskeline/haskeline. hiPrio wins over the ghci
    # of a stock GHC installed alongside it.
    home.packages = [(lib.hiPrio self.packages.${system}.ghci)];
  };
}

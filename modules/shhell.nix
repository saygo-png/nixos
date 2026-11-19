{
  lib,
  pkgs,
  self,
  system,
  conUsername,
  ...
}: let
  shellPkg = lib.getExe' self.packages.${system}.saygo-utils "shhell";
in {
  environment.sessionVariables.SHHELL_DEV_ENVS = builtins.toFile "dev-envs.json" (builtins.toJSON (
    lib.mapAttrs (name: packages: {
      installable = "/etc/flake-src#devEnvs.${system}.${name}";
      packages = map lib.getName packages;
    })
    (import (lib.my.relativeToRoot "dev-envs.nix") pkgs)
  ));

  custom.persist.home.cache.directories = [".config/haskeline" ".config/shhell" ".cache/shhell" ".cache/ghc-link-cache"];

  home-manager.users.${conUsername} = {
    programs.alacritty.settings.terminal.shell = lib.mkForce shellPkg;
    # GHCi that reads ~/.config/haskeline/haskeline. hiPrio wins over the ghci
    # of a stock GHC installed alongside it.
    home.packages = [(lib.hiPrio self.packages.${system}.ghci)];
  };
}

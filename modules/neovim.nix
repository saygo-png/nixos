{
  lib,
  pkgs,
  config,
  inputs,
  conUsername,
  ...
}: {
  environment.systemPackages = [
    inputs.my-neovim.packages.${pkgs.stdenv.hostPlatform.system}.neovim
  ];

  custom.persist.home.cache.directories = [
    ".config/nvim/after/syntax"
  ];

  environment.variables = rec {
    EDITOR = "nvim";
    VISUAL = EDITOR;
    SUDO_EDITOR = EDITOR;
  };

  home-manager.users.${conUsername} = {
    programs.neovide = {
      enable = true;
      settings = {
        fork = true;
        vsync = false;
        font = {
          normal = ["${config.stylix.fonts.monospace.name}"];
          size = lib.mkForce 13;
        };
      };
    };

    home = {
      shellAliases."neov" = "neovide";
      packages = [pkgs.tree-sitter];
    };

    stylix.targets.nixvim.enable = false;
  };
}

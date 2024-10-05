{
  pkgs,
  conUsername,
  conFlakePathRel,
  ...
}: {
  programs.zsh.enable = true;

  # Set shell of the user.
  users.users.${conUsername}.shell = pkgs.zsh;

  # Declare zsh as an available shell.
  environment.shells = [pkgs.zsh];

  # Provides autocompletion for system programs for zsh.
  environment.pathsToLink = ["/share/zsh"];
  home-manager.users.${conUsername} = {
    home.shellAliases = {"nix-shell" = "nix-shell --run zsh";};
    programs.yazi.enableZshIntegration = true;
    programs.zsh = {
      enable = true;
      history.save = 50;
      history.size = 50;
      defaultKeymap = "viins";
      enableCompletion = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      historySubstringSearch.enable = false;
      syntaxHighlighting.highlighters = ["brackets"];
      initExtra = builtins.readFile "${conFlakePathRel}/resources/zsh-extraConfig";
      plugins = [
        {
          name = "vi-mode";
          src = pkgs.zsh-vi-mode;
          file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
        }
      ];
    };
  };
}

{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  environment.systemPackages = [
    pkgs.tmux-sessionizer # Tmux session manager
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    programs.fzf.tmux.enableShellIntegration = true;
    xdg.configFile."tms/config.toml".text =
      # toml
      ''
        [[search_dirs]]
        path = "${config.home.homeDirectory}/Sync/art/diploma"
        depth = 10
        [[search_dirs]]
        path = "${config.home.homeDirectory}/Sync/art/generative-art"
        depth = 10
        [[search_dirs]]
        path = "${config.home.homeDirectory}/Sync/builds"
        depth = 10
      '';

    stylix.targets.tmux.enable = false;
    programs.tmux = {
      baseIndex = 1;
      enable = true;
      shell = "${lib.getExe pkgs.fish}"; # Full path is needed here
      keyMode = "vi";
      prefix = "C-a";
      tmuxinator.enable = true;
      escapeTime = 0; # Delay after pressing escape
      mouse = true; # Allows you to scroll a terminal
      historyLimit = 1000; # Alacritty already holds history
      extraConfig = ''
        #No hanging sessions.
        #set-option -sg destroy-unattached

        # For vim autoread.
        set-option -g focus-events on

        # For truecolor inside tmux
        set -g default-terminal 'tmux-256color'
        set -as terminal-overrides ",alacritty*:Tc"

        # Open tmux inside current dir
        bind c new-window -c "#{pane_current_path}"

        # Easy-to-remember split pane commands.
        bind | split-window -h
        bind - split-window -v
        unbind '"'
        unbind %

        # I don't know, read the docs.
        setw -g monitor-activity on
        set -g visual-activity off

        # Moving between panes with vim movement keys.
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R # moving between windows with vim movement keys.
        bind -r C-h select-window -t :-
        bind -r C-l select-window -t :+
        # Resize panes with vim movement keys.
        bind -r H resize-pane -L 5
        bind -r J resize-pane -D 5
        bind -r K resize-pane -U 5
        bind -r L resize-pane -R 5

        # Status line.
        set-option -sg status on
        set-option -sg status-keys vi
        set-option -sg status-left ""
        set-option -sg status-interval 30
        set-option -sg status-justify left
        set-option -sg status-left-length 10
        set-option -sg status-position bottom
        set-option -sg status-right-length 45
        set-option -sg status-left-style default
        set-option -sg status-right-style default
        set-option -sg status-style fg=green,bg=default
        set-option -sg status-right "#(tmux-mem) %Y-%m-%d (%Ob %a) %H:%M"

        # y and p as in vim.
        set-option -sg set-clipboard on
        bind Escape copy-mode
        # Bind p paste-buffer.
        bind-key -T copy-mode-vi 'p' send -X paste-buffer
        bind-key -T copy-mode-vi 'v' send -X begin-selection
        bind-key -T copy-mode-vi 'Bspace' send -X halfpage-up
        bind-key -T copy-mode-vi 'Space' send -X halfpage-down
        bind-key -T copy-mode-vi 'y' send-keys -X copy-pipe-and-cancel 'xclip -se c -i'
      '';
    };
  };
}

{
  conUsername,
  conFlakePathRel,
  ...
}: {
  home-manager.users.${conUsername} = {
    programs.fzf.tmux.enableShellIntegration = true;
    home.file."bin/tmux-mem-cpp".source = "${conFlakePathRel}/resources/static/tmux-mem-cpp";
    programs.tmux = {
      baseIndex = 1;
      enable = true;
      keyMode = "vi";
      prefix = "C-a";
      escapeTime = 0; # Delay after pressing escape
      mouse = true; # Allows you to scroll a terminal
      historyLimit = 1000; # Alacritty already holds history
      extraConfig = ''
        #No hanging sessions.
        set-option -sg destroy-unattached

        # For vim autoread.
        set-option -g focus-events on

        # For truecolor inside tmux
        set -g default-terminal 'tmux-256color'
        set -as terminal-overrides ",alacritty*:Tc"

        # Easy-to-remember split pane commands.
        bind | split-window -h
        bind - split-window -v
        unbind '"'
        unbind %

        # I don't know, read the docs.
        setw -g monitor-activity on
        set -g visual-activity on

        # Moving between panes with vim movement keys.
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R
        # moving between windows with vim movement keys.
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
        set-option -sg status-right "#(tmux-mem-cpp) %Y-%m-%d (%Ob %a) %H:%M"

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

{
  pkgs,
  conUsername,
  ...
}: {
  environment.systemPackages = [
    pkgs.tmux-sessionizer # Tmux session manager
    (pkgs.callPackage
      ({stdenv}:
        stdenv.mkDerivation {
          pname = "tmux-mem-cpp";
          version = "1.0";
          nativeBuildInputs = [stdenv.cc];
          dontUnpack = true;

          buildPhase = let
            main-cpp =
              builtins.toFile "main.cpp"
              # cpp
              ''
                #include <cstdint>
                #include <fstream>
                #include <iomanip>
                #include <iostream>
                #include <string>

                using i32 = int32_t;
                using u64 = uint64_t;
                using f32 = float;

                int main(void)
                {
                  std::fstream meminfo("/proc/meminfo", std::ios::in);

                  std::string name;
                  u64 value;
                  std::string unit;

                  u64 total_free{};

                  // Read in the amount of available memory and swap
                  while (meminfo >> name >> value >> unit)
                  {
                    if (name == "MemAvailable:" || name == "SwapAvailable:")
                      total_free += value;
                  }

                  // Convert the free memory from kilobytes to gigabytes
                  constexpr f32 divisor = 1.0f / 1024.0f / 1024.0f;
                  const f32 total_free_gb = total_free * divisor;

                  // Increase precision if the amount of free memory is more than 10 gigabytes
                  const i32 precision = total_free_gb > 10 ? 3 : 2;

                  std::cout << "MemF " << std::setprecision(precision) << total_free_gb << "G\n";
                }
              '';
          in ''
            g++ ${main-cpp} -o tmux-mem-cpp
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp tmux-mem-cpp $out/bin/
            chmod +x $out/bin/tmux-mem-cpp
          '';

          meta = {
            mainProgram = "tmux-mem-cpp";
            description = "Prints free RAM for use in a tmux statusline";
          };
        })
      {})
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
        set -g visual-activity on

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

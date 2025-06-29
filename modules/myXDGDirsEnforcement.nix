{
  config,
  conUsername,
  ...
}: let
  homeConfig = config.home-manager.users.${conUsername};
in {
  nix.settings.use-xdg-base-directories = true;
  nix.extraOptions = "use-xdg-base-directories = true";

  environment.sessionVariables = {
    GOPATH = "${homeConfig.xdg.dataHome}/go";
    GNUPGHOME = "${homeConfig.xdg.dataHome}/gnupg";
    WINEPREFIX = "${homeConfig.xdg.dataHome}/wine";
    CARGO_HOME = "${homeConfig.xdg.dataHome}/cargo";
    XAUTHORITY = ''''${XDG_RUNTIME_DIR}/Xauthority'';
    RUSTUP_HOME = "${homeConfig.xdg.dataHome}/rustup";
    ANDROID_HOME = "${homeConfig.xdg.dataHome}/android";
    ANDROID_USER_HOME = "${homeConfig.xdg.dataHome}/android";
    DOTNET_CLI_HOME = "${homeConfig.xdg.dataHome}/dotnet";
    HISTFILE = "${homeConfig.xdg.stateHome}/bash/history";
    GRADLE_USER_HOME = "${homeConfig.xdg.dataHome}/gradle";
    PARALLEL_HOME = "${homeConfig.xdg.configHome}/parallel";
    MYSQL_HISTFILE = "${homeConfig.xdg.stateHome}/mysql/history";
    NUGET_PACKAGES = "${homeConfig.xdg.cacheHome}/NuGetPackages";
    PYTHONSTARTUP = "${homeConfig.xdg.configHome}/python/pythonrc";
    ZDOTDIR = "${homeConfig.xdg.configHome}/zsh";
    COOKIECUTTER_CONFIG = "${homeConfig.xdg.configHome}/cookiecutter/config.yaml";
    NPM_CONFIG_USERCONFIG = "${homeConfig.xdg.configHome}/npm/npmrc";
    _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${homeConfig.xdg.configHome}/java";
  };

  nixpkgs.overlays = [
    (_final: prev: {
      steam = prev.steam.override (old: {
        extraBwrapArgs =
          (old.buildFHSEnv.extraBwrapArgs or [])
          ++ [
            "--bind $XDG_DATA_HOME/steam-home $HOME"

            "--unsetenv XDG_CACHE_HOME"
            "--unsetenv XDG_CONFIG_HOME"
            "--unsetenv XDG_DATA_HOME"
            "--unsetenv XDG_STATE_HOME"
          ];
      });
    })
  ];

  custom.persist = {
    home = {
      directories = [
        ".local"
        ".ssh"
        "Misc"
        "builds"
        "Documents"
        "Downloads"
        "Games"
        "Music"
        "nixos"
        "Pictures"
        "Sync"
        "Videos"
      ];
      cache.directories = [
        ".cache/Anki"
        ".cache/Borg"
        ".cache/Calibre"
        ".cache/keepassxc"

        ".cache/Dconf"

        ".cache/Cabal"
        ".cache/hie-bios"

        ".cache/nix"
        ".cache/Direnv"
        ".cache/nix-index"
        ".cache/npm"
        ".cache/tealdeer"

        ".cache/radv_builtin_shaders"
        ".cache/mesa_shader_cache_db"

        ".config/nix"
        ".config/librewolf-${conUsername}"
      ];
    };
  };

  home-manager.users.${conUsername} = {lib, ...}: {
    xdg = {
      enable = true;
      userDirs = {
        createDirectories = true;

        templates = null;
        publicShare = null;
        desktop = null;

        extraConfig = {
          MY_MISC_DIR = "${homeConfig.homeDirectory}/Misc";
          MY_SCREENSHOTS_DIR = "${homeConfig.homeDirectory}/Pictures/screenshots";
          MY_SCREENCAPTURES_DIR = "${homeConfig.homeDirectory}/Pictures/screencaptures";
          MY_AUDIOCAPTURES_DIR = "${homeConfig.homeDirectory}/Pictures/audiocaptures";
        };
      };
    };

    home.activation.make-dirs-for-xdg = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run mkdir -p "${homeConfig.xdg.dataHome}/steam-home"
      run mkdir -p "${homeConfig.xdg.configHome}/simplescreenrecorder"
    '';

    xresources.path = "${homeConfig.xdg.configHome}/.xresources";

    gtk.gtk2.configLocation = "${homeConfig.xdg.configHome}/gtk-2.0/gtkrc";
    home.file.".zshenv".enable = lib.mkForce false;
    programs.zsh = {
      dotDir = ".config/zsh";
      history.path = "${homeConfig.xdg.dataHome}/zsh/zsh_history";
    };

    home.shellAliases = {
      feh = "feh --no-fehbg";
      gdb = "gdb -n -x ${homeConfig.xdg.configHome}/gdb/init";
      pidgin = "pidgin --config=${homeConfig.xdg.dataHome}/purple";
      svn = "svn --config-dir ${homeConfig.xdg.configHome}/subversion";
      wget = "wget --hsts-file=\"${homeConfig.xdg.dataHome}/wget-hsts\"";
    };
    xdg = {
      configFile = {
        "cookiecutter/config.yaml".text = ''
          cookiecutters_dir: ${homeConfig.xdg.configHome}/cookiecutters/
          replay_dir: ${homeConfig.xdg.cacheHome}/cookiecutter/
        '';

        "npm/npmrc".text = ''
          prefix=${homeConfig.xdg.dataHome}/npm
          cache=${homeConfig.xdg.cacheHome}/npm
          tmp=\$XDG_RUNTIME_DIR/npm
          init-module=${homeConfig.xdg.configHome}/npm/config/npm-init.js
        '';

        "python/pythonrc".text = ''
          import os
          import atexit
          import readline

          history = os.path.join(os.environ['XDG_CACHE_HOME'], 'python_history')
          try:
              readline.read_history_file(history)
          except OSError:
              pass

          def write_history():
              try:
                  readline.write_history_file(history)
              except OSError:
                  pass

          atexit.register(write_history)
        '';
      };
    };
  };
}

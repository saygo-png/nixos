{
  config,
  conUsername,
  ...
}: let
  homeConfig = config.home-manager.users.${conUsername};
in {
  # DO THIS ONCE LIBREWOLF GETS A HOME MANAGER MODULE TO MOVE .mozzila INTO CONFIG HOME
  # programs = {
  #   # use firefox dev edition
  #   firefox = rec {
  #     enable = true;
  #     package = pkgs.firefox-devedition-bin.overrideAttrs (o: {
  #       # launch firefox with user profile
  #       buildCommand =
  #         o.buildCommand
  #         + ''
  #           wrapProgram "$executablePath" \
  #             --set 'HOME' '${config.xdg.configHome}' \
  #             --append-flags "--name firefox -P ${user}"
  #         '';
  #     });
  #
  #     vendorPath = ".config/.mozilla";
  #     configPath = "${vendorPath}/firefox";
  #   };
  # };
  nix.settings.use-xdg-base-directories = true;
  environment.sessionVariables = {
    GOPATH = "${homeConfig.xdg.dataHome}/go";
    GNUPGHOME = "${homeConfig.xdg.dataHome}/gnupg";
    WINEPREFIX = "${homeConfig.xdg.dataHome}/wine";
    CARGO_HOME = "${homeConfig.xdg.dataHome}/cargo";
    XAUTHORITY = "\$XDG_RUNTIME_DIR/Xauthority";
    RUSTUP_HOME = "${homeConfig.xdg.dataHome}/rustup";
    ANDROID_HOME = "${homeConfig.xdg.dataHome}/android";
    DOTNET_CLI_HOME = "${homeConfig.xdg.dataHome}/dotnet";
    HISTFILE = "${homeConfig.xdg.stateHome}/bash/history";
    GRADLE_USER_HOME = "${homeConfig.xdg.dataHome}/gradle";
    PARALLEL_HOME = "${homeConfig.xdg.configHome}/parallel";
    MYSQL_HISTFILE = "${homeConfig.xdg.stateHome}/mysql/history";
    NUGET_PACKAGES = "${homeConfig.xdg.cacheHome}/NuGetPackages";
    PYTHONSTARTUP = "${homeConfig.xdg.configHome}/python/pythonrc";
    NPM_CONFIG_USERCONFIG = "${homeConfig.xdg.configHome}/npm/npmrc";
    _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${homeConfig.xdg.configHome}/java";
  };

  home-manager.users.${conUsername} = {
    home.shellAliases = {
      gdb = "gdb -n -x ${homeConfig.xdg.configHome}/gdb/init";
      pidgin = "pidgin --config=${homeConfig.xdg.dataHome}/purple";
      svn = "svn --config-dir ${homeConfig.xdg.configHome}/subversion";
      wget = "wget --hsts-file=\"${homeConfig.xdg.dataHome}/wget-hsts\"";
    };
    xdg = {
      configFile = {
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

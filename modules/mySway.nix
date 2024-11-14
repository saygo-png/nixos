{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Polkit (needed for window managers)
  security.polkit.enable = lib.mkDefault true;

  # Also needed for secrets
  services.gnome.gnome-keyring.enable = true;

  # NixOS is retarded and turns on lightdm by default.
  services.displayManager.defaultSession = lib.mkDefault "none+awesome";
  services.xserver.displayManager = lib.mkDefault {
    startx.enable = true;
    lightdm.enable = false;
  };

  environment.systemPackages = with pkgs; [
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
  ];

  home-manager.users.${conUsername} = {
    osConfig,
    config,
    ...
  }: {
    stylix.targets.sway.enable = false;
    wayland.windowManager.sway = {
      enable = true;
      config = {
        modifier = "Mod4";
        # Use kitty as default terminal
        terminal = "${config.home.sessionVariables.TERMINAL}";
        startup = [
          # Launch Firefox on start
          {command = "${config.home.sessionVariables.BROWSER}";}
        ];
        input = {
          "type:pointer" = {
            accel_profile = osConfig.services.libinput.mouse.accelProfile;
            pointer_accel = lib.strings.floatToString(osConfig.const.accelSpeed + 0.1);
          };
          "type:keyboard" = {
            xkb_layout = "pl";
            xkb_options = osConfig.services.xserver.xkb.options;
            repeat_delay = builtins.toString osConfig.services.xserver.autoRepeatDelay;
            repeat_rate = builtins.toString osConfig.services.xserver.autoRepeatInterval;
          };
        };
        output = {
          HDMI-A-2 = {
            resolution = "1920x1080@144Hz";
            scale = "1";
          };
        };
      };
    };
  };
}

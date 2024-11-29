{
  lib,
  inputs,
  conUsername,
  conFlakePathRel,
  ...
}: {
  imports = [
    "${conFlakePathRel}/modules/x11/myXorgBase.nix"
  ];

  services.displayManager.defaultSession = lib.mkDefault "none+awesome";

  nixpkgs.overlays = [
    (_: prev: {
      awesome = prev.awesome.overrideAttrs (_: {
        version = "0.0.0";
        src = inputs.awesome-git;

        patches = [];

        postPatch = ''
          patchShebangs tests/examples/_postprocess.lua
        '';
      });
    })
  ];

  # X11 window manager for games
  services.xserver.windowManager.awesome.enable = true;

  home-manager.users.${conUsername} = {
    config,
    osConfig,
    ...
  }: {
    home.file.".xinitrc".text =
      osConfig.const.xinitBase
      + "\nexec awesome";

    xdg.configFile."awesome/" = {
      recursive = true;
      source = "${conFlakePathRel}/resources/awesome";
    };

    xdg.configFile."awesome/theme.lua" = {
      text = ''
        local theme_assets = require("beautiful.theme_assets")
        local xresources = require("beautiful.xresources")
        local dpi = xresources.apply_dpi
        local gfs = require("gears.filesystem")
        local themes_path = "${config.xdg.configHome}/awesome"
        local theme = {}

        theme.font = "${config.stylix.fonts.sansSerif.name} ${builtins.toString config.stylix.fonts.sizes.terminal}"

        theme.bg_normal = "${config.lib.stylix.colors.withHashtag.base00}66"
        theme.bg_focus = "#${osConfig.const.accentColor}"
        theme.bg_urgent = "${config.lib.stylix.colors.withHashtag.base08}"
        theme.bg_minimize = "${config.lib.stylix.colors.withHashtag.base0B}"
        theme.bg_systray = theme.bg_normal

        theme.fg_normal = "${config.lib.stylix.colors.withHashtag.base07}"
        theme.fg_focus = theme.fg_normal
        theme.fg_urgent = theme.fg_normal
        theme.fg_minimize = theme.fg_normal

        theme.gap_single_client = true
        theme.useless_gap = dpi(${builtins.toString osConfig.const.gaps})
        theme.border_width = dpi(${builtins.toString osConfig.const.borderSize})
        theme.border_normal = "#00000000"
        theme.border_marked = theme.bg_focus
        theme.border_focus = theme.bg_focus
        --snap
        theme.snap_bg = theme.bg_focus
        --notifications
        naughty.config = {
          defaults = {
            ontop = true,
            font = theme.font,
            timeout = 10,
            margin = 20,
            border_width = 1.5,
            font = theme.font,
            fg = beautiful.fg_normal,
            bg = beautiful.bg_normal,
            position = "top_middle",
          },
          padding = 60,
          spacing = 4,
        }
        theme.wallpaper = "${config.stylix.image}"
        return theme
      '';
    };
  };
}

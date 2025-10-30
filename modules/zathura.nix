{
  config,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    stylix.targets.zathura.enable = false;
    programs.zathura = let
      inherit (config.stylix) fonts;
      color = config.lib.stylix.colors.withHashtag;
    in {
      enable = true;
      options = {
        selection-clipboard = "clipboard";
        guioptions = "s";
        adjust-open = "width";
        statusbar-h-padding = 0;
        statusbar-v-padding = 0;
        scroll-page-aware = true;
        statusbar-home-tilde = true;

        font = "${fonts.serif.name} ${toString fonts.sizes.terminal}";
        recolor = false;
        recolor-keephue = false;

        notification-error-bg = "${color.base00}"; # bg
        notification-error-fg = "${color.base08}"; # bright:red
        notification-warning-bg = "${color.base00}"; # bg
        notification-warning-fg = "${color.base0A}"; # bright:yellow
        notification-bg = "${color.base00}"; # bg
        notification-fg = "${color.base0B}"; # bright:green

        completion-bg = "${color.base02}"; # bg2
        completion-fg = "${color.base05}"; # fg
        completion-group-bg = "${color.base01}"; # bg1
        completion-group-fg = "${color.base03}"; # gray
        completion-highlight-bg = "${color.base0B}"; # bright:blue
        completion-highlight-fg = "${color.base02}"; # bg2

        # Define the color in index mode
        index-bg = "${color.base02}"; # bg2
        index-fg = "${color.base05}"; # fg
        index-active-bg = "${color.base0B}"; # bright:blue
        index-active-fg = "${color.base02}"; # bg2

        inputbar-bg = "${color.base01}"; # bg
        inputbar-fg = "${color.base05}"; # fg

        statusbar-bg = "${color.base00}"; # bg2
        statusbar-fg = "${color.base05}"; # fg

        highlight-color = "${color.base0A}80"; # bright:yellow
        highlight-active-color = "${color.base09}80"; # bright:orange

        default-bg = "${color.base00}"; # bg
        default-fg = "${color.base05}"; # fg
        render-loading = true;
        render-loading-bg = "${color.base00}"; # bg
        render-loading-fg = "${color.base05}"; # fg

        # Recolor book content's color
        recolor-lightcolor = "${color.base00}"; # bg
        recolor-darkcolor = "${color.base05}"; # fg
      };
      extraConfig = ''
        unmap +
        unmap _
        map = zoom_in
        map - zoom_out
        map <C-j> zoom_in
        map <C-k> zoom_out

        map <C-r> reload
        map i toggle_statusbar;
      '';
    };
  };
}

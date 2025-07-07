{
  lib,
  config,
  osConfig,
  ...
}: {
  programs.neovide = {
    enable = true;
    settings = {
      font = {
        normal = ["${config.stylix.fonts.monospace.name}"];
        size = lib.mkForce 13;
      };
    };
  };

  home.shellAliases = {"neov" = "neovide";};

  programs.nixvim = {
    extraConfigLua = lib.mkBefore ''
      -- Neovide
      if vim.g.neovide then
        vim.cmd[[colorscheme gruvbox-material]]
        vim.o.background = 'dark'
        vim.o.guifont = '${config.stylix.fonts.monospace.name}:h${builtins.toString (config.stylix.fonts.sizes.terminal + 1)}:#e-antialias:#h-slight'
        vim.cmd [[ hi Normal guibg=${config.lib.stylix.colors.withHashtag.base00} ]]
      end
    '';

    globals = {
      neovide_transparency = config.stylix.opacity.terminal;
      neovide_transparency_point = 0;
      neovide_background_color = "${config.lib.stylix.colors.withHashtag.base00}";
      neovide_padding_top = lib.mkDefault 8;
      neovide_padding_bottom = lib.mkDefault 0;
      neovide_padding_right = lib.mkDefault 6;
      neovide_padding_left = lib.mkDefault 6;
      neovide_floating_blur_amount_x = 20.0;
      neovide_floating_blur_amount_y = 20.0;
      neovide_hide_mouse_when_typing = true;
      neovide_refresh_rate = osConfig.const.refreshRate;
      neovide_cursor_vfx_mode = "ripple";
      neovide_cursor_animation_length = 0.08;
      neovide_cursor_smooth_blink = true;
      neovide_floating_shadow = false;
      neovide_cursor_animate_command_line = true;
      neovide_cursor_animate_in_insert_mode = true;
    };
  };
}

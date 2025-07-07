{pkgs, ...}: {
  programs.nixvim = {
    extraPlugins = [
      pkgs.vimPlugins.gruvbox-material
    ];

    globals = {
      gruvbox_material_foreground = "original";
      gruvbox_material_enable_bold = 1;
      gruvbox_material_enable_italic = 1;
      gruvbox_material_transparent_background = 2;
    };

    extraConfigLuaPre = ''
      vim.cmd[[colorscheme gruvbox-material]]
    '';
  };
}

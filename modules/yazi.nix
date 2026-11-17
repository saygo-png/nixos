{
  pkgs,
  conUsername,
  ...
}: let
  saneSeparator = "";
in {
  home-manager.users.${conUsername} = {
    programs.yazi = {
      enable = true;
      package = pkgs.yazi.override {
        # Get rid of some unused deps
        optionalDeps = [
          pkgs.jq
          pkgs._7zz
          pkgs.fd
          pkgs.ripgrep
          pkgs.fzf
          pkgs.zoxide
        ];
      };
      shellWrapperName = "yy";
      keymap.mgr.prepend_keymap = [
        {
          on = ["d"];
          run = ["remove --permanently"];
          desc = "remove permanently";
        }
        {
          on = ["b"];
          run = [
            # https://github.com/sxyazi/yazi/discussions/327
            ''shell '${pkgs.ripdrag}/bin/ripdrag "$@" -x -n 2>/dev/null &' --confirm''
          ];
          desc = "Drag and drop selection";
        }
      ];
      initLua =
        #Lua
        ''
          function Entity:padding() return "${saneSeparator}" end
          function Linemode:padding() return "${saneSeparator}" end
        '';
      theme = let
        sep = {
          open = saneSeparator;
          close = saneSeparator;
        };
      in {
        status.sep_left = sep;
        status.sep_right = sep;
        tabs.sep_inner = sep;
        tabs.sep_outer = sep;
      };
      settings = {
        preview.tab_size = 2;
        mgr = {
          show_hidden = true;
          show_symlink = true;
          sort_by = "natural";
          sort_dir_first = true;
        };
        opener.extract = [
          {
            run = ''aunpack "$@"'';
            desc = "Extract here";
          }
        ];
        # Disable image previews
        plugin.prepend_previewers = [
          {
            mime = "image/*";
            run = "noop";
          }
        ];
      };
    };
  };
}

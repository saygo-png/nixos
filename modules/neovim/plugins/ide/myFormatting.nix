{
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.stylua # Lua formatter
    pkgs.shfmt # Shell formatter
    pkgs.yapf # Python formatter
    pkgs.isort # Python import sorter
    pkgs.prettierd # Javascript formatter
    pkgs.haskellPackages.fourmolu # Haskell formatter
    pkgs.nodePackages.prettier # Javascript formatter
  ];

  programs.nixvim = {
    opts = {
      # Use conform-nvim for gq formatting. ('formatexpr' is set to vim.lsp.formatexpr(),
      # so you can format lines via gq if the language server supports it).
      formatexpr = "v:lua.require'conform'.formatexpr()";
    };

    plugins = {
      conform-nvim = {
        enable = true;
        lazyLoad.settings = {
          keys = ["<leader>c" "gq"];
          cmd = "Conform";
        };
        settings = {
          lsp_fallback = false;
          formatters_by_ft = {
            # Conform will run multiple formatters sequentially.
            json = ["jq"];
            sh = ["shfmt"];
            lua = ["stylua"];
            css = ["prettierd"];
            nix = ["alejandra"];
            html = ["prettierd"];
            scss = ["prettierd"];
            jsonc = ["prettierd"];
            haskell = ["fourmolu"];
            graphql = ["prettierd"];
            markdown = ["prettierd"];
            python = ["isort" "yapf"];
            javascript = ["prettierd"];
            typescript = ["prettierd"];
            javascriptreact = ["prettierd"];
            typescriptreact = ["prettierd"];
            # Use the "*" filetype to run formatters on all filetypes.
            "*" = [
              "squeeze_blanks"
              "trim_whitespace"
              "trim_newlines"
            ];
          };
          formatters = {
            cljfmt = {
              command = "${lib.getExe pkgs.cljfmt}";
              args = ["fix" "-"];
              stdin = true;
            };
            shfmt.args = lib.mkOptionDefault ["-i" "2"];
            squeeze_blanks = {
              command = pkgs.lib.getExe' pkgs.coreutils "cat";
            };
          };
        };
      };

      which-key.settings.spec = [
        (lib.my.nRegister "<leader>c" "Conform" " ")
      ];
    };

    keymaps = [
      {
        key = "<leader>c";
        action.__raw = "function()
          require('conform').format({ timeout_ms = 500 })
        end";
        options.desc = "[c]onform";
      }
    ];

    userCommands = {
      Conform = {
        command.__raw = "function()
          require('conform').format({ timeout_ms = 500 })
        end";
        desc = "Format using Conform with a 500ms timeout";
      };
    };
  };
}

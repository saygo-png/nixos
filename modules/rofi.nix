{
  conUsername,
  lib,
  ...
}: {
  home-manager.users.${conUsername} = {
    osConfig,
    config,
    ...
  }: {
    programs.rofi = let
      color = osConfig.lib.stylix.colors.withHashtag;
      inherit (config.stylix) fonts;
      unbind = "";
      l = config.lib.formats.rasi.mkLiteral;
    in {
      enable = true;
      extraConfig = {
        sort = true;
        padding = 10;
        scrollbar = true;
        show-match = true;
        matching = "fuzzy";
        show-icons = false;
        auto-select = false;
        run-command = "{cmd}";
        fixed-num-lines = true;
        sorting-method = "fzf";
        disable-history = false;
        modi = "window,run,drun";
        separator-style = "dash";
        drun-show-actions = false;
        window-format = "{w} {c}  {t}";
        font = "${fonts.monospace.name} ${toString (fonts.sizes.terminal + 1)}";

        kb-remove-to-eol = unbind;
        kb-row-up = "Control+k";

        kb-accept-entry = "Return";
        kb-row-down = "Control+j";

        kb-mode-complete = unbind;
        kb-mode-next = "Control+l";

        kb-remove-char-forward = unbind;
        kb-clear-line = "Control+d";

        kb-remove-char-back = "BackSpace";
        kb-mode-previous = "Control+h";

        kb-cancel = "Escape,Control+q";
      };
      theme = lib.mkForce {
        "*" = {
          highlight = "bold";
          border-color = l "#${osConfig.const.accentColor}";
          gruvbox-dark-fg0 = l "${color.base06}";
          gruvbox-dark-fg1 = l "${color.base05}";
          gruvbox-dark-gray = l "${color.base04}";
          gruvbox-dark-red-dark = l "${color.base09}";
          gruvbox-dark-red-light = l "${color.base08}";
          gruvbox-dark-yellow-dark = l "${color.base0A}";
          gruvbox-dark-yellow-light = l "${color.base0C}";
          gruvbox-dark-bg0 = l "${color.base00}";
          gruvbox-dark-bg3 = l "${color.base01}";
          selected-normal-background = l "${color.base0B}";

          normal-background = l "@background";
          gruvbox-dark-bg0-soft = l "@background";
          alternate-normal-background = l "@background";
          background-color = l "@background";
          background = l "@gruvbox-dark-bg0";
          foreground = l "@gruvbox-dark-fg1";
          separatorcolor = l "@border-color";
          normal-foreground = l "@foreground";
          active-foreground = l "@background";
          scrollbar-handle = l "@border-color";
          urgent-foreground = l "@background";
          alternate-normal-foreground = l "@foreground";
          urgent-background = l "@gruvbox-dark-red-dark";
          active-background = l "@gruvbox-dark-yellow-dark";
          selected-normal-foreground = l "@background";
          alternate-urgent-foreground = l "@gruvbox-dark-fg1";
          selected-active-foreground = l "@active-foreground";
          selected-urgent-foreground = l "@urgent-foreground";
          alternate-active-background = l "@active-background";
          alternate-active-foreground = l "@active-foreground";
          alternate-urgent-background = l "@urgent-background";
          selected-urgent-background = l "@gruvbox-dark-red-light";
          selected-active-background = l "@gruvbox-dark-yellow-light";
        };
        "window" = {
          padding = 5;
          border = 0;
          background-color = l "@background";
        };
        "mainbox" = {
          border = 0;
          padding = 0;
        };
        "message" = {
          padding = l "1px";
          border = l "2px 0 0";
          border-color = l "@background";
        };
        "textbox" = {
          highlight = l "@highlight";
          text-color = l "@foreground";
        };
        "listview" = {
          spacing = l "0px";
          padding = l "0px 0 0";
          border = l "0px solid 0 0";
          border-color = l "@background";
        };
        "element" = {
          border = 0;
          padding = l "2px";
        };
        "inputbar" = {
          spacing = 2;
          padding = l "2px";
          text-color = l "@normal-foreground";
          children = l "[prompt, textbox-prompt-sep, entry, case-indicator]";
        };
        "case-indicator, entry, prompt, button" = {
          spacing = 0;
          text-color = l "@normal-foreground";
        };
        "case-indicator" = {
          spacing = 0;
          text-color = l "@normal-foreground";
        };
        "entry" = {
          spacing = 0;
          text-color = l "@normal-foreground";
        };
        "prompt" = {
          spacing = 0;
          text-color = l "@normal-foreground";
        };
        "button" = {
          spacing = 0;
          text-color = l "@normal-foreground";
        };
        "textbox-prompt-sep" = {
          str = ":";
          expand = false;
          margin = l "0 0.2em 0.3em 0";
          text-color = l "@normal-foreground";
        };
        "element.normal.normal" = {
          text-color = l "@normal-foreground";
          background-color = l "@normal-background";
        };
        "element.normal.urgent" = {
          text-color = l "@urgent-foreground";
          background-color = l "@urgent-background";
        };
        "element.normal.active" = {
          text-color = l "@active-foreground";
          background-color = l "@active-background";
        };
        "element.selected.normal" = {
          text-color = l "@selected-normal-foreground";
          background-color = l "@selected-normal-background";
        };
        "element.selected.urgent" = {
          text-color = l "@selected-urgent-foreground";
          background-color = l "@selected-urgent-background";
        };
        "element.selected.active" = {
          text-color = l "@selected-active-foreground";
          background-color = l "@selected-active-background";
        };
        "element.alternate.normal" = {
          text-color = l "@alternate-normal-foreground";
          background-color = l "@alternate-normal-background";
        };
        "element.alternate.urgent" = {
          text-color = l "@alternate-urgent-foreground";
          background-color = l "@alternate-urgent-background";
        };
        "element.alternate.active" = {
          text-color = l "@alternate-active-foreground";
          background-color = l "@alternate-active-background";
        };
        "scrollbar" = {
          border = 0;
          padding = 0;
          handle-width = l "8px";
          width = l "4px";
          handle-color = l "@normal-background";
        };
        "mode-switcher" = {
          border = l "2px 0 0";
          border-color = l "@normal-background";
        };
        "button.selected" = {
          text-color = l "@selected-normal-foreground";
          background-color = l "@selected-normal-background";
        };
        "element-icon" = {
          text-color = l "inherit";
          background-color = l "inherit";
        };
        "element-text" = {
          text-color = l "inherit";
          background-color = l "inherit";
        };
      };
    };
  };
}

{
  pkgs,
  config,
  ...
}: {
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.rainbow];

    globals = {
      rainbow_active = 1;
      rainbow_conf = let
        c = config.lib.stylix.colors.withHashtag;
      in {
        guifgs = [c.base0A c.base0C c.base0E c.base0B c.base0F c.base09];
      };
    };

    highlightOverride = {
      "@constructor".link = "";
      "@punctuation.bracket".link = "";
      "@punctuation.special".link = "";
      "@punctuation.delimiter".link = "";
      "@variable.parameter.haskell".link = "";
    };
  };
}

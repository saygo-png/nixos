{
  lib,
  config,
  pkgs,
  pkgs-unstable,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    ## Emacs itself
    binutils # native-comp needs 'as', provided by this
    emacs # HEAD + native-comp
    # (emacs.emacsWithPackages (emacsPackages: with emacsPackages; [
    #      vterm
    #      mu4e
    #    ]))

    ## Doom dependencies
    git
    ripgrep
    gnutls # for TLS connectivity

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    (lib.mkIf (config.programs.gnupg.agent.enable)
      pinentry-emacs) # in-emacs gnupg prompts
    zstd # for undo-fu-session/undo-tree compression

    ## Module dependencies
    # :email mu4e
    mu
    isync
    # :tools editorconfig
    editorconfig-core-c # per-project style config
    # :tools lookup & :lang org +roam
    sqlite
    # :lang latex & :lang org (latex previews)
    texlive.combined.scheme-medium
    # :lang beancount
    beancount
    fava
    # :lang nix
    age
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    home = {
      # Binary (or not) blobs.
      sessionPath = ["${config.home.sessionVariables.XDG_CONFIG_HOME}/emacs/bin"]; # Add ~/bin to path.
    };
  };

  fonts.packages = [
    (pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
  ];
}

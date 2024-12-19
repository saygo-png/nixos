_: {
  projectRootFile = "flake.nix";

  programs.alejandra.enable = true;
  programs.deadnix.enable = true;
  programs.statix.enable = true;
  settings.global.excludes = ["resources/cookiecutterTemplates/*"];
}

_: {
  projectRootFile = "flake.nix";

  programs.alejandra.enable = true;

  # Deadnix is used as a github action
  # programs.deadnix.enable = true;

  programs.statix.enable = true;
  settings.global.excludes = ["resources/cookiecutterTemplates/*"];
}

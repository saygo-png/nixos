{pkgs, ...}: {
  custom.persist.home.cache.directories = [".cache/nix-search-tv"];
  environment.systemPackages = [
    (pkgs.writeShellApplication {
      name = "ns";
      runtimeInputs = [pkgs.fzf pkgs.nix-search-tv];
      text = ''exec "${pkgs.nix-search-tv.src}/nixpkgs.sh" "$@"'';
    })
  ];
}

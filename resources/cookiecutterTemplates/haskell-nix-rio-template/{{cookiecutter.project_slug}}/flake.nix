{
  description = "{{cookiecutter.projectDescription}}";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              {{cookiecutter.binaryName}} = hfinal.callCabal2nix "{{cookiecutter.binaryName}}" ./. { };
            };
        };
        {{cookiecutter.binaryName}} = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.{{cookiecutter.binaryName}};
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = {{cookiecutter.project_slug}}-shell;
            {{cookiecutter.project_slug}}-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.{{cookiecutter.binaryName}} ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.fourmolu
                hspkgs.implicit-hie
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = {{cookiecutter.binaryName}};
            {{cookiecutter.binaryName}} = pkgs.{{cookiecutter.binaryName}};
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}

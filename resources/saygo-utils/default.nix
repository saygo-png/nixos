{niceHaskell, ...}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "saygo-utils";
  compiler = "ghc910"; # nixpkgs default, so dependencies come from the binary cache
}

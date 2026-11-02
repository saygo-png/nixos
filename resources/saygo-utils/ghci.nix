# GHCi with haskeline 0.8.4.1
{
  haskell,
  makeWrapper,
  runCommand,
  hp,
  env ? hp.ghc,
}: let
  inherit (hp.ghc) version;
  # GHC's build fills in this template; nothing else in it needs changing.
  src = runCommand "ghc-bin-src-${version}" {} ''
    cp -r ${hp.ghc.src}/ghc $out
    chmod -R u+w $out
    substitute $out/ghc-bin.cabal.in $out/ghc-bin.cabal \
      --replace-fail @ProjectVersionMunged@ ${version} \
      --replace-fail @ProjectVersion@ ${version}
  '';
  # Dynamic like the stock executable, so GHCi loads packages the same way.
  ghc-bin = haskell.lib.compose.enableSharedExecutables (
    hp.callCabal2nixWithOptions "ghc-bin" src "--flag=internal-interpreter" {
      haskeline = hp.haskeline_0_8_4_1;
    }
  );
  libdir = "${env}/lib/ghc-${version}/lib";
in
  # What the stock ghci does: the executable in interactive mode, pointed at
  # env's packages.
  runCommand "ghci-${version}" {nativeBuildInputs = [makeWrapper];} ''
    makeWrapper ${ghc-bin}/bin/ghc $out/bin/ghci \
      --set NIX_GHC ${env}/bin/ghc \
      --set NIX_GHCPKG ${env}/bin/ghc-pkg \
      --set NIX_GHC_LIBDIR ${libdir} \
      --add-flags "--interactive -B${libdir}"
  ''

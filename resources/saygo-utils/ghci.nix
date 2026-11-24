# GHCi with haskeline 0.8.4.1
{
  lib,
  makeBinaryWrapper,
  runCommand,
  hp,
  env ? hp.ghc,
}: let
  inherit (hp.ghc) version;
  # GHC's build fills in this template; nothing else in it needs changing.
  src = runCommand "ghc-bin-src-${version}" {} ''
    cp -r ${hp.ghc.src}/ghc $out
    chmod -R u+w $out
    # A command run with shh that fails has printed its own error, so GHCi
    # doesn't add shh's exception, like a shell.
    patch -d $out -p1 < ${./ghci-shh-failures.patch}
    # :forget takes names defined at the prompt out of scope, for :unuse.
    patch -d $out -p1 < ${./ghci-forget.patch}
    substitute $out/ghc-bin.cabal.in $out/ghc-bin.cabal \
      --replace-fail @ProjectVersionMunged@ ${version} \
      --replace-fail @ProjectVersion@ ${version}
  '';
  # Dynamic like the stock executable, so GHCi loads packages the same way.
  ghc-bin = hp.mkDerivation {
    pname = "ghc-bin";
    inherit version src;
    configureFlags = ["-finternal-interpreter"];
    isExecutable = true;
    enableSharedExecutables = true;
    executableHaskellDepends = with hp; [
      array
      base
      bytestring
      containers
      deepseq
      directory
      exceptions
      filepath
      ghc
      ghc-boot
      ghc-prim
      ghci
      haskeline_0_8_4_1
      process
      time
      transformers
      unix
    ];
    license = lib.licenses.bsd3;
    mainProgram = "ghc";
  };
  libdir = "${env}/lib/ghc-${version}/lib";
in
  # What the stock ghci does: the executable in interactive mode, pointed at
  # env's packages.
  runCommand "ghci-${version}" {nativeBuildInputs = [makeBinaryWrapper];} ''
    makeBinaryWrapper ${ghc-bin}/bin/ghc $out/bin/ghci \
      --set NIX_GHC ${env}/bin/ghc \
      --set NIX_GHCPKG ${env}/bin/ghc-pkg \
      --set NIX_GHC_LIBDIR ${libdir} \
      --add-flags "--interactive -B${libdir}"
  ''

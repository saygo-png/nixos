{ mkDerivation, base, bytestring, Cabal, cassava, containers
, cryptohash-sha256, directory, file-io, filepath, granite, lib
, microaeson, optparse-applicative, os-string, process, relude, shh
, shh-extras, tasty, tasty-bench, template-haskell, text, time, tz
, unix, vector, yocto
}:
mkDerivation {
  pname = "saygo-utils";
  version = "0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory filepath os-string relude shh
    shh-extras template-haskell unix yocto
  ];
  executableHaskellDepends = [
    base bytestring Cabal cassava containers cryptohash-sha256
    directory file-io filepath granite microaeson optparse-applicative
    os-string process relude shh tasty tasty-bench template-haskell
    text time tz unix vector yocto
  ];
  license = "unknown";
}

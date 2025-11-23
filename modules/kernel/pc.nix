# Thanks to @Gerg-L for showing how to override these `features` when using `linuxManualConfig`
# https://github.com/Gerg-L/nixos/blob/8d82feb56fec9204ea7c67409b0178fb6ff086ed/nixosConfigurations/gerg-desktop/kernel.nix
{
  pkgs,
  lib,
  config,
  pkgs-frozen,
  ...
}: let
  customKernel = (pkgs.linuxManualConfig
    {
      inherit (pkgs-frozen.linuxKernel.kernels.linux_xanmod_latest) version src;
      configfile = lib.my.relativeToRoot "modules/kernel/pc.config";
      modDirVersion = "6.16.10-saygo-xanmod1";
    }).overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.lz4];
    passthru =
      old.passthru or {}
      // {
        features =
          lib.foldr (x: y: x.features or {} // y) {
            efiBootStub = true;
            netfilterRPFilter = true;
            ia32Emulation = true;
          }
          config.boot.kernelPatches;
      };
  });
in {
boot = {
  kernelPackages = pkgs.linuxPackagesFor customKernel;
  initrd.allowMissingModules = true;
};
}

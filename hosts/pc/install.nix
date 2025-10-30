# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{lib, ...}: {
  imports = [
    (lib.my.relativeToRoot "hosts/install.nix")
    ({config, ...}: {
      options.const = config.constLib.mkConstsFromSet {
        host = "pc";
      };
    })
  ];

  networking.hostId = "a4e735aa";
}

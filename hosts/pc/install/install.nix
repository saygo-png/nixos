# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  pkgs,
  config,
  conUsername,
  ...
}: {
  imports = [
    ({config, ...}: {
      options = {
        const = config.constLib.mkConstsFromSet {
          host = "pc";
        };
      };
    })
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = config.const.host; # Define your hostname.
  networking.hostId = "a4e735aa";

  users = {
    mutableUsers = false;
    users = {
      ${conUsername} = {
        initialHashedPassword = "$y$j9T$vhbFbUi1uMXLH4qgYx13I1$dMmxiKFP4l236qd7ipfwmH.0PUnSehayI4jdRQmnzL0";
        hashedPasswordFile = "/etc/password.txt";
        isNormalUser = true;
        extraGroups = ["wheel" "networkmanager" "video"]; # Enable ‘sudo’ for the user.
      };
      root = {
        initialHashedPassword = "$y$j9T$vhbFbUi1uMXLH4qgYx13I1$dMmxiKFP4l236qd7ipfwmH.0PUnSehayI4jdRQmnzL0";
        hashedPasswordFile = "/etc/password.txt";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    neovim
    tree
    wget
    git
    busybox
  ];

  system.stateVersion = "25.05";
}

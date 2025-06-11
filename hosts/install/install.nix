# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  pkgs,
  host,
  conUsername,
  ...
}: {
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = host; # Define your hostname.
  networking.hostId = "a4e735aa";

  users = {
    mutableUsers = false;
    users.${conUsername} = {
      hashedPasswordFile = "/home/${conUsername}/.config/password.txt";
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
    };
  };

  environment.systemPackages = with pkgs; [
    neovim
    tree
    wget
    git
    toybox
  ];

  system.stateVersion = "25.05";
}

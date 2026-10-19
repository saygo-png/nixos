# Shared minimal install config
{
  pkgs,
  conUsername,
  ...
}: {
  nix.settings.experimental-features = ["nix-command" "flakes"];

  boot.loader.limine.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users = {
    mutableUsers = false;
    users = {
      ${conUsername} = {
        initialHashedPassword = "$y$j9T$vhbFbUi1uMXLH4qgYx13I1$dMmxiKFP4l236qd7ipfwmH.0PUnSehayI4jdRQmnzL0";
        hashedPasswordFile = "/etc/password.hash";
        isNormalUser = true;
        extraGroups = ["wheel" "networkmanager" "video" "audio"]; # Enable ‘sudo’ for the user.
      };
      root = {
        initialHashedPassword = "$y$j9T$vhbFbUi1uMXLH4qgYx13I1$dMmxiKFP4l236qd7ipfwmH.0PUnSehayI4jdRQmnzL0";
        hashedPasswordFile = "/etc/password.hash";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    nh
    neovim
    busybox
  ];

  system.stateVersion = "26.11";
}

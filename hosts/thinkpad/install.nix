{lib, ...}: {
  imports = [(lib.my.relativeToRoot "hosts/install.nix")];
  networking.hostName = "thinkpad";

  networking.hostId = "91b1153b";
  networking.networkmanager.enable = true;
}

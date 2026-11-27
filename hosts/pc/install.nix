{lib, ...}: {
  imports = [(lib.my.relativeToRoot "hosts/install.nix")];
  networking.hostName = "pc";

  networking.hostId = "a4e735aa";
}

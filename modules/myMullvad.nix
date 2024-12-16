{
  lib,
  ...
}: {
  services.mullvad-vpn.enable = true;
  networking.firewall.checkReversePath = lib.mkForce "strict";
  # environment.systemPackages = with pkgs; [
  #   mullvad-client
  # ];

  # home-manager.users.${conUsername} = {config, ...}: {
  #   home.packages = with pkgs; [
  #     mullvad-client
  #   ];
  # };
}

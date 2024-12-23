{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  services.mullvad-vpn.enable = true;
  networking.firewall.checkReversePath = lib.mkForce "strict";
  home-manager.users.${conUsername} = {
    home.packages = with pkgs; [
      mullvad-vpn
    ];
  };
}

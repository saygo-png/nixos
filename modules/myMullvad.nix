{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  # https://github.com/mullvad/mullvadvpn-app/issues/3651
  systemd.services.mullvad-daemon.environment = {
    TALPID_NET_CLS_MOUNT_DIR = "/run/net-cls-v1";
  };

  services.mullvad-vpn = {
    enable = true;
    enableExcludeWrapper = false;
  };

  networking.firewall.checkReversePath = lib.mkForce "strict";
  home-manager.users.${conUsername} = {
    home.packages = with pkgs; [
      mullvad-vpn
    ];
  };
}

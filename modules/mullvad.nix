{lib, ...}: {
  # https://github.com/mullvad/mullvadvpn-app/issues/3651
  systemd.services.mullvad-daemon.environment = {
    TALPID_NET_CLS_MOUNT_DIR = "/run/net-cls-v1";
  };

  services.mullvad-vpn = {
    enable = true;
    enableExcludeWrapper = false;
    gui.enable = true;
  };

  networking.firewall.checkReversePath = lib.mkForce "strict";

  custom.persist.home.cache.directories = [".config/Mullvad VPN"];
}

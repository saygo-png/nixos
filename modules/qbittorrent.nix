{pkgs, ...}: {
  environment.systemPackages = [
    pkgs.qbittorrent
  ];

  custom.persist.home.directories = [
    ".config/qBittorrent"
    ".local/share/qBittorrent"
  ];

  services.fail2ban.enable = true;

  networking.firewall = rec {
    # Open port for qbittorrent.
    allowedTCPPorts = [39578];
    allowedUDPPorts = allowedTCPPorts;
  };
}

{pkgs, ...}: {
  environment.systemPackages = [
    pkgs.qbittorrent
  ];

  custom.persist.home.directories = [
    ".config/qBittorrent"
    ".local/share/qBittorrent"
  ];

  networking.firewall = rec {
    # Open port for qbittorrent.
    allowedTCPPorts = [39578];
    allowedUDPPorts = allowedTCPPorts;
  };
}

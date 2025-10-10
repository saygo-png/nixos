{conUsername, ...}: {
  virtualisation.incus = {
    enable = true;
    preseed = {
      networks = [
        {
          config = {
            "ipv4.address" = "auto";
            "ipv6.address" = "none";
          };
          name = "incusbr0";
          type = "bridge";
        }
      ];

      profiles = [
        {
          devices = {
            eth0 = {
              name = "eth0";
              network = "incusbr0";
              type = "nic";
            };
            root = {
              path = "/";
              pool = "default";
              type = "disk";
            };
          };
          name = "default";
          project = "default";
        }
      ];

      storage_pools = [
        {
          config.size = "50GiB";
          driver = "zfs";
          name = "default";
        }
      ];
    };
  };

  users.users.${conUsername}.extraGroups = ["incus-admin"];

  networking.nftables.enable = true;
  networking.firewall.interfaces.incusbr0 = {
    allowedTCPPorts = [53 67];
    allowedUDPPorts = [53 67];
  };

  custom.persist.root.cache.directories = ["/var/lib/incus"];
}

{config, ...}: {
  custom.persist = {
    root.directories = [
      "/srv"
      config.services.jellyfin.dataDir
    ];
    root.cache.directories = [config.services.jellyfin.cacheDir];
  };

  services = {
    jellyfin = {
      enable = true;
      openFirewall = true;
    };
  };
}

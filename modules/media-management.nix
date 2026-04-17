{config, ...}: {
  custom.persist = {
    root.directories = [
      config.services.jellyfin.dataDir
      config.services.prowlarr.dataDir
    ];
    root.cache.directories = [config.services.jellyfin.cacheDir];
  };

  services = {
    jellyfin = {
      enable = true;
    };

    prowlarr = {
      enable = true;
    };
  };
}

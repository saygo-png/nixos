{
  config,
  lib,
  conUsername,
  ...
}: {
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

  # Lets the desktop entry start (and you stop) Jellyfin without a password.
  security.polkit.extraConfig =
    # js
    ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.systemd1.manage-units" &&
            action.lookup("unit") == "jellyfin.service" &&
            ["start", "stop"].indexOf(action.lookup("verb")) >= 0 &&
            subject.user == "${conUsername}") {
          return polkit.Result.YES;
        }
      });
    '';

  # I rarely use it, so I want to start it on demand
  systemd.services.jellyfin.wantedBy = lib.mkForce [];
}

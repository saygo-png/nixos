{
  lib,
  pkgs,
  conHome,
  conUsername,
  ...
}: {
  imports = lib.my.withModules [
    "pipewire.nix"
    "qbittorrent.nix"
    "media-management.nix"
  ];

  networking.hostName = "pc";
  const = {
    refreshRate = 144;
    screenWidth = 1920;
    screenHeight = 1080;
    gaps = 0;
    borderSize = 2;
    vsync = false;
    extrasNixosPath = "${conHome}/builds/extrasNixos";
  };

  networking.hostId = "a4e735aa";

  # https://gitlab.freedesktop.org/drm/amd/-/issues/1500
  services.udev.extraRules = ''
    KERNEL=="card1", SUBSYSTEM=="drm", DRIVERS=="amdgpu", ATTR{device/power_dpm_force_performance_level}="manual", ATTR{device/pp_power_profile_mode}="1"
  '';

  # Change cpu governor to performance for increased performance.
  powerManagement.cpuFreqGovernor = "performance";

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # autologin
  services.getty.autologinUser = "${conUsername}";

  services.borgmatic = {
    enable = true;
    configurations.persist = {
      exclude_caches = true;
      source_directories = ["/persist"];
      exclude_patterns = ["*/.zfs/*"]; # Don't backup zfs snapshots
      unknown_unencrypted_repo_access_is_ok = true;
      repositories = [
        {
          path = "/media/backup/backup.borg";
          label = "local";
        }
      ];
      commands = [
        {
          before = "repository";
          run = ["findmnt /media/backup > /dev/null || exit 75"];
        }
      ];
      keep_monthly = -1;
    };
  };

  services.ratbagd.enable = true; # For piper
  environment.systemPackages = [
    pkgs.piper # For ratbagd
    pkgs.blender
  ];
}

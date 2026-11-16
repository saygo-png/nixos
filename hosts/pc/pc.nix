{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  imports = lib.my.withModules [
    "qbittorrent.nix"
    "media-management.nix"
  ];

  networking.hostName = "pc";
  networking.hostId = "a4e735aa";

  # https://gitlab.freedesktop.org/drm/amd/-/issues/1500
  services.udev.extraRules = ''
    KERNEL=="card[0-9]", SUBSYSTEM=="drm", DRIVERS=="amdgpu", ATTR{device/power_dpm_force_performance_level}="manual", ATTR{device/pp_power_profile_mode}="1"
  '';

  # Change cpu governor to performance for increased performance.
  powerManagement.cpuFreqGovernor = "performance";

  # The ZFS cache may by default take all RAM but 1 GiB, and gives it back slower
  # than the kernel swaps programs out, so games like Overwatch got pushed to swap.
  boot.kernelParams = let
    toGb = n: n * 1024 * 1024 * 1024;
    limit = toGb 2;
  in ["zfs.zfs_arc_max=${toString limit}"];

  hardware.graphics.enable = true;

  # autologin
  services.getty.autologinUser = conUsername;

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
  ];
}

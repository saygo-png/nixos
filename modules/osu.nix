{pkgs, ...}: {
  custom.persist.home.directories = [".local/share/osu"];
  custom.allowedUnfreePkgs = [pkgs.osu-lazer-bin];
  environment.systemPackages = [pkgs.osu-lazer-bin];
}

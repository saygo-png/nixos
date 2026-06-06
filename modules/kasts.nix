{pkgs, ...}: {
  custom.persist.home.cache.directories = [".local/share/KDE/kasts"];
  environment.systemPackages = [
    pkgs.kdePackages.kasts
  ];
}

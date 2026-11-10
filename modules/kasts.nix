{pkgs, ...}: {
  custom.persist.home.cache.directories = [".local/share/KDE/kasts"];
  custom.lazyPackages = [
    pkgs.kdePackages.kasts
  ];
}

{pkgs, ...}: {
  custom.persist.home = {
    files = [".config/kritarc" ".config/kritadisplayrc"];
    directories = [".local/share/krita"];
  };
  environment.systemPackages = [
    pkgs.krita
  ];
}

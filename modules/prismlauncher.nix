{
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    home.packages = [
      (pkgs.prismlauncher.override {
        jdks = [
          pkgs.temurin-jre-bin-8
          pkgs.temurin-jre-bin-17
          pkgs.temurin-jre-bin-21
          pkgs.temurin-jre-bin-25
        ];
      })
    ];
  };
}

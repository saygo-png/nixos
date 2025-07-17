{
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    home.packages = [
      (pkgs.prismlauncher.override {
        jdks = [
          # pkgs.temurin-bin-8
          # pkgs.temurin-bin-17
          pkgs.temurin-bin-21
        ];
      })
    ];
  };
}

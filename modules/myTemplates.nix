{
  lib,
  pkgs,
  conFlakePathRel,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    cookiecutter
  ];

  home-manager.users.${conUsername} = {config, ...}: {
    xdg.configFile."cookiecutters/" = {
      recursive = true;
      source = "${conFlakePathRel}/resources/cookiecutterTemplates";
    };
  };
}

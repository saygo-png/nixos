{
  lib,
  pkgs,
  inputs,
  conUsername,
  ...
}: {
  environment.systemPackages = with pkgs; [
    cookiecutter
  ];

  home-manager.users.${conUsername} = _: {
    xdg.configFile."cookiecutters/" = {
      recursive = true;
      source = lib.my.getSafePath "${inputs.cookiecutter-templates}/templates";
    };
  };
}

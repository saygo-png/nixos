{
  conUsername,
  conFlakePathRel,
  ...
}: {
  home-manager.users.${conUsername} = {
    qt = {
      enable = true;
      style.name = "kvantum";
      platformTheme.name = "qtct";
    };

    xdg.configFile = {
      "Kvantum/kvantum.kvconfig".text = ''
        [General]
        theme=gruvbox-fallnn
      '';

      "Kvantum/gruvbox-fallnn".source = "${conFlakePathRel}/resources/static/qt/gruvbox-fallnn";
    };
  };
}

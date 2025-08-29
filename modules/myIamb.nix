{conUsername, ...}: {
  custom.persist = {
    home = {
      cache = {
        directories = [
          ".local/share/iamb"
        ];
      };
    };
  };

  home-manager.users.${conUsername} = _: {
    programs.iamb = {
      enable = true;
      settings = {
        default_profile = "saygo";
        profiles.saygo.user_id = "@saygo.2:tchncs.de";
        layout.style = "restore";
        settings = {
          message_user_color = true;
          username_display = "localpart";
          user_gutter_width = 9;
          notifications = {
            enabled = true;
            via = "desktop";
            show_message = true;
          };
        };
        macros.normal = {
          ";" = ":";
          u = "\"+p";
          s = "<C-w>m";
          gx = ":open l<Enter>";
          mr = ":reply<Enter>";
          mq = ":cancel<Enter>y";
          mv = ":editor<Enter>";
        };
      };
    };
  };
}

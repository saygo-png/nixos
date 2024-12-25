{conUsername, ...}: {
  programs.thunar.enable = true;

  # Thumbnails for Thunar.
  services.tumbler.enable = true;

  # Virtual filesystem support
  services.gvfs.enable = true;

  home-manager.users.${conUsername} = _: {
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = "thunar.desktop";
        "inode/x-empty" = "thunar.desktop";
      };
    };
  };
}

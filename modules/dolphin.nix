{pkgs, ...}: {
  # Camera files support.
  programs.gphoto2.enable = true;

  environment.systemPackages = [
    pkgs.kdePackages.dolphin
    pkgs.kdePackages.ark
  ];

  # Fixes dolphin not having mime types.
  environment.etc."/xdg/menus/applications.menu".text =
    # This one also works but has to pull in entire plasma workspaces, but might be more future proof
    # builtins.readFile "${pkgs.kdePackages.plasma-workspace}/etc/xdg/menus/plasma-applications.menu";
    ''
      <!DOCTYPE Menu PUBLIC "-//freedesktop//DTD Menu 1.0//EN"
       "http://www.freedesktop.org/standards/menu-spec/1.0/menu.dtd">
      <Menu>
          <Name>Applications</Name>
          <DefaultAppDirs/>
          <DefaultDirectoryDirs/>
          <DefaultMergeDirs/>
      </Menu>
    '';
}

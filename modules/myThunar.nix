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
    xdg.configFile."Thunar/uca.xml".text =
      # XML
      ''
        <?xml version="1.0" encoding="UTF-8"?>
        <actions>
        <action>
        	<icon>utilities-terminal</icon>
        	<name>Open Terminal Here</name>
        	<submenu></submenu>
        	<unique-id>1734179588135391-1</unique-id>
        	<command>cd %f &amp;&amp; "$TERMINAL"</command>
        	<description>Example for a custom action</description>
        	<range></range>
        	<patterns>*</patterns>
        	<startup-notify/>
        	<directories/>
        </action>
        </actions>
      '';
  };
}

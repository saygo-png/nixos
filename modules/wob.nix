{
  conUsername,
  lib,
  self,
  system,
  config,
  ...
}: {
  home-manager.users.${conUsername} = {
    services.wob = {
      enable = true;
      settings."" = {
        anchor = "bottom center";
        margin = 50;
        bar_color = lib.mkForce config.lib.stylix.colors.base0B;
      };
    };

    systemd.user.sockets.volume = {
      Unit.Description = "Pipe for volume changes";
      Socket.ListenFIFO = "%t/volume.sock";
      Socket.SocketMode = "0600";
      Socket.RemoveOnStop = true;
      Install.WantedBy = ["sockets.target"];
    };

    systemd.user.services.volume = {
      Unit.Description = "Applies volume changes and shows them with wob";
      Service.StandardInput = "socket";
      Service.ExecStart = "${lib.getExe' self.packages.${system}.saygo-utils "volume"} daemon";
    };
  };
}

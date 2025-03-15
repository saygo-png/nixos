{
  pkgs,
  lib,
  ...
}: {
  # factory.name = support.null-audio-sink
  environment.etc."/pipewire/pipewire.conf.d/10-default-null-sink.conf".text =
    /*
    JSON-SPA
    */
    ''
      context.objects = [
        {
          factory = adapter
          args = {
            factory.name = support.null-audio-sink
            node.name = "virtual_mic"
            media.class = Audio/Source/Virtual
            node.description = "Virtual microphone device"
            audio.position = [ FL FR ]
            audio.channels = "2"
            monitor.channel-volumes = true
            monitor.passthrough = true
          }
        }
      ]
    '';

  # services.pipewire.wireplumber.extraConfig = {
  #   "virtual-mic-as-default-device" = {
  #     "monitor.bluez.rules" = [
  #       {
  #         matches = [
  #           {
  #             # Match any bluetooth device with ids equal to that of a WH-1000XM3
  #             "device.name" = "~bluez_card.*";
  #             "device.product.id" = "0x0cd3";
  #             "device.vendor.id" = "usb:054c";
  #           }
  #         ];
  #         actions = {
  #           update-props = {
  #             # Set quality to high quality instead of the default of auto
  #             "bluez5.a2dp.ldac.quality" = "hq";
  #           };
  #         };
  #       }
  #     ];
  #   };
  # };

  # node.restore-default-targets

  systemd.user.services."enable-carla-postprocessing" = {
    description = "Load Carla Rack JACK host";
    wantedBy = ["default.target"];
    environment = {PIPEWIRE_LINK_PASSIVE = "true";};
    serviceConfig = {
      type = "exec";
      ExecStart = lib.mkForce "${lib.getExe' pkgs.carla "carla-rack"} --no-gui %h/Documents/carla.carxp";
    };
  };

  # home-manager.users.${conUsername} = {config, ...}: {
  # };
}

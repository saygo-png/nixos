# Update main.device to match the main disk name from lsblk
{
  disko.devices = {
    disk = {
      main = {
        device = "/dev/disk/by-uuid/74e85579-cadb-405e-98af-e30f64b98b4f";
        type = "disk";
        content = {
          type = "gpt";

          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = ["umask=0077"];
              };
            };

            swap = {
              size = "50G";
              content = {
                type = "swap";
                discardPolicy = "both";
                resumeDevice = true; # resume from hiberation from this device
              };
            };

            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}

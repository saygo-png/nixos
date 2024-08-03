{
  lib,
  host,
  pkgs,
  config,
  inputs,
  conHome,
  conUsername,
  conFlake-path,
  pkgs-unstable,
  conAccentColor,
  conRefresh-rate,
  conScreen-width,
  conScreen-height,
  ...
}: {
  # Battery saving.
  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };
}

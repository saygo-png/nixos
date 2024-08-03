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
    extraPackages = with pkgs; [
      intel-vaapi-driver
      intel-media-driver
      vpl-gpu-rt
    ];
  };
  boot.kernelParams = [
    # Disable "Panel Self Refresh".  Fix random freezes.
    "i915.enable_psr=0"
    "i915.enable_fbc=1"
    "i915.modeset=0"
  ];
  boot.initrd.kernelModules = ["i915"];
  boot.kernelModules = ["acpi_call"];
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  hardware.cpu.intel.updateMicrocode = config.hardware.enableRedistributableFirmware;

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
  };
}

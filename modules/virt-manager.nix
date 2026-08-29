{pkgs, ...}: {
  custom.persist.root.cache.directories = ["/var/lib/libvirt"];

  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;

  networking.firewall.trustedInterfaces = ["virbr0"];
  environment.systemPackages = [
    pkgs.dnsmasq
  ];

  virtualisation.spiceUSBRedirection.enable = true;
}

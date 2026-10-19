{pkgs, ...}: {
  environment.systemPackages = [
    (pkgs.nsxiv.overrideAttrs (old: {
      patches = (old.patches or []) ++ [./config.patch];
    }))
  ];
}

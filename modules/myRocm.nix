{pkgs, ...}: {
  systemd.tmpfiles.rules = let
    rocmEnv = pkgs.symlinkJoin {
      name = "rocm-combined";
      paths = with pkgs.rocmPackages; [
        rocblas
        hipblas
        clr
      ];
    };
  in [
    "L+    /opt/rocm   -    -    -     -    ${rocmEnv}"
  ];

  nixpkgs.config.rocmSupport = true;

  hardware = {
    amdgpu.opencl.enable = true;
    graphics = {
      extraPackages = with pkgs; [
        rocmPackages.clr.icd
        rocmPackages.clr
      ];
    };
  };
}

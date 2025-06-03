{
  self,
  pkgs,
  ...
}: {
  nixpkgs.overlays = [
    (_final: prev: {
      vesktop = prev.vesktop.overrideAttrs (oldAttrs: {
        preConfigurePhases = oldAttrs.preConfigurePhases or [] ++ ["patchImages"];

        patchImages = ''
          rm static/shiggy.gif

          substituteInPlace static/views/splash.html \
            --replace-fail shiggy.gif loading.gif

          install -m0644 ${self}/resources/vesktop/loading.gif static/
        '';
      });
    })
  ];

  custom.allowedUnfreePkgs = [pkgs.vesktop];

  environment.systemPackages = [
    pkgs.vesktop
  ];
}

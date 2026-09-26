{
  pkgs,
  lib,
  ...
}: {
  environment.systemPackages = let
    overrideContent = {patches = [(lib.my.relativeToRoot "modules/nsxiv/config.patch")];};
    patchedNsxiv = pkgs.nsxiv.overrideAttrs (_prev: _final: overrideContent);
  in [patchedNsxiv];
}

{lib, ...}: rec {
  # Use path relative to the root of the project
  # Yes this can be point free
  relativeToRoot = stringRelativePath: getSafePath (lib.path.append ../. stringRelativePath);

  withModules = map (rest: relativeToRoot ("modules/" + rest));

  getSafePath = path:
    if !(builtins.pathExists path)
    then throw "Path ${path} does not exist"
    else path;

  # Website opened as a standalone chromium app
  mkChromiumKiosk = pkgs: {
    name,
    url,
    flags ? [],
  }:
    pkgs.writeShellScriptBin name ''
      exec ${lib.getExe pkgs.ungoogled-chromium} ${lib.escapeShellArgs ([
          "--no-first-run"
          "--disable-features=TranslateUI"
          "--no-default-browser-check"
        ]
        ++ flags
        ++ ["--app=${url}"])}
    '';
}

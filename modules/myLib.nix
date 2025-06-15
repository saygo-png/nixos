{lib, ...}: rec {
  # Use path relative to the root of the project
  # Yes this can be point free
  relativeToRoot = stringRelativePath: getSafePath (lib.path.append ../. stringRelativePath);

  withModules = builtins.map (rest: relativeToRoot ("modules/" + rest));

  getSafePath = path:
    if !(builtins.pathExists path)
    then builtins.throw "Path ${path} does not exist"
    else path;
}

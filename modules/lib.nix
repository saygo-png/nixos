{lib, ...}: rec {
  # Use path relative to the root of the project
  # Yes this can be point free
  relativeToRoot = stringRelativePath: getSafePath (lib.path.append ../. stringRelativePath);

  withModules = map (rest: relativeToRoot ("modules/" + rest));

  getSafePath = path:
    if !(builtins.pathExists path)
    then throw "Path ${path} does not exist"
    else path;

  nWrapFunc = s: "function() ${s} end";
}

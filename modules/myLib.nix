{lib, ...}: {
  # Use path relative to the root of the project
  # Yes this can be point free
  relativeToRoot = stringRelativePath: lib.path.append ../. stringRelativePath;
}

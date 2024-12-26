{ lib, ... }:
{
  # Use path relative to the root of the project
  relativeToRoot = lib.path.append ../.;
}

{conFlakePath, ...}: {
  programs.nh = {
    enable = true;
    flake = conFlakePath;
  };

  # Save build times for loading bars
  custom.persist.home.cache.directories = [".local/state/rom"];
}

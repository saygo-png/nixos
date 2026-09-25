{
  pkgs,
  conUsername,
  ...
}: {
  users.users.${conUsername}.extraGroups = ["wireshark"];
  programs.wireshark.enable = true;

  environment.systemPackages = [
    pkgs.wireshark
  ];
}

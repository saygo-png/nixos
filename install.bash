#!/usr/bin/env bash
# On the install iso, put a password file made with `mkpasswd` in /etc/password.hash
# Run this from inside my dotfiles directory root since path to flake is relative.
# Put the disk path like /dev/nvme0n1 into the variable below

DISK=UNDEFINED
CONFIG=UNDEFINED

[ "$DISK" = "UNDEFINED" ] && exit
[ "$CONFIG" = "UNDEFINED" ] && exit

PASSWORD_FILE=/etc/password.hash

wipefs -a "$DISK"
blkdiscard -f "$DISK"
sgdisk --zap-all "$DISK"
zpool labelclear -f "$DISK"

sudo nix run 'github:nix-community/disko/latest#disko-install' \
  --extra-experimental-features "nix-command flakes" -- \
  --flake .#"$CONFIG" --disk main "$DISK" \
  --extra-files "$PASSWORD_FILE" /etc/password.hash

#!/usr/bin/env bash
# On the install iso, put a password file made with `mkpasswd` in /etc/password.txt
# Run this from inside my dotfiles directory root since path to flake is relative.
# Put the disk path like /dev/nvme0n1 into the variable below

DISK=UNDEFINED

[ "$DISK" = "UNDEFINED" ] && exit

PASSWORD_FILE=/etc/password.txt

wipefs -a "$DISK"
blkdiscard -f "$DISK"
sgdisk --zap-all "$DISK"
zpool labelclear -f "$DISK"

sudo nix run 'github:nix-community/disko/latest#disko-install' \
  --extra-experimental-features "nix-command flakes" -- \
  --flake .#install --disk main "$DISK" \
  --extra-files "$PASSWORD_FILE" /etc/password.txt

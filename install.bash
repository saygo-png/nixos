#!/usr/bin/env bash
# Run this from inside my dotfiles directory root since path to flake is relative.
# Put the disk path like /dev/nvme0n1 into the variable below

DISK=UNDEFINED
CONFIG=UNDEFINED

[ "$DISK" = "UNDEFINED" ] && exit
[ "$CONFIG" = "UNDEFINED" ] && exit
nixos-install --flake 'path/to/flake.nix#nixos'
wipefs -a "$DISK"
blkdiscard -f "$DISK"
sgdisk --zap-all "$DISK"
zpool labelclear -f "$DISK"

sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko/latest -- --mode destroy,format,mount PATH_TO_DISKO_CONFIG

nixos-install --flake 'path/to/flake.nix#HOST-install'

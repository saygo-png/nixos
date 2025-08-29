#!/usr/bin/env bash
# Put the disk path like /dev/nvme0n1 into the variable below.
# This wipes a lot of data. Do not run this if u don't understand what it's doing.

DISK=UNDEFINED
CONFIG=UNDEFINED
CONFIG_PATH="/home/nixos/nixos"

[[ ${DISK} = UNDEFINED ]] && { echo "DISK not defined"; exit 1; }
[[ ${CONFIG} = UNDEFINED ]] && { echo "CONFIG not defined"; exit 1; }
[[ ${EUID} -ne 0 ]] && { echo "Script must be ran as root"; exit 1; }

zpool destroy zroot # Might fail if it doesn't exist, that's okay.
wipefs -a "${DISK}" || exit 1
blkdiscard -f "${DISK}" || exit 1
sgdisk --zap-all "${DISK}" || exit 1

sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko/latest -- \
  --mode destroy,format,mount "${CONFIG_PATH}/hosts/${CONFIG}/disko-config.nix" || exit 1

nixos-install --flake "${CONFIG_PATH}#${CONFIG}-install"

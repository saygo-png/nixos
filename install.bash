#!/usr/bin/env bash
# On the install iso, put a password file made with `mkpasswd` in /etc/password.txt
# Run this from inside my dotfiles directory root since path to flake is relative.

sudo nix run 'github:nix-community/disko/latest#disko-install' \
  --extra-experimental-features "nix-command flakes" -- \
  --flake .#install --disk main DISKDEVICEHERE \
  --extra-files /etc/password.txt /etc/password.txt

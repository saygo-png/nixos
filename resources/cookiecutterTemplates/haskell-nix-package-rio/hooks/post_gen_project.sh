#!/usr/bin/env bash

# We don't use relative values here to avoid "chmod -R"ing
# unexpected paths if something goes wrong in cookiecutter

chmod +w default.nix
chmod +w Main.hs
chmod +w shell.nix
chmod +w '{{cookiecutter.binaryName}}.cabal'

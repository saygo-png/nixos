#!/usr/bin/env bash

# We don't use relative values here to avoid "chmod -R"ing
# unexpected paths if something goes wrong in cookiecutter

chmod +w app/Main.hs
chmod +w bench/Main.hs
chmod +w src/Lib.hs
chmod +w test/Spec.hs

chmod +w .envrc
chmod +w .gitignore
chmod +w .hlint.yaml
chmod +w LICENSE
chmod +w README.md
chmod +w cabal.project
chmod +w cabal.project.local
chmod +w flake.lock
chmod +w flake.nix
chmod +w test.json
chmod +w '{{cookiecutter.binaryName}}.cabal'

#!/usr/bin/env bash
if ! [ -x "$(command -v nix-env)" ] || [ "$(uname -s)" != 'Linux' ]; then
  echo "Error: Install nix package manager on Linux to continue: https://nixos.org/nix/.
For other operating systems build the project with stack(https://haskellstack.org)." >&2
  exit 1;
fi
nix-env -i $("$(nix-build --no-link -A fullBuildScript)")

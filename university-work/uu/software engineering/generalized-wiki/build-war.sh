#!/bin/sh 

gwprofile=/nix/var/nix/profiles/$USERNAME-gw-server

set -e
if test -e nix/pkgs; then
  echo "Building gw ..."
else
  echo "error: you must install nixpkgs first."
  exit 1
fi

nix-env -i -p  $gwprofile -f nix/gw/linux.nix -E 'gw: gw.war'

#! /bin/sh 

gwprofile=/nix/var/nix/profiles/$USERNAME-gw-server

set -e
if test -e nix/pkgs; then
  echo "Building gw installation ..."
else
  echo "error: you must install nixpkgs first."
  exit 1
fi

nix-env -i -p $gwprofile -f nix/server-instance.nix gw-server

echo "Starting jetty ..."
$gwprofile/bin/start-jetty

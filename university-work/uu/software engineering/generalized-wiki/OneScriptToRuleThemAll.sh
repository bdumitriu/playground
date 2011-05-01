#!/bin/sh

# Change to homedir
cd ~

# Something Nixy
source /nix/etc/profile.d/nix.sh

# Checkout the trunk into the 'gw' directory
svn checkout https://svn.cs.uu.nl:12443/repos/gw/trunk /tmp/gw

# Goto the gw directory
cd /tmp/gw

#  Install Nix Packages
./update-nixpkgs.sh

# Start the GW-server
./startup.sh

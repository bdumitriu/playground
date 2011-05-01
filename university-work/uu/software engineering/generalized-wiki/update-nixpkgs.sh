#!/bin/sh

set -e

channel=http://nix.cs.uu.nl/dist/courses/channels-v3/nixpkgs-swe-unstable

cd nix

echo "Cleaning up ..."
rm -rf nixpkgs-swe-*
rm -f nixexprs.tar.bz2

echo "Downloading fresh Nix expressions ..."
wget $channel/nixexprs.tar.bz2

echo "Unpacking the fresh Nix expressions ..."
tar jxvf nixexprs.tar.bz2

echo "Installing the fresh Nix expressions ..."
rm -rf pkgs
mv nixpkgs-swe-*/pkgs .

echo "Pulling the new manifest ..."
nix-pull $channel/MANIFEST

echo "Cleaning up ..."
rm -rf nixpkgs-swe-*
rm -f nixexprs.tar.bz2

cd ..

echo "Installing libraries for Eclipse ..."
nix-env -i -p /nix/var/nix/profiles/junit -f nix/pkgs/system/i686-linux.nix junit
nix-env -i -p /nix/var/nix/profiles/jetty -f nix/pkgs/system/i686-linux.nix jetty
nix-env -i -p /nix/var/nix/profiles/jdom -f nix/pkgs/system/i686-linux.nix jdom
nix-env -i -p /nix/var/nix/profiles/jing -f nix/pkgs/system/i686-linux.nix jing
nix-env -i -p /nix/var/nix/profiles/commons-fileupload -f nix/pkgs/system/i686-linux.nix commons-fileupload
nix-env -i -p /nix/var/nix/profiles/lucene -f nix/pkgs/system/i686-linux.nix lucene
nix-env -i -p /nix/var/nix/profiles/subversion-java -f nix/pkgs/system/i686-linux.nix -E 'pkgs: pkgs.subversionWithJava'
nix-env -i -p /nix/var/nix/profiles/httpunit -f nix/pkgs/system/i686-linux.nix httpunit
nix-env -i -p /nix/var/nix/profiles/mockobjects -f nix/pkgs/system/i686-linux.nix mockobjects
nix-env -i -p /nix/var/nix/profiles/javasvn -f nix/pkgs/system/i686-linux.nix javasvn
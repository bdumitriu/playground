#!/bin/sh

if [ $# -ne 1 ]
then
        echo "Usage: $0 path-to-coreutils"
        exit 1
fi

repo=/tmp/test-svn-repo

echo "----------------------"
echo "Stopping SVN server..."
echo "----------------------"

coreutils=$1/bin

$coreutils/kill `cat /tmp/svnserve.pid`

rm -rf $repo
rm -f /tmp/svnserve.pid

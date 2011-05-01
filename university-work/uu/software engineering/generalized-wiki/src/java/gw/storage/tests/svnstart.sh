#!/bin/bash

# the port on which the SVN server is started
SVN_SERVER_PORT=10000

if [ $# -ne 2 ]
then
        echo "Usage: $0 path-to-svn path-to-config-files"
        exit 1
fi

echo "----------------------"
echo "Starting SVN server..."
echo "----------------------"

svn=$1/bin
confdir=$2
repo=/tmp/test-svn-repo

rm -rf $repo
$svn/svnadmin create --fs-type fsfs $repo
cp -f $confdir/svnserve.conf $repo/conf/svnserve.conf
cp -f $confdir/passwd $repo/conf/passwd
$svn/svnadmin load $repo < $confdir/dump-file
$svn/svnserve -d --foreground -r $repo --listen-port $SVN_SERVER_PORT &
svnservepid=$!

echo -n $svnservepid > /tmp/svnserve.pid

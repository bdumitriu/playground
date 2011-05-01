#!/bin/sh -e

make -s $1.ant
make -s $2.ant
../simulator/simulator ../maps/$3.world $1.ant $2.ant 12345 WantProfile $4 $5 > $1-$2-$3-$4-$5.profile
tail -1 $1-$2-$3-$4-$5.profile | sed 's:profile::g'
#echo "View pofile graph? [Yn]"
#read v
#if [ "$v" != "n" ] && [ "$v" != "N" ]
#then
#    exec ./plot.sh $1-$2-$3-$4-$5.profile
#fi

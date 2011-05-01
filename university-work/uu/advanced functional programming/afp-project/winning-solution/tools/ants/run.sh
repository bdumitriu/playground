#!/bin/sh -e

make $1.ant
make $2.ant
../simulator/simulator ../maps/$3.world $1.ant $2.ant 12345 WantCommands $4 $5 +RTS -K10M > $1-$2-$3-$4-$5.dump
grep VScore $1-$2-$3-$4-$5.dump | tail -1
perl ./find_spin.pl < $1-$2-$3-$4-$5.dump
echo "Visualise? [Yn]"
read v
if [ "$v" != "n" ] && [ "$v" != "N" ]
then
    ../visualizer/visualizer $1-$2-$3-$4-$5.dump +RTS -K10M
fi


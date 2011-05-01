#!/bin/sh -e

make $1.ant
make $2.ant
../simulator/simulator ../maps/$3.world $1.ant $2.ant 12345 WantScores $4 > $1-$2-$3-$4.dump
tail -1 $1-$2-$3-$4.dump | tail -1


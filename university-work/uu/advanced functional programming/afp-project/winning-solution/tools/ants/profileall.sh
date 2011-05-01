#!/bin/bash -e

make $1.ant
make $2.ant

for mapnum in `seq 0 9`
do
    ./profile.sh $1 $2 sample$mapnum 100000 -1
done


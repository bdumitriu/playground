#!/bin/bash -e

make $1.ant
make $2.ant

for mapnum in `seq 0 9`
do
    map=../maps/sample$mapnum.world
    ../simulator/simulator $map $1.ant $2.ant 12345 WantScores $3 -1
done | {
redtotal=0
blacktotal=0
for mapnum in `seq 0 9`
do
    read
    read red
    read
    read black
    sum=$(($red+$black))
    printf "On $mapnum:   Red: %-6d Black: %-6d Sum: %-6d\n" $red $black $sum
    redtotal=$(($redtotal+$red))
    blacktotal=$(($blacktotal+$black))
done
sum=$(($redtotal+$blacktotal))
printf "Totals: Red: %-6d Black: %-6d Sum: %-6d\n" $redtotal $blacktotal $sum
}


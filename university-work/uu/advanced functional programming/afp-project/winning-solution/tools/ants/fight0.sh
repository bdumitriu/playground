#!/bin/bash -e

make $1.ant
make $2.ant

map=../maps/sample0.world
../simulator/simulator $map $1.ant $2.ant 12345 WantScores $3
../simulator/simulator $map $2.ant $1.ant 12345 WantScores $3


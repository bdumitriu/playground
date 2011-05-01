#!/bin/bash

F2=../../alter_leave_prob/icfp2004/ants/$1
F3=../../alter_leave_dir/icfp2004/ants/$1
F4=../../alter_dir_leave_prob/icfp2004/ants/$1
PLOT1="\"$1\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %lf, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Black score\""
PLOT2="\"$F2\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %lf, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Black score prob\""
PLOT3="\"$F3\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %lf, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Black score dir\""
PLOT4="\"$F4\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %lf, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Black score prob dir\""
SCRIPT="set key top left; plot $PLOT1, $PLOT2, $PLOT3, $PLOT4"

echo $SCRIPT > plot.tmp
gnuplot -persist plot.tmp


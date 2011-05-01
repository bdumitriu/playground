#!/bin/bash

PLOT1="\"$1\" using \"VProfile {profileRedScore = %lf, profileBlackScore = %*f, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Red score\""
PLOT2="\"$1\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %lf, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Black score\""
PLOT3="\"$1\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %*f, profileRedPickups = %lf, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Red pickups\""
PLOT4="\"$1\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %*f, profileRedPickups = %*f, profileBlackPickups = %lf, profileRedDeaths = %*f, profileBlackDeaths = %*f}\" title \"Black pickups\""
PLOT5="\"$1\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %*f, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %lf, profileBlackDeaths = %*f}\" title \"Red deaths\""
PLOT6="\"$1\" using \"VProfile {profileRedScore = %*f, profileBlackScore = %*f, profileRedPickups = %*f, profileBlackPickups = %*f, profileRedDeaths = %*f, profileBlackDeaths = %lf}\" title \"Black deaths\""
SCRIPT="set key top left; plot $PLOT1, $PLOT2, $PLOT3, $PLOT4, $PLOT5, $PLOT6"

echo $SCRIPT > plot.tmp
gnuplot -persist plot.tmp


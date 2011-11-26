#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 07.09.2000
# Function: reads text from the file dummy-file and prints it on the screen.
# Is working: yes
# Yet to be done: all done 

unless (open(FILE, "./files/dummy-file"))
{
    die("Error opening file!\n");
}

# An alternative to the above 4 lines would've been:
#	open(FILE, "./files/dummy-file") || die("Error opening file!\n);

{
    $line = <FILE>;
    while ($line ne "")
    {
	print($line);
	$line = <FILE>;
    }
}

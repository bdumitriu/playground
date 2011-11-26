#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 07.09.2000
# Function: reads text from the file dummy-file and writes it in a file 
#	    the user will desire.
# Is working: yes
# Yet to be done: all done 

print("The name of the file to write/append to: ");
$name = <STDIN>;
chop($name);
print("Do you wish to write or append to the file (w/a)? ");
$choice = <STDIN>;
chop($choice);
if ($choice eq "a")
{
    $name = ">>".$name;
}
elsif ($choice eq "w")
{
    $name = ">".$name;
}
else
{
    die("Choice not available!\n");
}
open(INFILE, "./files/dummy-file") || die ("Error opening input file!\n");
open(OUTFILE, "$name") || die("Error opening output file!\n");
@array = <INFILE>;
print OUTFILE (@array);
print("File written.\n");
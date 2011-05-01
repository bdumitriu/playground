#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 13.09.2000
# Function: writes all the possible combinations of 2 of the characters a-z,
#	    A-Z, 0-9, / and . to a file.
# Is working: yes
# Yet to be done: all done 

# create the list
$index = 0;
for ($i = "a"; $i lt "z"; $i++)
{
    $list[$index++] = $i;
}
$list[$index++] = "z";
for ($i = "A"; $i lt "Z"; $i++)
{
    $list[$index++] = $i;
}
$list[$index++] = "Z";
for ($i = "0"; $i lt "9"; $i++)
{
    $list[$index++] = $i;
}
$list[$index++] = "9";
$list[$index++] = ".";
$list[$index++] = "/";

$| = 1;
open(FILE, ">./files/a-zA-Z0-9");
print("Processing...  ");
for ($i = 0; $i < $index; $i++)
{
    for ($j = 0; $j < $index; $j++)
    {
	print FILE ("$list[$i]$list[$j]\n");
    }
    if ($i%4 == 0)
    {
	print("\b\|");
    }
    elsif ($i%4 == 1)
    {
	print("\b\\");
    }
    elsif ($i%4 == 2)
    {
	print("\b\-");
    }
    else
    {
	print("\b\/");
    }
}
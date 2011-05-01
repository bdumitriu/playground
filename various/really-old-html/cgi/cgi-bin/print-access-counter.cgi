#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 25.09.2000
# Function: prints an access counter for a web page
# Is working: yes
# Yet to be done: implement the actual counter (with a file and everything)
#		  (so far, I just assign a value to $counter and print it)

@digits = ("3c 66 66 66 66 66 66 66 66 3c",	#0
	   "30 38 30 30 30 30 30 30 30 30",	#1
	   "3c 66 60 60 30 18 0c 06 06 7e",	#2
	   "3c 66 60 60 38 60 60 60 66 3c",	#3
	   "30 30 38 38 34 34 32 7e 30 78",	#4
	   "7e 06 06 06 3e 60 60 60 66 3c",	#5
	   "38 0c 06 06 3e 66 66 66 66 3c",	#6
	   "7e 66 60 60 30 30 18 18 0c 0c",	#7
	   "3c 66 66 66 3c 66 66 66 66 3c",	#8
	   "3c 66 66 66 66 7c 60 60 30 1c");	#9

$count = 53741829;

# the following section builds the @display-bitmap array
$formattedcount = sprintf("%08d", $count);
print($formattedcount);
for ($y = 0; $y < 10; $y++)
{
    for ($x = 0; $x < 8; $x++)
    {
	$digit = substr($formattedcount, $x, 1);
	$byte = substr(@digits[$digit], $y*3, 2);
	push(@displaybitmap, $byte);
    }
}

# the following section does the actual printing in web-server format of the 
# array
print("Content-type: image/x-xbitmap\n\n");
print("#define count_width 64\n#define count_height 10\n");
print("static char count_bits[]=\n{\n");
$size = @displaybitmap;
for ($i = 0; $i < $size-1; $i++)
{
    print("0x$displaybitmap[$i],");
    if (($i+1)%8 == 0)
    {
	print("\n");
    }
}
print("0x$displaybitmap[$i]\n};\n");
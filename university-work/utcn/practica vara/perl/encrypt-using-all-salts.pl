#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 13.09.2000
# Function: Encrypts a given password using all possible salts. It uses the
#	    a-zA-Z0-9 file (which contains all possible salts) to do that. If it
#	    doesn't exist use the program write-a-zA-Z0-9-combinations to
#	    create it (or create your own file, either manually (which I really
#	    don't reccomand) or writing your own program if you don't like/have
#	    mine.
# Is working: yes
# Yet to be done: all done 

open(SALTSFILE, "./files/a-zA-Z0-9") || 
die("Can't open input file containing salts: \.\/files\/a-zA-Z0-9\n");

open(ENCPASSFILE, ">./files/password-encrypted-with-all-salts") ||
die("Can't open output file \.\/files\/password-encrypted-with-all-salts\n");

print("Enter a password (it will be seen on the screen)\n here: ");
$passwd = <STDIN>;
chop($passwd);
print("Encrypting... Please wait\n");
while ($salt = <SALTSFILE>)
{
    chop($salt);
    $pass = crypt($passwd, $salt);
    print ENCPASSFILE ("$pass\n");
}
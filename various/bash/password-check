#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 13.09.2000
# Function: asks the user to enter 5 passwords and shows the way it encrypts
#	    them using the "ef" salt for the crypt function.
# Is working: yes
# Yet to be done: all done 

for ($i = 0; $i < 5; $i++)
{
    print("Enter a password: ");    
    system("stty -echo");    
    $passwd = <STDIN>;
    chop($passwd);
    system("stty echo");
    print("\nPassword is: $passwd\n");
    $crpasswd = crypt($passwd, "ef");
    print("Encrypted password is: $crpasswd\n");    
}
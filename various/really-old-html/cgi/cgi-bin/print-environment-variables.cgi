#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 22.09.2000
# Function: Prints all the environment variables and the QUERY_STRING.
# Is working:
# Yet to be done: all done

use CGI;
&CGI::ReadParse(*input);

print("Content-type: text/html\n\n");
foreach $i (keys(%ENV))
{
    print << "    END";
    <STRONG>$i</STRONG> = $ENV{$i}
    <BR>
    END
}

foreach $i (keys(%input))
{
    print << "    END";
    $i = $input{$i}
    <BR>
    END
}

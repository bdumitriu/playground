#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 12.09.2000
# Function: illustrates how sort can be used with an user-defined function.
# Is working: yes
# Yet to be done: all done 

# The user defined function must use the global variables $a and $b. It has
# to return -1 if $a < $b, 0 if $a = $b, 1 if $a > $b. Then the call to the
# function sort must be:
#
#	sort user_function_name (@array_variable)
#

print("please type in several lines of input, terminating with C-d: \n");

# read what the user writes...
while ($line = <STDIN>)
{
    # replace all upper-case w/ lower-case
    $line =~ tr/A-Z/a-z/;
    # delete all leading and trailing spaces
    $line =~ s/(^\s*)|(\s*\n$)//g;
    # replace all punctuation w/ single spaces
    $line =~ s/\W/ /g;
    # get all the words from the line of input in the array @words
    @words = split(/\s+/, $line);
    # update associative array %occurrences
    foreach $word (@words)
    {
	$occurrences{$word} += 1;
    }
}

# print the number of occurrences for each word
foreach $i (sort sort_function(keys(%occurrences)))
{
    print("$i: $occurrences{$i} occurrences\n");
}

# the function I was talking about in the beginning...
sub sort_function
{
    if ($occurrences{$a} < $occurrences{$b})
    {
	return -1;
    }
    elsif ($occurrences{$a} == $occurrences{$b})
    {
	return 0;
    }
    else
    {	
	return 1;
    }
    
    # instead of all the lines above I could've used
    #
    #		$occurrences{$a} <=> $occurrences{$b};
    #
    # and the result would've been the same, but I didn't for the sake of 
    # clarity. (remember Perl returns the last value assigned if a return 
    # is not found, so indeed return is not needed above. However, if you
    # find that hard to concieve, you could say:
    #
    #		$return_value = $occurrences{$a} <=> $occurrences {$b};
    #		return $return_value;
    #
    # The effect would be the same.)
}
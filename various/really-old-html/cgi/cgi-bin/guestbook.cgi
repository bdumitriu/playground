#!/usr/bin/perl

# Author: John Callender
# Date: 18.09.2000
# Function: 
# Is working: no
# Yet to be done: everything

$data_file = '/home/bdumitriu/http/data-files/entries.txt';

$max_entries = 0; # how many guestbook entries to save?
                  # set to '0' (zero) for infinite entries...

use CGI;
use Fcntl;
$query = new CGI;

unless ($action = $query->param('action')) 
{
    $action = 'none';
}

print <<"EndOfText";
Content-type: text/html

<HTML>
<HEAD>
<TITLE>My guestbook</TITLE>
</HEAD>
<BODY>
<H1>My guestbook</H1>

<P>Here's my guestbook. You can <A HREF="#form">add 
your own comment</A> using the form at the bottom 
of the page.</P>
<HR>
EndOfText

if ($action eq 'Add comment') 
{

    # process the form submission
    # and assemble the guestbook entry

    $name = $query->param('name');
    $city = $query->param('city');
    $state = $query->param('state');
    $country = $query->param('country');
    $comment = $query->param('comment');

    # clean up and fiddle with $name
    unless ($name) 
    {
        $name = 'Anonymous';
    }
    if (length($name) > 50) 
    {
        $name = 'Someone with a really long name';
    }

    # disable all HTML tags
    $name =~ s/</&lt;/g;

    # untaint variable
    # the following expression matches the whole string except if it contains 
    # <-s (less than characters, which is pretty unlikely since we replaced 
    # them before
    unless ($name =~ /^([^<]*)$/) 
    {
        die "couldn't untaint name: $name\n";
    }
    $name = $1;

    # clean up and fiddle with $from_where
    # all the removing that follows is done in case some or all of the fields
    # necessary are left unfilled by the user
    $from_where = "$city, $state, $country";
    $from_where =~ s/, , /, /;        # remove duplicate ', '
    $from_where =~ s/^, //;           # remove initial ', '
    $from_where =~ s/, $//;           # remove final ', '
    
    if ($from_where =~ /^[,\s]*$/) 
    {
        # nothing but commas and whitespace
        $from_where = 'parts unknown';
    }
	
    if (length($from_where) > 75) 
    {
        $from_where = 'somewhere with a really long name';
    }

    # disable HTML tags
    $from_where =~ s/</&lt;/g;

    # untaint variable
    unless ($from_where =~ /^([^<]*)$/) 
    {
        die "couldn't untaint from_where: $from_where\n";
    }
    $from_where = $1;

    # clean up and fiddle with $comment
    if (length($comment) > 32768) 
    {
        $comment = '...more than I feel like posting in my guestbook.';
    }
    unless ($comment) 
    {
        $comment = '...nothing to speak of.';
    }

    # fix line-endings
    $comment =~ s/\r\n?/\n/g;

    # lose HTML tags
    $comment =~ s/</&lt;/g;

    # untaint variable
    unless ($comment =~ /^([^<]*)$/) 
    {
        die "couldn't untaint comment: $comment\n";
    }
    $comment = $1;

    # assemble finished guestbook entry
    $entry = <<"EndOfText";
<P><STRONG>$name</STRONG> <EM>from $from_where wrote:</EM><BR>
<BLOCKQUOTE>$comment</BLOCKQUOTE></P>
<HR>
EndOfText

    # open non-destructively, read old entries, write out new
    sysopen(ENTRIES, "$data_file", O_RDWR) or die "can't open $data_file: $!";
    flock(ENTRIES, 2) or die "can't LOCK_EX $data_file: $!";
    while(<ENTRIES>) 
    {
        $all_entries .= $_;
    }
    $all_entries .= $entry;

    if ($max_entries) 
    {
        # lop the head off the guestbook, if necessary
        @all_entries = split(/<HR>/i, $all_entries);
        $entry_count = @all_entries - 1;

        while ($entry_count > $max_entries) 
	{
            shift @all_entries;
            $entry_count = @all_entries - 1;
        }

        $all_entries = join('<HR>', @all_entries);
    }

    # now write out to $data_file
    seek(ENTRIES, 0, 0) or die "can't rewind $data_file: $!";
    truncate(ENTRIES, 0) or die "can't truncate $data_file: $!";
    print ENTRIES $all_entries or die "can't print to $data_file: $!";
    close(ENTRIES) or die "can't close $data_file: $!";
}

# display the guestbook
open (IN, "$data_file") or die "Can't open $data_file for reading: $!";
flock(IN, 1) or die "Can't get LOCK_SH on $data_file: $!";
while (<IN>)
{
    print;
}
close IN or die "Can't close $data_file: $!";

# display the form	
print <<"EndOfText";
<A NAME="form"><H2>Add a comment to the guestbook (no HTML):</H2></A>

<FORM METHOD="POST" ACTION="guestbook">
<TABLE>

<TR>
<TD ALIGN="right"><STRONG>Name:</STRONG></TD>
<TD><INPUT NAME="name" SIZE=30></TD>
</TR>

<TR>
<TD ALIGN="right"><STRONG>City:</STRONG></TD>
<TD><INPUT NAME="city" SIZE=30></TD>
</TR>

<TR>
<TD ALIGN="right"><STRONG>State:</STRONG></TD>
<TD><INPUT NAME="state" SIZE=30></TD>
</TR>

<TR>
<TD ALIGN="right"><STRONG>Country:</STRONG></TD>
<TD><INPUT NAME="country" SIZE=30></TD>
</TR>

<TR>
<TD ALIGN="right"><STRONG>Comment:</STRONG></TD>
<TD>
<TEXTAREA NAME="comment" ROWS=5 COLS=30 WRAP="virtual"></TEXTAREA>
</TD>
</TR>

<TR><TD COLSPAN=2> </TD></TR>

<TR>
<TD> </TD>
<TD><INPUT TYPE="submit" NAME="action" VALUE="Add comment"></TD>
</TR>
</TABLE>

</FORM>
</BODY>
</HTML>
EndOfText


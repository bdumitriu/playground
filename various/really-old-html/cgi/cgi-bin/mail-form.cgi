#!/usr/bin/perl

# Author: John Callender
# Modified by: Bogdan DUMITRIU
# Date: 18.09.2000
# Function: collects information from the web page and sends it to the address
#	    stored in the variable $recipient
# Is working: yes
# Yet to be done: do it without using the CGI library

# configuration:

$mail = '/bin/mail'; # where is sendmail?
$recipient = 'bdumitriu@whitewizard.bdumitriu.ro'; # who gets the form data?
$site_name = 'my site'; # name of site to return to afterwards
$site_url = '/index.html'; # URL of site to return to afterwards

# script actually begins...

use CGI;
$query = new CGI;

# bundle up form submissions into a mail_body

$mail_body = '';

foreach $field (sort ($query->param)) 
{
    foreach $value ($query->param($field)) 
    {
        $mail_body .= "$field: $value\n";
    }
}

# set an appropriate From: address

if ($email = $query->param('07_email')) 
{
    # the user supplied an email address
    if ($name = $query->param('01_name')) 
    {
        # the user supplied a name
        $name =~ s/"//g; # lose any double-quotes in name
        $sender = "\"$name\" <$email>";
    } 
    else 
    {
        # user did not supply a name
        $sender = "$email";
    }
}

# send the email message

open(MAIL, "|mail $recipient") or die "Can't open pipe to $mail: $!\n";
print MAIL "Web Form Submission\n";
print MAIL "$mail_body";
print MAIL "\.\n";
print MAIL "\n";
close(MAIL) or die "Can't close pipe to $mail: $!\n";

# now show the thank-you screen

print "Content-type: text/html\n\n";
print <<"EOF";
<HTML>
<HEAD>
<TITLE>Thank you</TITLE>
</HEAD>

<BODY>

<H1>Thank you</H1>

<P>Thank you for your form submission. You will be hearing 
from me soon.</P>

<P>Return to 
<A HREF="$site_url">$site_name</A>.</P>

</BODY>
</HTML>
EOF


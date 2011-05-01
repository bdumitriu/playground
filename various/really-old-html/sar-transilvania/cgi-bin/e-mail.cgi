#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 03.10.2000
# Function: Processes the information from a web page and sends an e-mail.
# Is working: yes
# Yet to be done: all done

use CGI;

# configure these and nothing else
$mail_program = '/usr/bin/sendmail'; 		# path to sendmail
$mail_address = 'office@sar.ro';		# e-mail to send message to

# read data from web page
$query = new CGI;
$mail_body = '';
$fname = $query->param('pren');
$lname = $query->param('nume');
$subject = $query->param('subj');
$email = $query->param('email');
$message = $query->param('mesaj');

# build the message to be sent
if ($subject)
{
    $mail_body .= "subject: $subject\n";
}
else
{
    $mail_body .= "subject: no subject available\n";
}

if (($fname) || ($lname))
{
    $mail_body .= "$fname $lname cu adresa ";
}
else
{
    $mail_body .= "Un necunoscut cu adresa ";
}

if ($email)
{
    $mail_body .= "$email a scris:\n";
}
else
{
    $mail_body .= "necunoscuta a scris:\n";
}

$message =~ s/\r\n?/\n/g; 		# fix all the various line endings
$mail_body .= $message;
$mail_body .= "\n\.\n";

# send the e-mail
open(FILE, "| $mail_program $mail_address");
print FILE ("$mail_body");
close(FILE);

# create the HTML page to be sent back to the client
print << 'END';
Content-type: text/html

<HTML>

<BODY BACKGROUND=../back.jpg>

<IMG STYLE="position: absolute; top: 10; left: 10" SRC=../datal.jpg>
<IMG STYLE="position: absolute; top: 10; left: 27" SRC=../datat.jpg>
<IMG STYLE="position: absolute; top: 359; left: 27" SRC=../datat.jpg>
<IMG STYLE="position: absolute; top: 10; left: 821" SRC=../datar.jpg>

<DIV STYLE="position: absolute; top: 25; left: 25">
<FONT FACE=arial SIZE=1>
<BR>
Mesajul dvs. a fost trimis. In cazul in care ne-ati indicat adresa de e-mail
in mod corect va vom contacta cat mai repede cu putinta. Va multumim.
</FONT>
</DIV>

</BODY>

</HTML>
END

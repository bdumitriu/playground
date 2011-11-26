#!/usr/bin/perl
#######################################################
# sendEmail
# Written by: Brandon Zehm <caspian@linuxfreak.com>
#
# License:
# This software may be freely distributed, used, and
# modified, provided that this disclaimer and my name
# are included in the code.
# This code is *NOT* to be used for sending SPAM email!
# If you are using this software to send spam email
# STOP RIGHT NOW, your license to use this software is 
# specifically revoked.
#
# *** The use of this software for sending spam ***
# *** or bulk email is explicitly denied.       ***
#
# Disclaimer:
# -- insert standard disclaimer here ;) --
# I am not responsible for anything you do with this
# software, or anything that happens from the use of
# this software, including but not limited to anything
# you, your lawyers, or anyone else can think of.
#
#######################################################
use Socket;
######################
#  Global Variables  #
######################
$version     = 'v1.32';
$server      = 'localhost';                        # Defualt server
$port        = '25';                               # Defualt port
$subject     = '';                                 # Default subject
$logging     = 0;                                  # Disable logging by default
$logfile     = '/var/log/messages';                # Default file for logging
$debug       = 0;                                  # Default verbosity level
$delimiter   = "----MIME delimiter for sendEmail-" # Default MIME Delimiter
               . rand(1000000);                    # Add some randomness to the delimiter
$from        = '';
@to          = ();
@cc          = ();
@bcc         = ();
$message     = '';
@attachments = ();
################################
#  required startup jibberish  #
################################
### Intercept signals and exit cleanly ###
$SIG{'QUIT'} = \&exit;
$SIG{'INT'} = \&exit;
$SIG{'KILL'} = \&exit;
$SIG{'TERM'} = \&exit;
$SIG{'HUP'} = \&exit;
### Abort program after 15 seconds ###
alarm(15) if ($^O !~ /win/i);  # alarm() doesn't work in win32
### Set terminal to non-buffering mode ###
select(STDOUT);
$| = 1;
### Display help if no command line options are present ###
if (!$ARGV[0]) { help(); }
##########################
#  Process command line  #
##########################
$numargv = @ARGV;
for ($counter = 0; $counter < $numargv; $counter++) {
  if ($ARGV[$counter] =~ /^-f/) {                  ### From ###
    $counter++;
    if ($ARGV[$counter] && $ARGV[$counter] !~ /^-/) {
      $from = $ARGV[$counter];
    }
  }
  elsif ($ARGV[$counter] =~ /^-t/) {               ### To ###
    $counter++;
    while ($ARGV[$counter] && ($ARGV[$counter] !~ /^-/)) {
      push (@to,$ARGV[$counter]);
      $counter++;
    } $counter--;
  }
  elsif ($ARGV[$counter] =~ /^-m/) {               ### Message ###
    $counter++;
    if ($ARGV[$counter] && $ARGV[$counter] !~ /^-/) {
      $message = $ARGV[$counter];
    }
  }
  elsif ($ARGV[$counter] =~ /^-u/) {               ### Subject ###
    $counter++;
    if ($ARGV[$counter] && $ARGV[$counter] !~ /^-/) {
      $subject = $ARGV[$counter];
    }
  }
  elsif ($ARGV[$counter] =~ /^-s/) {               ### Server ###
    $counter++;
    if ($ARGV[$counter] && $ARGV[$counter] !~ /^-/) {
      $server = $ARGV[$counter];
      if ($server =~ /:/) {                        ### Port ###
        ($server,$port) = split(":",$server);
      }
    }
  }
  elsif ($ARGV[$counter] =~ /^-a/) {               ### Attachments ###
    $counter++;
    while ($ARGV[$counter] && ($ARGV[$counter] !~ /^-/)) {
      push (@attachments,$ARGV[$counter]);
      $counter++;
    } $counter--;
  }
  elsif ($ARGV[$counter] =~ /^-cc/) {              ### Cc ###
    $counter++;
    while ($ARGV[$counter] && ($ARGV[$counter] !~ /^-/)) {
      push (@cc,$ARGV[$counter]);
      $counter++;
    } $counter--;
  }
  elsif ($ARGV[$counter] =~ /^-bcc/) {             ### Bcc ###
    $counter++;
    while ($ARGV[$counter] && ($ARGV[$counter] !~ /^-/)) {
      push (@bcc,$ARGV[$counter]);
      $counter++;
    } $counter--;
  }
  elsif ($ARGV[$counter] =~ /^-l/) {               ### Logging ###
    $counter++;
    $logging = 1;
    if ($ARGV[$counter] && $ARGV[$counter] !~ /^-/) {
      $logfile = $ARGV[$counter];
    }
  }
  elsif ($ARGV[$counter] =~ /^-v|^-q/) {           ### Verbosity ###
    if ($ARGV[$counter] eq "-q") {                   # Quiet
      $debug = -1;
    }
    elsif ($ARGV[$counter] eq "-v") {                # Verbose
      $debug = 1 if ($debug <= 1);
    }
    elsif ($ARGV[$counter] eq "-vv") {               # Very Verbose
      $debug = 6 if ($debug <= 6);
    }
  }
  elsif ($ARGV[$counter] =~ /^-h$|^--help$/) {     ### Help ###
    help();
  }
  else {                                           ### Invalid Option ###
    &exit("ERROR:  The option '$ARGV[$counter]' is unrecognised.\n");
  }
}
#################################################
#  Verify required variables are set correctly  #
#################################################
if ($server eq '') {
  $server = 'localhost';
}
if ($port eq '') {
  $port = 25;
}
if ($from eq '') {
  print "You must specify a 'from' field.\n\n";
  &help;
}
if ($to[0] eq '') {
  print "You must specify a 'to' field.\n\n";
  &help;
}
#################################################
#  Read $message from STDIN if -m was not used  #
#################################################
if (!($message)) {
  alarm(60) if ($^O !~ /win/i);  # alarm() doesn't work in win32
  unless ($debug < 0) {
    print "Reading message from STDIN because no '-m' option was used.\n";
    print "If you are manually typing in a message:\n"; 
    print "  - Program will abort if a new line is not received once every 60 seconds.\n" if ($^O !~ /win/i);
    print "  - End manual input with a CTRL-D on its own line.\n\n";
  }
  while (<STDIN>) {               # Read STDIN into $message
    $message .= $_;
    alarm(60) if ($^O !~ /win/i);  # alarm() doesn't work in win32
  }
  print "Message input complete.\n" unless ($debug < 0);
  $message =~ s/\012/\015\012/g;  # Fix lines with bare LF's
}
### Check message for bare periods and encode them
$message =~ s/(\015|\012)(\.{1,})(\015|\012)/$1.$2$3/;
### Get the current date for the email header ###
($sec,$min,$hour,$mday,$mon,$year,$day,$null,$null) = localtime(time);
$year += 1900; $mon = return_month($mon); $day = return_day($day);
$date = "$day, $mday $mon $year $hour:$min:$sec -0600";
#################################
#  Connect, and send the email  #
#################################
print "Connecting to: $server:$port\n" if ($debug > 0);
&connect or &exit();
&read_server_response or &exit("EXITING\n"); 
print "Sending mail commands: \n" if ($debug > 0);
print "\tHELO -> " if ($debug > 0);               # Helo
print SERVER "HELO $server\015\012";
&read_server_response or &exit("EXITING\n"); 
print "OK\n" if ($debug == 1);
print "\tMAIL FROM -> " if ($debug > 0);          # Mail From
print SERVER "MAIL FROM: <$from>\015\012";
&read_server_response or &exit("EXITING\n"); 
print "OK\n" if ($debug == 1);
print "\tRCTP TO -> " if ($debug > 0);            # RCPT To
for ($a = 0; $a < scalar(@to); $a++) {               # To
  print SERVER "RCPT TO: <$to[$a]>\015\012";
  &read_server_response or &exit("EXITING\n"); 
}
for ($a = 0; $a < scalar(@cc); $a++) {               # Cc
  print SERVER "RCPT TO: <$cc[$a]>\015\012";
  &read_server_response or &exit("EXITING\n"); 
}
for ($a = 0; $a < scalar(@bcc); $a++) {              # Bcc
  print SERVER "RCPT TO: <$bcc[$a]>\015\012";
  &read_server_response or &exit("EXITING\n"); 
}
print "OK\n" if ($debug == 1);
print "\tDATA -> " if ($debug > 0);               # DATA (the body of the email)
print SERVER "DATA\015\012";
&read_server_response or &exit("EXITING\n");         # EMAIL HEADERS:
print SERVER "From: <$from>\015\012";                    # From
print SERVER "To:";                                      # To
for ($a = 0; $a < scalar(@to); $a++) {                   
  if (($a + 1) != scalar(@to)) {
    print SERVER " <$to[$a]>,\015\012";
  } else {
    print SERVER " <$to[$a]>\015\012";
  }
}
if ($cc[0] ne "") {                                      # Cc
  print SERVER "Cc:";
  for ($a = 0; $a < scalar(@cc); $a++) {                   
    if (($a + 1) != scalar(@cc)) {
      print SERVER " <$cc[$a]>,\015\012";
    } else {
      print SERVER " <$cc[$a]>\015\012";
    }
  }
}
print SERVER "Subject: $subject\015\012";                # Subject
print SERVER "Date: $date\015\012";                      # Date
print SERVER "X-Mailer: sendEmail-$version\015\012";     # X-Mailer
 
# IF NO ATTACHMENTS
if ($attachments[0] eq '') {
  print SERVER "\015\012$message\015\012.\015\012";
  &read_server_response or &exit("EXITING\n");
  print "OK\n" if ($debug == 1);
}
# IF ATTACHMENTS
else {
  # Disable the alarm so people on modems can send big attachments
  alarm(0) if ($^O !~ /win/i);  # alarm() doesn't work in win32
  
  # Append these lines to the header
  print SERVER "Content-Type: multipart/mixed; boundary=\"$delimiter\"\015\012";
  print SERVER "Mime-Version: 1.0\015\012";
  print SERVER "\015\012";
  print SERVER "This is a multi-part message in MIME format.\015\012";
  print SERVER "\015\012";
  # MIME format the message
  print SERVER "--$delimiter\015\012";
  print SERVER "Content-Type: text/plain;\015\012";
  print SERVER "        charset=\"iso-8859-1\"\015\012";
  print SERVER "Content-Transfer-Encoding: 7bit\015\012";
  print SERVER "\015\012";
  print SERVER "$message\015\012";
  print SERVER "\015\012";
  # Send the attachments
  foreach(@attachments) {
    send_attachment($_);
  }
  
  print SERVER "\015\012--$delimiter--\015\012";  # Terminate mime message
  
  # Tell the server we are done sending the email
  print SERVER "\015\012.\015\012";
  &read_server_response or &exit("EXITING\n");
  print "OK\n" if ($debug == 1);
}
####################
#  We are done!!!  #
####################
print "Disconnecting.\n" if ($debug > 0);
&close_server;
####################################
#  Generate exit message/log entry #
####################################
if (($debug > 0) or ($logging)) {
  print "Generating exit/log entry\n" if ($debug > 5);
  
  # Get the date
  print "Getting the date\n" if ($debug > 5);
  $date = "$mon $mday $hour:$min:$sec";
    
  # Get HOSTNAME
  print "Getting the hostname\n" if ($debug > 5);
  my $hostname = $ENV{'HOSTNAME'};
  $hostname =~ s/\..*$//;
    
  # Put the message together
  $output = "$date $hostname sendEmail\[$$\]: Email sent successfully,  From: [$from] To: ";
  for ($a = 0; $a < scalar(@to); $a++) {
    $output .= "[$to[$a]] ";
  }
  if ($cc[0] ne "") {
    $output .= "Cc: ";
    for ($a = 0; $a < scalar(@cc); $a++) {
      $output .= "[$cc[$a]] ";
    }
  }
  if ($bcc[0] ne "") {
    $output .= "Bcc: ";
    for ($a = 0; $a < scalar(@bcc); $a++) {
      $output .= "[$bcc[$a]] ";
    }
  }
  $output .= "Subject: [$subject] " if ($subject ne "");
  if ($a[0] ne "") { 
    $output .= "Attachment(s): ";
    for ($a = 0; $a < scalar(@attachments); $a++) {
      $output .= "[$attachments[$a]] ";
    }
  }
  $output .= "Server: [$server:$port]\n";
}    
#####################################
#  Log to a file if -l is specified #
#####################################
if ($logging) {
  print "Logging this transaction to: $logfile\n" if ($debug > 0);
  print "Opening file: " if ($debug > 5);
  if (!(open(LOG, ">> $logfile"))) {
    print "failure\n" if ($debug > 5);
    print "WARNING -- Error opening the file: [$logfile]\n";
  }
  else {
    print "success\n" if ($debug > 5);
    print "Writing the log entry\n" if ($debug > 5);
    print LOG "$output";
    print "Closing log file\n" if ($debug > 5);
close(LOG);
  }
}
######################
#  Exit the program  #
######################
print "Exiting the program\n" if ($debug > 5);
&exit("$output\n") if ($debug > 0);
&exit("Email was sent successfully!\n") if ($debug > -1);
&exit();
#########################################################
# SUB: help
#
# hehe, for all those newbies ;)
#########################################################
sub help {
print <<EOM;
sendEmail-$version by Brandon Zehm <caspian\@linuxfreak.com>
Usage:  sendEmail [options]   or   command | sendEmail [options]
  Required:
    -f <from>          from email address
    -t <to> ... ...    to email address(es)
  
  Common:
    -u <subject>       
    -m <message>       if -m is absent the message is read from STDIN
    -s <server[:port]> default is $server:$port
  
  Optional:
    -a <file> ... ...  file attachment(s)
    -cc  <to> ... ...  cc  email address(es)
    -bcc <to> ... ...  bcc email address(es)
  
  Paranormal:
    -l <logfile>       (partially broken) logging is disabled by default
    -q,-v,-vv          -q = quiet, -v = verbose, -vv = very verbose
EOM
exit;
}
#########################################################
# SUB: &connect
# Returns 1 on success 0 on failure.
#########################################################
sub connect {
  print "connect() - sub entry\n" if ($debug > 5);
  my $return = 1;
  
  ### Open a IP socket in stream mode with tcp protocol. 
  print "connect() - requesting a streaming tcp/ip socket from the system\n" if ($debug > 5);
  socket(SERVER, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || ($return = 0);
  print "ERROR:  Problem opening a tcp/ip socket with the system.\n" if ($return == 0);
  ### Create the data structure $dest by calling sockaddr_in(port, 32bit IP)
  print "connect() - creating data structure from server name and port\n" if ($debug > 5);
  my $dest = sockaddr_in ($port, inet_aton($server)) || ($return = 0);
  print "ERROR: Calling sockaddr_in() returned an error\n" if ($return == 0);
  ### Connect our socket to SERVER
  print "connect() - connecting the socket to the server\n" if ($debug > 5);
  connect(SERVER, $dest) || ($return = 0);
  print "ERROR:  Connection attempt to [$server:$port] failed!\nTry specifying another SMTP server with the '-s' option.\n" if ($return == 0);
  print "connect() - successfully connected to $server:$port\n" if (($debug > 5) && ($return));
  ### Force our output to flush immediatly after a print.
  print "connect() - setting non-buffering mode on the network connection\n" if (($debug > 5) && ($return));
  select(SERVER);
  $| = 1;
  select(STDOUT);
  # Return success
  print "connect() - sub exit: returning [$return]\n" if ($debug > 5);
  return($return);
}
#########################################################
# SUB: &close_server
# Closes the SERVER socket.
# Returns 1 on success 0 on failure.
#########################################################
sub close_server {
  print "close_server() - sub entry\n" if ($debug > 5);
  my $return = 1;
  print SERVER "QUIT\015\012";        # Send Quit command
  if (!(close SERVER)) {              # and drop the connection.
    print "ERROR:  There was an error disconnecting form the server\n";
    $return = 0;                      # Return failure if we didn't disconnect correctly
  } 
  print "close_server() - disconnected from the server successfully\n" if ($debug > 5);
  print "close_server() - sub exit: returning [$return]\n" if (($debug > 5) && ($return));
  return($return);                    # Return
}
#########################################################
# SUB: &read_server_response
# Reads lines form the server until a line contains 
# a SMTP response.
#
# Returns 1 on positive SMTP response and 0 on negative
# SMTP response.
#########################################################
sub read_server_response {
  print "\nread_server_response()\n" if ($debug > 5);
  my $return = 0;
  print "  reading a line from the server\n" if ($debug > 5);
  my $string = <SERVER>;                           # Read a line from the remote server
  print "  The server returned:\n  $string" if ($debug > 5);
  if ($string =~ /^211|^220|^221|^250|^354/) {     # A positive SMTP response
    $return = 1;
  }
  elsif ($string =~ /^500|^501|^502|^503|^504|^214|^421|^450|^550|^451|^551|^452|^552|^553|^554/) {  # A negative SMTP response
    print "ERROR:  The server returned the following error:\n$string\n";
    $return = 0;                                   # set return to failure
  }
  print "read_server_response() - returning [$return]\n\n" if ($debug > 5);
  return($return);                                 # Return
}
#########################################################
# SUB: &return_month(0,1,etc)
#  returns the name of the month that corrosponds
#  with the number.  returns 0 on error.
#########################################################
sub return_month {
  my $x = $_[0];
  if ($x == 0)  { return 'Jan'; }
  if ($x == 1)  { return 'Feb'; }
  if ($x == 2)  { return 'Mar'; }
  if ($x == 3)  { return 'Apr'; }
  if ($x == 4)  { return 'May'; }
  if ($x == 5)  { return 'Jun'; }
  if ($x == 6)  { return 'Jul'; }
  if ($x == 7)  { return 'Aug'; }
  if ($x == 8)  { return 'Sep'; }
  if ($x == 9)  { return 'Oct'; }
  if ($x == 10) { return 'Nov'; }
  if ($x == 11) { return 'Dec'; }
  return (0);
}
#########################################################
# SUB: &return_day(0,1,etc)
#  returns the name of the day that corrosponds
#  with the number.  returns 0 on error.
#########################################################
sub return_day {
  my $x = $_[0];
  if ($x == 0)  { return 'Sun'; }
  if ($x == 1)  { return 'Mon'; }
  if ($x == 2)  { return 'Tue'; }
  if ($x == 3)  { return 'Wed'; }
  if ($x == 4)  { return 'Thu'; }
  if ($x == 5)  { return 'Fri'; }
  if ($x == 6)  { return 'Sat'; }
  return (0);
}
#########################################################
# SUB: send_attachment("/path/filename")
# Sends the mime headers and base64 encoded file
# to the email server.
#########################################################
sub send_attachment {
  my ($x) = @_;                                 # Get filename passed
  my @fields, $y, $filename, $filename_name;    # Local variables
  my $encoding, @attachlines, $content_type;    # Local variables
  my $bin = 1;
  
  $filename = $x;                               # Save filename before butchering it
  @fields = split(/\/|\\/, $filename);          # Get the actual filename without the path  
  $filename_name = pop (@fields);       
  
  ########################
  # Autodetect Mime Type #
  ########################
    
  @fields = split(/\./, $x);
  $encoding = $fields[$#fields];
  
  if ($encoding =~ /txt|text|log|conf|^c$|cpp|^h$|inc|m3u/i) {   $content_type = 'text/plain';                      }
  elsif ($encoding =~ /html|htm|shtml|shtm|asp|php|cfm/i) {      $content_type = 'text/html';                       }
  elsif ($encoding =~ /sh$/i) {                                  $content_type = 'application/x-sh';                }
  elsif ($encoding =~ /tcl/i) {                                  $content_type = 'application/x-tcl';               }
  elsif ($encoding =~ /pl$/i) {                                  $content_type = 'application/x-perl';              }
  elsif ($encoding =~ /js$/i) {                                  $content_type = 'application/x-javascript';        }
  elsif ($encoding =~ /man/i) {                                  $content_type = 'application/x-troff-man';         }
  
  elsif ($encoding =~ /gif/i) {                                  $content_type = 'image/gif';                       }
  elsif ($encoding =~ /jpg|jpeg|jpe|jfif|pjpeg|pjp/i) {          $content_type = 'image/jpeg';                      }
  elsif ($encoding =~ /tif|tiff/i) {                             $content_type = 'image/tiff';                      }
  elsif ($encoding =~ /xpm/i) {                                  $content_type = 'image/x-xpixmap';                 }
  elsif ($encoding =~ /bmp/i) {                                  $content_type = 'image/x-MS-bmp';                  }
  elsif ($encoding =~ /pcd/i) {                                  $content_type = 'image/x-photo-cd';                }
  elsif ($encoding =~ /png/i) {                                  $content_type = 'image/png';                       }
  elsif ($encoding =~ /aif|aiff/i) {                             $content_type = 'audio/x-aiff';                    }
  elsif ($encoding =~ /wav/i) {                                  $content_type = 'audio/x-wav';                     }
  elsif ($encoding =~ /mp2|mp3|mpa/i) {                          $content_type = 'audio/x-mpeg';                    }
  elsif ($encoding =~ /ra$|ram/i) {                              $content_type = 'audio/x-pn-realaudio';            }
  elsif ($encoding =~ /mpeg|mpg/i) {                             $content_type = 'video/mpeg';                      }
  elsif ($encoding =~ /mov|qt$/i) {                              $content_type = 'video/quicktime';                 }
  elsif ($encoding =~ /avi/i) {                                  $content_type = 'video/x-msvideo';                 }
  elsif ($encoding =~ /zip/i) {                                  $content_type = 'application/x-zip-compressed';    }
  elsif ($encoding =~ /tar/i) {                                  $content_type = 'application/x-tar';               }
  elsif ($encoding =~ /jar/i) {                                  $content_type = 'application/java-archive';        }
  elsif ($encoding =~ /exe|bin/i) {                              $content_type = 'application/octet-stream';        }
  elsif ($encoding =~ /ppt|pot|ppa|pps|pwz/i) {                  $content_type = 'application/vnd.ms-powerpoint';   }
  elsif ($encoding =~ /mdb|mda|mde/i) {                          $content_type = 'application/vnd.ms-access';       }
  elsif ($encoding =~ /xls|xlt|xlm|xld|xla|xlc|xlw|xll/i) {      $content_type = 'application/vnd.ms-excel';        }
  elsif ($encoding =~ /doc|dot/i) {                              $content_type = 'application/msword';              }
  elsif ($encoding =~ /rtf/i) {                                  $content_type = 'application/rtf';                 }
  elsif ($encoding =~ /pdf/i) {                                  $content_type = 'application/pdf';                 }
  elsif ($encoding =~ /tex/i) {                                  $content_type = 'application/x-tex';               }
  elsif ($encoding =~ /latex/i) {                                $content_type = 'application/x-latex';             }
  elsif ($encoding =~ /vcf/i) {                                  $content_type = 'application/x-vcard';             }
  else { $content_type = 'base64';  }
##########################
# Process the attachment #
##########################
  
  ###################################
  # Generate and print MIME headers #
  ###################################
  
  $y =      "--$delimiter\015\012";
  $y = $y . "Content-Type: $content_type;\015\012";
  $y = $y . "        name=\"$filename_name\"\015\012";
  $y = $y . "Content-Transfer-Encoding: base64\015\012";
  $y = $y . "Content-Disposition: attachment; filename=\"$filename_name\"\015\012";
  $y = $y . "\015\012";
  print SERVER $y;
  
    
  #########################################################
  # Convert the file to base64 and print it to the server #
  #########################################################
  
  open (FILETOATTACH, "$filename") || die "Could not open file attachment $filename\n";
  binmode(FILETOATTACH);
  
  my $res = "";
  my $tmp = "";
  my $base64 = "";
  while (<FILETOATTACH>) {               # Read a line from the binary file
  $res .= $_;
    
    #################################
    # Convert binary data to base64 #
    #################################
    while ($res =~ s/(.{45})//s) {       # Get 45 bytes from the binary string
      $tmp = substr(pack('u', $&), 1);   # Convert the binary to uuencoded text
      chop($tmp);
      $tmp =~ tr|` -_|AA-Za-z0-9+/|;     # Translate from uuencode to base64
      $base64 .= $tmp;
    }
    
    ##############################
    # Print chunks to the server #
    ##############################
    while ($base64 =~ s/(.{76})//s) {
      print SERVER "$1\015\012";
    }
  
  }
  
  #################################
  # Encode and send the leftovers #
  #################################
    my $padding = (3 - length($res) % 3) % 3;  # Set flag if binary data isn't divisible by 3
    $res = substr(pack('u', $res), 1);         # Convert the binary to uuencoded text
    chop($res);
    $res =~ tr|` -_|AA-Za-z0-9+/|;             # Translate from uuencode to base64
  
    ##########################
    # Fix padding at the end #
    ##########################
    $res = $base64 . $res;                                  # Get left overs from above
    $res =~ s/.{$padding}$/'=' x $padding/e if $padding;    # Fix the end padding if flag (from above) is set
    while ($res =~ s/(.{1,76})//s) {                        # Send it to the email server.
      print SERVER "$1\015\012";
    }
  
  close (FILETOATTACH) || die "Could not close file $filename\n";
  return(1);
}
sub exit {
  print @_[0];
  exit;
}
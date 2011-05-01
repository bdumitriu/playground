#!/usr/bin/perl

# Author: Bogdan DUMITRIU
# Date: 03.11.2001
# Function: Encodes all .wav files from the directory supplied to the program
#	    by means of --dir=/dir/with/wav/files/ (or ./ if no --dir is
#	    supplied) to mp3 format using lame encoder. The new names will be
#	    identical to the old ones with .wav changed to .mp3. If --rip 
#	    option is supplied, then it will also rip all tracks from
#	    /dev/cdrom into the same directory as above using cdda2wav.
# Is working: yes
# Yet to be done: allow an alternative encoder&ripper, allow user command 
#		  line params for encoder&ripper, etc.

use Getopt::Long;

$noargs = 0;
if ($ARGV[0] eq '')
{
    $noargs = 1;
}

if (!&GetOptions("dir=s" => \$dir, "rip" => \$rip))
{
    print "Usage: encode_wavs.pl [--dir=/dir/with/wav/files/] [--rip]\n";
    exit 1;
}

if ($noargs == 1)
{
    $dir = "./";
}

chdir($dir);

if ($rip == 1)
{
    print "Ripping all tracks from /dev/cdrom...\n";
    system("cdda2wav -c 2 -v 1 -H -D /dev/cdrom -s -x -t 1 -O wav -B");
    print "Done with the ripping.\n";
}

$number = 0;
if (-e "encoder.names")
{
    open(NF, "./encoder.names");
    $i = 0;
    $artist[$i] = <NF>;
    chop($artist[$i]);
    $title[$i] = <NF>;
    chop($title[$i]);
    while ($artist[$i] ne "")
    {
	$i++;
	$artist[$i] = <NF>;
	chop($artist[$i]);
        $title[$i] = <NF>;
	chop($title[$i]);
    }
    $number = $i;
}

$idx = 0;
opendir($curDir, $dir);
@files = readdir($curDir);

$any = 0;
for ($i = 1; $i <= @files; $i++)
{
    if (-f $files[$i])
    {
	$name = $files[$i];
	$ext = substr($name, length($name)-4);
	if ($ext eq ".wav")
	{
	    if ($idx >= $number)
	    {
		$new_name = substr($name, 0, length($name)-4);
		$new_name = "\"$new_name.mp3\"";
	    }
	    else
	    {
		$temp = $idx+1;
		if ($idx < 9)
		{
		    $new_name = "\"0$temp. $artist[$idx]_$title[$idx].mp3\"";
		}
		else
		{
		    $new_name = "\"$temp. $artist[$idx]_$title[$idx].mp3\"";
		}
	    }
	    $any = 1;
	    $idx++;
	    system("lame -h $name $new_name\n");
	    #print "lame -h $name $new_name\n";
	}
    }
}

if ($any == 1)
{
    print "Done.\n";
}
else
{
    print "No .wav files found in directory $dir\n";
}
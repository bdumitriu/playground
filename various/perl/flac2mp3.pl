#!/usr/bin/perl

use MP3::Info;

foreach $file (@ARGV) {
  if (!($file =~ /\.flac$/)) {
    print "Skipping $file\n";
    next;
  }
  if ($tag = get_mp3tag($file)) {
    $artist = $tag->{ARTIST};
    $title = $tag->{TITLE};
    $album = $tag->{ALBUM};
    $track = $tag->{TRACKNUM};
    chomp($artist);
    chomp($title);
    chomp($album);
    chomp($track);
    $track = sprintf("%2.2d", $track);
  } else {
    print "Couldn't get MP3 tag for $file.\n";
  }
  `flac -d "$file"`;
  $file =~ s/\.flac$/.wav/;
  if (($artist) && ($title) && ($track)) {
    $outfile = "$artist - ($track)$title.mp3";
  } else {
    $outfile = $file;
    $outfile =~ s/\.wav$/.mp3/;
  }
  print "$file\n";
  print "$outfile\n";
  `lame -V0 -q0 -b192 --tt "$title" --ta "$artist" --tl "$album" --tn "$track" "$file" "$outfile"`;
  `rm "$file"`;
  print "-------------------------------------------\n";
}
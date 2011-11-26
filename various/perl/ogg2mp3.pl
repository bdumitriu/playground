#!/usr/bin/perl

foreach $file (@ARGV) {
  if (!($file =~ /\.ogg$/)) {
    print "Skipping $file\n";
    next;
  }
  `oggdec "$file"`;
  $file =~ s/\.ogg$/.wav/;
  $outfile = $file;
  $outfile =~ s/\.wav$/.mp3/;
  print "$file\n";
  print "$outfile\n";
  `lame -V0 -q0 -b192 "$file" "$outfile"`;
  `rm "$file"`;
  print "-------------------------------------------\n";
}
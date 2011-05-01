#!/usr/bin/perl

foreach $file (@ARGV) {
  if (!($file =~ /\.shn$/)) {
    print "Skipping $file\n";
    next;
  }
  `shorten -x "$file"`;
  $file =~ s/\.shn$/.wav/;
  $outfile = $file;
  $outfile =~ s/\.wav$/.mp3/;
  print "$file\n";
  print "$outfile\n";
  `lame -V0 -q0 -b192 "$file" "$outfile"`;
  `rm "$file"`;
  print "-------------------------------------------\n";
}

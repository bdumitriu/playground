#!/usr/bin/perl -w

$step = 0;

while (<>) {
    $step++ if /^VStep/;
    if (/spin_dizzily/) {
        print "${step}: $_\n";
        last;
    }
}


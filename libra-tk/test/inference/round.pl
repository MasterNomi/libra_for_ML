#!/usr/bin/perl -w

if ($#ARGV >= 0 and $ARGV[0] =~ /^[0-9]+$/) {
    $places = $ARGV[0];
    shift @ARGV;
} else {
    $places = 3;
}
$fmtstring = sprintf("%%.%df", $places);

while (<>) {
    if (/[0-9]/) {
        @l = split /\s/;
        @l2 = ();
        for $x (@l) {
            if ($x =~ /^[0-9.-]+$/) {
                $x = sprintf($fmtstring, $x);
            }
            push @l2, $x;
        }
        $" = ' ';
        print "@l2\n";
    } else {
        print;
    }
}

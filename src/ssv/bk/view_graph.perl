#!/usr/bin/perl -w

use strict;

my $dot = $ARGV[0];

my $png = $dot;
$png =~ s/\.dot$/\.png/ or die;

!system("dot -Tpng $dot -o $png") or die;

!system("xloadimage $png") or die;

#!system("rm $png") or die;

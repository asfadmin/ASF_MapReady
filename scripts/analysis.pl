#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config pass_through);
use XML::Simple;
use Data::Dumper;

my $usage = q~Usage:
  defranz.pl <xml file> [...]
~;

if(scalar(@ARGV) < 1) { print $usage; exit; }

# read in all the xml files
my $tree = [];
foreach my $file (@ARGV) {
  foreach(glob($file)) {
    push(@$tree, new XML::Simple->XMLin($_));
  }
}

# grab the info we want
my @data = (["Scene Name", "Orbit Direction", "Corner Reflector", "X Pos", "Y Pos", "X Offset", "Y Offset", "Total Error"]);
foreach my $report (@$tree) {
  $report->{DatasetInformation}->{Filename} =~ /^(\w+)/;
  my $granule = $1;
  my $ascdesc = $report->{DatasetInformation}->{OrbitDir};
  foreach my $ref (@{$report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}}) {
    my $ref_name = $ref->{ReflectorNumber};
    my $ref_xpos = $ref->{ImagePosition_X_ofPointTarget};
    my $ref_ypos = $ref->{ImagePosition_Y_ofPointTarget};
    my $ref_xoff = $ref->{GeolocationOffsetIn_X_Meter};
    my $ref_yoff = $ref->{GeolocationOffsetIn_Y_Meter};
    my $ref_error = sprintf("%.5f", sqrt($ref_xoff**2 + $ref_yoff**2));
    push(@data, [$granule, $ascdesc, $ref_name, $ref_xpos, $ref_ypos, $ref_xoff, $ref_yoff, $ref_error]);
  }
}

# spit out some csv
foreach my $row (@data) {
  print join(',', map({"\"$_\""} @$row)) . "\n";
}


exit;


sub LOG {
  my $now = `date`;
  chomp($now);
  my @msg = @_;
  foreach(@msg) {
    chomp;
    print "[$now] $_\n";
#    print LOG "[$now] $_\n";
  }
}
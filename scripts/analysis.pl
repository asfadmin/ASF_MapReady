#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config pass_through);
use XML::Simple;
use List::MoreUtils qw(uniq);
use GD::Graph::linespoints;
use Data::Dumper;

my $usage = q~Usage:
  analysis.pl <xml file> [...]
~;

if(scalar(@ARGV) < 1) { print $usage; exit; }

# read in all the xml files
my $tree = [];
my @files;
foreach(@ARGV) {
  push(@files, glob);
}
foreach(sort(uniq(@files))) {
  push(@$tree, new XML::Simple->XMLin($_));
}

# grab the info we want
my @data;
my $total_error = 0;
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
    $total_error += $ref_error;
    push(@data, [$granule, $ascdesc, $ref_name, $ref_xpos, $ref_ypos, $ref_xoff, $ref_yoff, $ref_error]);
  }
}
my $count = scalar(@data);
my $avg_error = $total_error / $count;
my $std_dev = 0;
unless(scalar(@data) <= 1) {
  my $sqtotal = 0;
  foreach(@data) {
    $sqtotal += ($avg_error - $_->[7]) ** 2;
  }
  $std_dev = sqrt($sqtotal / $count);
}

# spit out some csv
my $header = ["Scene Name", "Orbit Direction", "Corner Reflector", "X Pos", "Y Pos", "X Offset", "Y Offset", "Total Error"];
my @footer = (['', '', '', '', '', '', 'Average Error', sprintf("%.5f", $avg_error)],
              ['', '', '', '', '', '', 'Standard Deviation', sprintf("%.5f", $std_dev)]);
foreach my $row ($header, @data, @footer) {
  print join(',', map({"\"$_\""} @$row)) . "\n";
}

exit;
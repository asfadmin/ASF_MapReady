#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config pass_through);
use XML::Simple;
use List::MoreUtils qw(uniq);
use Text::CSV;
use Data::Dumper;

my $usage = q~Usage:
  analysis.pl [--out=<csv file>] [--plot=<html file>] [--include=<csv files>] <xml files> [...]
~;

my $outfile;
my $plotfile;
my @include;
GetOptions( "out=s" => \$outfile,
            "plot=s" => \$plotfile,
            "include=s" => \@include);

if(scalar(@ARGV) < 1 and !@include) { print $usage; exit; }

# read in all the xml files
my $tree = [];
my @files;
foreach(@ARGV) {
  push(@files, glob);
}
foreach(sort(uniq(@files))) {
  push(@$tree, new XML::Simple->XMLin($_));
}

# grab the info we want from the xml files
my @data;
my $total_error = 0;
foreach my $report (@$tree) {
  $report->{DatasetInformation}->{Filename} =~ /^(\w+)/;
  my $granule = $1;
  my $ascdesc = $report->{DatasetInformation}->{OrbitDir};
  if($report->{PointTargetAnalysisReport} and $report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}) {
    my @reflectors;
    if(ref($report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}) eq 'ARRAY') {
      @reflectors = @{$report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}};
    } else {
      @reflectors = ($report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults});
    }
    foreach my $ref (@reflectors) {
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
}

# grab any extra csv data
foreach(@include) {
  foreach(glob) {
    my $csv = Text::CSV->new();
    open (CSV, "<", $_) or die $!;
    while (<CSV>) {
        if ($csv->parse($_)) {
            my @columns = $csv->fields();
            if($columns[0] !~ /^Scene/ and $columns[0] !~ /^\s*$/) { # ignore headers and footers
              push @data, [@columns];
            }
        } else {
            my $err = $csv->error_input;
            print "Failed to parse line: $err";
        }
    }
    close CSV;
  }
}

# do some extra calculations
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
my $csv = '';
my @header = (["Scene Name", "Orbit Direction", "Corner Reflector", "X Pos", "Y Pos", "X Offset", "Y Offset", "Total Error"]);
my @footer = (['', '', '', '', '', '', 'Average Error', sprintf("%.5f", $avg_error)],
              ['', '', '', '', '', '', 'Standard Deviation', sprintf("%.5f", $std_dev)]);
foreach my $row (@header, @data, @footer) {
  $csv .= join(',', @$row) . "\n";
#  $csv .= join(',', map({"\"$_\""} @$row)) . "\n";
}



if($outfile) {
  open(OUT, ">$outfile");
  print OUT $csv;
  close(OUT);
}

if($plotfile) {
  open(PLOT_OUT, ">$plotfile");
  print PLOT_OUT get_plot_html(@data);
  close(PLOT_OUT);
}

if(!$outfile and !$plotfile) {
  print $csv;
}

exit;

sub ingest_csv {
  my $file = shift;
  my @data;
  return @data;
}

sub get_plot_html {
  my @data = @_;
  
  my $template = q~
<html>
  <head>
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript">
      google.load("visualization", "1", {packages:["corechart"]});
      google.setOnLoadCallback(drawChart);
      function drawChart() {
        var data = google.visualization.arrayToDataTable([/***plot_data***/]);

        var options = {
          title: 'Something About Offsets Or Whatever',
          hAxis: {title: 'X Offset'},
          vAxis: {title: 'Y Offset'},
          legend: 'none',
          maximize: 1
        };

        var chart = new google.visualization.ScatterChart(document.getElementById('chart_div'));
        chart.draw(data, options);
      }
    </script>
  </head>
  <body>
    <div id="chart_div" style="width: 900px; height: 500px;"></div>
  </body>
</html>
~;
  
  my $js_array_rows = '';
  $js_array_rows = join(',', map({'[' . join(',', @$_[5..6]) . ']'} @data));
  $template =~ s/\/\*\*\*plot_data\*\*\*\//$js_array_rows/;
  return $template;
}
#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config pass_through);
use XML::Simple;
use List::Util qw(max sum);
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

if($outfile and $outfile !~ /\.csv$/) {
  $outfile .= ".csv";
}
if($plotfile and $plotfile !~ /\.html$/) {
  if($plotfile =~ /\.htm$/) {
    $plotfile .= "l";
  } else {
    $plotfile .= ".html";
  }
}

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
  if($report->{PointTargetAnalysisReport} and $report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}) {
    my @reflectors;
    if(ref($report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}) eq 'ARRAY') {
      @reflectors = @{$report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults}};
    } else {
      @reflectors = ($report->{PointTargetAnalysisReport}->{CornerReflectorPTAResults});
    }
    foreach my $ref (@reflectors) {
      my $ref_xoff = $ref->{GeolocationOffsetIn_X_Meter};
      my $ref_yoff = $ref->{GeolocationOffsetIn_Y_Meter};
      my $ref_error = sprintf("%.5f", sqrt($ref_xoff**2 + $ref_yoff**2));
      $total_error += $ref_error;
      push(@data, [
        $granule,
        $report->{DatasetInformation}->{OrbitDir},
        $ref->{ReflectorNumber},
        max($report->{DatasetInformation}->{RngPxSize}, $report->{DatasetInformation}->{AzPxSize}),
        $ref->{Resolution_X_from_Neg3db_Width_Meter},
        $ref->{PSLR_X_left_dB},
        $ref->{PSLR_X_right_dB},
        $ref->{Resolution_Y_from_Neg3db_Width_Meter},
        $ref->{PSLR_Y_left_dB},
        $ref->{PSLR_Y_right_dB},
        $ref->{ImagePosition_X_ofPointTarget},
        $ref->{ImagePosition_Y_ofPointTarget},
        $ref_xoff * $report->{DatasetInformation}->{RngPxSize},
        $ref_yoff * $report->{DatasetInformation}->{AzPxSize},
        $ref_xoff,
        $ref_yoff,
        $ref_error]);
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
            if($columns[0] !~ /^(Scene Name|Average|Standard Deviation|RMSE|CE95)/i and $columns[0] !~ /^\s*$/) { # ignore headers and footers
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

# spit out some csv
my $csv = '';
my @header = (["Scene Name", "Orbit Direction", "Corner Reflector", "Resolution", "Resolution_X_from_Neg3db_Width_Meter", "PSLR_X_left_dB", "PSLR_X_right_dB", "Resolution_Y_from_Neg3db_Width_Meter", "PSLR_Y_left_dB" ,"PSLR_Y_right_dB", "X Pos", "Y Pos", "X Offset Pixels", "Y Offset Pixels", "X Offset Meters", "Y Offset Meters", "Total Error Meters"]);
my @footer = (
  ['Average', '', '', '',
    mean(map($_->[4], @data)), mean(map($_->[5], @data)), mean(map($_->[6], @data)),
    mean(map($_->[7], @data)), mean(map($_->[8], @data)), mean(map($_->[9], @data)),
    '', '',
    mean(map($_->[12], @data)), mean(map($_->[13], @data)),
    mean(map($_->[14], @data)), mean(map($_->[15], @data)),
    mean(map($_->[16], @data))],
  ['Standard Deviation', '', '', '',
    std_dev(map($_->[4], @data)), std_dev(map($_->[5], @data)), std_dev(map($_->[6], @data)),
    std_dev(map($_->[7], @data)), std_dev(map($_->[8], @data)), std_dev(map($_->[9], @data)),
    '', '',
    std_dev(map($_->[12], @data)), std_dev(map($_->[13], @data)),
    std_dev(map($_->[14], @data)), std_dev(map($_->[15], @data)),
    std_dev(map($_->[16], @data))],
  ['RMSE', '', '', '', '', '', '', '', '', '', '', '',
    sqrt(mean(map($_->[12] ** 2, @data))), sqrt(mean(map($_->[13] ** 2, @data))),
    sqrt(mean(map($_->[14] ** 2, @data))), sqrt(mean(map($_->[15] ** 2, @data))),
    sqrt(mean(map($_->[14] ** 2 + $_->[15] ** 2, @data)))],
  ['CE95', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', sqrt(3) * sqrt(mean(map($_->[14] ** 2 + $_->[15] ** 2, @data)))]);
foreach my $row (@header, @data, @footer) {
  $csv .= join(',', @$row) . "\n";
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


sub mean {
  return sum(@_) / @_;
}

sub std_dev {
  my $mean = mean(@_);
  my $sqtotal = 0;
  foreach(@_) {
    $sqtotal += ($mean - $_) ** 2;
  }
  return sqrt($sqtotal / scalar(@_));
}

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
  $js_array_rows = join(',', map({'[' . join(',', @$_) . ']'} @data));
  $template =~ s/\/\*\*\*plot_data\*\*\*\//$js_array_rows/;
  return $template;
}
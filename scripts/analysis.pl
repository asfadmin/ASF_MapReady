#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long qw(:config pass_through);
use XML::Simple;
use List::Util qw(max sum);
use List::MoreUtils qw(uniq);
use Text::CSV;
use Data::Dumper;

my $usage = q~USAGE:
  analysis.pl [--help] [--out=<csv file>] [--plot=<html file>] [--include=<csv files>]
    [--title=<title>] <xml files> [...]
~;

my $outfile;
my $plotfile;
my @include;
my $title = "";
my $helpf;
GetOptions( "out=s" => \$outfile,
            "plot=s" => \$plotfile,
            "include=s" => \@include,
            "title=s" => \$title,
            "help" => \$helpf);

if($helpf) {
  print get_help_text();
  exit;
}

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
  my $granule_full = $report->{DatasetInformation}->{Filename};
  $granule_full =~ /^(\w+)/;
  my $granule = $1;
  if ($granule eq "IMG") {
    $granule_full =~ /^IMG-\w\w-(\w+)/;
    $granule = $1;
  }
  if($granule =~ /^(.*)(_SIGMA)$/) {
    $granule = $1; #uuurrrggghhh
  }
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
        $report->{DatasetInformation}->{RngPxSize},
        $report->{DatasetInformation}->{AzPxSize},
        $ref->{Resolution_X_from_Neg3db_Width_Meter},
        $ref->{PSLR_X_left_dB},
        $ref->{PSLR_X_right_dB},
        $ref->{Resolution_Y_from_Neg3db_Width_Meter},
        $ref->{PSLR_Y_left_dB},
        $ref->{PSLR_Y_right_dB},
        $ref->{ImagePosition_X_ofPointTarget},
        $ref->{ImagePosition_Y_ofPointTarget},
        $ref_xoff / $report->{DatasetInformation}->{RngPxSize},
        $ref_yoff / $report->{DatasetInformation}->{AzPxSize},
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
my @header = (["Scene Name", "Orbit Direction", "Corner Reflector", "RngPxSize", "AzPxSize", "Resolution X from -3db Width (meters)", "PSLR X left dB", "PSLR X right dB", "Resolution Y from -3db Width (meters)", "PSLR Y left dB" ,"PSLR Y right dB", "X Pos (pixels)", "Y Pos (pixels)", "X Offset (pixels)", "Y Offset (pixels)", "X Offset (meters)", "Y Offset (meters)", "Total Offset (meters)"]);
my @footer = (
  ['Average', '', '', '', '',
    mean(map($_->[5], @data)), mean(map($_->[6], @data)), mean(map($_->[7], @data)),
    mean(map($_->[8], @data)), mean(map($_->[9], @data)), mean(map($_->[10], @data)),
    '', '',
    mean(map($_->[13], @data)), mean(map($_->[14], @data)),
    mean(map($_->[15], @data)), mean(map($_->[16], @data)),
    mean(map($_->[17], @data))],
  ['Standard Deviation', '', '', '', '',
    std_dev(map($_->[5], @data)), std_dev(map($_->[6], @data)), std_dev(map($_->[7], @data)),
    std_dev(map($_->[8], @data)), std_dev(map($_->[9], @data)), std_dev(map($_->[10], @data)),
    '', '',
    std_dev(map($_->[13], @data)), std_dev(map($_->[14], @data)),
    std_dev(map($_->[15], @data)), std_dev(map($_->[16], @data)),
    std_dev(map($_->[17], @data))],
  ['RMS', '', '', '', '', '', '', '', '', '', '', '', '',
    sqrt(mean(map($_->[13] ** 2, @data))), sqrt(mean(map($_->[14] ** 2, @data))),
    sqrt(mean(map($_->[15] ** 2, @data))), sqrt(mean(map($_->[16] ** 2, @data))),
    sqrt(mean(map($_->[15] ** 2 + $_->[16] ** 2, @data)))],
  ['CE95', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', 1.7308 * sqrt(mean(map($_->[15] ** 2 + $_->[16] ** 2, @data)))]);
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
  print PLOT_OUT get_plot_html($title, @header, @data);
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
  my $title = shift;
  my $template = q~<html>
  <head>
    <title>/***title***/</title>
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript">
      google.load("visualization", "1.0", {packages:['corechart', 'table']});
      google.setOnLoadCallback(drawCharts);
      
      var ascdesc_table;
      var reflector_table;
      var granule_table;
      var xx_view;
      var xy_view;
      var yx_view;
      var yy_view;
      var ascdesc_options;
      var reflector_options;
      var granule_options;
      var xx_options;
      var xy_options;
      var yx_options;
      var yy_options;
      
      var plotWindowDataTable;
      var plotWindowOptions;
      
      // function to create a new window with just a single plot
      function plotToWindow(dataTable, options) {
        console.log(dataTable);
        plotWindowDataTable = dataTable;
        plotWindowOptions = options;
        plotWindow = window.open('','plotWindow',
          'width=910,height=500,menubar=0,toolbar=0,status=0,scrollbars=1,resizable=1');
        plotWindow.document.writeln(
          "<html><head><script type='text/javascript' src='https://www.google.com/jsapi'><\/script><script>" +
          "google.load('visualization', '1.0', {packages:['corechart']});" +
          "google.setOnLoadCallback(drawCharts);" +
          "function drawCharts() {" +
          "  var plot = new google.visualization.ScatterChart(document.getElementById('plot'));" +
          "  plot.draw(opener.plotWindowDataTable, opener.plotWindowOptions);" +
          "}" +
          "<\/script></head><body><div id='plot' style='width: 900px; height: 500px;'></div></body></html>");
        plotWindow.document.close();
      }
      
      // function to derive the standard label for a scatterplot point
      function getScatterPlotLabel(dataTable, rowNum) {
        var granule = dataTable.getValue(rowNum, 0);
        var ascdesc = dataTable.getValue(rowNum, 1);
        var reflector = dataTable.getValue(rowNum, 2);
        var moffset_x = dataTable.getValue(rowNum, 15);
        var moffset_y = dataTable.getValue(rowNum, 16);
        var poffset_x = dataTable.getValue(rowNum, 13);
        var poffset_y = dataTable.getValue(rowNum, 14);
        return(granule + "\n" + ascdesc + "\nReflector: " + reflector +
          "\nX Offset: " + moffset_x + "m\nY Offset: " + moffset_y +
          "m\nX Offset: " + poffset_x + "px\nY Offset: " + poffset_y + "px");
      }
      
      // function to convert data to multiple columns based on unique entries in a column
      function uniqueToColumns(dataTable, colID) {
        var groups = dataTable.getDistinctValues(colID);
        var newTable = new google.visualization.DataView(dataTable);
        newTable.setColumns([14]);
        var allcols = [];
        for(var g in groups) {
          var t = new google.visualization.DataView(dataTable);
          t.setRows(t.getFilteredRows([{column: colID, value: groups[g]}]));
          t.setColumns([15, 16, {calc: getScatterPlotLabel, type: 'string', role: 'tooltip', label: 'tooltip'}]);
          var tempTable = google.visualization.data.join(newTable, t, 'full', [[0, 0]], allcols, [1, 2]);
          tempTable.setColumnLabel(tempTable.getNumberOfColumns() - 2, groups[g]);
          newTable = tempTable;
          allcols.push(allcols.length + 1, allcols.length + 2);
        }
        return(newTable);
      }
      
      function drawCharts() {
        // build a data table with our data
        var data = google.visualization.arrayToDataTable([/***plot_data***/]);
        var formatter = new google.visualization.NumberFormat({fractionDigits: 5});
        formatter.format(data, 4);
        formatter.format(data, 5);
        formatter.format(data, 6);
        formatter.format(data, 7);
        formatter.format(data, 8);
        formatter.format(data, 9);
        formatter.format(data, 10);
        formatter.format(data, 13);
        formatter.format(data, 14);
        formatter.format(data, 15);
        formatter.format(data, 16);
        formatter.format(data, 17);
        
        // gather some metadata and display it
        var granules = data.getDistinctValues(0);
        var gran_count = granules.length;
        var data_type = 'SAR Data';
        if(window.document.title.length > 0) {
          data_type = window.document.title;
        } else {
          var temp = new google.visualization.DataView(data);
          temp.setColumns([{calc: function(dataTable, rowNum) {
            return(dataTable.getValue(rowNum, 0)[0]);
          }, type: 'string'}]);
          var gran_types = temp.getDistinctValues(0);
          if(gran_types.length == 1) {
            switch(gran_types[0]) {
              case 'A':
                data_type = 'ALOS Data';
                break;
              case 'R':
                data_type = 'Radarsat Data';
                break;
              case 'E':
                data_type = 'ERS Data';
                break;
              case 'J':
                data_type = 'JERS Data';
                break;
            }
          } else if(gran_types.length > 1) {
            var legacy = true;
            for(t in gran_types) {
              if(!gran_types[t].match(/[REJ]/))
                legacy = false;
            }
            if(legacy)
              data_type = 'Legacy Data';
          }
          window.document.title = data_type;
        }
        document.getElementById('header').innerHTML = data_type;
        document.getElementById('subheader').innerHTML = data.getNumberOfRows() + " reflectors found in " + gran_count + " scenes";
        
        // set up the spreadsheet
        var spreadsheet = new google.visualization.Table(document.getElementById('spreadsheet'));
        spreadsheet.draw(data);
        
        // set up the asc/desc-grouped plots
        ascdesc_options = {
          title: 'Geolocation Offsets Grouped by Orbit Direction',
          hAxis: {title: 'X Offset (meters)'},
          vAxis: {title: 'Y Offset (meters)'},
          legend: {position: 'right', textStyle: {fontSize: 14}},
          maximize: 1
        };
        var ascdesc_plot = new google.visualization.ScatterChart(document.getElementById('ascdesc_plot'));
        ascdesc_table = uniqueToColumns(data, 1);
        ascdesc_plot.draw(ascdesc_table, ascdesc_options);
        
        
        // set up the reflector-grouped plot
        reflector_options = {
          title: 'Geolocation Offsets Grouped by Corner Reflector',
          hAxis: {title: 'X Offset (meters)'},
          vAxis: {title: 'Y Offset (meters)'},
          legend: {position: 'right', textStyle: {fontSize: 10}},
          maximize: 1
        };
        var reflector_plot = new google.visualization.ScatterChart(document.getElementById('reflector_plot'));
        reflector_table = uniqueToColumns(data, 2);
        reflector_plot.draw(reflector_table, reflector_options);
        
        // set up the granule-grouped plot
        granule_options = {
          title: 'Geolocation Offsets Grouped by Granule',
          hAxis: {title: 'X Offset (meters)'},
          vAxis: {title: 'Y Offset (meters)'},
          legend: {position: 'right', textStyle: {fontSize: 10}},
          maximize: 1
        };
        var granule_plot = new google.visualization.ScatterChart(document.getElementById('granule_plot'));
        granule_table = uniqueToColumns(data, 0);
        granule_plot.draw(granule_table, granule_options);
        
        // set up the xx/xy/yx/yy error/position plots
        
        xx_view = new google.visualization.DataView(data);
        xx_view.setColumns([11, 13,
          {calc:getScatterPlotLabel, type:'string', label:'Tooltip', role:'tooltip'}]);
        xx_options = {
          title: 'X Offset vs. X Position',
          hAxis: {title: 'X Position (pixels)'},
          vAxis: {title: 'X Offset (pixels)'},
          legend: 'none',
          maximize: 1
        };
        var xx_plot = new google.visualization.ScatterChart(document.getElementById('xx_plot'));
        xx_plot.draw(xx_view, xx_options);
        
        xy_view = new google.visualization.DataView(data);
        xy_view.setColumns([12, 13, {calc:getScatterPlotLabel, type:'string', label:'Tooltip', role:'tooltip'}]);
        xy_options = {
          title: 'X Offset vs. Y Position',
          hAxis: {title: 'Y Position (pixels)'},
          vAxis: {title: 'X Offset (pixels)'},
          legend: 'none',
          maximize: 1
        };
        var xy_plot = new google.visualization.ScatterChart(document.getElementById('xy_plot'));
        xy_plot.draw(xy_view, xy_options);
        
        yx_view = new google.visualization.DataView(data);
        yx_view.setColumns([11, 14, {calc:getScatterPlotLabel, type:'string', label:'Tooltip', role:'tooltip'}]);
        yx_options = {
          title: 'Y Offset vs. X Position',
          hAxis: {title: 'X Position (pixels)'},
          vAxis: {title: 'Y Offset (pixels)'},
          legend: 'none',
          maximize: 1
        };
        var yx_plot = new google.visualization.ScatterChart(document.getElementById('yx_plot'));
        yx_plot.draw(yx_view, yx_options);
        
        yy_view = new google.visualization.DataView(data);
        yy_view.setColumns([12, 14, {calc:getScatterPlotLabel, type:'string', label:'Tooltip', role:'tooltip'}]);
        yy_options = {
          title: 'Y Offset vs. Y Position',
          hAxis: {title: 'Y Position (pixels)'},
          vAxis: {title: 'Y Offset (pixels)'},
          legend: 'none',
          maximize: 1
        };
        var yy_plot = new google.visualization.ScatterChart(document.getElementById('yy_plot'));
        yy_plot.draw(yy_view, yy_options);
      }
    </script>
  </head>
  <body>
    <h1 id="header"></h1>
    <h2 id="subheader"></h2>
    <div style="border: 1px solid #808080; width: 900px;"><div id="ascdesc_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(ascdesc_table, ascdesc_options)" value="Open in New Window"/></div><br />
    <div style="border: 1px solid #808080; width: 900px;"><div id="reflector_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(reflector_table, reflector_options)" value="Open in New Window"/></div><br />
    <div style="border: 1px solid #808080; width: 900px;"><div id="granule_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(granule_table, granule_options)" value="Open in New Window"/></div><br />
    <div style="border: 1px solid #808080; width: 900px;"><div id="xx_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(xx_view, xx_options)" value="Open in New Window"/></div><br />
    <div style="border: 1px solid #808080; width: 900px;"><div id="xy_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(xy_view, xy_options)" value="Open in New Window"/></div><br />
    <div style="border: 1px solid #808080; width: 900px;"><div id="yx_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(yx_view, yx_options)" value="Open in New Window"/></div><br />
    <div style="border: 1px solid #808080; width: 900px;"><div id="yy_plot" style="width: 900px; height: 500px;"></div>
    <input type="button" onclick="plotToWindow(yy_view, yy_options)" value="Open in New Window"/></div><br />
    <br /><br />
    <div id="spreadsheet"></div>
  </body>
</html>
~;
  
  my $js_array_rows = '';
  $js_array_rows = join(',', map({'[' . join(',', map({$_ =~ /[a-z]/i ? "\"$_\"" : $_} @$_)) . ']'} @_));
  $template =~ s/\/\*\*\*plot_data\*\*\*\//$js_array_rows/;
  $template =~ s/\/\*\*\*title\*\*\*\//$title/;
  return $template;
}

sub get_help_text {
return $usage . q~

SUMMARY:
  This script is designed to automatically produce a consolidated set of results
    from a number of xml files as produced by Franz's ENVI/IDL tools. The output
    is a csv file and/or an html file. The only required argument is a list of
    xml files (see below for exceptions). Any number of xml files can be
    specified, and may include the usual shell wildcards such as *. Additional
    options are described below. The order of any arguments or options is
    irrelevant.


OPTIONS:
    --help                Optional: displays this help text. Ignores all other
                            options and arguments.
    --out=<csv file>      Optional: this option specifies the output csv file.
                            The specified file, if it already exists, will be
                            over-written with the new csv data. If neither this
                            option nor --plot is specified, the csv data will be
                            written to std out.
    --plot=<html file>    Optional: this option specifies the output html file.
                            The specified file, if it already exists, will be
                            over-written with the new html data. If neither this
                            option nor --out is specified, the csv data will be
                            written to std out.
    --include=<csv files> Optional: this option specifies any number of
                            additional csv files to be included in the final
                            output. The included csv files must be of the same
                            format as those produced by this script. This option
                            exists to allow separate runs of this script to be
                            consolidated into one single result set. This option
                            may be specified multiple times, and supports the
                            usual shell wildcards such as *. If this option is
                            specified, the usually-required xml files become
                            optional.
    --title=<title>       Optional: this option specifies the title that will be
                            displays at the top of the html page produced using
                            the --plot option, as well as in the window title.
                            If your title includes spaces, be sure to use
                            quotation marks or the arguments will become con-
                            fused. The first and last quotation marks will
                            not be included in the actual title, so if you
                            actually want them to appear, use double-marks
                            (i.e. ""title"" or ''title'')


EXAMPLES:
  analysis.pl --help
    Display this help text and exit.

  analysis.pl ./data1.xml ./data2.xml ./morefiles/*.xml
    Process data1.xml, data2.xml, and all .xml files in morefiles/, output csv
    to standard out

  analysis.pl *.xml --out=out.csv
    Process all .xml files in the current directory to the csv file out.csv

  analysis.pl *.xml --plot=out.html
    Process all .xml files in the current directory to the html file out.html

  analysis.pl *.xml --plot=out.html --include=run1.csv --include=run2.csv *.xml
    Process all .xml files in the current directory to the html file out.html,
    including the previously-generated csv files run1.csv and run2.csv

  analysis.pl *.xml --plot=out.html --out=out.csv
    Process all .xml files in the current directory to the csv file out.csv
    as well as to the html file out.html

  analysis.pl *.xml --plot=out.html --title="Legacy FFT Matching (Whole Image)"


REQUIREMENTS:
  This script makes use of a few CPAN modules which may or may not be installed
  by default. If you see an error about a module not being found in @INC or
  similar, be sure all of the following modules are installed:
    Getopt::Long
    XML::Simple
    List::Util
    List::MoreUtils
    Text::CSV
    Data::Dumper


~;
}

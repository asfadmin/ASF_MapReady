#!/usr/bin/perl -w
=head1 NAME

filter_tie_points.perl - filter tie points from a JPL tie point file

=cut

use diagnostics;
use strict;

# Standard libraries (they ship with perl).
use File::Basename qw(basename);
use FileHandle;
use Getopt::Long;
use POSIX qw(floor);
use Pod::Usage;

use Math::Trig qw(:radial deg2rad rad2deg);

# For nonstandard libraries I installed myself.
use lib "/home/bkerin/local/lib/site_perl";

# Non-standard libraries (available from CPAN).
use Params::Validate;

# Locally developed libraries.
use CoordinateConversion qw(line_sample2sc sch2xyz xyz2llr
			    sc2line_sample xyz2sch llr2xyz);

my $progname = basename($0);
my $version = '0.2.0';

=head1 SYNOPSIS

B<filter_tie_points.perl> I<gridcellsize> I<infile> I<outfile>

=head1 DESCRIPTION

This program is intended to ease the process of mosaicing a pair of
JPL AIRSAR/TOPSAR images together by selecting a spatially well
distributed high quality set of tie points from a large number of tie
points.

=head1 OPTIONS AND ARGUMENTS

This program first sorts the tie points found in the JPL AIRSAR/TOPSAR
system tie point file I<infile> into a grid of cells of size
I<gridcellsize> meters.  The optimum (in the sense of some hardwired
criteria, see the source) tie point in each cell is then selected and
added to flattened (gridless unsorted) output tie point file I<outfile>.
If I<outfile> already exists when the script is run, it is clobbered.

There is only one nonstandard option:

=over 4

=item B<-l> I<latlongfile>, B<--latlongfile>=I<latlongfile>

In addition to I<outfile>, a file I<latlongfile> consisting of
latitude, longitude, and height for each selected tie point is also
generated.  The orders of the listings of points in I<outfile> and
I<latlongfile> correspond.

=back

The standard options are also available:

=over 4

=item B<-?>, B<--help>

Print usage information.

=item B<--version>

Print name and version of program.

=cut

# Command line parameters, initialized to default values.
my %p = (
	 # Required arguments.
	 'gridcellsize' => undef,
	 'infile'       => undef,
	 'outfile'      => undef,

	 # Options.
	 'latlongfile' => undef,

	 # Standard options.
	 'help'    => 0,	# If given print help and exit.
	 'version' => 0,	# If given print program version and exit.
	 );

# Get the options, doing our own error printing.
$SIG{__WARN__} = sub { pod2usage("$progname: Option parse failed: ${my $tmp = shift; chomp($tmp); \$tmp; }") };
unless ( GetOptions(\%p, "latlongfile|l=s", "help|?", "version") ) {
    exit(1);
}
$SIG{__WARN__} = 'DEFAULT';     # Restore default __WARN__ handler.

if ( $p{version} ) {
    print <<END_VERSION_INFO;
filter_tie_points.perl version $version
END_VERSION_INFO
    exit(0);
}

if ( $p{help} ) {
    pod2usage('-exitval' => 0);
}

# Process arguments.
($p{gridcellsize}, $p{infile}, $p{outfile})
    = validate_pos(@ARGV, # Mandatory arguments.
		          { callbacks =>
			    { 'is integer' => sub { $_[0] =~ m/\s*\d+\s*/; }}},
		          { callbacks => 
			    { 'readable file' => sub { -r shift; }}},
		          1);

open(INFILE, "<$p{infile}")
    or die "could not open $p{infile} for reading: $!";

# Read information about one the reference image.

# We save the entire tie point header so we can regurgitate it into
# the output file.
my $tie_point_header = "";

# Scan to beginning of reference image description paragraph.
while ( <INFILE> ) { 
    $tie_point_header .= $_;
    last if m/sch ; REFF Data file type/; 
}
unless ( m/sch ; REFF Data file type/ ) {
    die "didn't find REFF Data file type header while parsing $p{infile}";
}

# Read dimensions of image in pixels.
my %dimensions_in_pixels;
while ( <INFILE> ) {
    $tie_point_header .= $_;
    if ( m/\s*(\d+)\s+(\d+)\s+; REFF Data file dimensions/ ) {
	$dimensions_in_pixels{'along track'} = $1;
	$dimensions_in_pixels{'cross track'} = $2;
	last;
    }
}
unless ( %dimensions_in_pixels ) {
    die "didn't find REFF Data file dimensions while parsing $p{infile}";
}


my $decimal_number_rgx = '[-+]?\d+(?:\.\d+)'; # For parsing number fields.

# Read pixel sizes.
my %pixel_sizes;
while ( <INFILE> ) {
    $tie_point_header .= $_;
    if ( m/\s*($decimal_number_rgx)\s+($decimal_number_rgx)\s+
     	   ;\sREFF\sPost\sSpacing/xo ) {
        $pixel_sizes{'along track'} = $1;
        $pixel_sizes{'cross track'} = $2;
	last;
    }
}
unless ( %pixel_sizes ) {
    die "didn't find REFF Pose Spacing while parsing $p{infile}";
}


# Offset of starting corer of image (relative to peg point).
my %start_offset;
while ( <INFILE> ) {
    $tie_point_header .= $_;
    if ( m/\s*($decimal_number_rgx)\s+($decimal_number_rgx)\s+
     	   ;\sREFF\sStarting\scorner\sposition/xo ) {
        $start_offset{'along track'} = $1;
        $start_offset{'cross track'} = $2;
	last;
    }
}
unless ( %start_offset ) {
    die "didn't find REFF Starting corner position while parsing $p{infile}";
}

# Image peg point.
my %peg_point;
while ( <INFILE> ) {
    $tie_point_header .= $_;
    if ( m/
	   \s*($decimal_number_rgx)\s+ # Peg point latitude.
	   ($decimal_number_rgx)\s+    # Peg point longitude.
	   ($decimal_number_rgx)\s+    # Aircraft heading at peg point.
     	   ;\sREFF\sPeg\sposition\s\(WGS-84\)   # Field label.
          /xo ) {
        $peg_point{latitude} = $1;
        $peg_point{longitude} = $2;
	$peg_point{heading} = $3;
	last;
    }
}
unless ( %peg_point ) {
    die "didn't find REFF Peg position (WGS-84) while parsing $p{infile}";
}

# Since all tie points are by definition in both images, we don't need
# the header data for the second image.  We store and work with
# positions of points relative to the first image.  So scan to last
# line of that paragraph.
while ( <INFILE> ) { 
    $tie_point_header .= $_;
    last if m/; SRCH Peg position/; 
}
unless ( m/; SRCH Peg position/ ) {
    die "didn't find anything that looked like the end of the header data while parsing $p{infile}";
}

# Remainder of file is a list of tie points.
my @tie_points;			# Array of tie point hash references.
# There are supposed to be twelve columns, but they are sometimes split between
# two lines, so we have to keep track of the field number.
while ( <INFILE> ) {
    my $tie_point_line_pattern = '^'.("\\s+($decimal_number_rgx)" x 12).'\s*$';
    if ( m/$tie_point_line_pattern/o ) {
	push(@tie_points, { 'whole line' => $&,
	                    'along track position in pixels' => $1,
			    'cross track position in pixels' => $2,
			    'height in meters' => $3,
			    'signal to noise ratio' => $9,
			    '1st diagonal correlation' => $10,
			    '2nd diagonal correlation' => $11 });
    }
}

close(INFILE) or die "could not close $p{infile}";

# Create a view of the tie points sorted into grid cells.
my %gridded_tie_points;		# Hash of lists of tie point hashes.
for ( @tie_points ) {
    my $along_track_position_in_meters 
	= $_->{'along track position in pixels'} * $pixel_sizes{'along track'};
    my $cross_track_position_in_meters
	= $_->{'cross track position in pixels'} * $pixel_sizes{'cross track'};

    # Indicies for the grid hash.
    my $ati = floor($along_track_position_in_meters / $p{gridcellsize});
    my $cti = floor($cross_track_position_in_meters / $p{gridcellsize});
    
    # Create cell hash entry if it doesn't exist yet.
    $gridded_tie_points{"$ati $cti"} ||= [];

    push(@{ $gridded_tie_points{"$ati $cti"}; }, $_);
} 

# Select most suitable tie point in each grid cell.
my %selected_tie_points;	# Hash of tie point hashes.
# Compute the fitness of a tie point.
sub tie_point_fitness {
    my $arg = shift;		# A reference to a tie point hash.
    return $arg->{'signal to noise ratio'} 
           / sqrt($arg->{'1st diagonal correlation'} 
		  + $arg->{'2nd diagonal correlation'});
}
foreach my $key ( keys %gridded_tie_points ) {
    my $best_so_far = $gridded_tie_points{$key}[0]; # Best tie point so far.
    foreach ( @{$gridded_tie_points{$key};} ) {
	if ( &tie_point_fitness($_) > &tie_point_fitness($best_so_far) ) {
	    $best_so_far = $_;
	}
    }
    $selected_tie_points{$key} = $best_so_far;
}

# Find the lat/longs of the selected tie points.
foreach ( keys %selected_tie_points ) {
    # Find s and c coordinates.
    my ($s, $c) = line_sample2sc(
        $selected_tie_points{$_}{'along track position in pixels'}, 
	$selected_tie_points{$_}{'cross track position in pixels'}, 
	$pixel_sizes{'along track'}, 
	$pixel_sizes{'cross track'}, 
        $start_offset{'along track'}, 
   	$start_offset{'cross track'});

    # Convert s, c, and height to global cartesian coordinates.
    my ($x, $y, $z) = sch2xyz($s, $c, 
			      $selected_tie_points{$_}{'height in meters'},
			      $peg_point{latitude}, $peg_point{longitude},
			      $peg_point{heading});
    # Convert global cartesian coordinates to geodetic lat/long.
    my ($lat, $lon, $radius) = xyz2llr($x, $y, $z);
    # Stick lat and lon into the tie point structure.  Note that these
    # don't end up part of the 'whole line' field, since they weren't
    # in the original tie point file.
    $selected_tie_points{$_}{latitude} = $lat;
    $selected_tie_points{$_}{longitude} = $lon;
}

# Open outpuf file(s).
open(OUTFILE, ">$p{outfile}")
    or die "couldn't open $p{outfile} for writing: $!";
if ( defined($p{latlongfile}) ) {
    open(LATLONGFILE, ">$p{latlongfile}")
	or die "couldn't open $p{latlongfile} for writing: $!";
}


# Input files have a couple newlines that don't get memorized.
print OUTFILE "$tie_point_header\n\n";

# Output selected tie points.    
foreach ( keys %selected_tie_points ) {
    print OUTFILE $selected_tie_points{$_}{'whole line'};
    if ( defined($p{latlongfile}) ) {
	printf LATLONGFILE "%.8f %.8f %.4f\n", 
  	                   $selected_tie_points{$_}{'latitude'},
	                   $selected_tie_points{$_}{'longitude'},
	                   $selected_tie_points{$_}{'height in meters'}
    }
}

close(OUTFILE) or die "couldn't close $p{outfile}";
if ( defined($p{latlongfile}) ) {
    close(LATLONGFILE) or die "couldn't close $p{latlongfile}";
}

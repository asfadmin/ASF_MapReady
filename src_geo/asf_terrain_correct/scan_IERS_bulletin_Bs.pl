#!/usr/bin/perl -w

use Math::Trig qw( pi );

# This script processes a bunch of issues of International Earth
# Rotation Service (IERS) Bulletin B and generates a data file
# containing a lookup table which can be used to relate various time
# scales at various points in time.  See data_time.h for details about
# the time scales.  This script takes two arguments: the first is a
# directory which contains the bulletin B issues, in files with names
# which must m/bulletinb.\d\d\d/, the second is the name to use for
# the datafile formed from these issues.

my $program_name = "scan_IERS_bulletin_Bs.pl";

# Things to fix/add:

# I'd like to have a -o option wherein the script would go online and
# check for the latest data from IERS.

# Insist that we have a consistent notion of our own name.
$0 =~ m/$program_name$/ or die;
    
@ARGV == 2 or (print "Usage: $program_name bulletin_directory output_file\n" 
	       and exit(1));
opendir(INPUT_DIR, "$ARGV[0]") or die;
my $input_dir = $ARGV[0];
#!-e $ARGV[1] or die "file $ARGV[1] already exists";
open(OUTPUT_FILE, ">$ARGV[1]") or die;
my $output_file = $ARGV[1];

# Form a list of IERS bulletin B issues in the input directory.
my @bulletin_bs;
my @files_in_input_dir = readdir(INPUT_DIR);
foreach ( @files_in_input_dir ) {
    if ( m/^bulletinb\.\d?\d\d$/ ) {
	push(@bulletin_bs, "$input_dir/$_");
    }
}

# Assuming @bulletin_bs has been accurately populated, work with all
# the files it refers to and scan them for data we want.  Put the
# desired data in @final_data.
my @final_data;
foreach ( @bulletin_bs ) 
{
    # Open the file or die.
    open(CUR_FILE, $_) or die "Couldn't open \"$_\": $!\n\t";
    @file_data = <CUR_FILE>;    #read the file
    close(CUR_FILE) or die;    #close the file

    # Hash of different TAI minus UTC offsets of the entries in this
    # Bulletin B issue, indexed by capitol three letter abbreviation
    # for month.  TAI minus UTC offsets don't change within months.
    my %tai_minus_utc;

    # End of the time span for this file.
    my ($end_month, $end_day);

    # Loop through the data.
    for ( my $ii = 0; $ii < @file_data; ++$ii ) 
    {
        # The start of our data range.
        if ( $file_data[$ii] =~ /(Final Bulletin)/ )
        {
            ++$ii;
            # The end of our data range.
            while ( $file_data[$ii] !~ /Preliminary extension/ )
            {
                # This is ugly, but all it does is grab the month and
                # day, along with UT1R-UTC, and UT1R-TAI
                if ( $file_data[$ii] =~ / (JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP
					   |OCT|NOV|DEC)\s* # Month
		                           (\d+)\s+         # Day of month
                                           (-?\d*\.?\d*)\s+ # Unused
                                           (-?\d*\.?\d*)\s+ # Unused
                                           (-?\d*\.?\d*)\s+ # Unused
		                           (-?\d*\.?\d*)\s+ # UT1R-UTC
                                           (-?\d*\.?\d*)\s+ # UT1R-TAI
                                        /x ) {

                    # We also need an offset, TAI - UTC, which is
                    # equal to (UT1R-UTC)-(UT1R-TAI).  This is usually
                    # constant within a given bulletin B issue, but
                    # can change between months if the product spans
                    # time containing a leap second.  So we have an
                    # hash of TAI to UTC offsets for the different
                    # months abbreviations.
		    $tai_minus_utc{$1} ||= $6 - $7;

                    # Save the last month listed in this section for later,
                    $end_month = $1;
                    # and the same for the day, plus one.
                    $end_day = $2 + 1;
                }
                ++$ii;
            }
        }

	# Did we find what we want?
        if ( $file_data[$ii] =~ /(SMOOTHED VALUES)/ )
        {
            ++$ii;		# Move to the next line.
	    my $current_year;	# Year this line falls in.
            # Are we past the valid data range?
            while ( $file_data[$ii] !~ /($end_month)\ +($end_day)/ )
            {
                # If this is the line at the begining that starts with
                # the year, grab that year.
                if ( $file_data[$ii] =~ /^\s*(\d{4})/ )
                {
                    $current_year = $1;
                    ++$ii;
                }
                # Is it a valid line in this section?
                # Month, Day, MJD, x, y, UT1-UTC, UT1-UT1R, D, dPsi, dEpsilon
                if ( $file_data[$ii] =~ / (JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP
                                           |OCT|NOV|DEC)\s+
                                          (\d+)\s+ # Day of month.
                                          (\d+)\s+ # Modified julian day.
                                          (-?\d*\.?\d*)\s+ # x pole offset.
                                          (-?\d*\.?\d*)\s+ # y pole offset.
                                          (-?\d*\.?\d*)\s+ # UT1-UTC
                                          (-?\d*\.?\d*)\s+ # UT1-UT1R
                                          (-?\d*\.?\d*)\s+ # unused
                                          (-?\d*\.?\d*)\s+ # Delta psi
                                          (-?\d*\.?\d*) # unused
                                       /x )
                {
                    # If it is, save the data we want for later.

		    # Convert from milliseconds to seconds.
                    $converted = $7 / 1000.0;  

		    # Convert from hundredths of arc seconds to radians.
		    my $delta_psi = ($9 / (1000.0 * 60.0 * 60.0)) * (pi / 180.0);

                    # Tie all the data together in our order:
                    # MJD, Year, Month, Day, UT1-UTC, UT1-UT1R, delta psi
		    push(@final_data, $3."\t".$current_year."\t".$1."\t".$2
			 ."\t".$6."\t".$converted."\t".$tai_minus_utc{$1}."\t"
			 .$delta_psi."\n");
                    # Need to adjust the year after December 31
                    if ( $1 eq "DEC" && $2 == 31 )
                    {
                        ++$current_year;
                    }
                }
                # Move to the next line.
                ++$ii;
            }
        }
    }
}

# Sort the data.  Since we start with MJD in the first field we have a
# convenient sort: MJD will supercede everything else.
@final_data = sort(@final_data);

# Some of the older files partially duplicated other files...we
# haven't tried to catch that yet, so we'll do it here: quite simply,
# delete any duplicate lines (or just replace them with null
# strings...this way, we can still detect that there was a duplicate
# line, but it won't show up in the output file).
for ( my $ii = 0 ; $ii < @final_data ; ++$ii ) # Loop through the data.
{
    $_ = $final_data[$ii];	# Grab the line.
    if ( /^(\d+)/ ) # Make sure it's valid (starts with an integer: MJD).
    {
	# It its not the last line, is it the same as the next line?
        if ( $ii < $#final_data and $final_data[$ii + 1] =~ /^$1/ )
        {
            $final_data[$ii] = ""; # If so, kill it.
        }
    }
}

# At this point, we should have a complete set of data, @final_data,
# and all we need to do is write it to a final output file.

# First, print a one-line header, for examination purposes.
print OUTPUT_FILE "MJD\tYear\tMonth\tDay\tUT1-UTC(s)\tUT1-UT1R(s)\t"
                  ."TAI-UTC(s)\tdelta_psi (radians)\n" 
    or die "can't write to $output_file: $!";
# Print each line to the file. If anything goes wrong, freak out.
foreach my $line (@final_data)
{
    print OUTPUT_FILE "$line" or die "can't write to $output_file: $!";
}

close(OUTPUT_FILE) or die;  

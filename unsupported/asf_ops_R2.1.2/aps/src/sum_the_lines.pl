#! /usr/local/bin/perl
#
#       Usage: $0 <filename>
#
#       DESCRIPTION: takes lines of single numbers in <filename> and
#			adds them together
#


if ($#ARGV != 0)
{
	print "Usage: ", $0, " <filename>\n" ;
	exit( 1 ) ;
}

$numLines = 0 ;
while (<ARGV>)
{
	chop ;
	if ($_ =~ "^[ 	]*[0-9]*[ 	]*$")
	{
		$numLines += $_ ;
	}
}

print $numLines, "\n" ;

exit( 0 ) ;

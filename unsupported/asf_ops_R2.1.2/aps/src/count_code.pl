#! /usr/local/bin/perl
#
#       Usage: $0 -c | -f <fname> ...
#
#       DESCRIPTION: counts # of non-comment and non-blank lines in the files
#

sub Usage
{
	print "Usage: ", $0, " -c|f <filename> ...\n" ;
	exit( 1 ) ;
}

sub c_nosrc	# returns 1 if line contains no src code (or is a blank line)
{
	if ($_ =~ /^[ 	]*$/)
	{
		return 1 ;	# blank line
	}

	if (! $inComment)
	{
		if ($_ =~ /^[ 	]*\/\*/)	# line begins w/ a comment, test rest of it
		{
			$inComment = 1 ;
			$_ = substr( $_, $position ) ;
			return &c_nosrc( $_ ) ;
		}
		else
		{
			return 0 ;	# line begins with src code
		}
	}
	else	# are in the middle of a comment
	{
		if (($position = index( $_, "*/", $position ) + 2) < 2)
		{
			return 1 ;	# still in the middle of a comment
		}
		else	# found end of comment, test the rest of the line
		{
			$inComment = 0 ;
			$_ = substr( $_, $position ) ;
			return &c_nosrc( $_ ) ;
		}
	}

	print "***ERROR***: should NEVER get here\n" ;
	return 1 ;
}


if ($#ARGV < 1)
{
	&Usage() ;	#EXITs
}

$_ = shift ;
if ($_ eq '-c')
{
	$isFortran = 0 ;
	$inComment = 0 ;
}
elsif ($_ eq '-f')
{
	$isFortran = 1 ;
}
else
{
	&Usage() ;	# EXITs
}

$lineCount = 0 ;
while ($_ = <ARGV>)
{
	# if FORTRAN
	if ($isFortran)
	{
		# ignore lines of comment-only
		next if ($_ =~ /^[cC\*dD\!]/ || $_ =~ /^[ 	]*\!/) ;
		# ignores blank lines
		next if ($_ =~ /^[ 	]*$/) ;
	}
	else	# if C
	{
		next if (&c_nosrc( $_ ) == 1) ;
	}

	$lineCount++ ;
}

print $lineCount, "\n" ;

exit( 0 ) ;

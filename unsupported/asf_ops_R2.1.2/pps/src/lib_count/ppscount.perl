#!/usr/local/bin/perl

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================

# Usage: ppscount.perl input_file hdr_file;
# SCCS: "@(#)ppscount.perl	1.1    11/21/96"

# check usage
if ($#ARGV != 1) {
    die "Usage: ppscount.perl input_file hdr_file";
}

# get args
($in, $defs) = @ARGV;

# open files
open(in) || die "Can't open $in";
open(defs, ">$defs") || die "Can't open $defs";

# write headers
print defs "/* ppscount.h - generated, do not edit */\n\n";
print defs "#ifndef PPSCOUNT_H\n#define PPSCOUNT_H\n\n";

# set offset to first message
$num = 1000;
print defs "#define PPS_MSG_OFFSET $num\t\t/* offset to first message */\n";

# write defines
for ($_ = <in>; $_; $_ = <in>) {
	/^([^ \t]*)[ \t]+(.*)/ && ($name = $1, $mess = $2);
	print defs "#define $name $num\n";
	$num++;
}
print defs "#define PPS_MSG_LIMIT $num\t\t/* max message value + 1 */\n";

# write other defines
print defs "\n#define PPSLOGNAME \"ppscount.log\"\n";

# write table
close(in);
open(in) || die "Can't reopen $in";

# struct header
print defs "\n";
print defs "#ifdef MAIN\n";
print defs "struct {\n";
print defs "\tchar *txt;\n";
print defs "} ppscount[] = {\n";

# struct contents
for ($_ = <in>; $_ && !/^}/; $_ = <in>) {
	/^([^ \t]*)[ \t]+(.*)/ && ($name = $1, $mess = $2);
	print defs "\t\"$mess\",\n";
	$num++;
}

# finish da file
print defs "\tNULL\n};\n";
print defs "#endif /* MAIN */\n";
print defs "\n#endif /* PPSCOUNT_H */\n";


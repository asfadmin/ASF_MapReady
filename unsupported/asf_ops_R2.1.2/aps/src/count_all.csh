#! /usr/bin/csh -f
#
# Usage $0 <filename> ...
#
# prints the number of lines in <filename> ...

if ($#argv) then
	set tmpTotal = `wc -l $* | tail -1`
	if ($#tmpTotal) then
		echo $tmpTotal[1]
	endif
endif

exit( 0 ) ;

#!/bin/sh
#
# @(#)log_lpr.sh	1.3 93/05/21 16:27:17 (c)NOCC
#
# script for printing a log print file to the lw printer.
# following line is for old printer control:
#PRINT_ID=_PRINTER_

#new printer control mechanism:
cd; PRINT_ID="`awk -F= '$1 == "LOG_PRINTER" {print $2}' .printer`"
printer=$PRINT_ID

PATH=$PATH:/usr/X11/bin

#
# A function for how to use this shell script
#
usage() {
echo "usage: `basename $0` [-p startline numlines] [-e|f filter1 [AND|OR filter2]] filename "
echo ""
echo "	-p <start> <num> 	partial print	"
echo "	-e <filter exp> 	exclusion filter 	"
echo "	-f <filter exp> 	inclusion filter 	"
echo "	-P <printer>		printer "
echo "	-t <title>			page title"
echo ""
}

#
# test the arguments
#


title=""

startline=""
numlines=""
op1="NONE"
op2="NONE"
filter1=""
filter2=""
filename=""
mail=""

while [ "$1" != "" ]
do
	case "$1" in
	-x)
		set -x
		;;
	-P)
		shift
		printer="$1"
		;;
	-t)
		shift
		title="$1"
		;;
	-p)
		shift
		startline=$1
		shift
		numlines=$1
		;;
	-m)
		mail="-m"
		;;
	-e)
		op1="NOT"
		op2="ONE"
		shift
		filter1=$1
		;;
	-f)
		op2="ONE"
		shift
		filter1=$1
		;;
	AND)
		op2="AND"
		shift
		filter2=$1
		;;
	OR)
		op2="OR"
		shift
		filter2=$1
		;;
	-help)
		usage
		exit
		;;
	*)
		filename=$1
		;;
	esac
	shift
done

#  Make sure the one required option is there
if [ -z "$filename" ];then 
	usage
	exit
fi

#	Begin building the printout title
if [ -z "$title" ]; then 
	title="LOG Printout"
fi

tmp="/tmp/`basename $0`.$$"


#	Continue building the title and do the filtering
case $op2 in 
ONE)
	title="Filtered $title "
	case $op1 in
	NOT) 
		grep -iv "$filter1" $filename >$tmp
		;;
	*)
		grep -i "$filter1" $filename >$tmp
	esac
	;;
AND)
	title="Filtered $title "
	case $op1 in
	NOT) 
		set noglob
		perl -e '
			$fil1 = shift(@ARGV) ;
			$fil1 =~y/A-Z/a-z/ ;
			$fil2 = shift(@ARGV) ;
			$fil2 =~y/A-Z/a-z/ ;
			while (<>) {
				$entry = $_ ;
				$entry =~y/A-Z/a-z/ ;
				if (index($entry,$fil1)<$[ || index($entry,$fil2)<$[) {
					print;
			}
		}' $filter1 $filter2 $filename > $tmp
		unset noglob
		;;
	*)
		grep -i "$filter1" $filename | grep -i "$filter2" >$tmp
	esac
	;;
OR)
	title="Filtered $title "
	case $op1 in
	NOT) 
		egrep -iv "$filter1|$filter2" $filename >$tmp
		;;
	*)
		egrep -i "$filter1|$filter2" $filename >$tmp
	esac
	;;
*)
	cat $filename >$tmp
	;;
esac

#	Finish building the title and do the printing
logname="`basename $filename`"

if [ -z "$startline" ] ; then 
	lpr $mail -P$printer -p \
		-T "$title $logname" -C "$title" -J "$logname" $tmp
else 
	title="Partial $title"
	tail +"$startline"l $tmp | head -$numlines | \
		lpr $mail -P$printer -p \
		-T "$title $logname" -C "$title" -J "$logname"
fi

#	Cleanup
rm $tmp
exit
	
# end of script

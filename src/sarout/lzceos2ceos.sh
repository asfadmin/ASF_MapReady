#!/bin/sh
#
#
#
#
#

USAGE()
{
	echo "Usage: lzceos2ceos [-l] <input lz ceos> <output level-1 ceos>"
	echo ""
	echo "   Creates an ASF formatted level-one (detected) image"
	echo "   from a Level-Zero CEOS formatted signal file"
	echo ""
	echo "   Defaults to creation of 12.5 meters ground range image"
	echo "   Use -l switch to create 100 meter (lo-res) image"
	echo " "
	echo "   Do not give extensions for the files on the command line!"
	echo " "
	exit 1
}




if [ ! $# -eq 2 -a ! $# -eq 3 ]
then
	echo "No Parameters Passed"
	USAGE
fi

lores="n"


while [ $# -gt 2 ]
do
  case $1 in
	-l)
	    lores="y"
	    shift;;
	*)
	    echo "Unrecognized option "$1
	    USAGE 
  esac
done
	

echo ceos2raw $1.raw $1_raw
ceos2raw $1.raw $1_raw

echo fix_in $1.par $1_raw.in
fix_in $1.par $1_raw.in

echo mpirun -np 8 `which -f paisp` -d 1 -e 1 $1_raw $1_proc
mpirun -np 8 `which paisp` -d 1 -e 1 $1_raw $1_proc

echo sr2gr $1_proc_amp $1_proc $1_proc_gr $1_proc_gr 12.5
sr2gr $1_proc_amp $1_proc $1_proc_gr $1_proc_gr 12.5

if [ $lores = "y" ]
then
  echo remap -scale 0.125 0.125 -kernel 9 9 $1_proc_gr $1_lores
  remap -scale 0.125 0.125 -kernel 9 9 $1_proc_gr $1_lores

  echo sarout d $1_proc_gr $1_lores $2
  sarout d $1_proc_gr $1_lores $2

  rm $1_lores.*

else

  echo sarout d $1_proc_gr $1_proc_gr $2
  sarout d $1_proc_gr $1_proc_gr $2

fi

rm $1_proc_gr.*





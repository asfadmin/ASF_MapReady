#!/bin/ksh -p
# NAME:  refine_base
#
# SYNOPSIS:
#
#  refine_base [-k __iter__ ]  phase tie_points meta old_base new_base
#
#  -s seed_val    Change seed_phase value. Default = 0.0.";
#  -k __iter__    Keep intermediate products.  Iterations=__iter__";
#
#  phase       unwrapped interferogram phase file" ;
#  tie_points  tie-point location file.";
#  meta        filename containing interferogram's metadata.";
#  old_base    baseline file containg four parameters:" ;
#              Bn delta_Bn Bp delta_Bp" ;
#  new_base    refined baseline file containg four parameters:" ;
#              Bn delta_Bn Bp delta_Bp" ;
#
#	Version 1.1, ASF IFSAR Tools
#
# DESCRIPTION:
# 
#     Perform baseline refinement using a set of 4 programs: getphase, genab,
#     bp, and test_base. The last one, test_base, does not refine the
#     baseline at all. Its only purpose is to check the validity of the new
#     baseline.
#
#     Getphase extracts phase values from an associated tie point file. 
#
#     Genab creates a matrix (A) and a vector (b) that are related by the
#     equation Ax=b. 
#
#     Bp solves the above equation in which the vector x is our new baseline
#     values.
#
#
#
# EXTERNAL:
#
#     getphase, genab, bp, and test_base are programs external to this
#     script, but in the refine-base directory
#
# PROGRAM HISTORY:
#
#   1.0 - original program     Mike Shindle  1996 November 14
#   1.1 - Takes Parameters from metadata & ddr.  O. Lawlor 8/97
#
#
# LIMITATIONS:
#
#	
#****************************************************************************
#								            *
#   refine_base - perform baseline refinement.				    *
#   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
#									    *
#   This program is free software; you can redistribute it and/or modify    *
#   it under the terms of the GNU General Public License as published by    *
#   the Free Software Foundation; either version 2 of the License, or       *
#   (at your option) any later version.					    *
#									    *
#   This program is distributed in the hope that it will be useful,	    *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
#   GNU General Public License for more details.  (See the file LICENSE     *
#   included in the asf_tools/ directory).				    *
#									    *
#   You should have received a copy of the GNU General Public License       *
#   along with this program; if not, write to the Free Software		    *
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
#									    *
#       ASF Advanced Product Development LAB Contacts:			    *
#	APD E-mail:	apd@asf.alaska.edu 				    *
# 									    *
#	Alaska SAR Facility			APD Web Site:	            *	
#	Geophysical Institute			www.asf.alaska.edu/apd	    *
#      	University of Alaska Fairbanks					    *
#	P.O. Box 757320							    *
#	Fairbanks, AK 99775-7320					    *
#								  	    *
#**************************************************************************/
#

# set initial variables
USG1="Usage: refine_base [-k __iter__] phase tie_points meta old_base new_base"
USG5="refine_base -h"
VER="Version 1.1, ASF SAR Tools"
# default values for the look_window
seed_val=0.0

# define function print_usage
function print_usage {
      print ;
      print $USG1 ;
      print ;
      print "  -k __iter__  Keep intermediate products.  Iterations=__iter__";
      print "  phase        unwrapped interferogram phase file" ;
      print "  tie_points   tie-point location file.";
      print "  meta         filename containing interferogram's metadata.";
      print "  old_base     baseline file containg four parameters:" ;
      print "               Bn delta_Bn Bp delta_Bp" ;
      print "  new_base     refined baseline file containg four parameters:" ;
      print "               Bn delta_Bn Bp delta_Bp" ;
      print ;
      print "Corrects a given baseline for an interferogram using tie points.";
      print "$VER\n" ;
      exit 1 ;
}

iter=$$
keepIter=0

# obtain command line args
while getopts "hk:v" arg
do
  case $arg in
     k) 
      iter=$OPTARG 
      keepIter=1 ;;
     h|v)
      print_usage 
      exit 1 ;;
     *)
      print_usage
      exit 1;;
  esac
done
shift $(( $OPTIND - 1 ))

# make sure they did everything right
print "CLAs = $#"
if [[ $# -eq 5 ]] 
then
  phase_file=$1
  tp_file=$2 
  meta_file=$3
  oldbase_file=$4
  newbase_file=$5
  ctrlpt_file=ctrlpts.$iter
  matrix_file=matrixA.$iter
  vec_file=vecB.$iter
else
  print "Incorrect Number of CLAs"
  print_usage
fi

#
# get phases from unwrapped phase file
#
print 
print "###############################################"
print "Running getphase:\n"
print "getphase $phase_file $tp_file $ctrlpt_file"
getphase $phase_file $tp_file $ctrlpt_file


#
# generate matricies from phases & tie points
#
print
print "###############################################"
print "Running genab:\n"
print "genab $ctrlpt_file $oldbase_file $meta_file $matrix_file $vec_file"
genab $ctrlpt_file $oldbase_file $meta_file $matrix_file $vec_file
 
#
# create baseline and test it
#
print
print "###############################################"
print "Running bp & test_base:\n"
print "bp $matrix_file $vec_file $newbase_file"
bp $matrix_file $vec_file $newbase_file
print "test_base $newbase_file $matrix_file $vec_file"
test_base $newbase_file $matrix_file $vec_file

#
# remove intermediate files unless requested not to
#
if (( $keepIter == 0 )); then
  rm $ctrlpt_file
  rm $matrix_file
  rm $vec_file
fi

print "\nEnd refine_base."
print "Total Time = $SECONDS seconds"

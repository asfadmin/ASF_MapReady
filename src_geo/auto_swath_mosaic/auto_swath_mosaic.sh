#!/bin/sh
#
# NAME: auto_swath_mosaic - create a mosaic from two or more nongeocoded
#                           images. Will also calibrate the input images and
#                           resample the final mosaic.
# SYNOPSIS:
#        auto_swath_mosaic [-c] [-r sample_size] <output_file> <input files...>
#
# DESCRIPTION:
#     Auto_swath_mosaic takes as input a set of files from a single
#     swath.  The output is a pasted-together image of the entire
#     swath.
#       
# EXTERNAL ASSOCIATES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#      Relies on various Bourne Shell functions as well as one utility program
#      (metadata) and four LAS programs (resample, calibrate, 
#      sarin, concatm). If for some reason these programs were not included
#      in your tar file, each program is available by anonymous ftp from ASF. 
#
#      The information used by metadata is used to determine which
#      images are part of the final mosaic.
#
# FILE REFERENCES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#
# PROGRAM HISTORY:
#   Mike Shindle  
#   1995 May 31
#
#   1.0 - original program
#   1.1 - added calibrate. included option to turn calibrate off.
#       - added version number info.
#       - added resample capability.
#   1.2 - handles new resample and .tlr files
#   1.21 - Modified to use the new metadata routine, T. Logan 5/96
#   2.0 - Moved to Bourne Shell
#   2.1 - Since the .meta is replacing the DDR but hasn't quite yet entirely
#         replaced it, I added some if statements to watch for both the
#         .meta and .ddr when moving or deleting -- P. Denny 02/2004
#
# HARDWARE/SOFTWARE LIMITATIONS:
#
# ALGORITHM DESCRIPTION:
#
# ALGORITHM REFERENCES:
#
# BUGS
#	
#****************************************************************************
#								            *
#   auto_swath_mosaic - create a mosaic from two or more nongeocoded	    *
#                       images. Will also calibrate the input images and    *
#                       resample the final mosaic. 			    *
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
# set initial flags
calibrate_flag=1
search_string=""
out_mosaic=""
TMP=asm.$$
sample_size=0

#
# check_error- enter here to trap run-time errors
#
check_error ()
{
	if [ ! $? -eq 0 ]
	then
		echo "auto_swath_mosaic: Last command returned in error."
		exit 1
	fi
}

print_usage()
{ 
  echo ""
  echo "USAGE:"
  echo "   auto_swath_mosaic [-c] [-r sample_size] <output_file> <input images...>"
  echo ""
  echo "OPTIONAL ARGUMENTS:"
  echo "   -c   do not calibrate files"
  echo "   -r   resample mosaic to sample_size"
  echo ""
  echo "REQUIRED ARGUMENTS:"
  echo "   output_file     mosaic image file name"
  echo "   input files...  CEOS SAR images (do not include extensions)"
  echo ""
  echo "DESCRIPTION:"
  echo "   Assembles SAR frames from a single swath into one long image."
  echo ""
  echo "Version 2.10, ASF SAR Tools"
  echo ""
  exit 1 ;
}

# obtain command line arguments
running=1
while [ $running -eq 1 ]
do
  case $1 in
     -c)
      calibrate_flag=0
      shift;;
     -r)
      sample_size=$2
      shift
      shift;;
    *)
      running=0;;
  esac
done

# if date-time string & out_mosaic are not passed, exit.
if [ $# -lt 2 ] 
then
  print_usage
fi

# Assemble the file list

out_mosaic=$1
shift
file_list=""
while [ ! $# -eq 0 ]
do
	echo "Adding image " $1 "to the list."
	file_list=$file_list" "$1
	shift
done

# Create LAS images from SAR data
echo "Ingesting the following images:"
for i in `echo $file_list`
do
  echo "   "
  echo "Ingesting image "$i
  if [ $calibrate_flag -eq 1 ]
  then
     calibrate $i $i
  else
     sarin $i $i
  fi
  check_error
done

# Determine offsets 
echo "Obtaining offset data values..."
accum_offset $file_list > $TMP
check_error
image_list=`awk 'NF == 6 {print $2, $4, $6}' $TMP`
total_lines=`awk '/lines/{print $NF}' $TMP`
total_cols=`awk '/columns/{print $NF}' $TMP`
cat $TMP

# concatm images together. place in out_file.
echo " "
echo "Combining multiple images into single images with concatm"
echo " "
concatm $out_mosaic $total_lines $total_cols $image_list
check_error

# free up some disk space before resample
for i in  $file_list
do
  to_remove="${i}.img"
  if [ -r ${i}.meta ]
  then
     to_remove="${to_remove} ${i}.meta"
  fi
  if [ -r ${i}.ddr ]
  then
     to_remove="${to_remove} ${i}.ddr"
  fi
  rm ${to_remove}
done

# preform resample on ingested images
if [ $sample_size -ne 0 ]
then
   echo "Resampling mosaic to $sample_size meters per pixel."

   if [ -r ${out_mosaic}.meta ]
   then
      mv ${out_mosaic}.meta rs${out_mosaic}.meta
   fi
   if [ -r ${i}.ddr ]
   then
      mv ${out_mosaic}.ddr rs${out_mosaic}.ddr
   fi
   mv ${out_mosaic}.img rs${out_mosaic}.img

   resample rs${out_mosaic} ${out_mosaic} ${sample_size }
   check_error
   
   to_remove="rs${out_mosaic}.img"
   if [ -r rs${out_mosaic}.meta ]
   then
      to_remove="${to_remove} rs${out_mosaic}.meta"
   fi
   if [ -r rs${out_mosaic}.ddr ]
   then
      to_remove="${to_remove} rs${out_mosaic}.ddr"
   fi
   rm ${to_remove}

fi

# Cleanup & exit
rm $TMP
exit 0

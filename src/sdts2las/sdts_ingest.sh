#!/bin/sh
#
# NAME: sdts_ingest -- a shell script for reading in a set of SDTS files.
#
# SYNOPSIS: sdts_ingest <outputDEM> <files> [<more files> ...]
#
# DESCRIPTION:
#	This program will unpack the given USGS SDTS
#        DEMs (format: #####.tar.gz), convert them to
#        LAS format image (format: las_#.img), and concatenate
#        them into a large, seamless output DEM.
#       
# EXTERNAL ASSOCIATES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#    sdts2las
#
# FILE REFERENCES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#
# PROGRAM HISTORY:
#    VERS:   DATE:  AUTHOR:      PURPOSE:
#    ---------------------------------------------------------------
#    1.0	
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
#   sdts_ingest -- a shell script for reading in a set of SDTS files.       *
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
if [ $# -eq 0 ] 
then
	echo 
	echo "Usage: sdts_ingest <outputDEM> <files> [<more files> ...]"
	echo
	echo "This program will unpack the given USGS SDTS"
	echo "DEMs (format: #####.tar.gz), convert them to "
	echo "LAS format, and concatenate them into a large"
	echo "output DEM."
	echo
	echo "Version 1.00, ASF SAR TOOLS"
	echo
	exit
fi

outFile=$1
shift

# Convert USGS DEMs to LAS format
inFileList=""
inFileNo=1
while [ ! $# -eq 0 ]
do
	echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "Unpacking file $inFileNo-- $1"
	mkdir unpack
	cd unpack
	gunzip < ../$1 | tar xvf -
	echo "Converting DEM to LAS format..."
	sdts2las *CEL0.DDF ../las_$inFileNo.img | tail
	inFileList="$inFileList las_$inFileNo"
	cd ..
	rm -r unpack
	
	inFileNo=`expr $inFileNo "+" 1`
	shift
done


# Concatenate DEMs into a single image
echo "Combining DEMs into $outFile image..."
concat $outFile $inFileList

echo "sdts_ingest complete"

#!/bin/sh
#
# NAME: usgs_ingest - unpack and convert a set of USGS DEMs
#
# SYNOPSIS:
#        usgs_ingest <outputDEM> <files> [<more files> ...]
#                <outputDEm>  seamless output DEM.
#                <files...>   Input USGS DEM files
# DESCRIPTION:
#        This program will unpack the given old-format USGS
#        DEMs (format: #####.gz), convert them to
#        LAS format images (format: las_#.img), and concatenate
#        them into a large, seamless output DEM.
#       
# EXTERNAL ASSOCIATES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#    usgs2las
#
# FILE REFERENCES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#
# PROGRAM HISTORY:
#    VERS:   DATE:  AUTHOR:      PURPOSE:
#    ---------------------------------------------------------------
#    1.0   5/25/99  Orion Lawlor
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
#   usgs_ingest - unpack and convert a set of USGS DEMs		    	    *
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
	echo "Usage: usgs_ingest <outputDEM> <files> [<more files> ...]"
	echo "	<outputDEM>   Large seamless output DEM."
	echo " 	<files...>    Input USGS DEM files\n"
	echo "This program will unpack the given old-format USGS"
	echo "DEMs (format: #####.gz), convert them to "
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
inFileNo=1
while [ ! $# -eq 0 ]
do
	echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "Unpacking file $inFileNo-- $1"
	extention=`echo $1 | awk -F. '{print $2}'`
	if [ "x$extention" = "xgz" ]
	then
		gunzip -c $1 > unpack_$inFileNo
	else
		cp $1 unpack_$inFileNo
	fi
	echo "Converting DEM to LAS format..."
	usgs2las unpack_$inFileNo las_$inFileNo.img
	rm unpack_$inFileNo
	
	inFileNo=`expr $inFileNo "+" 1`
	shift
done


# Concatenate DEMs into a single image
echo "Combining DEMs into $outFile image..."
concat_dem $outFile las_*.img

echo "usgs_ingest complete"

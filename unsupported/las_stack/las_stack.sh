#!/bin/sh
#
# NAME:  las_stack
#
# SYNOPSIS:  las_stack <master> [ <img1> [...]]
#
# DESCRIPTION:
#	Las_stack lines up each of the images you specify with
#	the given master image, then concatenates them into a colorized
#	combination image.
#       It does the lining up using correlate(1), then
#	creates a lined-up "corr_<imgN>.img" for each input image.
#       The input images must be all be single-band LAS 6.0 byte
#	images.
#       
# EXTERNAL ASSOCIATES:
#    NAME:               USAGE:
#    ---------------------------------------------------------------
#   	dspddr
#	correlate
#	fit_plane
#	remap
#	concatm
#
# FILE REFERENCES:
#    NAME:               USAGE:
#    --------------------------------------------------------------- 
# 
# PROGRAM HISTORY:
# 	VERS:   DATE:   AUTHOR:      PURPOSE:
#    ---------------------------------------------------------------
#       1.0   10/20/98  Orion Lawlor
#       1.01  Mar 2002  P. Denny    Update call to concatm
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
#   las_stack -- Stacks a set of same-sized LAS images atop one another.    *
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
if [ $# -lt 2 ]
then
	echo ""
	echo "Usage: las_stack <master> [ <img1> [...]]"
	echo ""
	echo "    <master>   Single-band LAS 6.0 master image."
	echo "    <img1>     Second input image."
	echo "    [...]      Additional images."
	echo ""
	echo "Stacks a set of same-sized LAS images atop"
	echo "one another."
	echo "Uses sub-pixel co-registration to do the overlay."
	echo "Creates a color output image, stack.img"
	echo ""
	echo "Version 1.0, ASF SAR TOOLS"
	echo ""
	exit 1
fi

# Set variables
run=" "
master_image=$1
overlay_list=""
resamp="-bilinear"

# Figure out size of master image.
dspddr $master_image | grep NL | awk '{print substr($1,4,6),substr($2,4,6);}' - > ddrout
sizeY=`cat ddrout | awk '{print $1;}' -`
sizeX=`cat ddrout | awk '{print $2;}' -`
rm ddrout
imageNo="1"

# Line up and resample all other images to the master image
shift
while [ ! $# -eq 0 ]
do
	baseName="img$imageNo"
	$run correlate $master_image $1 coef_$baseName
	# Use awk to convert Terrain Correction tie point file to IFM tie point file
	$run cat cor_points.tpl | awk '{print $2,$1,$4,$3}' - > cor_ifm_style
	$run fit_plane cor_ifm_style co_matrix_$baseName
	$run remap -sameSize $resamp -matrix co_matrix_$baseName $1 corr_$baseName.img
	overlay_list="$overlay_list corr_$baseName 1 1"
	shift
	imageNo=`expr $imageNo + 1`
done

# Concatenate master image and rest of images, in color.
$run concatm -c stack $sizeY $sizeX $master_image 1 1 $overlay_list

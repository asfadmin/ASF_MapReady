#!/bin/sh
# demIFM: A script to extract seed points from a ground-range DEM
# for use with tandem_ifm.
#
# Program History:
#   9/97: O. Lawlor  Initial Creation
#   5/98: O. Lawlor  Added more reliable FFT Matching
#   6/98: O. Lawlor  Removed all matching, since geolocations are so accurate.

#
# Do: exectues the given shell command
#
Do()
{
	params=""
	while [ ! $# -eq 0 ]
	do
		params=$params" "$1
		shift
	done
	echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	echo "Performing:"$params
	$params
	if [ ! $? -eq 0 ]
	then
		echo "demIFM: Last command returned in error:"
		echo $params
		exit 1
	fi
}

if [ $# != "4" ]
then
	echo "\nUsage: demIFM <SAR image.ext> <metadata> <baseline file> <DEM.ext>"
	echo 
	echo "  This program co-registers a DEM to a SAR image."
	echo "  It then generates a simulated phase image from the DEM (out_dem_phase),"
	echo "  and well-distributed seed points (out_dem_seeds)"
	echo "  This is useful when running tandem_ifm(1)."
	echo "\nVersion 1.1, ASF SAR TOOLS\n"
	exit 1
fi

# Parse CLA's
sar=$1
ceos=$2
baseLine=$3
dem=$4

if [ ! -r $sar ]
then
	echo "Couldn't find image file "$sar"."
	echo "Remember- you must include the extension (.img?)"
	exit 1000
fi

if [ ! -r $dem ]
then
	echo "Couldn't find image file "$dem"."
	echo "Remember- you must include the extension (.img?)"
	exit 1001
fi


# XXXXXXX Extract the width and height of the $sar image
Do dspddr $sar | grep NL | awk '{print substr($1,4,6),substr($2,4,6);}' - > ddrout
sarHt=`cat ddrout | awk '{print $1;}' -`
sarWid=`cat ddrout | awk '{print $2;}' -`
sarHtBig=`cat ddrout | awk '{print $1+0;}' -`
sarWidBig=`cat ddrout | awk '{print $2+400;}' -`
rm ddrout


# XXXXXX Find the chunk of the DEM which corresponds to this SAR image
Do create_dem_grid $dem $sar $ceos dem_grid > /dev/null
Do fit_plane dem_grid dem_plane k 1.0


# XXXXXX Extract this chunk of the DEM
move="-matrix dem_plane -translate 0 0 -width $sarWidBig -height $sarHtBig"
Do remap $move -bilinear -float $dem dem_big.dem
Do makeddr dem_big.dem $sarHtBig $sarWidBig -float


# XXXXXXX Create the slant range height and simulated SAR image
Do reskew_dem dem_big.dem $ceos dem_lined.ht dem_sim.amp


# XXXXXX All this code just does image matching...
#Do amp2img -l 1x1 -s 1x1 dem_sim.amp dem_simbyte.img
#Do trim dem_simbyte.img dem_trimsim.img 0 0 $sarHt $sarWid
#Do fftMatch $sar dem_trimsim.img dem_corr

# Compute the image shift
#imgShiftX=`cat dem_corr | awk '{print int(0-$1);}' -`
#imgShiftY=`cat dem_corr | awk '{print int(0-$2);}' -`

# Extract the slant range height image, shifted appropriately
#Do trim dem_slant.ht dem_lined.ht $imgShiftY $imgShiftX $sarHt $sarWid
#Do trim dem_simbyte.img dem_lined_amp.img $imgShiftY $imgShiftX $sarHt $sarWid


# XXXXXXXX Convert the slant range height image into phase and seed point files.
Do dem2phase dem_lined.ht $ceos $baseLine out_dem_phase.phase
Do dem2seeds dem_lined.ht $sar out_dem_seeds

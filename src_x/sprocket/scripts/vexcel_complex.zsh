#!/bin/bash
CONVERTER_STAGE1=vexcel_complex_metadata
CONVERTER_STAGE2=vexcel_complex_data_planes
IMAGE_CONVERTER=image2pgm

##
##  Check args
##

# Check for no args
if [ $# -ne "2" ]
then
   echo  "Usage: vexcel_complex.zsh <vexcel_suffix> <output root>"
   exit 1
fi

DATAFILE='dat.'$1
LEADER_FILE='lea.'$1
TRA_FILE='tra.'$1
VOL_FILE='vol.'$1

echo $DATAFILE 


#Check to see if data file exists
if [ ! -f $DATAFILE ]
then
   echo "The data file $DATAFILE cannot be found."
   exit 1
fi

#Check to see if metadata file exists
if [ ! -f $LEADER_FILE ]
then
   echo "The leader file $LEADER_FILE cannot be found."
   exit 1
fi

#Check to see if metadata file exists
if [ ! -f $TRA_FILE ]
then
   echo "The leader file $TRA_FILE cannot be found."
   exit 1
fi

#Check to see if metadata file exists
if [ ! -f $VOL_FILE ]
then
   echo "The leader file $VOL_FILE cannot be found."
   exit 1
fi



echo "Converting complex FOCUS image " "'" $1 "'"
echo 

##
##   Convert image
##
echo "Generating metadata.."

$CONVERTER_STAGE1 $DATAFILE $LEADER_FILE $VOL_FILE $2

echo "Generating image planes.."

$CONVERTER_STAGE2 $DATAFILE $LEADER_FILE $2

echo "Finished conversion."

##
##   Convert image to pgm
##

echo "Generating pgm from image."

$IMAGE_CONVERTER $2.data1 $2.metadata $2.pgm

##
##  Convert pgm to gif
##

echo "Converting pgm to gif."
rm -f $2.gif
cat $2.pgm | pgmtoppm .9,.9,.9| ppmtogif > $2.gif
rm -f $2.pgm

echo "Finished."
echo

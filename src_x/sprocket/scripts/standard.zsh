#!/bin/bash
CONVERTER=convert_standard
IMAGE_CONVERTER=image2pgm


# Check for no args
if [ $# -ne "1" ]
then
   echo  "Usage: standard.zsh <root>"
   exit 1
fi

#Check to see if data file exists
if [ ! -f $1.D ]
then
   echo "The data file $1.D cannot be found."
   exit 1
fi

#Check to see if metadata file exists
if [ ! -f $1.L ]
then
   echo "The leader file $1.L cannot be found."
   exit 1
fi

echo "Converting standard ASF ASP or PP image " "'" $1 "'"
echo 

##
##   Convert image
##

$CONVERTER $1.D $1.L $1.converted

echo "Finished conversion."

##
##   Convert image to pgm
##

echo "Generating pgm from image."

$IMAGE_CONVERTER $1.converted.data1 $1.converted.metadata $1.converted.pgm

##
##  Convert pgm to gif
##

echo "Converting pgm to gif."
rm -f $1.converted.gif
cat $1.converted.pgm | pgmtoppm .9,.9,.9| ppmtogif > $1.converted.gif
rm -f $1.converted.pgm

echo "Finished."
echo

#!/bin/sh 

# This script was inspired by a sample provided by Brian J. Swift.
# Modified by Rich Norman.

# Capture this shells stdin and append to a file one line at a time.

# The file will be opened and closed for each line of output
# allowing the file to mv(ed) when the file grows large.

# A single parameter must be provided to this script. The parameter
# specifies the name of the file to which the output will be piped.

if test "$1" = ""
then
   echo "ERROR... $0 was executed without the required file name parameter."
   exit 0
fi

out_file=$1

echo $0 starting at `date` >> $out_file

while IFS=, read "LN"

do

  echo "$LN" >> $out_file

done

echo "Input pipe appears to have broken." >> $out_file

echo $0 stopping at `date` >> $out_file

exit 0

#!/bin/ksh -p
# DopPlot: this program extracts the doppler frequency 
# from ordinary ASF CEOS metadata, plots it against
# the frame number, and dumps the result to a file.
# The output file has the format:
#     <frame number> <Doppler value (Hz)>

if [ -z $1 ]
then
	echo "Usage: dopPlot <rev. number>"
	exit 1
fi

echo 'Printing dopplers for rev. '$1'...'
metaList=`ls | grep $1 | grep L`
if [ -z $metaList ]
then
	echo "Error! No files matching pattern "$1" found!"
	exit 1
fi

rm -f out_file_$1 &> /dev/null

# Read in the doppler for each file matching the given pattern
for fileName in $metaList
do
# Print frame number
	echo 'Processing file ' $fileName
	rm -f tmp_file
	echo $fileName > tmp_file
	awk '{ORS=" ";print substr($1,14,3)}' tmp_file >> out_file_$1
	rm -f tmp_file

# Print doppler frequency
	metadata f $fileName | \
		grep "Doppler frequency at the near range" | \
		awk -F= '{print $2}' - >> out_file_$1
done

# Plot the result graphically.
cat out_file_$1 | /export/step1/olawlor/asf_tools/bin/solaris/plotcol 1 2
echo "The frame number vs. doppler data plotted above is in out_file_"$1

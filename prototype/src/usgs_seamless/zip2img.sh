#!/bin/sh
#
# Read a USGS Seamless .zip file (from http://seamless.usgs.gov)
# and ingest it into the ASF tools.
#

inZip="$1"
out="$2"

Do() {
	echo "$0 executing> $@"
	"$@" || exit 1
}

u="unpack.$$"
echo "Unzipping $inZip into $u"
Do mkdir $u
unzip -d $u $inZip
dir=`cd $u;echo *`
echo "Extracting file $dir"  
[ -x $u/$dir ] || exit 1

Do asf_import -format geotiff $u/$dir/$dir $out
mv pre_bad_data_remap.jpeg $out.jpg

echo "Removing old directory"
Do rm -fr $u

#!/usr/bin/ksh
#

# Make sure we got a bad-frame list
if [ ! $# -eq 1 ]
then
	echo "Usage: reorder <file name list>"
	exit 1
fi

# Check to see if the bad-frame list is empty.
grep R $1 > /dev/null
if [ $? -eq 1 ]
then
	echo "No re-ordering need be done"
	exit 0
fi

# Offer the operator a chance to bail out.
echo "Would you like to re-order these bad frames:"
cat $1
echo 'Re-order these frames? (y/n)'
read resp

if [ $resp = "y" -o $resp = "Y" -o $resp = "yes" ]
then
	echo "Now re-ordering the bad frames."
	export BIN=..

	$BIN/order_parse.ksh $1
	$BIN/order_create.ksh
	echo "Re-ordered Frames." >> mail_message
	$BIN/order_submit.ksh | tee -a mail_message
fi

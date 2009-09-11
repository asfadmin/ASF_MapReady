#!/bin/sh

for f in `echo *.test`
do
	echo "------------------ Test: $f -------------------"
# 	(../bin/clui $f | tee $f.cur) || exit 1
	../bin/clui $f > $f.cur 
	if [ $? -ne 0 ]
	then
	# Testing program died
		grep "$f" compile_errors_ok.txt 
		if [ $? -ne 0 ]
		then
			cat $f.cur
			exit 1
		fi
	fi
	
	if [ -r $f.good ]
	then
	# There's a known-good value to compare to:
		diff $f.good $f.cur || exit 1
		echo "  $f passed test (compare to good output)"
	fi

	if [ -r $f.good_img ]
	then
	# There's a known-good image to compare to:
		diff $f.good_img $f.img || exit 1
		echo "  $f passed test (compare to good image)"
	fi
done

#!/bin/sh

# Old DLL-based testing scheme (misguided)
#export ASF_LIBRARY_PATH=`pwd`
#./test_all.test && echo "Tests passed!"

for f in `echo *.exe`
do
	echo "--------------------- Running test $f ---------------------"
	./$f || exit 1
done
echo "Tests passed!"


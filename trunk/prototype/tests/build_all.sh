#!/bin/sh

for dir in src/asf src/asf_meta src/pup 
do

for f in `echo ../$dir/test_*.cpp`
do
	../build/compile -I../src "$@" ../lib/asf_core.dll $f -o `basename $f .cpp`.exe || exit 1
done

done

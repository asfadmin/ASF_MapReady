#!/bin/sh 

# Exit immediately if a command exists with a non-zero status.
set -e

dirs="/usr/local /usr $HOME/local"

for dir in $dirs; do
    if [ -r $dir/lib/libgsl.a -a -r $dir/lib/libgslcblas.a \
         -a -r $dir/include/gsl/gsl_math.h ] ; then
	echo "$dir"
        exit 0
    fi
done

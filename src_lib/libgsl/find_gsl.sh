#!/bin/sh 

# This script looks in a hard coded list of directories for the GNU
# Scientific library library and header files.  As soon as it finds
# readable files lib/libgsl.a, lib/libgslcblas.a, and
# include/gsl/gsl_math.h under a common root directory, it prints the
# name of that root directory and exits.  If it doesn't find such
# files under any of the hard coded root directories, it exits without
# printing anything.

# Exit immediately if a command exits with a non-zero status.
set -e

# Directories to look in.
dirs="/usr/local /usr $HOME/local"

for dir in $dirs; do
    if [ -r $dir/lib/libgsl.a -a -r $dir/lib/libgslcblas.a \
         -a -r $dir/include/gsl/gsl_math.h ] ; then
	echo "$dir"
        exit 0
    fi
done

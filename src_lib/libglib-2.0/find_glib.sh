#!/bin/sh 

# This script looks in a hard coded list of directories for GLIB.

# Exit immediately if a command exits with a non-zero status.
set -e

# Directories to look in.
dirs="/usr/local /usr $HOME/local"
# dirs="/usr/local"

for dir in $dirs; do
    if [ -r $dir/lib/libglib-2.0.a -a -r $dir/lib/libiconv.a \
         -a -r $dir/include/glib-2.0/glib.h ] ; then
	echo "$dir"
        exit 0
    fi
done

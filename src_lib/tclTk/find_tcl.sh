#!/bin/sh 

# This script looks in a hard coded list of directories for the tcl library
# It prints the name of that root directory and exits. If it doesn't find the
# files under any of the hard coded root directories, it exits without
# printing anything.

# Exit immediately if a command exits with a non-zero status.
set -e

# Directories to look in.
dirs="/usr/local /usr $HOME $HOME/tcl $HOME/local"

for dir in $dirs; do
    if [    -r $dir/include/tcl.h \
         -a -r $dir/include/tclDecls.h \
         -a -r $dir/include/tclPlatDecls.h \
         -a -r $dir/lib/libtcl8.4.a \
	 -a -r $dir/lib/tclConfig.sh \
         -a -d $dir/lib/tcl8.4 ] ; then
	echo "$dir"
        exit 0
    fi
done

#!/bin/sh
# SARview driver script
# This program simply passes control to sarview_bin, in the
# same directory.  This script is needed because C programs'
# argv[0] contain the command-line name of the program; while
# Bourne shell programs' $0 contain the absolute path to the 
# script.  Sarview_bin needs to be called with the absolute path
# so it can find its "support" directory.
# Otherwise, this script does nothing
# Orion Sky Lawlor, 5/21/1999, ASF-STEP

echo "Running SARview..."
$0_bin $1 &

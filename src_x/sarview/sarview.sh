#!/bin/sh
# SARview driver script
# This program simply passes control to sarview_bin, in the
# same directory.  This script is needed because C programs'
# argv[0] contain the command-line name of the program; while
# Bourne shell programs' $0 contain the absolute path to the 
# script.  Sarview_bin needs to be called with the absolute path
# so it can find its "support" directory.


osType=`uname`
case "${osType}" in
	IRIX*)
		sys="irix" ;;
	SunOS)
		sys="solaris" ;;
	Linux)
		sys="linux" ;;
	CYGWIN*)
		sys="win32" ;;
	*)
		echo "Dur... SARview can't figure out where Tcl/Tk libraries are."
		echo "Exiting..."
		exit 1 ;;
esac

# Dynamically get the path to the tcl init script 
# Take the absolute path to this scipt ($0), and work backwards to the tools
# base directory, then append the /lib/system directory.
# Example
#    $0 = /usr/local/asf_tools/bin/linux/script_name
#    dirname returns /usr/local/asf_tools/bin/linux
#    dirname returns /usr/local/asf_tools/bin
#    dirname returns /usr/local/asf_tools
#    then append /java
#    and BLAMO, our classpath is /usr/local/asf_tools/java
tempDir=`dirname $0`
tempDir=`dirname $tempDir`
tempDir=`dirname $tempDir`
TCL_LIBRARY=${tempDir}/lib/${sys}/tcl8.1
export TCL_LIBRARY
echo $TCL_LIBRARY

echo "Running SARview..."
$0_bin $1 &

#!/bin/csh -f

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	PPS_startgui.csh [configfile]
# Description:	PPS GUI start-up script	
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#               @(#)PPS_startgui.csh	1.2  11/26/96
#===============================================================================

echo "Usage:"
echo "       PPS_startgui.csh   [configfile]"
echo ""

# ASF must be set first
if ($?ASF) then
	echo ASF = $ASF
else
	echo ASF not set, exiting...
	exit 1
endif

# LOCAL must be set first
if ($?LOCAL) then
	echo LOCAL = $LOCAL
else
	echo LOCAL not set, exiting...
	exit 1
endif

# PRINTER for Print Screen functions
if ($?PRINTER) then
	echo PRINTER = $PRINTER
else
	echo PRINTER not set, setting to asf1
	setenv PRINTER asf1
endif

# source the script that sets common PPS environment variables
source $ASF/bin/PPS_env.csh

# Get values of DB user, DB passwd, DB server_name and DB name
# of PPS and IMS from the config file 
#
if ($#argv > 0) then
	set configfile = "$1"
        if (! -e $configfile) then
                echo "config file '$1' doesn't exist, exiting..."
                exit
        endif
else
	set configfile = $LOCAL/pps/config/pps.config
endif

echo Starting PPS GUI, please wait...
$ASF/bin/pps_gui -c $configfile

#!/bin/csh -f

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:     pps_env.csh
# Description:  PPS common environment variables script
# Creator:      Nadia Adhami
# Notes:
#
# SCCS Info:
#               @(#)PPS_env.csh	1.1  11/21/96
#===============================================================================
 
##################################################
# environment variables needed for Sybase
##################################################

# SYBASE must be set first
if ($?SYBASE) then
	echo SYBASE = $SYBASE
else
	echo SYBASE not set, exiting...
	exit 1
endif

# append sybase library to the LD_LIBRARY_PATH
echo $LD_LIBRARY_PATH | egrep -s $SYBASE/lib
if ($status != 0) then
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:$SYBASE/lib
endif

#!/bin/csh -f

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	PPS_stopserver
# Description:	PPS server start-up script	
# Creator:	Nadia Adhami
# Notes:	
#
# SCCS Info:
#               @(#)PPS_stopserver.csh	1.1  11/21/96
#===============================================================================

#DCE entry must be defined.
if ($?RPC_DEFAULT_ENTRY) then
        echo RPC_DEFAULT_ENTRY is $RPC_DEFAULT_ENTRY
else
        echo RPC_DEFAULT_ENTRY not set, Bye.
	exit
endif

# ASF must be set first
if ($?ASF) then
	echo ASF = $ASF
else
	echo ASF not set, exiting...
	exit 1
endif

# set common PPS environment variables
source $ASF/bin/PPS_env.csh

echo "Check if PPS Server is listening..."
set num = `PPS_pingserver.csh |& grep -c 'is listening'`
if ($num == 0) then
	echo "PPS server is not runing."
	exit
endif

# Run the stop server 
$ASF/bin/pps_server_stop

#!/bin/csh -f

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	PPS_pingserver
# Description:	ping PPS server to see if it is runing
# Creator:	Nadia Adhami
# Notes:	
#
# SCCS Info:
#               @(#)PPS_pingserver.csh	1.1  11/21/96
#===============================================================================

#DCE entry must be defined.
if ($?RPC_DEFAULT_ENTRY) then
        echo RPC_DEFAULT_ENTRY is $RPC_DEFAULT_ENTRY
else
        echo RPC_DEFAULT_ENTRY not set, Bye.
	exit
endif

# check if server is already runing
echo "wait..."
set num = `cdsping $RPC_DEFAULT_ENTRY |& grep -c 'is listening'`
if ($num > 0) then
       	echo "PPS Server is listening."
else
       	echo "PPS Server is not listening."
endif

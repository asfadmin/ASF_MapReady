#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
#
# Author:	Nadia.Adhami@Jpl.Nasa.Gov
# Purpose:	Setup environment for the pps client
# Usage:
#		source pps_setup.csh
# Note:
#		In order to run as a pps client, you have to
#		1. dce_login
#		2. set the following environment variables
#                  (modify dce entry with your name)
#
#  SCCS:
#       "@(#)pps_setup.csh	1.1    11/21/96"
#===============================================================================

#! /bin/csh -f

setenv SYBASE   /db/sybase/sol	
setenv DSQUERY 	DAPPSDEV
setenv DSCOMMIT DAPPSDEV
setenv RPC_DEFAULT_ENTRY /.:/users/nadia/pps_nadia
setenv PPS_SERVER_ENTRY /.:/users/nadia/pps_nadia


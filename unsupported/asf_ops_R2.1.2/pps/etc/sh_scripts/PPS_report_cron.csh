#!/bin/csh -f

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Description:  cron job for report status to IMS
#
# SCCS Info:
#               @(#)PPS_report_cron.csh	1.2  10/31/97
#===============================================================================
 
####################################################
# run pps_report_to_ims and write output to log file 
####################################################

source ~/.cshrc
 
$ASF/bin/pps_report_to_ims >>& /LOCAL/pps/logs/PPSrun.log

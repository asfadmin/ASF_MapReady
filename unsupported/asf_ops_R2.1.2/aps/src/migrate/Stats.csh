#!/bin/csh -f
#
#   As part of installation, add stats_calls to everything before 
#   today so only activities from tomorrow forward will be reported to 
#   IMS.  Run with IMS calls disabled:  
#
aps_stats -E -l 30 -k -N IMS



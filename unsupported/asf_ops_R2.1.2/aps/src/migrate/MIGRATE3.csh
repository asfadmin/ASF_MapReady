#!/bin/csh -f
#
#   Data migration STEP 3 for operational database.  
#
#   About ?? minutes for oprapsdb..dtk on cajun.  
#
#
#   For all dtk records in the recent past, starting today, 
#   run the aps_stats program using the -N IMS feature.  
#
#   This will suppress the IMS calls but will still allow the 
#   storing of info into the stats_calls relation.  This will 
#   have the effect of only allowing future statistics, i.e.,
#   events taking place after statistics installation, to be 
#   reported to the IMS.  
#
#
#   SETUP
#
banner MIGRATE3
date
setenv APSDB larry

#
#  Program will look at yesterday's events, going back 30 days.  
#

aps_stats -E -l 30   -N IMS
date
banner DONE MIGRATE3

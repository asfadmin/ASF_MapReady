#!/bin/csh -f
#
#   Data migration STEP 2 for operational database.  
#
#   About 2 minutes for oprapsdb..dtk on cajun.  
#
#
#   For all dtk records in the future, starting today, 
#   run the aps_migrate program.  
#
#   You first use aps_time2rev to determine the correct rev numbers.  
#
#   Program started with arguments: 
#   
#   
#   usage: aps_migrate { -E | [-U Sybase_userid] -P Sybase_password }
#          -s satellite  -f first_rev -l last_rev 
#   
#       This program selects the dtk records according to the input
#       parameters.  For each record, a migration of data is then 
#       performed as follows:  
#       The strttime and stoptime values are copied to fa_strttime and 
#       fa_stoptime.   The value of fa_duration_min is computed and set.  
#       Finally, the value of asf_reduction_min is set to 0.0.  
#       This program is to be run as desired during installation at ASF.
#   
#       aps_migrate  Version compiled:  Feb 26 1998 16:59:01
#   
#
#
#   SETUP
#
banner MIGRATE2
date
setenv APSDB larry
setenv Sybase_userid larry
setenv Sybase_password iceland

set SYB = " -U $Sybase_userid -P $Sybase_password "

banner "ERS-1"
date
sleep 1
#   E1
#   1998/03/02     1998:061      rev 34660
#
#  Program runs only with brackets of 1000 revs or less, about 10 weeks.  
#
aps_migrate -E -s E1 -f 34661 -l 35660
banner "DONE ERS-1"
sleep 5

banner "-----"
banner "ERS-2"
date
sleep 1
#   E2
#   1998/03/02     1998:061      E2 rev 14971
#
#  Program runs only with brackets of 1000 revs or less, about 10 weeks.  
#
aps_migrate -E -s E2 -f 14971 -l 15970
sleep 1
aps_migrate -E -s E2 -f 15971 -l 16970
banner "----------"
banner "DONE ERS-2"
sleep 5

banner "J-ERS-1"
date
sleep 1
#   J1
#   1998/03/02     1998:061      J1 rev 33113
#
#  Program runs only with brackets of 1000 revs or less, about 10 weeks.  
#
aps_migrate -E -s J1 -f 33111 -l 34110
sleep 1
aps_migrate -E -s J1 -f 34111 -l 35110
sleep 1
aps_migrate -E -s J1 -f 35111 -l 36110
sleep 1
aps_migrate -E -s J1 -f 36111 -l 37110
banner DONE "J-ERS-1"
sleep 5

banner "RADARSAT"
date
sleep 1
#   R1
#   1998/03/02     1998:061      R1 rev 12125
#
#  Program runs only with brackets of 1000 revs or less, about 10 weeks.  
#
aps_migrate -E -s R1 -f 11921 -l 12120
aps_migrate -E -s R1 -f 12121 -l 13120
sleep 1
aps_migrate -E -s R1 -f 13121 -l 14120
sleep 1
date

banner DONE RADARSAT
date
banner DONE MIGRATE2

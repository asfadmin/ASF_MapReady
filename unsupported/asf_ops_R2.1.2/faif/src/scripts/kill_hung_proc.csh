#!/bin/csh -f 

# Periodically check on a given process id. Kill it if it
# appears to be "hung".

# Usage:
#
# source kill_hung_proc.csh pid interval num_loops
#
# pid       = process id of process to watch
# interval  = time interval (seconds) between each check
# num_checks= max trys. If process is still running after this
#             many check, kill it.

set pid        = $1
set interval   = $2
set num_checks = $3

#/usr/ucb/logger -p local6.debug -t "kill_hung_proc.csh" -i "$pid $interval $num_checks"

@ loop_cnt = 0
set tmp = " "

# Loop until max iterations is reached, or until ftp finishes by itself.
# At present, the script checks every minute, up to 45 minutes max.

while ($loop_cnt < $num_checks && $#tmp > 0)

   @ loop_cnt = $loop_cnt + 1
   sleep $interval

   set tmp = `/bin/ps -ef | grep -v grep | grep " $pid " | grep -v $0 | awk '{printf("%s"), $2}'`

#/usr/ucb/logger -p local6.debug -t "kill_hung_proc.csh" -i "$loop_cnt"

end

# Kill the ftp process, if it is still running.

set tmp = `/bin/ps -ef | grep -v grep | grep " $pid " | grep -v "$0" | awk '{printf("%s"), $2}'`

@ kill_status = 0

if ($#tmp > 0) then

   kill -9 $pid

   @ kill_status = 5
   
endif

exit $kill_status

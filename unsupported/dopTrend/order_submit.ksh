#!/usr/bin/ksh
#
# this will submit the ODLs to the vZero Server
# the ODLs and the script 2.ksh must be in the
# current directory

history_log="old/orders.log"
order_script="./order_2.ksh"
order_log="order_2.log"


echo "" >> $history_log
echo "**start**" >> $history_log
date >> $history_log

$order_script 2>&1 | tee -a $history_log | tee $order_log

printf "***start errors***\n"
grep -i error $order_log
printf "***end  errors***\n"

printf "\nstart order_id:\t" | tee -a $history_log 
head -5 $order_log | tail -1 | cut -d ' ' -f 2 | tee -a $history_log 

printf "end   order_id:\t" | tee -a $history_log 
tail -4 $order_log | head -1 | cut -d ' ' -f 2 | tee -a $history_log 

printf "total orders:" | tee -a $history_log 
ls -1 *.odl | wc -l | tee -a $history_log 

printf "total frames:" | tee -a $history_log 
grep -i package *.odl | wc -l | tee -a $history_log 

printf "\n\n" >> $history_log
date >> $history_log
echo "**end**" >> $history_log
echo "" >> $history_log

/bin/rm order*
/bin/rm *.odl

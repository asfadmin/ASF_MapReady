#! /usr/bin/ksh
# Script to run sample progrmas
# 
# SCCS_Id : @(#)run.transfer.S	2.41 3/24/98
# (C) COPYRIGHT IBM CORP 1994
# All Rights Reserved
#  
#
echo "Copyright (C) 1996, California Institute of Technology. U.S. Government"
echo "Sponsorship under NASA Contract NAS7-1260 is acknowledged."


if test ! -r $1
then
   echo  "can't open $1"
   exit 2
fi
cnt=1
for i in `more $1`
 do
   if [ $cnt = '6' ] ; then
      location="$i"
   fi
   if [ $cnt = '2' ] ; then
      num_of_nodes="$i"
   fi
   if [ $cnt = '3' ] ; then
      shared_mode="$i"
   fi
   if [ $cnt = '19' ] ; then
      dump_file="$i"
   fi
   cnt=`expr $cnt + 1`
 done

echo $location


PREFIX="/usr/bin"
DATA_PATH="$location"
echo "RUNNING TRANSFER"
echo "Setting the required environment variables"
export MP_EUILIB="$shared_mode"
export MP_EUIDEVICE=css0
export MP_SAVEHOSTFILE=$DATA_PATH/run.save
export MP_HOSTFILE=$DATA_PATH/run.host
export MP_RESD=yes
export MP_PROCS="$num_of_nodes"
export MP_RETRY=5
export MP_RETRYCOUNT=2
export MP_INFOLEVEL=0
export MP_RMPOOL=0
export MP_LABELIO=yes
echo " "
echo "Executing the parallel program with $MP_PROCS tasks"
#
  for i_file_type in 1 2 3 4 5 6 7
  do
    $PREFIX/poe $DATA_PATH/input_prep $1 $i_file_type $2
    rc=$?
    if [ $rc -ne 0 ] ;
     then
      if [ $rc -ge 127 ] ;
        then
          echo 'INTERRUPTED: EXITING'
          break 1
      fi
      echo 'NOT WORK: RETRYING...'
      itry_done=0
      i_try=1
      while [ $itry_done = '0' ] ; do
       sleep 1
       $PREFIX/poe $DATA_PATH/input_prep $1 $i_file_type $2
       rc=$?
       if [ $rc -ge 127 ] ;
        then
          echo 'INTERRUPTED: EXITING'
          break 2
       fi
       if [ $rc -eq 0 ] ;
        then
         echo 'RETRYING : successful at try #'  $i_try
         itry_done=1
       fi
       if [ $i_try -eq '4' ] ;
        then
         itry_done=1
       fi
       i_try=`expr $i_try + 1`
      done
      if [ $rc -ne 0 ] ;
       then
       echo 'RETRYING : 4 times FAILED ; EXITING'
       break 1
      fi
    fi
  done
#
#
#
echo " "
echo "Parallel program return code was" $rc
if [ $rc -eq 255 ]
then
  rc=1
elif [ $rc -gt 127 ]
then
  rc=24
fi
echo "Parallel program exit code was" $rc
exit $rc

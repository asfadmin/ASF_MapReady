#!/bin/csh -f

source $HOME/set_env.csh


#------------------------------------------------------------------------------


# Check that no other J1_ftpget_all is in progress.

if (-e $FAIF_ROOTPATH/FTP/gtmp/J1_ftpget_all.lock) then
   /usr/ucb/logger -p local6.debug -t "J1_ftpget_all.csh" -i "Error... there is another run active. Exiting."

   /bin/mailx -s "Error during J1_all retrieval" $mail_user << EndMailx
The J1_all retrieval process was launched, only to find a lockfile in place.
This probably means that the previous run bombed, and did not remove its
lockfile. Or, it may mean that ftp is hung, and the previous run is still
running. In either case, the present run is terminated without doing anything.
Future runs may not proceed until the lockfile named 
$FAIF_ROOTPATH/FTP/gtmp/J1_ftpget_all.lock is removed.
In most cases, it will probably be sufficient to perform the following;
 1-Use the ps command to find the process id for the hung ftp process
 2-Use kill -9 to terminate that ftp process
The J1_ftpget script should then go ahead and finish, and should automatically
delete the lockfile so that future runs may proceed.
EndMailx

exit 0

endif

/bin/touch $FAIF_ROOTPATH/FTP/gtmp/J1_ftpget_all.lock


#------------------------------------------------------------------------------


$FAIF_BINPATH/J1_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/NASDAget.upw \
                             sen $FAIF_ROOTPATH/NASDA REQA binary

$FAIF_BINPATH/J1_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/NASDAget.upw \
                             sen $FAIF_ROOTPATH/NASDA  REQM binary

$FAIF_BINPATH/J1_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/NASDAget.upw \
                             sen $FAIF_ROOTPATH/NASDA ELMF binary

$FAIF_BINPATH/J1_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/NASDAget.upw \
                             sen $FAIF_ROOTPATH/NASDA OPLN binary

$FAIF_BINPATH/J1_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/NASDAget.upw \
                             sen $FAIF_ROOTPATH/NASDA MSGC binary

$FAIF_BINPATH/J1_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/NASDAget.upw \
                             sen $FAIF_ROOTPATH/NASDA MSGN binary

dirmon:

$FAIF_BINPATH/NASDAdirmon.csh

done:

/bin/rm $FAIF_ROOTPATH/FTP/gtmp/J1_ftpget_all.lock

exit 0

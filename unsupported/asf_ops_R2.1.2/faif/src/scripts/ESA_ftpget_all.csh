#!/bin/csh -f

source $HOME/set_env.csh


#------------------------------------------------------------------------------


# Check that no other ESA_ftpget_all is in progress.

if (-e $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget_all.lock) then
   /usr/ucb/logger -p local6.debug -t "ESA_ftpget_all.csh" -i "Error... there is another run active. Exiting."

   /bin/mailx -s "Error during ESA_all retrieval" $mail_user << EndMailx
The ESA_all retrieval process was launched, only to find a lockfile in place.
This probably means that the previous run bombed, and did not remove its
lockfile. Or, it may mean that ftp is hung, and the previous run is still
running. In either case, the present run is terminated without doing anything.
Future runs may not proceed until the lockfile named 
$FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget_all.lock is removed.
In most cases, it will probably be sufficient to perform the following;
 1-Use the ps command to find the process id for the hung ftp process
 2-Use kill -9 to terminate that ftp process
The ESA_ftpget script should then go ahead and finish, and should automatically
delete the lockfile so that future runs may proceed.
EndMailx

exit 0

endif

/bin/touch $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget_all.lock


#------------------------------------------------------------------------------

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA shaq  ascii >&! /dev/null

set hour = `date +"%H"`

if ($hour < "13" || $hour > "17") then

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA odmc_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA odmr_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA orpd_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA orre_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA patc_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA rqst_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA rqvr_ binary >&! /dev/null

$FAIF_BINPATH/ESA_ftpget.csh  $FAIF_ROOTPATH/FTP/upw/ESAget.upw from_eecf \
                              $FAIF_ROOTPATH/ESA mpsg_ ascii >&! /dev/null

endif

dirmon:

/bin/rm $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget_all.lock

$FAIF_BINPATH/ESAdirmon.csh

done:

exit 0

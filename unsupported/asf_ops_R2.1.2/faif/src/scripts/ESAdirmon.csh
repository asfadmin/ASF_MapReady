#!/bin/csh -f

source $HOME/set_env.csh


#------------------------------------------------------------------------------


# Check that ESA_ftpget_all is not in progress.

if (-e $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget_all.lock) then
   /usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/ESAdirmon.csh" -i "Error... there is an ESA_ftpget_all run active. Exiting."

   /bin/mailx -s "Error during ESAdirmon.csh" $mail_user << EndMailx
The ESAdirmon script was launched, only to find an ESA_ftpget_all
lockfile. The presence of the lockfile means that the retrieval process
is still running. It is not a good idea to run ESAdirmon until the
ESA_ftpget_all process is completed, as part of the ftpget_all includes
renaming the files so that they are recognized by the dirmon.
EndMailx

exit 0

endif

$FAIF_BINPATH/ESAdirmon \
        -c $FAIF_ROOTPATH/ESA/config/ESAdirmon.config

exit 0

#!/bin/csh -f

# Delete all .orb files in the CSA ufdrf directory. We only need one
# copy of each .orb file, and we already obtain them elsewhere.

#------------------------------------------------------------------------------


# If "help" is requested, or if there are too few arguments provided, print 
# usage banner.

if ($1 == "-h" || $#argv < 3) then
   echo Usage:mdel_ufdrf_orb.csh host upw_file src_dir
   exit 0
endif


#------------------------------------------------------------------------------


# Start logging.

/usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Starting mdel_ufdrf_orb.csh."


#------------------------------------------------------------------------------


# Pick up environment definitions.

source ~/set_env.csh

# Store all arguments into internal variables.

set host      = $1
set upw_file  = $2
set src_dir   = $3


#------------------------------------------------------------------------------


# Check that no other mdel_ufdrf_orb is in progress.

if (-e $FAIF_ROOTPATH/FTP/gtmp/mdel_ufdrf_orb.lock) then
   /usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Error... there is another mdel_ufdrf_orb run active. Exiting."

   /bin/mailx -s "Error during CSA orb mdel" $mail_user << EndMailx
The CSA orb mdel process was launched, only to find a lockfile in place.
This probably means that the previous run bombed, and did not remove its
lockfile. Or, it may mean that ftp is hung, and the previous run is still
running. In either case, the present run is terminated without doing anything.
Future runs may not proceed until the lockfile named 
$FAIF_ROOTPATH/FTP/gtmp/mdel_ufdrf_orb.lock is removed.
EndMailx

exit 0

endif

/bin/touch $FAIF_ROOTPATH/FTP/gtmp/mdel_ufdrf_orb.lock


#------------------------------------------------------------------------------


# Put out a log message showing the src_dir, so if something goes wrong
# we'll know what it was trying to do.

/usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Directory = $src_dir."


#------------------------------------------------------------------------------


# Get remote username and password, for login to remote site.

set upw_line = `$FAIF_BINPATH/par qwerty $upw_file`
set user     = $upw_line[1]
set password = $upw_line[2]

set ftp_output = $FAIF_ROOTPATH"/FTP/gtmp""/$src_dir""_mdel_orb.log"


#------------------------------------------------------------------------------


# Go get'em!

/bin/ftp -i -n -v $host << EndFTPinput >&! $ftp_output
user $user $password
debug
cd $src_dir
mdel "m*.orb"
quit
EndFTPinput


#------------------------------------------------------------------------------


# Check the ftp output to see if login was successful.

set logged_in = `grep " logged in." $ftp_output`

if ($status != 0) then

   /usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Login failure during mdel_ufdrf_orb."

   /bin/mailx -s "ftp error during orb mdel" $mail_user << EndMailx
There was a login failure while trying to mdel $src_dir orb files.

The ftp output is in $ftp_output.

The following line shows host upw_file src_dir.
$host $upw_file $src_dir
EndMailx
   goto done

endif


#------------------------------------------------------------------------------


# Check the ftp output to see if cd command was successful.

egrep " CWD command successful." $ftp_output >&! /dev/null

if ($status != 0) then

   /usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "cd to $src_dir failure in mdel_ufdrf_orb."

   /bin/mailx -s "ftp error during mdel_ufdef_orb" $mail_user << EndMailx
Could not cd to $src_dir, trying to mdel orb files.
The ftp output is in $ftp_output.

The following line shows host upw_file src_dir
$host $upw_file $src_dir
EndMailx

endif


#------------------------------------------------------------------------------


# Check the ftp output to see if files were present.

egrep " DELE " $ftp_output | egrep "No such file or directory" >&! /dev/null

if ($status == 0) then

   /usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "No files present at remote site."

   goto done

endif


#------------------------------------------------------------------------------


# Check status from delete attempt.

   egrep " DELE command successful." $ftp_output >&! /dev/null


   if ($status != 0) then
      /usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Error deleting ufdrf_orb file on remote host."
      
   /bin/mailx -s "Error during mdel_ufdrf_orb" $mail_user << EndMailx
An ftp delete for files in $src_dir could not be performed.

The attempt to delete the file was logged to $ftp_output.

The following line shows host upw_file src_dir.
$host $upw_file $src_dir
EndMailx

   goto done

endif


#------------------------------------------------------------------------------


# All error tests passed, so if we get here it means we have a winner!

/usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Success"

done:

/bin/rm $FAIF_ROOTPATH/FTP/gtmp/mdel_ufdrf_orb.lock

/usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i "Done."

exit 0

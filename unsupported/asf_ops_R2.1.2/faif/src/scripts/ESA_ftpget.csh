#!/bin/csh -f

#
# This script is used to obtain files from ESA. The script ftp-s two copies
# of files matching the file_name spec. It then compares the two local copies.
# If they are the same the script then deletes the original file on the ESA
# host, and also deletes one of the local copies. The other local copy is
# left for the dirmon to process.
#

# Errors during the ftp phase will trigger mail to the user. At present
# the only error tested for is:
#    1-Copy the file over twice, and compare. They better "zero diff"!


#------------------------------------------------------------------------------


# If "help" is requested, or if there are too few arguments provided, print 
# usage banner.

if ($1 == "-h" || $#argv < 5) then
   echo Usage:ESA_ftpget.csh upw_file src_dir dest_dir file_name mode
   exit 0
endif


#------------------------------------------------------------------------------


# Start logging.

/usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Starting."


#------------------------------------------------------------------------------


# Pick up environment definitions.

source ~/set_env.csh

# Store all arguments into internal variables.

set host_tmp = `grep ESA_SRCHOST $FAIF_ROOTPATH/ESA/config/ESAdirmon.config`
set host = $host_tmp[2]
set upw_file  = $1
set src_dir   = $2
set dest_dir  = $3
set file_name = $4
set mode      = $5


#------------------------------------------------------------------------------


# Check that no other ESA_ftpget is in progress.

if (-e $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget.lock) then
   /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Error... there is another run active. Exiting."

   /bin/mailx -s "Error during ESA file retrieval" $mail_user << EndMailx
The ESA file retrieval process was launched, only to find a lockfile in place.
This probably means that the previous run bombed, and did not remove its
lockfile. Or, it may mean that ftp is hung, and the previous run is still
running. In either case, the present run is terminated without doing anything.
Future runs may not proceed until the lockfile named 
$FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget.lock is removed.
In most cases, it will probably be sufficient to perform the following;
 1-Use the ps command to find the process id for the hung ftp process
 2-Use kill -9 to terminate that ftp process
The ESA_ftpget script should then go ahead and finish, and should automatically
delete the lockfile so that future runs may proceed.
EndMailx

exit 0

endif

/bin/touch $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget.lock

#------------------------------------------------------------------------------


# Put out a log message showing the file type, so if something goes wrong
# we'll know what it was trying to do.

/usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "File name = $file_name."


#------------------------------------------------------------------------------


# Get remote username and password, for login to remote site.

set upw_line = `$FAIF_BINPATH/par qwerty $upw_file`
set user     = $upw_line[1]
set password = $upw_line[2]

set ftp_output = $FAIF_ROOTPATH"/FTP/gtmp""/$file_name""_ftpget.log"


#------------------------------------------------------------------------------


# Go get'em! (First to a temp dir below dest_dir, then to dest_dir itself.)

# Make sure the temp dir below dest_dir is empty.

/bin/rm $dest_dir/ftp_the_2nd/* >&! /dev/null

/bin/ftp -i -n -v $host << EndFTPinput >&! $ftp_output &
user $user $password
debug
$mode
cd $src_dir
dir $file_name"*"
lcd $dest_dir/ftp_the_2nd
mget $file_name"*"
lcd $dest_dir
mget $file_name"*"
quit
EndFTPinput

# Check for (and kill if necessary) hung ftp.

set ftp_pid = `/bin/ps -ef | grep -v grep | grep "ftp -i -n -v $host"  | grep $$ | awk '{printf("%s"), $2}'`

$FAIF_BINPATH/kill_hung_proc.csh $ftp_pid 60 45

@ kill_stat = $status

if ($kill_stat != 0) then

   /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Ftp process was hung... killed it."

   /bin/mailx -s "ftp hung during ESA file retrieval" $mail_user << EndMailx
There was a hung ftp while trying to obtain ESA $file_name files.

The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

endif


#------------------------------------------------------------------------------


# Check the ftp output to see if login was successful.

set logged_in = `grep " logged in." $ftp_output`

if ($status != 0) then

   /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Login failure."

   /bin/mailx -s "ftp error during ESA file retrieval" $mail_user << EndMailx
There was a login failure while trying to obtain ESA $file_name files.

The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx
   goto done

endif


#------------------------------------------------------------------------------


# Check the ftp output to see if file transfer was successful.

egrep "Permission denied" $ftp_output >&! /dev/null

if ($status == 0) then

   /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "File transfer failure."

   /bin/mailx -s "ftp error during ESA file retrieval" $mail_user << EndMailx
There was a file transfer problem of some sort while trying to obtain new ESA
$file_name files. The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

   goto error_cleanup

endif


#------------------------------------------------------------------------------


# Check the ftp output to see if files were present.

egrep " file not found." $ftp_output >&! /dev/null

if ($status == 0) then

   /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "No files present at remote site."

   goto done

endif


#------------------------------------------------------------------------------


# Compare the files in dest_dir and ftp_the_2nd. If any file does not "zero
# diff" then abort the entire procedure and send mail to operator.

foreach fn (`/bin/ls $dest_dir/ftp_the_2nd`)

   diff $dest_dir/ftp_the_2nd/$fn $dest_dir/$fn >&! $dest_dir/ftp_the_2nd/diff.tmp

   if (! -z $dest_dir/ftp_the_2nd/diff.tmp) then

      /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "File transfer failure."

      /bin/mailx -s "ftp error during ESA file retrieval" $mail_user << EndMailx
Two copies of $fn were ftp-ed from the remote site.
They should be identical, but are not. All files which were brought over
have been deleted, so it is as if this particular run never happened. It should
be re-run after determining what went wrong the first time.
The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

      /bin/rm $dest_dir/ftp_the_2nd/diff.tmp

      goto error_cleanup

   endif

end

/bin/rm $dest_dir/ftp_the_2nd/diff.tmp >&! /dev/null


#------------------------------------------------------------------------------

# Delete the files on the remote host. Since the ftp connection is
# rather intermittent and unreliable, the delete will be attempted
# five times before throwing in the towel.

foreach fn (`/bin/ls $dest_dir/ftp_the_2nd`)

   /bin/rm $dest_dir/ftp_the_2nd/$fn

   foreach del_try (1 2 3 4 5)

/bin/ftp -i -n -v $host << EndFTPdelete >>&! $ftp_output &
user $user $password
debug
$mode
cd $src_dir
delete $fn
quit
EndFTPdelete

# Check for (and kill if necessary) hung ftp.

set ftp_pid = `/bin/ps -ef | grep -v grep | grep "ftp -i -n -v $host"  | grep $$ | awk '{printf("%s"), $2}'`

$FAIF_BINPATH/kill_hung_proc.csh $ftp_pid 10 30

@ kill_stat = $status

if ($kill_stat != 0) then

   /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Ftp process was hung... killed it."

   /bin/mailx -s "ftp hung during ESA file delete" $mail_user << EndMailx
There was a hung ftp while trying to delete $fn.

The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

endif

# Check status from delete attempt.

      grep " DELE of $fn successful." $ftp_output >&! /dev/null

      @ del_status_1 = $status

      grep "$fn deleted." $ftp_output >&! /dev/null

      @ del_status = $status

      if ($del_status_1 == 0 || $del_status == 0) goto del_try_finished

   end

del_try_finished:

echo $del_try $del_status >>&! $ftp_output

   if ($del_status_1 != 0 && $del_status != 0) then
      /usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Error deleting file on remote host."
      
   /bin/mailx -s "Error during ESA file delete" $mail_user << EndMailx
An ftp delete for file = $fn could not be performed. The file had been
succesfully brought over to ASF, but could not be deleted at the remote
host. In order to avoid repeatedly processing this file it will be removed
locally without being run through the dirmon.

The attempt to delete the file was logged to $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

      /bin/cp $dest_dir/$fn ~/developers_stash/ESA_ARCH/
      /bin/rm $dest_dir/$fn

   endif

end


#------------------------------------------------------------------------------

# All error tests passed, so if we get here it means we have a winner!

/usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Success"

# Remove the Vax version number from the end of the file names.

if ($file_name != "shaq") goto rename_part_2

foreach fn (`/bin/ls $dest_dir/ | grep -i shaqp`)

   if (! -d $dest_dir/$fn) then

      set fn1 = `basename $fn '.*\;\(.*\)'`
      set fn2 = `echo $fn1 | tr "[:lower:]" "[:upper:]"`
                      set fn3 = `echo $fn2 | nawk '{if (sub("shaqp", "SHQP_")) print $0}'`
      if ($fn3 == "") set fn3 = `echo $fn2 | nawk '{if (sub("SHAQP", "SHQP_")) print $0}'`
      if ($fn3 != $fn) mv $dest_dir/$fn $dest_dir/$fn3

      if ($status != 0) echo "Could not rename $dest_dir/$fn" >>&! $ftp_output

   endif

end

rename_part_2:

foreach fn (`/bin/ls $dest_dir/ | grep -i $file_name`)

   if (! -d $dest_dir/$fn) then

      set fn1 = `basename $fn '.*\;\(.*\)'`
      set fn2 = `echo $fn1 | tr "[:lower:]" "[:upper:]"`
      if ($fn2 != $fn) mv $dest_dir/$fn $dest_dir/$fn2

      if ($status != 0) echo "Could not rename $dest_dir/$fn" >>&! $ftp_output

   endif

end

goto done


#------------------------------------------------------------------------------


error_cleanup:

# In case an error was detected, get rid of "everything"... that is, make
# it look as if this run never even happened.

/bin/rm $dest_dir/ftp_the_2nd/diff.tmp >&! /dev/null


#------------------------------------------------------------------------------


done:

# Also, remove any leftover files from the "diff" portion of the transfer.

foreach fn (`/bin/ls $dest_dir/ftp_the_2nd`)

 /bin/rm $dest_dir/ftp_the_2nd/$fn $dest_dir/$fn >&! /dev/null

end

/bin/rm $FAIF_ROOTPATH/FTP/gtmp/ESA_ftpget.lock

/usr/ucb/logger -p local6.debug -t "ESA_ftpget.csh" -i "Done."

exit 0

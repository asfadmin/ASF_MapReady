#!/bin/csh -f

#
# This script is used to obtain J1 files from NASDA. For ease of discussion
# the comments will only address OPLN files. All other files are handled in
# the same fashion as OPLN files.

# At any given time, there will be three days worth of files in the NASDA J1
# directory. They will be, for example,
#    OPLN (most recent)
#    OPLN000006 (file for previous day)
#    OPLN000005 (file for day before yesterday)
# When a new file becomes available, the directory will be updated to look
# as follows;
#    OPLN (newly created most recent file)
#    OPLN000007 (yesterdays OPLN file is renamed)
#    OPLN000006 
# Note that the OPLN000005 file has fallen from the bottom of the list.

# There are six cases the script must handle. (The comments below are not
# in the same order as the various cases are actually handled in the code.)
#  1-Nominal case; When the script looks to the J1 directory it finds
#    a new file, and that new file is the next one in sequence after the
#    file retrieved last time it ran. Action taken will be to get the new 
#    file and update a log file to indicate what file was just retrieved.
#  2-Almost nominal; The script finds the same file as the last time it ran.
#    Action taken will be to go back to sleep. No files are retrieved and the
#    log file is not updated.
#  3-Error; the file in question already resids in the destination
#    directory. Send a mail message to the operator. Do not transfer
#    any files, and do not update the log.
#  4-Error; The newly found OPLN file is NOT next in sequence after the previous
#    file. Action taken is to send a mail message to the operator, describing
#    how to recover and process the files which were skipped in the input
#    sequence. No files are retrieved and the log file is not updated.
#  5-Error; No backup files are present. We cannot determine if there is
#    a backup, or regression, in the sequence. Action: delete the new file
#    and notify operator.
#  6-There is no file of the given name. Action; delete any backup files
#    which were brought over, and send mail.

# Errors during the ftp phase will trigger mail to the user. Some of the
# errors tested for include:
#    1-Copy the file over twice, and compare. They better "zero diff"!
#    2-Compare the size of the file on the remote host to the size locally.

# It is necessary to initialize the log file for each file type. Before
# running for the first time, check the files on the NASDA host. If the
# highest numbered "backup" file was presently (for example) OPLN000005,
# then the log file should contain a single line with the string OPLN000004
# on it. The log file must be $FAIF_ROOTPATH/NASDA/log/OPLN_previous.log.


#------------------------------------------------------------------------------


# If "help" is requested, or if there are too few arguments provided, print 
# usage banner.

if ($1 == "-h" || $#argv < 5) then
   echo Usage:J1_ftpget.csh upw_file src_dir dest_dir file_name mode
   exit 0
endif


#------------------------------------------------------------------------------


# Start logging.

/usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Starting."


#------------------------------------------------------------------------------


# Pick up environment definitions.

source ~/set_env.csh

# Store all arguments into internal variables.

set host_tmp = `grep NASDA_SRCHOST $FAIF_ROOTPATH/NASDA/config/NASDAdirmon.config`
set host = $host_tmp[2]
set upw_file  = $1
set src_dir   = $2
set dest_dir  = $3
set file_name = $4
set mode      = $5


#------------------------------------------------------------------------------

# Check that no other J1_ftpget is in progress.

if (-e $FAIF_ROOTPATH/FTP/gtmp/J1_ftpget.lock) then
   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Error... there is another run active. Exiting."

   /bin/mailx -s "Error during J1 file retrieval" $mail_user << EndMailx
The J1 file retrieval process was launched, only to find a lockfile in place.
This probably means that the previous run bombed, and did not remove its
lockfile. Or, it may mean that ftp is hung, and the previous run is still
running. In either case, the present run is terminated without doing anything.
Future runs may not proceed until the lockfile named 
$FAIF_ROOTPATH/FTP/gtmp/J1_ftpget.lock is removed.
In most cases, it will probably be sufficient to perform the following;
 1-Use the ps command to find the process id for the hung ftp process
 2-Use kill -9 to terminate that ftp process
The J1_ftpget script should then go ahead and finish, and should automatically
delete the lockfile so that future runs may proceed.
EndMailx

exit 0

endif

/bin/touch $FAIF_ROOTPATH/FTP/gtmp/J1_ftpget.lock

#------------------------------------------------------------------------------


# Put out a log message showing the file type, so if something goes wrong
# we'll know what it was trying to do.

/usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "File name = $file_name."


#------------------------------------------------------------------------------


# First, check to see if the file is already here. (It should not be.)

if (-e $dest_dir/$file_name) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Error... file is already here."

   /bin/mailx -s "J1 file retrieval" $mail_user << EndMailx
The J1 file retrieval process was launched for file = $file_name. This
file is already present on the ASF end. It should not be here. The
NASDAdirmon program probably failed to process the file which is already
here. The J1 file retrieval process will not bring the new file
over until the present one has been completely processed.
EndMailx

   goto done

endif


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

/bin/ftp -i -n -v $host << EndFTPinput >&! $ftp_output
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


#------------------------------------------------------------------------------


# Check the ftp output to see if login was successful.

set logged_in = `grep " logged in." $ftp_output`

if ($status != 0) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Login failure."

   /bin/mailx -s "ftp error during J1 file retrieval" $mail_user << EndMailx
There was a login failure while trying to obtain J1 $file_name files from NASDA.

The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx
   goto done

endif


#------------------------------------------------------------------------------


# Check the ftp output to see if file transfer was successful.

egrep "Permission denied|No such file or directory" $ftp_output >&! /dev/null

if ($status == 0) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "File transfer failure."

   /bin/mailx -s "ftp error during J1 file retrieval" $mail_user << EndMailx
There was a file transfer problem of some sort while trying to obtain new J1
$file_name files from NASDA. The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

   goto error_cleanup

endif


#------------------------------------------------------------------------------


# Compare the files in dest_dir and ftp_the_2nd. If any file does not "zero
# diff" then abort the entire procedure and send mail to operator.

foreach fn (`/bin/ls $dest_dir/ftp_the_2nd`)

   diff $dest_dir/ftp_the_2nd/$fn $dest_dir/$fn >&! $dest_dir/ftp_the_2nd/diff.tmp

   if (! -z $dest_dir/ftp_the_2nd/diff.tmp) then

      /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "File transfer failure."

      /bin/mailx -s "ftp error during J1 file retrieval" $mail_user << EndMailx
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


# Check that the files in dest_dir are the same size as on the remote host.

foreach fn (`/bin/ls $dest_dir/ftp_the_2nd`)

   set size_str = `egrep -e " "$fn\$ $ftp_output | grep -v "local:"`
   set remote_size = $size_str[5]

   set size_str = `wc $dest_dir/$fn`
   set local_size = $size_str[3]
  
   if ($local_size != $remote_size) then

      /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "File transfer failure."

      /bin/mailx -s "ftp error during J1 file retrieval" $mail_user << EndMailx
The file named $fn has a different size locally, after ftp, than
it did on the remote host. All files which were brought over
have been deleted, so it is as if this particular run never happened. 
It should be re-run after determining what went wrong the first time.
The ftp output is in $ftp_output.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

      goto error_cleanup

   endif

# If the file has the correct size, go ahead and delete the 2nd (tmp) version.
# Leave the version in dest_dir alone... it will be dealt with later.

   /bin/rm $dest_dir/ftp_the_2nd/$fn

end


#------------------------------------------------------------------------------


# Check to see if "file_name" was brought over.

if (! -e $dest_dir/$file_name) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "No such file found at NASDA."


   /bin/mailx -s "Error during J1 file get... missing file" $mail_user << EndMailx
An ftp get for file_type = $file_name could not find that file.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

   goto error_cleanup

endif


#------------------------------------------------------------------------------


# Compare the previous "first backup" file to the present "first backup" file.

set previous_file_log = $dest_dir"/log/""$file_name""_previous.log"

set previous_first_old = `cat $previous_file_log`
set tmp = `/bin/ls $dest_dir | egrep -e "$file_name""[0-9][0-9][0-9][0-9][0-9][0-9]"$`

# Check for "no backups present" case.

if ($#tmp == 0) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "No backup file present."
   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Cannot use file since its place in the sequence cannot be determined."

   /bin/mailx -s "Sequence problem with J1 file retrieval" $mail_user << EndMailx
There were no backup files present at NASDA for file type = $file_name.
This is an error, since it cannot be determined where the new file belongs in
its sequence. No file will be transfered to ASF without the correct backups.

The following line shows host upw_file src_dir dest_dir file_name mode.
$host $upw_file $src_dir $dest_dir $file_name $mode
EndMailx

   goto error_cleanup

endif

#------------------------------------------------------------------------

# Check to see if there are backup files "piling up" at NASDA. There should 
# normally only be two backup files at any time. 

if ($#tmp >= 7) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "WARNING: Too many backup files at remote site."

   /bin/mailx -s "Proliferation of backup files at NASDA" $mail_user << EndMailx
Dear Faif Operator,
     How are you today? I am the shell script that gets files from NASDA. I am
writing this letter to to warn you and to also ask you to do something for me.
What I am asking you to do is to inform NASDA that there are too many backup
$file_name files at $host, in the $src_dir directory. There should
be only two backup files, and at this time there are $#tmp files.  I would
appreciate it if you could ask them to clean it up. This needs to be done as
soon as possible, otherwise you will (unfortunately) receive this message from
me every 6 hours (that is because I am automated). Thank you very much and
sorry to bother you.
                                                Sincerely,
                                                J1_ftpget script
EndMailx

   goto error_cleanup

endif

set present_first_old = $tmp[$#tmp]
set file_name_length = `echo $file_name | wc`
@ file_name_len = $file_name_length[3]
@ prev_ver = `echo $previous_first_old | cut -c $file_name_len-255`
@ pres_ver = `echo $present_first_old | cut -c $file_name_len-255`


#------------------------------------------------------------------------------


# Present "first backup" is the same as the one the previous run obtained.
# Go back to sleep.

if ($pres_ver == $prev_ver) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "No new files."

   /bin/rm $dest_dir/$file_name

   goto done

endif


#------------------------------------------------------------------------------


# Present "first backup" is lower than the one the previous run obtained.
# Notify operator that NASDA has goofed up.

if ($pres_ver < $prev_ver) then

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "File out of sequence."
   /bin/mailx -s "Sequence problem with J1 file retrieval" $mail_user << EndMailx
The previous highest backup for filetype = $file_name was $previous_first_old.
The current run finds that the highest backup is now $present_first_old.
It appears that NASDA has reset their file counter. It will probably be
neccessary to edit $FAIF_ROOTPATH/NASDA/log/$file_name"_previous.log" and
replace the value $prev_ver with $pres_ver minus 1.

EndMailx

   /bin/rm $dest_dir/$file_name

   goto done

endif


#------------------------------------------------------------------------------


# The present "first backup" is not next in sequence. Files have been skipped!
# Send mail message to operator.

if ($pres_ver - $prev_ver > 1) then

   @ rget_first = $prev_ver + 2
   @ rget_last  = $pres_ver
   set reget_first = `echo $rget_first | awk '{printf("%6.6d"), $1}'`
   set reget_last  = `echo $rget_last  | awk '{printf("%6.6d"), $1}'`

   /usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Discontinuity in file sequence."

   /bin/mailx -s "Discontinuity in J1 file retrieval" $mail_user << EndMailx
There is a gap in the $file_name sequence of files obtained from NASDA. 

Each file starting from $file_name$reget_first
through (including)     $file_name$reget_last
and also the file named $file_name
must be processed as follows;
 1-get the file via ftp
 2-rename it $file_name
 3-run through NASDAdirmon.

After all files have been reprocessed edit
$previous_file_log and replace the value
     $previous_first_old
with $present_first_old
EndMailx

   goto error_cleanup

endif


#------------------------------------------------------------------------------


# All error tests passed, so if we get here it means we have a winner!

/usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Success"

# Update the log file by placing the name of the present "first backup" file
# in it.

echo $present_first_old >! $previous_file_log

goto done


#------------------------------------------------------------------------------


error_cleanup:

# In case an error was detected, get rid of "everything"... that is, make
# it look as if this run never even happened.

/bin/rm $dest_dir/ftp_the_2nd/diff.tmp >&! /dev/null
/bin/rm $dest_dir/$file_name           >&! /dev/null


#------------------------------------------------------------------------------


done:

# Get rid of the "backup" files which were brought over with the new file.
# The use of egrep ensures that only the "backup" files are deleted. The
# new file is untouched.

foreach fn (`/bin/ls $dest_dir | egrep -e "$file_name""[0-9][0-9][0-9][0-9][0-9][0-9]"$`)

   /bin/rm $dest_dir/$fn

end

# Also, remove any leftover files from the "diff" portion of the transfer.

foreach fn (`/bin/ls $dest_dir/ftp_the_2nd`)

 /bin/rm $dest_dir/ftp_the_2nd/$fn $dest_dir/$fn >&! /dev/null

end

#/bin/rm $dest_dir/$file_name"_"* >&! /dev/null

/bin/rm $FAIF_ROOTPATH/FTP/gtmp/J1_ftpget.lock

/usr/ucb/logger -p local6.debug -t "J1_ftpget.csh" -i "Done."

exit 0

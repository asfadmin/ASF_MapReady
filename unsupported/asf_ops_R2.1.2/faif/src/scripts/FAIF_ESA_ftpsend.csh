#!/bin/csh -f
#===============================================================================
# Filename:	FAIF_ESA_ftpsend.csh
# Description:
#	This is the FTP script that is used to transfer files from FAIF
# to a remote system.
#
#       Note: This script is a special case, to handle transfers
#             to ESA. They require that the file be renamed from
#             one directory to another, after the file has been
#             transfered to them.
# 
# Creator:	Norbert Piega
# Notes:	
# 1.  Commented out first ftp - RHoffman - Nov. '97
# 2.  Retry under more circumstances, esp. "Login failed" - RHoffman - Jan. '98
#
# SCCS Info:
#  FAIF_ESA_ftpsend.csh
#  1
#  03/13/98
#  @(#)FAIF_ESA_ftpsend.csh	1.3
#===============================================================================

###############################################################
# Usage banner for FTP to Unix machine script
###############################################################
if ($#argv < 3) then
   echo "Usage: FAIF_ESA_ftpsend.csh RemoteHost RemoteDir FileToSend Mode FAIFRootpath Username Password"
   exit 1
endif

###############################################################
# Get command options:
#   argument 1 - remote machine to send file to
#   argument 2 - directory in remote machine to put file in
#   argument 3 - file to send to remote machine
#   argument 4 - FTP transfer mode (ascii or binary)
###############################################################
set nodename = $1
set destdir = $2
set sentpath = $3
set sentfile = $sentpath:t 
set vaxsentfile = $sentfile. 
set rootpath = $5
set tempdir = $rootpath/FTP/stmp
set unique = `uuidgen`
set tempfile0 = $tempdir/$sentfile.tmp.0.$unique
set tempfile1 = $tempdir/$sentfile.tmp.1.$unique

#set mail_user = `/usr/ucb/whoami`", richard@ditto.jpl.nasa.gov"
source ~/set_env.csh

###############################################################
# Determine transfer mode from 4th argument 
# Use binary mode if this fails
###############################################################
if ($4 == 'binary') then
   set mode = binary
else if ($4 == 'ascii') then
   set mode = ascii
else
   set mode = binary
endif

set first_4 = `echo $sentfile | cut -c 1-4`
if ($first_4 == 'reex' || $first_4 == 'REEX') set mode = ascii

set username = $6
set password = $7

###############################################################
# Same file that is sent is pulled via FTP into
# the copydir to check if it was sent properly
###############################################################
set copydir = $tempdir

###############################################################
# Delete the old temp files, if they exist
###############################################################
if (-e $tempfile0) then
   rm $tempfile0
endif

if (-e $tempfile1) then
   rm $tempfile1
endif

###############################################################
# The following ftp attempt and checking its results has been
# commented out as superfluous (and, because it's not protected
# by a kill_hung_proc.csh, trouble-prone).
###############################################################

################################################################
## Try to cd to the remote directory
## Redirect FTP messages to a temporary file
################################################################
#ftp -i -n $nodename <<.  >& $tempfile0
#user $username $password
#cd $destdir
#bye
#.
#
################################################################
## Check if destination host known
################################################################
#set remhostOk = `grep "unknown host" $tempfile0 > $tempfile1`
#if (! -z $tempfile1) then
#   echo "Unknown remote host $nodename." >> $tempfile0
#   exit 2
#else
#   echo "Remote host $nodename known." >> $tempfile0
#endif
#rm $tempfile1
#
################################################################
## Check if login succeeded
################################################################
#set loginOk = `grep "Login failed" $tempfile0 > $tempfile1`
#if (! -z $tempfile1) then
#   echo "Unable to login to remote host $nodename." >> $tempfile0
#   exit 2
#else
#   echo "Login to $nodename ok." >> $tempfile0
#endif
#rm $tempfile1
#
################################################################
## Check if you were able to cd to the destdir
################################################################
#set direxists = `grep "No such file or directory" $tempfile0 > $tempfile1`
#if (! -z $tempfile1) then
#   echo "Remote directory $destdir does not exist." >> $tempfile0
#   exit 3
#else
#   set direxists = `grep "directory not found" $tempfile0 > $tempfile1`
#   if (! -z $tempfile1) then
#      echo "Remote directory $destdir does not exist." >> $tempfile0
#      exit 3
#   else
#      echo "Found source directory $destdir ok." >> $tempfile0
#endif
#rm $tempfile1
#
###############################################################
# End of commented-out superfluous ftp
###############################################################

###############################################################
# Perform FTP transfer
# Redirect FTP messages to a temporary file
###############################################################

# The script will make 5 connection attempts, and will then give up.

@ num_tries = 1

connect_try:

ftp -i -n $nodename <<.  >& $tempfile0 &
user $username $password
$mode
cd $destdir
put $sentpath $sentfile
lcd $copydir 
get $vaxsentfile $sentfile"_copy"
rename $sentfile [-.to_eecf\]
bye
.

# Check for (and kill if necessary) hung ftp.

set ftp_pid = `/bin/ps -ef | grep -v grep | grep "ftp -i -n $nodename"  | grep $$ | awk '{printf("%s"), $2}'`

$FAIF_BINPATH/kill_hung_proc.csh $ftp_pid 60 5

@ kill_stat = $status

if ($kill_stat != 0) then

   /usr/ucb/logger -p local6.debug -t "ESA xmit" -i "Ftp process was hung... killed it."

   /bin/mailx -s "ftp hung during ESA file send" $mail_user << EndMailx
There was a hung ftp while trying to send an ESA outbound file.

The ftp output is in $tempfile0.

The following line shows node, destdir, sentpath, sentfile
$nodename $destdir $sentpath $sentfile
EndMailx

endif

# Check for conditions suitable for retry

if (-z $tempfile0) then
   @ num_tries = $num_tries + 1
   if ($num_tries < 6) goto connect_try
endif

grep "Login failed" $tempfile0 >&! /dev/null
if ($status == 0) then
   @ num_tries = $num_tries + 1
   if ($num_tries < 6) goto connect_try
endif

grep "Not connected" $tempfile0 >&! /dev/null
if ($status == 0) then
   @ num_tries = $num_tries + 1
   if ($num_tries < 6) goto connect_try
endif

###############################################################
# Do a diff on sent file and pulled copy
#
# If the diff.result.$sentfile is empty, NO difference
#    return 0 for transfer OK
# Else
#    return 4 for transfer ERROR
# Endif
###############################################################
set copyfile = $copydir/$sentfile"_copy"
if (-e $copyfile) then
   diff $sentpath $copyfile >& $copydir/diff.result.$sentfile
   if (-z $copydir/diff.result.$sentfile) then
      @ exit_status = 0
   else
      @ exit_status = 4
   endif
   rm $copydir/diff.result.$sentfile
   rm $copyfile
   rm $tempfile0
   exit $exit_status
else
   @ num_tries = $num_tries + 1
   if ($num_tries < 6) then
      goto connect_try
   else
      exit 5
   endif
endif


#!/bin/csh -f
#===============================================================================
# Filename:	ADEOS_ftpget.csh
# Description:
#	This is the script that is used to transfer files from 
# a remote host to the local host via FTP.
# 
# Creator:	Norbert Piega
# Notes:	
#1.  June '94 - R. Hoffman
#    a. Added umask command to correct file permissions
#    b. Added filetype to use for temp filenames so they won't accumulate
#    c. Removed srcpath - never used
#    d. Added final check for retrieved file
#    e. Corrected comments
# SCCS Info:
#  ADEOS_ftpget.csh
#  1
#  05 Jun 1996
#  @(#)ADEOS_ftpget.csh	1.4
#===============================================================================

###############################################################
# Usage banner for ADEOS_ftpget.csh script                       
###############################################################
if ($#argv < 8) then
   echo "Usage:"
   echo "ADEOS_ftpget.csh RemoteHost RemoteDir Receptdir Filename Mode Rootpath Username Password"
   exit 1
endif

###############################################################
# Assign variable values from command line
###############################################################
set nodename  = $1
set srcdir    = $2
set receptdir = $3
set filename  = $4
set complete = $srcdir"/"$filename
set srcdir = $complete:h
set filename = $complete:t
set filetype = `echo $filename | cut -c1-4`

###############################################################
# Determine transfer mode from 5th argument
# Use binary mode if this fails
###############################################################
if ($5 == 'binary') then
   set mode = binary
else if ($5 == 'ascii') then
   set mode = ascii
else
   set mode = binary
endif

set rootpath = $6
set username = $7
set password = $8

###############################################################
# Assign names of temp dir and temp files
###############################################################
set machtype = Unix
set tempdir =   $rootpath/FTP/gtmp
set remote_listing = $tempdir/remotelisting.$filetype
set tempfile0 = $tempdir/ADEOS_ftpget.activity.$filetype
set tempfile1 = $tempdir/tmp1.tmp1.$filetype
set tempfile2 = $tempdir/tmp2.tmp2.$filetype
set filenamedlist = $tempdir/filenamedlistfile.$filetype
set receivedlist = $tempdir/receivedlistfile.$filetype
umask 0

###############################################################
# Remove old activity log; Create new one which will remain
# unerased after execution of this script (until next FTP of
# same filetype)
###############################################################
if (-e $tempfile0) then
   rm $tempfile0
endif
echo "ADEOS_ftpget.csh script activity log:" > $tempfile0
echo "username = $username"   >> $tempfile0
echo "nodename = $nodename"   >> $tempfile0
echo "srcdir    = $srcdir"    >> $tempfile0
echo "receptdir = $receptdir" >> $tempfile0
echo "filename = $filename" >> $tempfile0

###############################################################
# Perform FTP Long Directory Listing (ls -l or dir)
# Redirect FTP messages (ls -l output) to a temporary file
###############################################################
if (-e $remote_listing) then
   rm $remote_listing
endif
ftp -i -n $nodename <<EndFTPl  >& $remote_listing 
user $username $password
$mode
cd $srcdir
ls -l
bye
EndFTPl

###############################################################
# Check if destination host known
###############################################################
set remhostOk = `grep "unknown host" $remote_listing > $tempfile1`
if (! -z $tempfile1) then
   echo "Unknown remote host $nodename." >> $tempfile0
   exit 2
else
   echo "Remote host $nodename known." >> $tempfile0
endif
rm $tempfile1

###############################################################
# Check if login succeeded
###############################################################
set loginOk = `grep "Login failed" $remote_listing > $tempfile1`
if (! -z $tempfile1) then
   echo "Unable to login to remote host $nodename." >> $tempfile0
   exit 2
else
   echo "Login to $nodename ok." >> $tempfile0
endif
rm $tempfile1

###############################################################
# Check if you were able to cd to the srcdir
###############################################################
set direxists = `grep "No such file or directory" $remote_listing > $tempfile1`
if (! -z $tempfile1) then
   echo "Remote directory $srcdir does not exist." >> $tempfile0
   exit 3
else
   echo "Found source directory $srcdir ok." >> $tempfile0
endif
rm $tempfile1

###############################################################
# If "failed" occurs in FTP output, there was an error       
# while in ftp.  Get out if error, continue otherwise.
###############################################################
set liststat = `grep failed $remote_listing > $tempfile1`
if (! -z $tempfile1) then
   echo "Failed to get file listing of remote directory." >> $tempfile0
   exit 4
else
   echo "Got file listing of remote directory." >> $tempfile0
endif
rm $tempfile1


###############################################################
# Perform the file transfer (get files) via FTP
###############################################################

ftp -i -n $nodename <<EndFTPg  >& $tempfile1
   user $username $password
   $mode
   cd $srcdir
   lcd $receptdir
   get $filename
   bye
EndFTPg


###############################################################
# If "failed" occurs in FTP output, there was an error       
# while in ftp.  Get out if error, continue otherwise.
###############################################################
if (-e $tempfile1) then
   set getstat = `grep failed $tempfile1 > $tempfile2`
   if (! -z $tempfile2) then
      echo "Failure in FTP operation." >> $tempfile0
      exit 5
   endif
   rm $tempfile2
else
   echo "No tempfile" >> $tempfile0
endif

###############################################################
# Final check for retrieved file
###############################################################
set retrievedfile = $receptdir"/"$filename
if (! -e $retrievedfile) then
   echo "Failure in FTP operation." >> $tempfile0
   exit 5
else
   echo "Got files from remote directory." >> $tempfile0
endif

###############################################################
# Exit with status 0 for success
###############################################################
exit 0


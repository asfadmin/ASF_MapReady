#!/bin/csh -f
#===============================================================================
# Filename:	FAIF_VAX_ftpsend.csh
# Description:
#	This is the FTP script that is used to transfer files from FAIF
# to a remote system.
# 
# Creator:	Norbert Piega
# Notes:	
# SCCS Info:
#  %M%
#  %R%
#  %G%
#  %W%
#===============================================================================

###############################################################
# Usage banner for FTP to Unix machine script
###############################################################
if ($#argv < 3) then
   echo "Usage: FAIF_VAX_ftpsend.csh RemoteHost RemoteDir FileToSend Mode FAIFRootpath Username Password"
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
# Try to cd to the remote directory
# Redirect FTP messages to a temporary file
###############################################################
ftp -i -n $nodename <<.  >& $tempfile0
user $username $password
cd $destdir
bye
.

###############################################################
# Check if destination host known
###############################################################
set remhostOk = `grep "unknown host" $tempfile0 > $tempfile1`
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
set loginOk = `grep "Login failed" $tempfile0 > $tempfile1`
if (! -z $tempfile1) then
   echo "Unable to login to remote host $nodename." >> $tempfile0
   exit 2
else
   echo "Login to $nodename ok." >> $tempfile0
endif
rm $tempfile1

###############################################################
# Check if you were able to cd to the destdir
###############################################################
set direxists = `grep "No such file or directory" $tempfile0 > $tempfile1`
if (! -z $tempfile1) then
   echo "Remote directory $destdir does not exist." >> $tempfile0
   exit 3
else
   set direxists = `grep "directory not found" $tempfile0 > $tempfile1`
   if (! -z $tempfile1) then
      echo "Remote directory $destdir does not exist." >> $tempfile0
      exit 3
   else
      echo "Found source directory $destdir ok." >> $tempfile0
endif
rm $tempfile1

###############################################################
# Perform FTP transfer
# Redirect FTP messages to a temporary file
###############################################################
ftp -i -n $nodename <<.  >& $tempfile0
user $username $password
$mode
cd $destdir
put $sentpath $sentfile
lcd $copydir 
get $vaxsentfile $sentfile
bye
.

###############################################################
# Do a diff on sent file and pulled copy
#
# If the diff.result.$sentfile is empty, NO difference
#    return 0 for transfer OK
# Else
#    return 4 for transfer ERROR
# Endif
###############################################################
set copyfile = $copydir/$sentfile
if (-e $copyfile) then
   diff $sentpath $copyfile >& $copydir/diff.result.$sentfile
   if (-z $copydir/diff.result.$sentfile) then
      @ exit_status = 0
   else
      @ exit_status = 4
   endif
   rm $copydir/diff.result.$sentfile
   rm $copyfile
   exit $exit_status
else
   exit 5
endif


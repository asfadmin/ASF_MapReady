#!/bin/csh -f
#===============================================================================
# Filename:	CSA_ftpget.csh
# Description:
#	This is the script that is used to transfer files from 
# a remote host to the local host via FTP.
# 
# Creator:	Norbert Piega
# Notes:	
# 1.  Feb. '96 - R. Hoffman
#     (a) Revise handling of idfilename return value for .orb files
#     (b) Change PredOrRes to CSAPredOrRes
# SCCS Info:
#  CSA_ftpget.csh
#  1
#  23 Feb 1996
#  @(#)CSA_ftpget.csh	1.2
#===============================================================================

###############################################################
# Usage banner for CSA_ftpget.csh script                       
###############################################################
if ($#argv < 5) then
   echo "Usage:"
   echo "CSA_ftpget.csh RemoteHost RemoteDir Receptdir FileToGetType Mode Rootpath Username Password"
   exit 1
endif


###############################################################
# Assign variable values from command line
###############################################################
set nodename  = $1
set srcdir    = $2
set receptdir = $3
set ftype_str = $4
if ($ftype_str == "CSA_PREDORBIT")  then
   @ ftype = 200
else if ($ftype_str == "CSA_DEFVORBIT")  then
   @ ftype = 201
else if ($ftype_str == "CSA_RECRQST")    then
   @ ftype = 202
else if ($ftype_str == "CSA_RECSCHED")   then
   @ ftype = 203
else if ($ftype_str == "CSA_CALIBRQST")  then
   @ ftype = 204
else if ($ftype_str == "CSA_CALIBSCHED") then
   @ ftype = 205
else if ($ftype_str == "CSA_SARPROCPRM") then
   @ ftype = 206
else if ($ftype_str == "CSA_RRQ_MCM") then
   @ ftype = 207
else if ($ftype_str == "CSA_RSH_MCM") then
   @ ftype = 208
else if ($ftype_str == "CSA_CRQ_MCM") then
   @ ftype = 209
else if ($ftype_str == "CSA_CSH_MCM") then
   @ ftype = 210
endif


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
# Get value of FAIF_BINPATH.  Use default if not defined
###############################################################
set var2 = (`env | grep FAIF_BINPATH`)
if ($#var2 == 0) then
   set srcpath = /asf/bin
else
   set srcpath = $FAIF_BINPATH
endif

###############################################################
# Assign name of temp dir, idfilename, get CSA state vector
# precision utility program, directory of received orbit files
# prior to precision determination
###############################################################
set machtype = Unix
set idfilename = $srcpath/idCSAfilename
set getprecis = $srcpath/CSAPredOrRes
set orbitdir = $rootpath/CSA/CSA_ORBIT
set tempdir =   $rootpath/FTP/gtmp
set remote_listing = $tempdir/remotelisting.$ftype
set rlistingvax = $tempdir/remotelisting2.$ftype
set tempfile0 = $tempdir/CSA_ftpget.activity.$ftype
set tempfile1 = $tempdir/tmp1.tmp1.$ftype
set tempfile2 = $tempdir/tmp2.tmp2.$ftype
set ftypedlist = $tempdir/ftypedlistfile.$ftype
set receivedlist = $tempdir/receivedlistfile.$ftype

###############################################################
# Remove old activity log; Create new one which will remain
# unerased after execution of this script
###############################################################
if (-e $tempfile0) then
   rm $tempfile0
endif
echo "CSA_ftpget.csh script activity log:" > $tempfile0
echo "username = $username"   >> $tempfile0
echo "nodename = $nodename"   >> $tempfile0
echo "srcdir    = $srcdir"    >> $tempfile0
echo "receptdir = $receptdir" >> $tempfile0
echo "ftype_str = $ftype_str" >> $tempfile0

###############################################################
# Perform FTP Long Directory Listing (ls -l or dir)
# Redirect FTP messages (ls -l output) to a temporary file
###############################################################
ftp -i -n $nodename <<EndFTPl  >& $remote_listing
user $username $password
$mode
cd $srcdir
ls -l
bye
EndFTPl

#
# Check number of files. If there are too many, produce error message 
# and return in error.
#
set num_lines_str = `wc $remote_listing`
@ num_lines = $num_lines_str[1]
if ($num_lines > 750) then
   /usr/ucb/logger -p local6.debug -t "CSA_ftpget.csh" -i "Too many files on remote node."
   /usr/ucb/logger -p local6.debug -t "CSA_ftpget.csh" -i "Must be run in blocks of no more than 750 at a time."
   exit 5
endif

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
# Extract filenames from FTP listing to generate
# a list containing filenames only called "fnames".
###############################################################
if ($machtype == Vax) then
   set grepcmd = `grep ';' $remote_listing >$rlistingvax`
   set fnames = (`cat $rlistingvax | awk -F\; '{print $1}'`)
else
   set fnames = (`cat $remote_listing | awk '{print $9}'`)
endif

###############################################################
# Create a sublist from "fnames" of the type of files that
# need to be pulled from the remote host.  The list will
# be in the file "ftypedlistfile"
###############################################################
if (-e $ftypedlist) then
   rm $ftypedlist
endif

foreach getfile ($fnames)
   echo "Trying to identify $getfile" >> $tempfile0
   $idfilename $getfile
   @ got_type = $status 
#  $got_type = 200 for all .orb files. Also accept either type of RRQ file
#  regardless of which type was requested. Likewise for RSH files.
   if (($got_type == $ftype) || (($got_type == 200) && ($ftype == 201)) \
                             || (($got_type == 202) && ($ftype == 207)) \
                             || (($got_type == 203) && ($ftype == 208))) then
      if (-e $ftypedlist) then
         echo $getfile >> $ftypedlist
      else
         echo $getfile > $ftypedlist
      endif
   else
      echo "$getfile is $got_type not $ftype" >> $tempfile0
   endif
end

if (-e $ftypedlist) then
   echo "Files to get are" >> $tempfile0
   set toget = (`cat $ftypedlist`)
   foreach getfile ($toget)
      echo $getfile >> $tempfile0
   end
   echo "End listing" >> $tempfile0
else
   echo "No files to get" >> $tempfile0
   exit 0
endif

###############################################################
# Set an intermediate reception dir for orbit files
###############################################################
if ($ftype_str == "CSA_PREDORBIT" || $ftype_str == "CSA_DEFVORBIT") then
    set realreceptdir = $receptdir
    set receptdir = $orbitdir
endif
echo "Reception directory is $receptdir" >> $tempfile0

###############################################################
# Perform the file transfer (get files) via FTP
###############################################################
set toget = (`cat $ftypedlist`)
foreach togetfile ($toget)

ftp -i -n $nodename <<EndFTPg  >& $tempfile1
   user $username $password
   $mode
   cd $srcdir
   lcd $receptdir
   get $togetfile
   bye
EndFTPg

   ############################################################
   # Delete files obtained successfully via FTP
   ############################################################
   set getstat = `grep failed $tempfile1 > $tempfile2`
   if (-z $tempfile2) then
      echo "Got $togetfile" >> $tempfile0
ftp -i -n $nodename <<EndFTPd  >& $tempfile1
      user $username $password
      $mode
      cd $srcdir
      delete $togetfile
      bye
EndFTPd
   endif

end # End FOR loop
rm $ftypedlist

###############################################################
# If "failed" occurs in FTP output, there was an error       
# while in ftp.  Get out if error, continue otherwise.
###############################################################
if (-e $tempfile1) then
   set getstat = `grep failed $tempfile1 > $tempfile2`
   if (! -z $tempfile2) then
      echo "Failure in FTP operation." >> $tempfile0
      exit 5
   else
      echo "Got files from remote directory." >> $tempfile0
   endif
   rm $tempfile2
else
   echo "No tempfile" >> $tempfile0
endif


###############################################################
# Determine state vector precision and separate out to
# appropriate directory.
###############################################################
if ($ftype_str == "CSA_PREDORBIT" || $ftype_str == "CSA_DEFVORBIT") then
   foreach gotfile ($receptdir/*)
      echo "Checking $getprecis -f $gotfile" >> $tempfile0
      $getprecis -f $gotfile > $tempfile1

#if status is 1, restituted
#if status is 2, predicted

      @ predorres = $status
      if ($ftype_str == "CSA_PREDORBIT") then
         if ($predorres == 2) then
            mv $gotfile $realreceptdir/.
	    echo "Moved PREDICTED $gotfile to $realreceptdir/." >> $tempfile0
         endif

         if ($predorres == 1) then
	    set destin = $rootpath/CSA/CSA_DEFVORBIT
	    mv $gotfile $destin/.
	    echo "Moved RESTITUTED $gotfile to $destin/." >> $tempfile0
         endif

         if ($predorres != 1 && $predorres != 2) then
	    echo "Error in $getprecis. Unexpected status $predorres" >> $tempfile0
         endif
      endif

      if ($ftype_str == "CSA_DEFVORBIT") then
         if ($predorres == 1) then
            mv $gotfile $realreceptdir/.
	    echo "Moved RESTITUTED $gotfile to $realreceptdir/." >> $tempfile0
         endif

         if ($predorres == 2) then
	    set destin = $rootpath/CSA/CSA_PREDORBIT
	    mv $gotfile $destin/.
	    echo "Moved PREDICTED $gotfile to $destin/." >> $tempfile0
         endif

         if ($predorres != 1 && $predorres != 2) then
	    echo "Error in $getprecis. Unexpected status $status" >> $tempfile0
         endif
      endif
      
   end
endif

# Rename rrq and rsh files according to "facility". Each of these file types
# can come for either MCM of F, but their names contain only "rev" and "file 
# type". IMS will overwrite a F file with a later MCM file for the same rev.
# The following code will insert either "_f" or "_m" right before the "." in
# the file name, thereby making the file names unique.

if ($ftype_str == "CSA_RECRQST") then
   cd $receptdir
   foreach fn ( m[0-9][0-9][0-9][0-9][0-9][0-9][0-9].rrq )
      set fn_1 = `echo $fn | cut -c 1-8`
      set fn_2 = `echo $fn | cut -c 9-12`
      mv $fn $fn_1"_f"$fn_2
   end
endif

if ($ftype_str == "CSA_RECSCHED") then
   cd $receptdir
   foreach fn ( m[0-9][0-9][0-9][0-9][0-9][0-9][0-9].rsh )
      set fn_1 = `echo $fn | cut -c 1-8`
      set fn_2 = `echo $fn | cut -c 9-12`
      mv $fn $fn_1"_f"$fn_2
   end
endif

if ($ftype_str == "CSA_RRQ_MCM") then
   cd $receptdir
   foreach fn ( m[0-9][0-9][0-9][0-9][0-9][0-9][0-9].rrq )
      set fn_1 = `echo $fn | cut -c 1-8`
      set fn_2 = `echo $fn | cut -c 9-12`
      mv $fn $fn_1"_m"$fn_2
   end
endif

if ($ftype_str == "CSA_RSH_MCM") then
   cd $receptdir
   foreach fn ( m[0-9][0-9][0-9][0-9][0-9][0-9][0-9].rsh )
      set fn_1 = `echo $fn | cut -c 1-8`
      set fn_2 = `echo $fn | cut -c 9-12`
      mv $fn $fn_1"_m"$fn_2
   end
endif

###############################################################
# Exit with status 0 for success
###############################################################
exit 0

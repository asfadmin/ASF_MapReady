#!/bin/csh -f
#===============================================================================
# Filename:	mails.csh
# Description:	Script used for sending email
# Creator:	Cameron Cooper
# Notes:	
# 1.  May '96 - R. Hoffman
#     Changed `uuidgen` to `/bin/uuidgen`
#
# SCCS Info:
#  mails.csh
#  1
#  17 May 1996
#  @(#)mails.csh	1.2
#===============================================================================

set rootpath = $4
set tempdir =   $rootpath/FTP/gtmp
set unique = `/bin/uuidgen`
set tempfile = $tempdir/temp$unique

if (-e $tempfile) then
   rm $tempfile
endif

set tmp = `mailx -s "$1" "$2" < $3 > $tempfile`

sleep 10

if (-z $tempfile) then
  rm $tempfile
  exit 0
else
  rm $tempfile
  exit 1
endif




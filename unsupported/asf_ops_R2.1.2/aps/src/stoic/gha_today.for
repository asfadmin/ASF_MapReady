C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  gha_today.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================

      PROGRAM GHA_TODAY
C********************************************************************
C  Name:	GHA_TODAY
C  Module Type: MAIN PROGRAM	Language: FORTRAN
C  $Logfile:   ACS003:[BLD.MPS.GHA.SRC]GHA_TODAY.FOV  $
C  Purpose:	PLACES A GREENWICH HOUR ANGLE FIX AT 00 hours TODAY INTO THE 
C		FILE IDENTIFIED BY THE LOGICAL NAME ACS_GHA_FIXES
C  Variables:
C  Locals :
C  Externals :
C  Modification History:                                            
C  Date			Revision	Author
C  $Date$ $Revision$ $Author$
C********************************************************************/
      character*100 SccsFileID
     -/'@(#)gha_today.for	5.1 98/01/08 APS/ASF\0'/

C     IMPLICIT NONE
C     INTEGER  IPRINT

      INTEGER  IER
      INTEGER  tc_systime2asf
      EXTERNAL tc_systime2asf !$PRAGMA C(tc_systeme2asf)
      CHARACTER*21  NOWTIME
      CHARACTER*1024  curr_dir
      CHARACTER*1024 stoic_path


C-- Get path of current directory....
      call getcwd(curr_dir(:LNBLNK(curr_dir)))
C-- Get path of stoicfile...
      call getenv('latest_stoicfile',stoic_path)
C-- Change directory to stoicfile storage...
      call chdir(stoic_path(:LNBLNK(stoic_path)))
C-- Begin to get stoicfile...
      IER = tc_systime2asf(NOWTIME)
      NOWTIME(10:21) = '00:00:00.000'
      WRITE(*,*)'  GHA_TODAY CALLING GHA_APPEND FOR UTC = ', NOWTIME
      CALL GHA_APPEND(NOWTIME,2,IER)
C-- Change back to current dirctory to run....
      call chdir(curr_dir(:LNBLNK(curr_dir)))
      IF(IER.EQ.1) STOP
      WRITE(*,*)'  ERROR IN APPENDING TODAYS GHA TO THE '
      WRITE(*,*)'  GHA_FIXES.DAT FILE.  '
      WRITE(*,*)'  IER = ', IER
      STOP
      END

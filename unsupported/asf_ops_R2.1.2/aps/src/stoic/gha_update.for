C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  gha_update.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================

      PROGRAM GHA_UPDATE
C********************************************************************
C  Name:	GHA_UPDATE
C  Module Type: MAIN PROGRAM	Language: FORTRAN
C  $Logfile:   QDISK:[BLD.MPS.GHA.SRC]GHA_UPDATE.FOV  $
C  Purpose:	APPENDS A GREENWICH HOUR ANGLE FIX FOR THE INPUT ASF TIME.
C
C		THE ROUTINE ADDS 1 RECORD ONTO THE END OF THE GREENWICH 
C		HOUR ANGLE FIXES FILE WHOSE NAME IS THE VALUE OF THE 
C		LOGICAL NAME ACS_GHA_FIXES.
C  Variables:
C  Locals :
C  Externals :
C  Modification History:                                            
C  Date			Revision	Author
C  $Date$ $Revision$ $Author$
C********************************************************************/
      character*100 SccsFileID
     -/'@(#)gha_update.for	5.1 98/01/08 APS/ASF\0'/

C     IMPLICIT NONE
      INTEGER IER
      CHARACTER*22 ASFTIME
      INTEGER  tc_validate_asf_datetime
      EXTERNAL tc_validate_asf_datetime 

      CHARACTER*1024  curr_dir		! Current path name
      CHARACTER*1024 stoic_path		! Stoic file pathname	


C-- Get path of current directory....
      call getcwd(curr_dir(:LNBLNK(curr_dir)))
C-- Get path of stoicfile...
      call getenv('latest_stoicfile',stoic_path)
C-- Change directory to stoicfile storage...
      call chdir(stoic_path(:LNBLNK(stoic_path)))

      WRITE(*,7001)
 7001 FORMAT ( 
     ?'$ENTER ASF TIME WITH SINGLE QUOTES FOR GHA FIX: ')
      ACCEPT *,ASFTIME
C
      WRITE(*,*)'  GHA_UPDATE FOR ASF TIME = ', ASFTIME
C--   CALL CHASFT(ASFTIME,IER)				Lisa Nguyen 1/96
      IER = tc_validate_asf_datetime(ASFTIME//char(0))
      IF ( IER .NE. 0 ) THEN
	WRITE (*,*)' ERROR IN ASFTIME.  CHECK COLUMN ', IER
	STOP
      ENDIF
C-- Append GHA...
      CALL GHA_APPEND(ASFTIME,2,IER)

C-- Change back to current dirctory to run....
      call chdir(curr_dir(:LNBLNK(curr_dir)))

      IF(IER.EQ.1) GO TO 6999
      WRITE(*,*)'  ERROR IN APPENDING TODAYS GHA TO THE '
      WRITE(*,*)'  GHA_FIXES.DAT FILE.  '
      WRITE(*,*)'  IER = ', IER
      STOP
 6999 CONTINUE
      WRITE(*,*) ' GHA_APPEND:  ASFTIME = ',ASFTIME
      END

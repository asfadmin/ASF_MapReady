C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  gha_append.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================

      SUBROUTINE GHA_APPEND(ASFTIME,IPRINT,IER)
C********************************************************************
C  Name:	GHA_APPEND.FOR
C  Module Type: SUBROUTINE	Language: FORTRAN
C  $Logfile:   ACS003:[BLD.MPS.GHA.SRC]GHA_APPEND.FOV  $
C  Purpose:	APPENDS A GREENWICH HOUR ANGLE FIX FOR THE BEGINNING OF THE 
C		CURRENT DAY.  ADDS A RECORD ONTO THE END OF THE GREENWICH 
C		HOUR ANGLE FIXES FILE WHOSE NAME IS THE VALUE OF THE 
C		LOGICAL NAME ACS_GHA_FIXES.
C  Functions called:	USES THE VECTOR LIBRARY.  
C  Input Parameters:
C  Name         Type    Definition
C  ASFTIME	CH*21	INPUT TIME UTC IN THE ASF-SIS TIME FORMAT.
C  IPRINT	INT	1 = ASKING FOR DIAGNOSTIC PRINT.  
C			2 = ASKING FOR OPERATIONAL PRINT.
C			-1= ASKING FOR ONLY THE UT1-GHA PRINT.
C  Output Parameters:
C  old from Larry IER		INT	ERROR CODE.  0 = O.K.
C  IER                          INT     ERROR COD    1 = TRUE,
C                                                    0 = FALSE 
C                                                   -1 = ERROR
C  Variables:
C  Locals :
C  Externals :
C  Modification History:                                            
C  Date			Revision	Author
C  $Date$ $Revision$ $Author$
C********************************************************************/
      character*100 SccsFileID
     -/'@(#)gha_append.for	5.1 98/01/08 APS/ASF\0'/

C     IMPLICIT NONE
      INTEGER IPRINT, IFILE, ISTAT, IER, H, M
      CHARACTER*21 ASFTIME
      character*1024 fullname		!name of gha_fixes
      character*1024 prefix		!pathname
      REAL*8 GHA, S, UT1, XD, XGHA, GHA0
      INTEGER wrt_gha_metadata
      EXTERNAL wrt_gha_metadata !$PRAGMA C(wrt_gha_metatdata)
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'

      IER = 1
      IFILE = 4
      call getenv('latest_gha_fixes',prefix)
      fullname = prefix(:LNBLNK(prefix)) // '/ACS_GHA_FIXES'
      OPEN (IFILE,ACCESS='APPEND',ERR=8001,FILE=fullname,
     ?      IOSTAT=ISTAT,STATUS='OLD')
      IF(IPRINT.EQ.1)WRITE(*,*) ' GHA_APPEND:  ASFTIME = ',ASFTIME
C---	NOW USE THE VECTOR LIBRARY TO GET THE ACCURATE GHA:
      CALL MPS2GHA(ASFTIME,GHA,UT1,0)
      IF(IPRINT.EQ.0)GO TO 1000
C---	COMPUTE GHA FIX AT 0H UT1.  
C---	FRACTIONAL DAY XD:
      IF(IPRINT .EQ. 1) WRITE(*,*)'  UT1 = ', UT1
      IF(IPRINT .EQ. 1) WRITE(*,*)'  GHA = ', GHA
      XD = (UT1 - INT(UT1)) 
C---	FRACTIONAL DAY OFF FROM 0 hours:  
      XD = XD - 0.5D0
      IF(IPRINT .EQ. 1) WRITE(*,*)'  XD = ', XD
C---	DEGREES OFF FROM GHA DUE TO FRACTIONAL DAY OFF.  
C---	EROTI = EARTH ROTATION RATE INERTIAL CY/DAY.
      XGHA = XD * EROTI * 360.0D0
C---	DEGREES GHA AT 0 hours UT1 TO BE COMPARED TO ALMANAC:
C--	IF THE UT1 WAS AFTER .5 DAYS, NEED TO SUBTRACT FROM THE GHA.  
C--	IF THE UT1 WAS BEFORE .5 DAYS, NEED TO ADD TO THE GHA.  
      GHA0 = GHA - XGHA
      IF(GHA0 .GE. 360.0D0) GHA0 = GHA0 - 360.0D0
      IF(GHA0 .LT.   0.0D0) GHA0 = GHA0 + 360.0D0
      IF(IPRINT .GT. 0)WRITE(*,*)' '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' GHA_APPEND:  THE FOLLOWING DATA IS USED WHEN YOU WANT TO CHECK'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' THE ACCURACY OF THIS GHA FIX:  '
      IF(IPRINT .EQ. 1)WRITE(*,*)
     ?' GHA_APPEND:  Julian date at UT1 0 hours = ', (UT1 - XD)
      IF(IPRINT .EQ. 1)WRITE(*,*)' GHA_APPEND:  GHA AT 0h UT1 = ', GHA0
      H = (GHA0/15.0D0)
      M = (GHA0 - (H*15.0D0)) * 4.0D0
      S = (GHA0   -   ( (H*15.0D0) + (M/4.0D0) ) ) * 4.0D0 * 60.0D0
      IF(IPRINT .NE. 0)WRITE(*,7007)(UT1-XD), H, M, S
 7007 FORMAT(/,14X,'JULIAN DATE UT1       GHA in  h  m   s ',/,
     ?  '              ----------------             -- --  ------',/,
     ?  '              ',F16.8,13X,I2,I3,F8.4/)
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' NOW YOU CAN CHECK THE VALUE OF THE GREENWICH HOUR ANGLE.'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' USE THE ASTRONOMICAL ALMANAC SECTION B pp.8-15.  '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' YOU HAVE BEEN GIVEN THE UT1 AND THE APPARENT GHA.'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' NOTE THAT THE GHA IS GIVEN IN THE HOURS/MINUTES/SECONDS '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' FORMAT, WHERE ONE HOUR = 15 DEGREES.  THIS IS TO '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' CORRESPOND TO THE BOOK.  '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' THE ALMANAC LISTS UT (THIS MEANS UT1) AND GREENWICH'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' SIDERIAL TIME (GHA OF THE EQUINOX) APPARENT AND MEAN.'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' COMPARE THE APPARENT GHA IN THE BOOK WITH THE GHA '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' LISTED HERE AT THE CORRESPONDING JULIAN DAY.'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' AN ERROR OF 1 SECOND IN THE GHA CORRESPONDS ROUGHLY TO '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' AN ERROR OF 464 METERS AT THE EARTHS EQUATOR.'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' Note that you do NOT take the value in the GHA fixes'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' file, those are fixes at UTC, at a time up to +- .9 seconds'
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' different from the above UT1 fix.  Only compare the above '
      IF(IPRINT .GT. 0)WRITE(*,*)
     ?' UT1 - GHA fix with Astronomical Almanac. '
 1000 CONTINUE
      WRITE(*,7002)ASFTIME,GHA
 7002 FORMAT(' APPENDING RECORD TO GHA_FIXES.DAT ===>',1X,A21,1X,F12.6)
      WRITE(IFILE,7001)ASFTIME,GHA
 7001 FORMAT(1X,A21,1X,F12.6)
      CLOSE (IFILE)
      IER = wrt_gha_metadata(fullname(:LNBLNK(fullname))//char(0)) 
      GO TO 9999
 8001 CONTINUE
C---	ERROR IN OPENING THE FILE.  
      WRITE(*,*)
     ?'  GHA_APPEND:  ERROR IN OPENING THE ACS GHA FIXES DATA FILE.'
      WRITE(*,*)'  This file has fixes for the Greenwich Hour Angle.'
      WRITE(*,*)'  Check to make sure that you have the correct value'
      WRITE(*,*)'  for the Logical name ACS_GHA_FIXES.  The value '
      WRITE(*,*)'  of this logical name should be the name of the '
      WRITE(*,*)'  file including directory path and including '
      WRITE(*,*)'  the node name, if necessary.  '
      WRITE(*,*)'  ***************************************'
      WRITE(*,*)'  ***************************************'
      WRITE(*,*)'  USER-GENERATED TERMINATION DUE TO INVALID '
      WRITE(*,*)'  VALUE OF ACS_GHA_FIXES LOGICAL NAME.  '
      S = SQRT(-1.0)
      WRITE(*,*)'  S = ', S
      CLOSE (IFILE)
 9999 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION UTC2GHA(IY,ID,IH,IM,XS)
C********************************************************************
C*  Name:	UTC2GHA
C*  Module Type: DOUBLE PRECISION FUNCTION SUBROUTINE	Language: FORTRAN
C*  $Logfile:   ACS003:[BLD.ACS.LIB.STVEC]UTC2GHA.FOV  $
C*  Purpose:	Computes Greenwich Hour Angle from UTC time.  It converts the 
C		input time to ASF format, then calls ASF2GHA, which then reads 
C		the ACS GHA Fixes file, identified by the value of the logical 
C		name ACS_GHA_FIXES.  ASF2GHA then returns the GHA.  
C*  Input Parameters:
C*  Name         Type    Definition
C   IY 		INT	YEAR of the input time in UTC
C   ID 		INT	DAY NUMBER of the input time in UTC.
C			Note that this number must be 1-365 or 366.  For a time
C			of 00:00 on January 21st, for example, the day number 
C			will be 21.  [Note that the time elapsed since the 
C			start of the year will be 20 days, and not 21 days.]
C   IH 		INT	HOUR of the input time in UTC
C   IM 		INT	MUNITES of the input time in UTC
C   XS		REAL*8	SECONDS of the input time in UTC
C*  Output Parameters:
C   UTC2GHA	REAL*8	Greenwich Hour Angle for input time in degrees from 
C			0-360.
C*  Externals :
C   ACS_GHA_FIXES	A DCL-level logical name.  It is the complete name of 
C			the Greenwich Hour Angle Fixes file, including 
C			directory path and node, if necessary, 
C*  Modification History:                                            
C*  $Date$ $Revision$ $Author$
C*  Date			Revision	Author
C*********************************************************************/
      IMPLICIT NONE
      INTEGER IY, ID, IH, IM, IS, IPRINT
      REAL*8 XS, FS, GHA
      CHARACTER*21 ASFT
      character*80 sccsid
      data sccsid /'@(#)utc2gha.f	1.3 96/04/09 22:51:58\0'/
      IPRINT = 0
      IS = XS
C---	TAKE THE FIRST 3 DECIMALS OF XS, I.E. TRUNCATE TO THE MILLISECOND.
      FS = (INT(XS*1000.0D0))/1000.0D0 - IS
      IF(IPRINT.NE. 0)WRITE(*,*)' UTC2GHA ==> IY = ', IY
      IF(IPRINT.NE. 0)WRITE(*,*)' ID = ', ID
      IF(IPRINT.NE. 0)WRITE(*,*)' IH = ', IH
      IF(IPRINT.NE. 0)WRITE(*,*)' IM = ', IM
      IF(IPRINT.NE. 0)WRITE(*,*)' XS = ', XS
      IF(IPRINT.NE. 0)WRITE(*,*)' IS = ', IS
      IF(IPRINT.NE. 0)WRITE(*,*)' FS = ', FS
      WRITE(ASFT,7001)IY, ID, IH, IM, IS, FS
 7001 FORMAT(I4,':',I3.3,':',I2.2,':',I2.2,':',I2.2,F4.3)
      IF(IPRINT.NE. 0)WRITE(*,*)' ASFT = ', ASFT
      CALL ASF2GHA(ASFT,GHA)
      IF(IPRINT.NE. 0)WRITE(*,*)' RETURN GHA=', GHA
      UTC2GHA = GHA
      RETURN
      END
      SUBROUTINE ASF2GHA(ASFTIME,GHA)
C********************************************************************
C*  Name:	ASF2GHA
C*  Module Type: SUBROUTINE	Language: FORTRAN
C*  $Logfile:   ACS003:[BLD.ACS.LIB.STVEC]ASF2GHA.FOV  $
C*  Purpose:	Computes Greenwich Hour Angle from an ASF time.  It reads the 
C		ASC GHA Fixes file, identified by the value of the logical 
C		name ACS_GHA_FIXES.  
C*  Input Parameters:
C*  Name         Type    Definition
C   ASFTIME	CH*21	TIME in the format:  yyyy:ddd:hh:mm:ss.sss
C*  Output Parameters:
C   GHA		REAL*8	Greenwich Hour Angle for ASFTIME in degrees from 0-360.
C*  Externals :
C   ACS_GHA_FIXES	A DCL-level logical name.  It is the complete name of 
C			the Greenwich Hour Angle Fixes file, including 
C			directory path and node, if necessary, 
C                       -- the open statement is modified slightly to
C                          work on the MASSCOMP-UNIX.
C*  Modification History:                                            
C*  $Date$ $Revision$ $Author:   David
C*  $Date$ $Revision$ $Author$
C*  Date			Revision	Author
C*********************************************************************/
      IMPLICIT NONE
      CHARACTER*(*) ASFTIME
      CHARACTER*21 ASFT
      REAL*8 GHA, GHA1, GHA2, GHAR
      INTEGER IY1, ID1, IH1, IM1
      INTEGER IY2, ID2, IH2, IM2
      INTEGER IY, ID, IH, IM
      REAL*8 XS1, XS2, XS, XD, XT1, XT2, XT, XANG
      INTEGER IPRINT, IFILE, ISTAT, IER , JD, JD1
      CHARACTER*21 ASFT1, ASFT2, ASFTR
C---	     ROTATION (CYCLES/DAY INERTIAL)
      REAL*8 EROTI
      DATA EROTI/1.0027379093D0/	!AR 1645 correction
C---	THE ABOVE CONSTANT WAS OBTAINED FROM THE VECTOR LIBRARY JUN-89
C---	VIA SUBROUTINE PSCONS.  
      ASFT = ASFTIME
      ASFT(5:5) = ':'
      CALL CHASFT(ASFT,IER)
      IF(IER.NE.0) GO TO 8002
      IPRINT =  0
      IER = 0
      IF(IPRINT .NE. 0)WRITE(*,*)' ASF2GHA:  ASFT = ', ASFT
      IFILE = 16
      OPEN (IFILE,FILE='/usr/local/ACS_GHA_FIXES',
     ?      ERR=8001,IOSTAT=ISTAT,STATUS='OLD')
C---	ASFT1 WILL BE THE LATEST TIME BEFORE ASFT.
C---	ASFT2 WILL BE THE EARLIEST TIME AFTER ASFT.
      ASFT1 = '1000:001:00:00:00.000'
      ASFT2 = '3000:001:00:00:00.000'
C 	INPUT RECORD ===> 1978:006:00:00:00.000   105.218857
C	COLUMNS      ===>12345678901234567890123456789012345
 1000 CONTINUE
C---		READ LOOP.  READING ASFTR/GHAR
      READ (IFILE,7001,END=2000)ASFTR,GHAR
 7001 FORMAT(1X, A21, 3X, F10.6)
C---	TEST TO SELECT FOR ASFT1 OR ASFT2.  
      IF(ASFTR .GT. ASFT1 .AND. ASFTR .LE. ASFT) GHA1 = GHAR
      IF(ASFTR .GT. ASFT1 .AND. ASFTR .LE. ASFT) ASFT1 = ASFTR
      IF(ASFTR .LT. ASFT2 .AND. ASFTR .GE. ASFT) GHA2 = GHAR
      IF(ASFTR .LT. ASFT2 .AND. ASFTR .GE. ASFT) ASFT2 = ASFTR
      IF(IPRINT .NE. 0)
     ?WRITE(*,*)' ASFT1 = ', ASFT1, '  ASFT2 = ',ASFT2
      GO TO 1000
 2000 CONTINUE
      CLOSE (IFILE)
C---	EOF.  NOW SELECT ASFT1 OR ASFT2 AS THE FIX TO USE.  
      IF( ASFT1(1:8) .EQ. ASFT(1:8)  ) GO TO 7701
      IF( ASFT2(1:8) .EQ. ASFT(1:8)  ) GO TO 7702
      IF(IPRINT .NE. 0)WRITE(*,*)' PLACE 1.  '
C---	STILL LOOKING.  
      IF( 	ASFT1(1:4) .EQ. ASFT(1:4)  .AND.
     ?		ASFT2(1:4) .NE. ASFT(1:4)  ) GO TO 7701
      IF(IPRINT .NE. 0)WRITE(*,*)' PLACE 2.  '
      IF( 	ASFT2(1:4) .EQ. ASFT(1:4)  .AND.
     ?		ASFT1(1:4) .NE. ASFT(1:4)  ) GO TO 7702
      IF(IPRINT .NE. 0)WRITE(*,*)' PLACE 3.  '
C---	JUST USE CLOSEST FIX.  
      READ(ASFT1,7002)IY1, ID1, IH1, IM1, XS1
 7002 FORMAT(I4,1X,I3,1X,I2,1X,I2,1X,F6.3)
      IF(IPRINT .NE. 0)
     ?WRITE(*,*)' IY1 = ', IY1,'  ID1=',ID1,'  IH1=',IH1,
     ?'  IM1=',IM1,'  XS1=',XS1
      READ(ASFT2,7002)IY2, ID2, IH2, IM2, XS2
      IF(IPRINT .NE. 0)
     ?WRITE(*,*)' IY2 = ', IY2,'  ID2=',ID2,'  IH2=',IH2,
     ?'  IM2=',IM2,'  XS2=',XS2
      READ(ASFT,7002)IY, ID, IH, IM, XS
      IF(IPRINT .NE. 0)
     ?WRITE(*,*)' IY = ', IY,'  ID=',ID,'  IH=',IH,
     ?'  IM=',IM,'  XS=',XS
      XT1=XS1/3600.0D0/24.0D0 + IM1/60.0D0*24.0D0 + IH1/24.0D0 + ID1
     ?   + IY1*365
      XT2=XS2/3600.0D0/24.0D0 + IM2/60.0D0*24.0D0 + IH2/24.0D0 + ID2
     ?   + IY2*365
      XT = XS/3600.0D0/24.0D0 +  IM/60.0D0*24.0D0 +  IH/24.0D0 + ID
     ?   + IY *365
      IF(IPRINT .NE. 0)
     ?WRITE(*,*)' XT1 = ', XT1, '  XT2 = ', XT2, '  XT = ', XT 
      IF( XT2-XT .LT. XT-XT1) GO TO 7702
 7701 CONTINUE
      IF(IPRINT .NE. 0)WRITE(*,*)' PLACE 4.  '
C---	USE ASFT1.
      ASFTR = ASFT1
      GHAR  = GHA1
      GO TO 7900
 7702 CONTINUE
      IF(IPRINT .NE. 0)WRITE(*,*)' PLACE 5.  '
C---	USE ASFT2.
      ASFTR = ASFT2
      GHAR  = GHA2
 7900 CONTINUE
      IF(IPRINT .NE. 0)WRITE(*,*)' PLACE 6.  '
      IF(IPRINT .NE. 0)
     ?WRITE(*,*)' ASFTR = ', ASFTR, '  GHAR = ', GHAR
C---	COMPUTE THE TIME DELTA BETWEEN SELECTED FIX AND DESIRED TIME IN DAYS.
      READ(ASFTR,7002)IY1, ID1, IH1, IM1, XS1
      READ(ASFT,7002)IY, ID, IH, IM, XS
C---	START WITH JUST THE DAYS:  
      XD = (ID-ID1) + (IH-IH1)/24.0D0 + (IM-IM1)/24.0D0/60.0D0  +
     ?			(XS-XS1)/24.0D0/3600.0D0 
      IF(IPRINT .NE. 0) WRITE(*,*)' XD = ', XD
C---	JULIAN DAY FOR 1ST OF JANUARY OF YEAR.  
C	REFERENCE:  SPACECRAFT ATTITUDE DETERMINATION AND CONTROL /J.WERTZ.
C---	PAGE 20.  JD1 <=> IY1;  JD <=> IY
      JD1 = 1 - 32075 + 1461*(IY1+4800+(1-14)/12)/4
     ?       +367*(1-2-(1-14)/12*12)/12
     ?       -3*((IY1+4900+(1-14)/12)/100)/4
      JD = 1 - 32075 + 1461*(IY+4800+(1-14)/12)/4
     ?       +367*(1-2-(1-14)/12*12)/12
     ?       -3*((IY+4900+(1-14)/12)/100)/4
      IF(IPRINT .NE. 0) WRITE(*,*)'  JD = ', JD, '  JD1 = ', JD1
      XD = XD + (JD - JD1)
      IF(IPRINT .NE. 0) WRITE(*,*)' XD = ', XD
C---	COMPUTE GHA. EROTI = CYCLES/DAY INERTIAL.  
      XANG = XD*EROTI*360.0D0 + GHAR
      IF(IPRINT .NE. 0) WRITE(*,*)' XANG = ', XANG
      GHA = MOD(XANG,360.0D0)
      IF(GHA .LT. 0.0D0) GHA = GHA + 360.0D0
      IF(IPRINT .NE. 0) WRITE(*,*)' RETURN VALUE: GHA = ', GHA
      RETURN 
 8001 CONTINUE
C---	ERROR IN OPENING THE FILE.  
      WRITE(*,*)'  ERROR IN OPENING THE ACS GHA FIXES DATA FILE.'
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
c      XD = SQRT(-1.0)
      WRITE(*,*)'  XD = ', XD
	stop 'Program aborted in ASF2gha'
 8002 CONTINUE
C---	ERROR IN ASF TIME FORMAT.  
      WRITE(*,*)'  ERROR IN ASF TIME FORMAT.'
      WRITE(*,*)'  ASF TIME INPUT = ', ASFT
      WRITE(*,*)'  Check in column:',IER
      WRITE(*,*)'  and after for the error.  '
      WRITE(*,*)'  Check the values in the various fields. '
      WRITE(*,*)'  The day number range is from 1-366.  '
      WRITE(*,*)'  ***************************************'
      WRITE(*,*)'  ***************************************'
      WRITE(*,*)'  USER-GENERATED TERMINATION DUE TO INVALID '
      WRITE(*,*)'  VALUE OF ASF TIME INPUT.  '
c      XD = SQRT(-1.0)
      WRITE(*,*)'  XD = ', XD
c      RETURN
	stop 'Program aborted in ASF2gha'
      END
      SUBROUTINE CHASFT(ASFTIME,IER)
********************************************************************
*  Name: CHASFT
*  Module Type: FUNCTION SUBROUTINE Language: FORTRAN 77
*  $Logfile:   ACS003:[BLD.ACS.LIB.STVEC]ASF2GHA.FOV  $
*  Purpose: CHECK DATE IN ASF FORMAT.  
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  ASFTIME	CHAR*21	DATE IN ASF FORMAT
*  Output Parameters:
*  IER		INTEGER	0	- IF NO ERROR.
*			1-21	- COLUMN NUMBER OF THE ERROR.  
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
      CHARACTER*21 ASFTIME
      INTEGER IYEAR, IDAY, IHOUR, IMIN, ICOL
      REAL SEC
CCCC      WRITE(*,*)' @CHASFT:  ASFTIME = ',ASFTIME
      DO 3000 ICOL = 1, 21
      IF(ICOL.NE.5 .AND. ICOL.NE.9 .AND. ICOL.NE.12 .AND. ICOL.NE.15 )
     ?     GO TO 1000
      IF(ASFTIME(ICOL:ICOL) .EQ. ':') GO TO 3000
C---	ERROR.  THERE SHOULD BE A COLON HERE.  
      GO TO 9999
 1000 CONTINUE
      IF(ICOL.NE.18 ) GO TO 2000
      IF(ASFTIME(ICOL:ICOL) .NE. '.') GO TO 9999
      GO TO 3000
 2000 CONTINUE
C---	THIS CHAR SHOULD BE A NUMERIC.  
      ICODE = ICHAR(ASFTIME(ICOL:ICOL))
      IF( ICODE .LT. 48 .OR. ICODE .GT. 57) GO TO 9999
 3000 CONTINUE
C---	NUMERIC / CHARACTER OK.  NOW CHECK FOR VALUES.  
      READ(ASFTIME,11)IYEAR, IDAY,  IHOUR, IMIN,  SEC
   11 FORMAT(         I4,1X, I3,1X, I2,1X, I2,1X, F6.3)
      ICOL = 1
      IF(IYEAR .LT. 1978 .OR. IYEAR .GT. 2010) GO TO 9999
      ICOL = 6
C---	ALLOW FOR A LEAP YEAR AT THIS POINT.  
      IF(IDAY .LT. 1     .OR. IDAY .GT.366) GO TO 9999
      ICOL = 10
      IF(IHOUR .LT. 0    .OR. IHOUR .GT. 23) GO TO 9999
      ICOL = 13
      IF(IMIN  .LT. 0    .OR. IMIN  .GT. 59) GO TO 9999
      ICOL = 16
      IF(SEC .LT. 0.0 .OR. SEC .GT. 59.999 ) GO TO 9999
C---	NOW CHECK FOR NON-LEAP YEAR.
      ICOL = 0
      IF(IYEAR .EQ. 1980 .OR. IYEAR .EQ. 1984 .OR. IYEAR .EQ. 1988 .OR.
     ?   IYEAR .EQ. 1992 .OR. IYEAR .EQ. 1996 .OR. IYEAR .EQ. 2000 .OR. 
     ?   IYEAR .EQ. 2004 .OR. IYEAR .EQ. 2008 ) GO TO 9999
C---	WE HAVE A NON LEAP YEAR.  NOW ALLOW FOR ONLY 365 DAYS.  
      IF(IDAY .GT.365) ICOL = 6
 9999 CONTINUE
      IER = ICOL
CCCC      WRITE(*,*)' $CHASFT:  IER = ',IER 
CCCC      WRITE(*,*)' $CHASFT:  IER = ',IER 
      RETURN
      END


      DOUBLE PRECISION FUNCTION UTC2GHA(IY,ID,IH,IM,XS)
C******************************************************************************
C*  Name:       UTC2GHA
C*  Module Type: DOUBLE PRECISION FUNCTION SUBROUTINE   Language: FORTRAN
C*  $Logfile:   ACS003:[BLD.ACS.LIB.STVEC]UTC2GHA.FOV  $
C*  Purpose:    Computes Greenwich Hour Angle from UTC time.  It converts the
C               input time to ASF format, then calls ASF2GHA, which then reads
C               the ACS GHA Fixes file, identified by the value of the logical
C               name ACS_GHA_FIXES.  ASF2GHA then returns the GHA.
C*  Input Parameters:
C*  Name         Type    Definition
C   IY          INT     YEAR of the input time in UTC
C   ID          INT     DAY NUMBER of the input time in UTC.
C                       Note that this number must be 1-365 or 366.  For a time
C                       of 00:00 on January 21st, for example, the day number
C                       will be 21.  [Note that the time elapsed since the
C                       start of the year will be 20 days, and not 21 days.]
C   IH          INT     HOUR of the input time in UTC
C   IM          INT     MUNITES of the input time in UTC
C   XS          REAL*8  SECONDS of the input time in UTC
C*  Output Parameters:
C   UTC2GHA     REAL*8  Greenwich Hour Angle for input time in degrees from
C                       0-360.
C*  Externals :
C   ACS_GHA_FIXES       A DCL-level logical name.  It is the complete name of
C                       the Greenwich Hour Angle Fixes file, including
C                       directory path and node, if necessary,
C*  Modification History:
C*  $Date$ $Revision$ $Author$
C*  Date                        Revision        Author
C*  22 Mar 1995			1.3		T. Truong
C******************************************************************************/
      INTEGER JD, JD1
      INTEGER IY, ID, IH, IM
      INTEGER IY1, ID1, IH1, IM1
      REAL*8 GHA, GHAR, XANG, XD, XS, XS1, FS
      REAL*8 EROTI			!ROTATION (CYCLES/DAY INERTIAL)
      DATA EROTI/1.0027379093D0/        !AR 1645 CORRECTION

      character*80 sccsid
      data sccsid /'@(#)utc2gha.f	1.3 96/04/23 11:50:53\0'/

C---  TRUNCATE TO THE MILLISECOND
      FS = INT(XS*1000.0D0)/1000.0D

C---  GET REFERENCE GHA ANGLE AND TIME
      CALL GET_REF_GHA(IY1, ID1, IH1, IM1, XS1, GHAR)
C*TNT type *, 'REF2GHA:', IY1, ID1, IH1, IM1, XS1, '  ANGLE:', GHAR

C---  JULIAN DAY FOR 1ST OF JANUARY OF YEAR.
C     REFERENCE:  SPACECRAFT ATTITUDE DETERMINATION AND CONTROL /J.WERTZ.
C---  PAGE 20.  JD1 <=> IY1;  JD <=> IY

      JD1 = 1 - 32075 + 1461*(IY1+4800+(1-14)/12)/4
     ?        + 367*(1-2-(1-14)/12*12)/12
     ?        - 3*((IY1+4900+(1-14)/12)/100)/4
      JD =  1 - 32075 + 1461*(IY+4800+(1-14)/12)/4
     ?        + 367*(1-2-(1-14)/12*12)/12
     ?        - 3*((IY+4900+(1-14)/12)/100)/4
      XD = (ID-ID1) + (IH-IH1)/24.0D0 + (IM-IM1)/24.0D0/60.0D0
     ?        + (FS - XS1)/24.0D0/3600.0D0
     ?        + (JD - JD1)
      XANG = XD * EROTI * 360.0D0 + GHAR
      GHA = MOD(XANG,360.0D0)
      IF(GHA .LT. 0.0D0) GHA = GHA + 360.0D0
      UTC2GHA = GHA
C*TNT type *, 'UTC2GHA:', IY, ID, IH, IM, FS, '  ANGLE:', GHA
      RETURN
      END

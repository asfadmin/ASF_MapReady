C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	stereo.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C STEREO.FOR
C
C PURPOSE
C	CONVERSION ROUTINES FOR LAT/LON TO X/Y
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]STEREO.FOV  $
C
C SUBROUTINES
C	VAXSTEREO
C	VAXINSTEREO
C	
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------

C---------------------------------------------------------------------------
C SUBROUTINE VAXSTEREO
C
C PURPOSE
C	FRONT END FOR SUBROUTINE STEREO. CONVERSION INTERFACE
C	BETWEEN VAX GKS AND STEREO COORDINATE SYSTEMS
C
C INPUT :
C	PROJN		PROJECTION NUMBER
C	XLT		LATITUDE
C	XLN		LONGITUDE
C
C OUTPUT :
C	X		X AXIS COORDINATE
C	Y		Y AXIS COORDINATE
C	HIDDEN		FLAG INDICATING WHETHER THE POINT OUT OF VIEW
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C---------------------------------------------------------------------------
      SUBROUTINE VAXSTEREO(PROJN,OBSLAT,OBSLON,
     1                     XLT,XLN,X,Y,HIDDEN)

      character*100 SccsFileID
     -/'@(#)stereo.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
                        
C INPUT
      INTEGER PROJN

      REAL OBSLAT,OBSLON
      REAL XLT,XLN

C OUTPUT
      REAL X,Y
      LOGICAL HIDDEN

C INTERNAL
      DOUBLE PRECISION SMA,FLAT,ECC,PI
      DOUBLE PRECISION XLAT,XLON,SIGN,RHO,XK
      DOUBLE PRECISION XLAT0,XLON0
      DOUBLE PRECISION CIRCUM
      DOUBLE PRECISION XX,YY
      DOUBLE PRECISION RAD
      DOUBLE PRECISION REFLON

      DATA PI  /3.14159265358979D0/

C INITIALIZE CONSTANTS
      SMA = 6378.273
      FLAT = 1.D0 - SQRT(1.D0 - 0.006693883D0)
      ECC = SQRT(1.D0 - (1.D0 - FLAT) ** 2)

C CONVERT REFERENCE LONGITUDE
      REFLON = DBLE(OBSLON)
      IF (PROJN .EQ. 5) THEN
        REFLON = REFLON - 180.0
        IF (REFLON .GE. -180.0) REFLON = REFLON + 360.0
      END IF

C CONVERT PARAMETERS TO DOUBLE PRECISION
      XLAT = DBLE(XLT)
      XLON = DBLE(XLN)

C CONVERT XLAT AND XLON TO RADIANS
      XLAT = RAD(XLAT)
      XLON = RAD(XLON)

C SET SIGN FOR NORTH/SOUTH PROJECTION
      IF (PROJN .EQ. 5) THEN
        SIGN = 1.0
        XLAT0 = RAD(70.0)
C        XLON0 = -RAD(45.D0)
        XLON0 = RAD(REFLON)
      ELSE
        SIGN = -1.0
        XLAT0 = RAD(-70.0)
C        XLON0 = 0.D0
        XLON0 = RAD(REFLON)
      ENDIF

C     QUARTER OF THE CIRCUMFERENCE
      CIRCUM = 2.0 * PI * SMA / 4.0

      CALL STEREO(SMA,ECC,XLAT,XLON,XLAT0,XLON0,
     *            SIGN,XX,YY,RHO,XK)

C     SCALE THE POINTS DOWN
      XX = XX * 0.75
      YY = YY * 0.75

C     SCALE THEM DOWN TO CIRCUMFERENCE OF GLOBE AND SHIFT TO 0-1.0
      XX = XX / CIRCUM / 2.0 + 0.5
      YY = YY / CIRCUM / 2.0 + 0.5

C     CONVERT BACK TO REALS
      X = REAL(XX)
      Y = REAL(YY)

C DETERMINE IF THE POINT IS HIDDEN OR NOT
      HIDDEN = .FALSE.
      IF ((PROJN .EQ. 5 .AND. XLT .LT. 0.0) .OR.
     1    (PROJN .EQ. 6 .AND. XLT .GT. 0.0)) THEN
        HIDDEN = .TRUE.
      END IF

      RETURN
      END
      
C---------------------------------------------------------------------------
C SUB VAXINSTEREO
C
C PURPOSE
C	FRONT END TO SUBROUTINE INSTEREO. CONVERSION INTERFACE
C	BETWEEN VAX GKS AND STEREO COORDINATE SYSTEMS.
C
C INPUT / OUTPUT : SEE SUBROUTINE INSTEREO
C
C WRITTEN BY CRAIG K. FUJIMOTO
C---------------------------------------------------------------------------
      SUBROUTINE VAXINSTER(XX,YY,SMA,ECC,XK0,XLAT0,XLON0,
     *                       SIGN,XLAT,XLON)

      IMPLICIT NONE

      REAL XX,YY
      REAL XLAT,XLON

      DOUBLE PRECISION SMA,ECC,XK0,SIGN
      DOUBLE PRECISION XLAT0,XLON0
      DOUBLE PRECISION XLT,XLN

      XX = (XX - 0.5) / 2.0
      YY = (YY - 0.5) / 2.0

      CALL INSTER(XX,YY,SMA,ECC,XK0,XLAT0,XLON0,SIGN,XLT,XLN)

      XLAT = REAL(XLT)
      XLON = REAL(XLN)

 9999 CONTINUE
      RETURN
      END




      SUBROUTINE STEREO(SMA,ECC,XLAT,XLON,XLAT0,XLON0,SIGN,
     +                  XX,YY,RHO,XK)
C*****************************************************************************
C*                                                                           *
C* NAME:	STEREO                                                       *
C* PURPOSE:	POLAR STEREOGRAPHIC CONFORMAL PROJECTION OF ELLIPSOIDAL      *
C*		EARTH WITH TRUE SCALE AT VARIABLE LATITUDE.                  *
C*		ITS INVERSE IS SUB INSTER.				     *
C* REFERENCE:	MAP PROJECTIONS, JOHN SNYDER, USGS PROFESSIONAL PAPER 1395,  *
C*		1987, PP 160-163                                             *
C*                                                                           *
C* INPUT PARAMETERS: (TYPE REAL DOUBLE PRECISION)                            *
C*                                                                           *
C*	SMA	ELLIPSOID EQUATORIAL RADIUS (KM)                             *
C*	ECC	ELLIPSOID ECCENTRICITY                                       *
C*	XLAT	GEODETIC LATITUDE OF POINT ON ELLIPSOID TO BE PROJECTED(RAD) *
C*	XLON	LONGITUDE OF POINT ON ELLIPSOID TO BE PROJECTED(RAD)         *
C*	XLAT0	GEODETIC LATITUDE WITH TURE SCALE (RAD)                      *
C*		SET TO RAD(90) FOR NORTH POLE, RAD(-90) FOR SOUTH POLE       *
C*	XLON0	LONGITUDE OFFSET (RAD)                                       *
C*		FOR SIGN=+1, XLON0 IS THE (0,-1) DIRECTION, XLON IS CCW.     *
C*		FOR SIGN=-1, XLON0 IS THE (0, 1) DIRECTION, XLON IS  CW.     *
C*	SIGN	+1 NORTH POLE ASPECT                                         *
C*		-1 SOUTH POLE ASPECT                                         *
C*		SIGN AFFECTS X,Y,XLAT,XLON,XLAT0,XLON0                       *
C*									     *
C* OUTPUT PARAMETERS: (TYPE REAL DOUBLE PRECISION)                           *
C*                                                                           *
C*	XX	STEREOGRAPHIC X-COORDINATE OF POINT                          *
C*	YY	STEREOGRAPHIC Y-COORDINATE OF POINT                          *
C*	RHO	LENGTH OF (X,Y)                                              *
C*	XK	(TRUE SCALE AT POLE)  SCALE FACTOR AT XLAT, XK=XK0 AT XLAT0. *
C*              (TRUE SCALE AT XLAT0) SCALE FACTOR AT POLE ONLY.             *
C*                                                                           *
C* MODIFICATION HISTORY:                                                     *
C*                                                                           *
C*   M. LO	7/22/89		INITIAL RELEASE                              *
C*                                                                           *
C*****************************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA XK0/1.D0/
C          XK0 IS SCALE AT THE POLE FOR CASE I.
C
C I.  TRUE SCALE AT POLE
C
C     CHECK IF THE ABSOLUTE SCALE LATITUDE XLAT0 > 89.9999.
C
      IF(DABS(XLAT0).GT.1.57079) THEN
         COSFI=DCOS(XLAT*SIGN)
         SINFI=DSIN(XLAT*SIGN)
         TT=TFUNC(SINFI,ECC)
         ECC1=1.D0+ECC
         ECC2=1.D0-ECC
         RHO=2.D0*SMA*XK0*TT/DSQRT((ECC1**ECC1)*(ECC2**ECC2))
         XL=(XLON-XLON0)*SIGN
C         WRITE (6,*) XL,XLON,XLON0,' XL,XLON,XLON0'
         XX= RHO*DSIN(XL)*SIGN
         YY=-RHO*DCOS(XL)*SIGN
         XM=COSFI/DSQRT(1.D0-ECC*ECC*SINFI*SINFI)
C     COMPUTE SCALE AT XLAT.  NOTE DIFFERNT FROM XK IN II.
         IF(XLAT.LT.1.57079) THEN
            XK=RHO/SMA/XM
         ELSE
            XK=XK0
         ENDIF
C
C II. TRUE SCALE AT XLAT0
C
      ELSE
         COSFIC=DCOS(XLAT0*SIGN)
         SINFIC=DSIN(XLAT0*SIGN)
         TC=TFUNC(SINFIC,ECC)      
         XMC=COSFIC/DSQRT(1.D0-ECC*ECC*SINFIC*SINFIC)
         COSFI=DCOS(XLAT*SIGN)
         SINFI=DSIN(XLAT*SIGN)
         TT=TFUNC(SINFI,ECC)
         RHO=SMA*XMC*TT/TC
         XL=(XLON-XLON0)*SIGN
         XX= RHO*DSIN(XL)*SIGN
         YY=-RHO*DCOS(XL)*SIGN
C     COMPUTE SCALE AT POLE.  NOTE DIFFERENT FROM XK IN I.
         ECC1=1.D0+ECC
         ECC2=1.D0-ECC
         XK=0.5D0*XMC*DSQRT((ECC1**ECC1)*(ECC2**ECC2))/(SMA*TC)
      ENDIF
      RETURN
      END
C
      FUNCTION TFUNC(SINFI,ECC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C THIS FUNCTION IS PART OF SUB STEREO AND INSTER.
C
      T1=(1.D0-SINFI)/(1.D0+SINFI)
      T2=((1.D0+ECC*SINFI)/(1.D0-ECC*SINFI))**ECC
      TFUNC=DSQRT(T1*T2)
      RETURN
      END
C
C-------------------------------------------------------------------
      SUBROUTINE INSTER(XX,YY,SMA,ECC,XK0,XLAT0,XLON0,SIGN,XLAT,XLON)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA PI  /3.14159265358979D0/,
     +     PIO2/1.57079632679489D0/,
     +     PI2 /6.28318530717958D0/
C
C I.  COMPUTE LONGITUDE
C
      RHO=DSQRT(XX*XX+YY*YY)
      IF(RHO.LT.1.D-10) THEN
C        XX,YY AT THE POLE
         XLON=XLON0
      ELSE
         XLON=XLON0+DATAN2(XX,-YY*SIGN)
      ENDIF
      IF(XLON.LT.0.D0) XLON=XLON+PI2
C
C II. TRUE SCALE AT POLE
C
      IF(DABS(XLAT0).GT.1.57079D0) THEN
         ECC1=1.D0+ECC
         ECC2=1.D0-ECC
         TT=RHO*DSQRT((ECC1**ECC1)*(ECC2**ECC2))/(2.D0*SMA*XK0)
         XLAT=FIFUNC(TT,ECC)*SIGN
C
C III. TRUE SCALE AT XLAT0
C
      ELSE
         COSFIC=DCOS(XLAT0*SIGN)
         SINFIC=DSIN(XLAT0*SIGN)
         TC=TFUNC(SINFIC,ECC)      
         XMC=COSFIC/DSQRT(1.D0-ECC*ECC*SINFIC*SINFIC)
         TT=RHO*TC/(SMA*XMC)
         XLAT=FIFUNC(TT,ECC)*SIGN
      ENDIF
      RETURN
      END
C
      FUNCTION FIFUNC(TT,ECC)
C
C  THIS FUNCTION IS PART OF INSTER.  IT ITERATIVELY COMPUTES FI (XLAT).
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA PI  /3.14159265358979D0/,
     +     PIO2/1.57079632679489D0/,
     +     PI2 /6.28318530717958D0/
     +    ,TOL/1.D-9/
C
      ECC2=0.5D0*ECC
      FI0=(PIO2-2.D0*DATAN(TT))
      DO 100 N=1,1000
         SINFI=DSIN(FI0)
         C1=((1.D0-ECC*SINFI)/(1.D0+ECC*SINFI))**ECC2
         FI=PIO2-2.D0*DATAN(TT*C1)
         DIFF=DABS(FI-FI0)
         IF(DIFF.LT.TOL) GOTO 101
         FI0=FI
  100 CONTINUE
  101 CONTINUE
C
      FIFUNC=FI
      RETURN
      END



C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	GEODET.FOR
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GEODET.FOV  $
C  Purpose:	THIS SUBROUTINE CONVERTS FROM GEOCENTRIC LATITUDE TO GEODETIC
C      		LATITUDE.  IT ALSO DETERMINES THE GEODETIC ALTITUDE.
*  Functions called:	
C          	TOGEOD (RE,X,ALTD,TRIG)    DETERMINES THE GEODETIC ALTITUDE.
C		(INCLUDED IN THIS FILE)
*  Input Parameters:
C  RE     	REAL*8	EARTH'S RADIUS (KM)
C  LATC   	REAL*8	GEOCENTRIC LATITUDE (DEG)
C  LONGC  	REAL*8	GEOCENTRIC LONGITUDE (DEG)
C  ALTC   	REAL*8	GEOCENTRIC ALTITUDE (KM)
*  Name         Type    Definition
*  Output Parameters:
C  LATD   	REAL*8	GEODETIC LATITUDE (DEG)
C  ALTD   	REAL*8	GEODETIC ALTITUDE (KM)
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
C-----------------------------------------------------------------------
C
C      SUBROUTINE GEODET (RE,LATC,LONGC,ALTC,LATD,ALTD)
C
C
C      VARIABLES
C      ---------
C
C        INTERNAL:  DTOR   - DEGREES TO RADIANS CONVERSION FACTOR
C                   PI     - PI = 3.1415926...
C                   RLONGC - GEOCENTRIC LONGITUDE IN RADIANS
C                   RLATC  - GEOCENTRIC LATITUDE IN RADIANS
C                   RADIUS - RADIUS OF THE POSITION IN EARTH'S ROTATING
C                            COORDINATE FRAME (KM)
C                   X()    - X,Y,Z POSITION IN EARTH'S ROTATING 
C                            COORDINATE FRAME
C                   TRIG() - TRIGONOMETRIC FUNCTIONS
C                   RLATD  - GEODETIC LATITUDE IN RADIANS
C                   
C
C        SUBROUTINE CALLS
C        ----------------
C          TOGEOD (RE,X,ALTD,TRIG)    DETERMINES THE GEODETIC ALTITUDE.
C
C
C        SU L. KIM       2-02-89  
C        RICHARD P. LEE  2-09-89
C
C-----------------------------------------------------------------------

       SUBROUTINE GEODET (RE,LATC,LONGC,ALTC,LATD,ALTD)
      character*100 SccsFileID
     -/'@(#)geodet.for	5.1 98/01/08 APS/ASF\0'/


       IMPLICIT NONE

C INPUT:
       DOUBLE PRECISION RE,LATC,LONGC,ALTC

C OUTPUT:
       DOUBLE PRECISION LATD,ALTD

C INTERNAL:
       DOUBLE PRECISION DTOR,PI,RLONGC,RLATC,RADIUS,X(3),TRIG(6),RLATD

       DATA PI / 3.141592653589793D0 /

C-----------------------------------------------------------------------

C CONVERT GEOCENTRIC LATITUDE AND LONGITUDE FROM DEGREES TO RADIANS

       DTOR = PI / 180.0D0
       RLONGC = LONGC * DTOR
       RLATC = LATC * DTOR

C DETERMINE POSITION IN EARTH'S ROTATING X,Y,Z COORDINATES

       RADIUS = RE + ALTC
       X(3) = RADIUS * DSIN(RLATC)
       X(1) = RADIUS * DCOS(RLATC) * DCOS(RLONGC)
       X(2) = RADIUS * DCOS(RLATC) * DSIN(RLONGC)

C DETERMINE THE GEODETIC ALTITUDE

       CALL TOGEOD (RE,X,ALTD,TRIG)

C DETERMINE THE GEODETIC LATITUDE IN RADIANS

       RLATD = DASIN(TRIG(3))

C CONVERT GEODETIC LATITUDE FROM RADIANS TO DEGREES

       LATD = RLATD / DTOR

C END OF SUBROUTINE

  999  CONTINUE
       RETURN
       END
********************************************************************
*  Name:	TOGEOD.FOR
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]GEODET.FOV  $
C  Purpose:	THIS SUBROUTINE IS A MODIFICATION OF THE SUBROUTINE EFGGHD, 
C      		INHERITED FROM HERB REYNOLDS OF ALASKA SAR FACITLIY IN JANUARY,
C      		1989.  IT HAS BEEN MODIFIED TO WORK IN KM INSTEAD OF FT.
C
C      		THIS SUBROUTINE DETERMINED THE GEODETIC ALTITUDE (KM) AND 
C      		TRIGONOMETRIC FUNCTIONS REQUIRED TO DETERMINE THE GEODETIC 
C      		LATITUDE.
C		An iterative method is used.  Ref. IOM 312/89.5-3016
C							13 Feb 1989.
*  Input Parameters:
C  RE     	REAL*8	EARTH'S RADIUS (KM)
C  X()    	REAL*8	POSITION IN EARTH'S ROTATING COORDINATES
*  Name         Type    Definition
*  Output Parameters:
C  HITE   	REAL*8 	GEODETIC ALTITUDE (KM)
C  TRGF() 	REAL*8	TRIGONOMETRIC FUNCTIONS
C			TRGF(1) = SLMDA  sin(LMDA) (LMDA = longitude)
C			TRGF(2) = CLMDA  cos(LMDA) (LMDA = longitude)
C			TRGF(3) = SPHI	 sin(PHI)  (PHI  = geodetic latitude)
C			TRGF(4) = CPHI   cos(PHI)  (PHI  = geodetic latitude)
C			TRGF(5) = TPHI   tan(PHI)  (PHI  = geodetic latitude)
C			TRGF(6) = GCNTRC tan(geocentric latitude)
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
C
C      SUBROUTINE TOGEOD (RE,X,HITE,TRGF)
C
C
C      VARIABLES
C      ---------
C        INTERNAL:  ESQR,ELLIP,ITER,DLST,YD,XD2,TPHI,SPHI,CPHI,ZD,
C                   GCNTRC,XD,SLMDA,CLMDA,C2PHI,S2PHI,RDCL,DLTA,EPSLN,
C                   MXITR,DNM
C
C
C        SU L. KIM       2-02-89
C        RICHARD P .LEE  2-10-89
C
C-----------------------------------------------------------------------

       SUBROUTINE TOGEOD (RE,X,HITE,TRGF)

       IMPLICIT NONE

C INPUT:
       DOUBLE PRECISION RE,X(3)

C OUTPUT:
       DOUBLE PRECISION HITE,TRGF(6)

C INTERNAL:
       DOUBLE PRECISION ESQR,ELLIP,ITER,DLST,YD,XD2,TPHI,SPHI,CPHI,ZD,
     1                  GCNTRC,XD,SLMDA,CLMDA,C2PHI,S2PHI,RDCL,DLTA,
     2                  EPSLN,MXITR,DNM

       DATA ELLIP / 0.8182D-01 /
       DATA EPSLN / 0.001D0 /
       DATA MXITR / 5 /

C-----------------------------------------------------------------------

       ESQR = ELLIP * ELLIP

       ITER = 0
       DLST = 1.0D+37

       YD = X(3)
       XD2 = X(1) * X(1) + X(2) * X(2)
       IF (XD2 .GT. 1.0D-20) GO TO 4

       TPHI = 1.0D10
       SPHI = 1.0D0
       CPHI = 0.0
       ZD = YD

       IF (ZD .GT. 0.0) GO TO 1
       SPHI = -SPHI
       TPHI = -TPHI
       ZD = -ZD

    1  CONTINUE
       GCNTRC = TPHI
       HITE = ZD - RE * DSQRT(1.0D0 - ESQR)

C IF LONGITUDE IS UNDEFINED, JUST LEAVE THE PREVIOUS VALUES OF SINE
C AND COSINE

       GO TO 25

    4  CONTINUE
       XD = DSQRT(XD2)
       SLMDA = X(2) / XD
       CLMDA = X(1) / XD

       GCNTRC = YD / XD

       TPHI = GCNTRC
  
    5  CONTINUE
       C2PHI = 1.0D0 / (1.0D0 + TPHI * TPHI)
       S2PHI = 1.0D0 - C2PHI

       CPHI = DSQRT(C2PHI)
       SPHI = CPHI * TPHI

       RDCL = DSQRT(1.0D0 - ESQR * S2PHI)
       HITE = XD * CPHI + YD * SPHI - RE * RDCL

       DLTA = XD * SPHI - (YD + RE * ESQR * SPHI / RDCL) * CPHI
       IF (DLTA .LT. 0) DLTA = -DLTA

       ITER = ITER + 1

       IF (DLTA .LT. EPSLN) GO TO 25
       IF (ITER .GT. MXITR) GO TO 25
       IF (DLTA .GT. DLST) GO TO 25
       DLST = 0.99 * DLTA

       DNM = 1.0D0 + HITE * RDCL / RE
       DNM = 1.0D0 - ESQR / DNM

       TPHI = GCNTRC / DNM
       GO TO 5

   25  CONTINUE
       TRGF(1) = SLMDA
       TRGF(2) = CLMDA
       TRGF(3) = SPHI
       TRGF(4) = CPHI
       TRGF(5) = TPHI
       TRGF(6) = GCNTRC

       RETURN
       END

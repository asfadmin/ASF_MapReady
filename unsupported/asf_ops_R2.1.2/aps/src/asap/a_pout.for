C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	a_pout.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
C--- port not necessary
c---port      OPTIONS /G_FLOATING

      SUBROUTINE POUT(T,X)  ! EASAP version

      character*100 SccsFileID
     -/'@(#)a_pout.for	5.1 98/01/08 APS/ASF\0'/

********************************************************************
*  Name:  POUT
*  Module Type: SUBROUTINE	Language: FORTRAN 77
*  $Logfile:   ACS003:[BLD.MPS.EPHM.SRC]A_POUT.FOV  $
*  Purpose: OUTPUT ROUTINE FOR ASAP; COMPILE IN G_FLOAT
*           called by ASAP in file EASAP.FOR
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  T		REAL	TIME
*  X		REAL	STATE VECTOR
*  Output Parameters:
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
c  modified for ASF
C  VERSION OF 4/13/87
C  PURPOSE
C    PRINTOUT ROUTINE FOR TRAJECTORY PROGRAM   
C  INPUT 
C    T      = CURRENT TIME (SEC)
C    X      = 6-D CARTESIAN COORD X,Y,Z,XD,YD,ZD (KM,KM/SEC)
C    SEE ROUTINES ASAP AND DER FOR EXPLANATION OF COMMON BLOCK
C    VARIABLES
C  OUTPUT
C    NONE  
C  CALL SUBROUTINES  
C    EQNOX 
C  REFERENCES
C    JPL EM 312/87-153, 20 APRIL 1987
C  ANALYSIS
C    J. H. KWOK - JPL  
C  PROGRAMMER
C    J. H. KWOK - JPL  
C  PROGRAM MODIFICATIONS 
C    outputs vectors in r,v format followed by time in julian days, revnumber,
c    longitude of ascending node, subsat lat/lon, and Greenwich hour angle.  
c    Output is directed to FOR080.  Also completes the ephemeris relation.
C  COMMENTS  
C    NONE
C   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      DIMENSION X(6)
      DIMENSION Y(18)

c *****
c
      character nodeflag
      dimension XX(12), yy(12), yyold(12)
      save yyold, rev
      common /A_Revs/ ib_rev, ie_rev, TSTART
c *****
      COMMON/OPTION/L,M,IRES,ISUN,IMOON,IEPHEM,IDRAG,IDENS,ISRP,IORB
     1  ,IPRINT,INODE,IPLOT
      COMMON/TIME/TI,TF,TR  
      COMMON/PLTCON/GE,RE,RATE,PM,AJ2,ELLIP,RATM
      COMMON/COUNT/NODE,NREV
      COMMON/CASE/ICASE
      DATA HTS,DTH,RTD,PI,TPI,ONE/3.6D3,24.D0,57.29577951308232D0,
     1  3.141592653589793D0,6.283185307179586D0,1.D0/
      DATA SMALL/1.D-3/

      common /ascii_opts/I88
c---	I88 is a flag for the ASCII file. I88=0 means no ascii output.  
c---	the value is set in asap_main.for

C        INITIALIZE REV NUMBER FIRST TIME THROUGH.  
C---	THIS IS SUPPOSED TO HAPPEN DOWN BELOW, BUT A ZERO WAS TURNING UP IN 
C---	THE OUTPUT AND I DIDN'T HAVE TIME TO FIGURE OUT WHY AT THIS TIME.   
      IF(rev .eq. 0) rev = ib_rev
C
      THOUR=(T-TI)/HTS
      DDAY=THOUR/DTH
      IDAY=IDINT(DDAY)
      HOUR=THOUR-IDAY*DTH
      CALL EQNOX(X,GE,Y)
      HA=DMOD(PM+(T-TR)*RATE,TPI)
      ENODE=DMOD(Y(9)-HA+TPI,TPI)
      ALONG=DATAN2(X(2),X(1))-HA
      ALONG=DMOD(ALONG+TPI+TPI,TPI)
      ALAT=DASIN(X(3)/Y(16))
      ARGL=DMOD(Y(14)-Y(9)+TPI,TPI)
      EALT=Y(16)-DSQRT(RE**2*(ONE-ELLIP**2)/(ONE-(ELLIP*DCOS(ALAT))**2))
      PALT=Y(1)*(ONE-Y(7))-RE
      ALT=Y(16)-RE
C
C *** DOUBLE CHECK APSIS CROSSING, THIS IS NEEDED IN CASE SOME
C *** PERTURBATION ON NEARLY CIRCULAR ORBIT IS CHANGING MEAN ANOMALY
C *** BACKWARDS
C
      IF (IPRINT.EQ.1) THEN
      IF (ICASE.EQ.3.OR.ICASE.EQ.4) THEN
        IF (DABS(Y(11)).LT.SMALL.OR.DABS(Y(11)-TPI).LT.SMALL) ICASE=3
        IF (DABS(Y(11)-PI).LT.SMALL) ICASE=4
      ENDIF
      ENDIF
      IF (IPRINT.EQ.1.OR.INODE.EQ.1) THEN
C
C *** CHANGE ICASE IF FIRST PRINT OUT IS A NODAL OR APSIS CROSSING
C
      IF (NODE.EQ.0.AND.(DABS(ARGL).LT.SMALL.OR.DABS(ARGL-TPI).LT.SMALL)
     1) ICASE=1
      IF (NODE.EQ.0.AND.DABS(ARGL-PI).LT.SMALL) ICASE=2
      IF (NREV.EQ.0.AND.(DABS(Y(11)).LT.SMALL.OR.DABS(Y(11)-TPI).LT.SMAL
     1L)) ICASE=3
      IF (NREV.EQ.0.AND.DABS(Y(11)-PI).LT.SMALL) ICASE=4
      ENDIF
      IF (IPRINT.EQ.1.OR.INODE.EQ.1) THEN
      IF (ICASE.EQ.0) GO TO 105
      GO TO (101,102,103,104) ICASE
  101 NODE=NODE+1
      GO TO 105
  102 GO TO 105
  103 NREV=NREV+1
      GO TO 105
  104 CONTINUE
  105 CONTINUE
      ENDIF
C
C *** START UNIT CONVERSION
C
      Y(6)=Y(6)*RTD
C---		Y(9), Longitude of ascending node, is converted here.
      DO 10 I=8,15
   10 Y(I)=Y(I)*RTD
      Y(18)=Y(18)/HTS
      ARGL=ARGL*RTD
      HA=HA*RTD
      ENODE=ENODE*RTD
      ALONG=ALONG*RTD
      ALAT=ALAT*RTD
c *********
c
      DO 15, j=1, 6
         yy(j) = x(j) 
 15   CONTINUE
      yy(7)  = T/86400.0d0
      yy(8)  = rev
C---	FROM EQNOX, Y(9) IS LONG ASC NODE IN RADIANS.  CONVERTED TO DEG.ABOVE.
      yy(9)  = Y(9)
      yy(10) = alat
      yy(11) = along
      yy(12) = ha
c
      trd = tr/86400.0d0 
      tfd = tf/86400.0d0 
      if (DDAY .LT. 2.777776D-5) then  ! this is the first call to pout
c        Initialize Rev Number
         rev = ib_rev
      else if ((tfd - yy(7)) .LT. 2.777776D-5) then
         ie_rev = rev + 0.05
      else
C---	 CHECK FOR CHANGE OF ACTION FROM ASCENDING TO DESCENDING OR VICE VERSA.
         IF(  (yy(6) .GT. 0.0D0 .AND. yyold(6) .LT. 0.0d0) 
     ?   .OR. (yy(6) .LT. 0.0D0 .AND. yyold(6) .GT. 0.0d0) ) THEN
            IF(yy(7) .GE. TSTART)CALL VSTAT(yyold,yy,XX)
C---	WRITE A RECORD AT THE 'STATIONARY' POINT:
            IF(yy(7) .GE. TSTART)write(80)     '_', (XX(I),I=1,12)
            IF(yy(7) .GE. TSTART .and. I88 .ne. 0)
     ?                           write(88,*)   '_', (XX(I),I=1,12)
	 ENDIF 
c        Check for node crossing
         if (sign(1.0D0, yy(3)) .NE. sign(1.0D0, yyold(3))) then
            a = yyold(3) / (yyold(3) - yy(3))   ! Calc interpolation weights
            b = 1.0d0 - a
            IF (yyold(3) .LT. 0.0d0) THEN 
               rev = rev + 1.0d0
               nodeflag = 'N'
            ELSE
               nodeflag = 'D'
            ENDIF
            yy(8) = rev
            yyold(8) = rev
            IF(yy(7) .GE. TSTART)CALL VNODE(yyold,yy,XX)
c---------  write(80) nodeflag, ((a*yy(i) + b*yyold(i)), i=1, 12)
c---------  write(88,*) nodeflag, ((a*yy(i) + b*yyold(i)), i=1, 12)
c---		WAIT TO START PRINTING.  TSTART IS THE USER'S START TIME
            IF(yy(7) .GE. TSTART)write(80)   nodeflag, (XX(I),I=1,12)
            IF(yy(7) .GE. TSTART .and. I88 .ne. 0)
     ?                           write(88,*) nodeflag, (XX(I),I=1,12)
C---		XX WAS TEMP STORAGE HERE.
 1000 continue
         endif
      endif
c
      nodeflag = '_'
      IF (yyold(1) .eq. 0.0d0 .AND. yyold(2) .eq. 0.0d0 .AND. 
     ?    yyold(3) .eq. 0.0d0 ) THEN
      WRITE(*,*)' A_POUT:  FIRST CALL BECAUSE yyold was = 0.'
         IF(yy(3) .eq. 0.0d0 .AND. yy(6) .GT. 0.0D0 )nodeflag = 'N'
         IF(yy(3) .eq. 0.0d0 .AND. yy(6) .GT. 0.0D0 )
     ?WRITE(*,*)' A_POUT:  ASCENDING NODE FOUND ON FIRST VECTOR.'
         IF(yy(3) .eq. 0.0d0 .AND. yy(6) .LT. 0.0D0 )nodeflag = 'D'
         IF(yy(3) .eq. 0.0d0 .AND. yy(6) .LT. 0.0D0 )
     ?WRITE(*,*)' A_POUT:  DESCENDING NODE FOUND ON FIRST VECTOR.'
      ENDIF
      do 17, i=1, 12
         yyold(i) = yy(i)
 17   continue
c---		WAIT TO START PRINTING.  TSTART IS THE USER'S START TIME
      IF(yy(7) .GE. TSTART) write(80) nodeflag, (yy(i), i=1, 12)
      IF(yy(7) .GE. TSTART .and. I88 .ne. 0)
     ?                     write(88,*)nodeflag, (yy(i), i=1, 12)
c
c *****
c *************************************************************************
      RETURN
      END

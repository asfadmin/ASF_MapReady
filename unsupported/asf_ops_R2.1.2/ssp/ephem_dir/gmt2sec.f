c SccsId = @(#)gmt2sec.f	2.41 3/24/98
c**********************************************************************
c*    Module Name:	gmt2sec.f
c	
c---------------------------------------------------------------------
c
c	9/5/94 A.Chu Initial code
c---------------------------------------------------------------------
	subroutine gmt2sec(gmt_str,time_sec)
ccc	subroutine gmt2sec(gmt_str,date_time,time_sec)

	implicit none

c	----------------
c	Abstract:
c               1).Converts the GMT string to real*8 form 
c	           YYYYMMDD.d0 & HHMMSS.mmmd0 
c	        2).Converts GMT string to second.
c	----------------
	
c	----------------
c	INPUT PARAMETERS PASSING
c	----------------
	character*24  gmt_str	 !GMT string
	integer	      gmt2double !C function call 	

c	----------------
c	OUTPUT PARAMETERS PASSING
c	----------------
	real*8   time_sec		!GMT in terms of sec

c	----------------
c	LOCAL VARIABLES
c	----------------
	real*8   date_time(2)		!YYYYMMDD & HHMMSS.ssssss

	if (gmt2double(gmt_str, date_time) .EQ. -1) then
            write (*,*)'GMT convert ERROR gmt2double.c'
            write(*,*) 'gmt_str:',gmt_str
cc            call exit(-1)
        endif

        call julian_ac(date_time,time_sec)

        time_sec=time_sec*86400.d0

c       write(6,*)'date_time',date_time(1),date_time(2),time_sec

        return 
	end
c***********************************************************
c	SUBROUTINES
c***********************************************************
      SUBROUTINE JULIAN_AC(TIN,TOUT)
C/*   SUBROUTINE JULIAN(TIN,TOUT) ---------------
C
C  VERSION OF 4/1/85
C  PURPOSE
C    COMPUTES JULIAN DATE WHEN GIVEN CALENDER DATE AND TIME
C  INPUT
C    TIN(1) = CALENDER DATE, YYYYMMDD.
C       (2) = CALENDER TIME, HHMMSS.SSSS
C  OUTPUT
C    TOUT   = JULIAN DATE (DAYS)
C  CALL SUBROUTINES
C    NONE
C  REFERENCES
C    JPL EM 312/87-153, 20 APRIL 1987
C  ANALYSIS
C    J. H. KWOK
C  PROGRAMMER
C    J. H. KWOK
C  PROGRAM MODIFICATIONS
C    NONE
C  COMMENTS
C    NONE
C*/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TIN(2)
      DATA D0,D1,D2,D3,D4,D5/2415020.5D0,1.D-4,1.D-2,24.D0,1.44D3
     1,8.64D4/
C
C  THIS IS THE JULIAN DATE FOR 19000101
C
      TOUT=D0
C
      IY=INT(TIN(1)*D1-1.9D3)
      IM=INT(TIN(1)*D2-1.9D5)-IY*100
      ID=INT(TIN(1)-1.9D7)-IY*10000-IM*100
      IHOUR=INT(TIN(2)*D1)
      IMIN=INT(TIN(2)*D2)-IHOUR*100
      SEC=TIN(2)-IHOUR*10000-IMIN*100
      JD=IY*365+(IY-1)/4
      IM1=IM-1
      IF (IM1.EQ.0) GO TO 12
      GO TO (1,2,3,4,5,6,7,8,9,10,11),IM1
    1 JD=JD+31
      GO TO 12
    2 JD=JD+59
      GO TO 12
    3 JD=JD+90
      GO TO 12
    4 JD=JD+120
      GO TO 12
    5 JD=JD+151
      GO TO 12
    6 JD=JD+181
      GO TO 12
    7 JD=JD+212
      GO TO 12
    8 JD=JD+243
      GO TO 12
    9 JD=JD+273
      GO TO 12
   10 JD=JD+304
      GO TO 12
   11 JD=JD+334
   12 CONTINUE
      IF (IY/4*4-IY.EQ.0.AND.IM.GT.2) JD=JD+1
      JD=JD+ID-1
      TOUT=TOUT+JD+IHOUR/D3+IMIN/D4+SEC/D5
      RETURN
      END
c*************************************************************


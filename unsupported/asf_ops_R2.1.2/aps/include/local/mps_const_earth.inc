C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mps_const_earth.inc
C--
C--  Description:	
C--	
C--  Notes:
C--
C--   character*100 SccsFileID
C--  -/'@(#)mps_const_earth.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-- ==========================================================================
C-----------------------------------------------------------------
C MPS_CONST_EARTH
C EARTH CONSTANTS FORTRAN INCLUDE FILE
C $LOGFILE$
C $DATE$ $REVISION$ $AUTHOR$
C-----------------------------------------------------------------
C---	EARTH CONSTANTS:  RADIUS(KM)- AS HARD CODED IN ORBG CODE.
      REAL*8     ERAD 
      DATA ERAD/6378.144/
C---	G*MASS(KM**3/SEC**2)
      REAL*8     GME
      DATA GME/398600.45/
C---	YEAR(DAYS)
      REAL*8              EYEAR
      DATA EYEAR/365.242191/
C---	     FLATNESS:  1/F = a/(a-b) OF EARTH REF. ELLIPSOID.  
      REAL*8 FLAT 
      DATA FLAT/298.257/
C---	     ROTATION (CYCLES/DAY INERTIAL)
      REAL*8 EROTI
      DATA EROTI/1.0027379093/
C---	THE ABOVE CONSTANTS WERE OBTAINED FROM THE VECTOR LIBRARY JUN-89
C---	VIA SUBROUTINE PSCONS, EXCEPT FOR EYEAR, THE TROPICAL YEAR, FROM
C---	THE ASTRONOMICAL ALMANAC 1989.  
C---	---> MPS_LIB:MPS_CONST_EARTH.FOR  

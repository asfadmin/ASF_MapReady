C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mps_const_math.inc
C--
C--  Description:	
C--	
C--  Notes:
C--
C--   character*100 SccsFileID
C--  -/'@(#)mps_const_math.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-- ==========================================================================
C-----------------------------------------------------------------
C MPS_CONST_MATH
C MATH CONSTANTS FORTRAN INCLUDE FILE
C $LOGFILE$
C $DATE$ $REVISION$ $AUTHOR$
C-----------------------------------------------------------------
      REAL*16 PI, DG2RAD
C---	MATH CONSTANTS PI AND DEGREES PER RADIANS.  
      DATA PI/3.14159265358979323846264338327950/
      DATA DG2RAD/57.2957795130823208767981548141052/
C---	THE ABOVE CONSTANTS WERE OBTAINED FROM FORTRAN INTRINSIC FUNCTION ASIN.
C---	THEY ARE REAL*16 FOR ACCURACY BUT IF YOU PASS THEM TO A SUBROUTINE,
C---	YOU MAY HAVE TO MOVE THEM TO A REAL*8 WORD AND PASS THAT IF THE 
C---	SUBROUTINE EXPECTS THE PARAMETER TO BE REAL*8.
C---	---> MPS_LIB:MPS_CONSTS_MATH.FOR  

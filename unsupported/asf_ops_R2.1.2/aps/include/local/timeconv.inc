C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	timeconv.inc
C--
C--  Description:	
C--	
C--  Notes:
C--
C--   character*100 SccsFileID
C--  -/'@(#)timeconv.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-- ==========================================================================

C---  This file contains declarations to support the time conversion 
C---  routine library.  
C---
C---  To include this in your .for source, use this inside the routine:  
C---  INCLUDE 'APS_HOME:include/timeconv.inc'
C
      EXTERNAL tc_asf2et !$pragma C(tc_asf2et)
      INTEGER tc_asf2et

      EXTERNAL tc_et2asf !$pragma C(tc_et2asf)
      INTEGER tc_et2asf

      EXTERNAL tc_systime2asf !$pragma C(tc_systime2asf)
      INTEGER tc_systime2asf

      EXTERNAL tc_systime2yr_date !$pragma C(tc_systime2yr_date)
      INTEGER tc_systime2yr_date

      EXTERNAL tc_validate_asf_datetime 
     ?         !$pragma C(tc_validate_asf_datetime)
      INTEGER tc_validate_asf_datetime

C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	begseg.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE BEGSEG
C
C PURPOSE
C	INIT INDEPENDENT SEG STORAGE COUNTER
C	INIT SEG NAME AND STATUS ARRAYS
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]BEGSEG.FOV  $
C
C INPUT/OUTPUT
C	NSEG		I	OVERLAY COUNTER
C	CRSEGN		I	ARRAY OF OVERLAY STATUS VALUES
C	CRSEGT		C	ARRAY OF OVERLAY NAMES
C
C INTERNAL
C	I		I	LOOP COUNTER
C	BLANK		C	ASCII BLANK
C
C QRIGINALLY WRITTEN BY RICHARD P. LEE
C MODIFIED FOR ASF BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 7/11/94  Nadia Adhami -port to UNIX/C 
C
C-----------------------------------------------------------------
       SUBROUTINE BEGSEG (NSEG,CRSEGN,CRSEGT)

      character*100 SccsFileID
     -/'@(#)begseg.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

C INPUT:   
       CHARACTER*(*) CRSEGT(*)
       INTEGER NSEG,CRSEGN(*)

C INTERNAL:
       INTEGER I
       CHARACTER*1    BLANK

       DATA BLANK     / ' ' /

C SET COUNTERS TO ZERO
       NSEG = 0

C ZERO AND BLANK SEGMENT ARRAYS
       DO 100 I = 1,200
         CRSEGN(I) = 0
         CRSEGT(I) = BLANK
  100  CONTINUE

 9999  CONTINUE
       RETURN
       END

C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	trim.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C
C SUBROUTINE TRIM
C
C PURPOSE
C	TRIM THE SPACES FROM THE FRONT OF AN ASCII STRING
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]TRIM.FOV  $

C INPUT
C	STR	STRING TO TRIM
C	LEN1	ORIGINAL LENGTH
C
C OUTPUT
C	LEN2	LENGTH AFTER TRIM
C
C INTERNAL
C	I,J	COUNTERS
C
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------
       SUBROUTINE TRIM(STR,LEN1,LEN2)

      character*100 SccsFileID
     -/'@(#)trim.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE

       INTEGER LEN1,LEN2,I,J

       CHARACTER*(*) STR
       CHARACTER*100 TEMP

       DO 100 I = 1,LEN1
            
         IF (STR(I:I) .EQ. ' ') GOTO 100

         TEMP = STR(I:LEN1)
         J = LEN1 - I + 1
         GO TO 110

  100  CONTINUE

  110  CONTINUE

       STR = TEMP(1:J)
       LEN2 = J                                 

 9999  CONTINUE
       RETURN
       END

C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	dismsg.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C SUBROUTINE DISMSG
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]DISMSG.FOV  $
C
C PURPOSE
C	DISPLAY A MESSAGE TO THE USER ON THE DEFAULT OUTPUT DEVICE
C
C INPUT
C	MSG	MESSAGE TO BE DISPLAYED
C
C OUTPUT
C
C INTERNAL
C
C FUNCTION CALLS
C	SLEN
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------
      SUBROUTINE DISMSG(MSG)

      character*100 SccsFileID
     -/'@(#)dismsg.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE 
      CHARACTER*(*) MSG
      INTEGER SLEN

      WRITE(6,100) MSG
  100 FORMAT(/,' ',A<SLEN(MSG)>)

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------
C SUBROUTINE DISMSGW
C
C PURPOSE
C	DISPLAY A MESSAGE TO THE USER ON THE DEFAULT OUTPUT DEVICE
C	PLACE CURSOR AT END OF MESSAGE - WAIT
C	THIS ROUTINE IS THE SAME AS DISMSG ONLY WITH THE WAIT.
C
C INPUT
C	MSG	MESSAGE TO BE DISPLAYED
C
C OUTPUT
C
C INTERNAL
C
C FUNCTION CALLS
C	SLEN
C
C WRITTEN BY CRAIG K. FUJIMOTO
C
C-----------------------------------------------------------------
      SUBROUTINE DISMSGW(MSG)

      IMPLICIT NONE 
      CHARACTER*(*) MSG
      INTEGER SLEN

      WRITE(6,100) MSG
  100 FORMAT(/,' ',A<SLEN(MSG)>,$)

 9999 CONTINUE
      RETURN
      END

C-----------------------------------------------------------------
C FUNCTION SLEN
C
C PURPOSE
C	RETURN THE LENGTH OF THE STRING
C INPUT
C	ST	CHARACTER STRING
C OUTPUT
C	SLEN	LENGTH OF STRING                              
C INTERNAL
C	I,J	COUNTERS
C
C-----------------------------------------------------------------
      INTEGER FUNCTION SLEN(ST)

      IMPLICIT NONE
C INPUT
      CHARACTER*(*) ST
C INTERNAL
      INTEGER I,J

C ARRAY SIZE
      J = LEN(ST)

C LOOP THROUGH STRING
      DO 1000 I = 1,J-1

        IF (I .EQ. J-1) THEN
          IF (ST(J:J).EQ.' ') THEN
            SLEN = J-1
            GO TO 9999
          ELSE
            SLEN = J
            GO TO 9999
          END IF
        ELSE IF (ST(I:I).EQ.' ' .AND. ST(I+1:I+1).EQ.' ') THEN
          SLEN = I-1
          GO TO 9999
        END IF

 1000 CONTINUE

 9999 CONTINUE
      RETURN
      END

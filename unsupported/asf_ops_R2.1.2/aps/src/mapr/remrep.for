C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	remrep.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C REMREP.FOR
C
C PURPOSE
C	ROUTINES FOR REMOVING,REPLACING, AND DELETING OVERLAYS
C
C $Logfile:   ACS003:[BLD.MPS.MAPR.SRC]REMREP.FOV  $
C
C SUBROUTINES
C	RMVSEG
C	RPLSEG
C	DELSEG
C	RMVALL
C
C WRITTEN BY CRAIG K. FUJIMOTO - SEP 89
C
C MODIFICATIONS
C $Date$ $Revision$ $Author$
C 10/4/94  Nadia Adhami  Display segment index as a 4-digit number
C-----------------------------------------------------------------

C-----------------------------------------------------------------------
C SUBROUTINE RMVSEG
C
C PURPOSE
C	ALLOWS USER TO REMOVE OVERLAY FROM DISPLAY
C	MARKS OVERLAY AS NODISPLAY (0) IN THE SEG STATUS ARRAY (CRSEGN)
C
C INPUT
C	CRSEGN()	SEG STATUS ARRAY
C	CRSEGT()	SEG NAME ARRAY
C	NSEG		SEG COUNT
C	DFLAG		FLAG TO INDICATE CHANGE IN DISPLAY
C
C INTERNAL
C	IOS		INPUT ERROR FLAG
C	RMV		NUMBER OF SEG TOBE REMOVED FROM DISPLAY
C	I		LOOP INDEX
C
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------------
       SUBROUTINE RMVSEG (NSEG,CRSEGN,CRSEGT,DFLAG)

      character*100 SccsFileID
     -/'@(#)remrep.for	5.1 98/01/08 APS/ASF\0'/

       IMPLICIT NONE
C INPUT:
       CHARACTER*(*) CRSEGT(200)
       INTEGER NSEG,CRSEGN(*),DFLAG
C INTERNAL:
       INTEGER IOS,RMV,I
       INTEGER LINECT
C-----------------------------------------------------------------------

C ASK WHICH OVERLAY TO REMOVE
    
 1000 CONTINUE
      LINECT = 0

      WRITE (6,100)
  100 FORMAT(//,' REMOVE OVERLAY')

      DO 2000 I = 1,NSEG

        IF (CRSEGN(I) .EQ. 1) THEN

          WRITE (6,200) I,CRSEGT(I)
  200     FORMAT (' ',I4,')  ',A20)
          LINECT = LINECT + 1

          IF (LINECT .GE. 20) THEN
            LINECT = 0
            WRITE (6,210)
  210       FORMAT (/,' Press <RETURN> to Continue ',$)
            READ (5,*)
            WRITE (6,220)
  220       FORMAT (/)
          END IF

        END IF

 2000 CONTINUE

      WRITE (6,300)
  300 FORMAT ('  0)  QUIT')

      WRITE (6,310)
  310 FORMAT (/,' Selection : ',$)

      READ (5,*,IOSTAT = IOS) RMV

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (RMV .LT. 0 .OR. RMV .GT. NSEG) THEN
        CALL DISMSG('Error: Invalid overlay number.')
        GO TO 1000
      ELSE IF (RMV .EQ. 0) THEN
        GO TO 9999
      END IF

C SET FLAG TO NO DISPLAY

      IF (CRSEGN(RMV) .NE. 1) THEN
        CALL DISMSG('Error : Overlay already removed or deleted.')
        GO TO 1000
      END IF

      CRSEGN(RMV) = 0

      DFLAG = 1

      GO TO 1000

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------------
C SUBROUTINE RPLSEG
C
C PURPOSE
C	ALLOW USER TO PLACE REMOVED OVERLAY BACK ON SCREEN
C	SETS THE SEG STATUS TO VISIBLE
C      
C INPUT
C	CRSEGN()	SEG STATUS ARRAY
C	CRSEGT()	SEG NAME ARRAY
C	NSEG		SEG COUNT
C	DFLAG		FLAG TO INDICATE CHANGE IN DISPLAY
C INTERNAL
C	IOS		INPUT ERROR FLAG
C	I		LOOP COUNTER
C	RPL		SEG NUMBER TO REPLACE
C
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------------
       SUBROUTINE RPLSEG (NSEG,CRSEGN,CRSEGT,DFLAG)

       IMPLICIT NONE
C INPUT:
       CHARACTER*(*) CRSEGT(*)
       INTEGER CRSEGN(*),NSEG,DFLAG
C INTERNAL:
       INTEGER IOS,RPL,I
       INTEGER LINECT
C-----------------------------------------------------------------------
    
C ASK WHICH OVERLAY TO REPLACE

 1000 CONTINUE
	  LINECT = 0

      WRITE (6,100)
  100 FORMAT (//,' REPLACE OVERLAY')

      DO 2000 I = 1,NSEG

        IF (CRSEGN(I) .EQ. 0) THEN

           WRITE (6,200) I,CRSEGT(I)
  200      FORMAT (' ',I4,')  ',A20)
	       LINECT = LINECT + 1

           IF (LINECT .GE. 20) THEN
	         LINECT = 0
             WRITE (6,210)
  210        FORMAT (/,' Press <RETURN> to Continue ',$)
             READ (5,*)
             WRITE (6,220)
  220        FORMAT (/)
           END IF

        END IF

 2000 CONTINUE

      WRITE (6,300)
  300 FORMAT ('  0)  QUIT')

      WRITE (6,310)
  310 FORMAT (/,' Selection : ',$)

      READ (5,*,IOSTAT = IOS) RPL

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (RPL .LT. 0 .OR. RPL .GT. NSEG) THEN
        CALL DISMSG('Error: Invalid overlay number.')
        GO TO 1000
      ELSE IF (RPL .EQ. 0) THEN
        GO TO 9999
      END IF

C ADD OVERLAY TO LIST OF OVERLAYS CURRENTLY IN WORKSTATION

      IF (CRSEGN(RPL) .NE. 0) THEN
        CALL DISMSG('Error : Overlay already replaced or deleted.')
        GO TO 1000
      END IF

      CRSEGN(RPL) = 1
      DFLAG = 1

      GO TO 1000

 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------------
C SUBROUTINE DELSEG
C
C PURPOSE
C	ALLOW USER TO COMPLETELY REMOVE OVERLAY FROM WORKSTATION MEMORY
C
C INPUT
C	NSEG		SEG COUNT
C	CRSEGN		SEG STATUS ARRAY
C	CRSEGT		SEG NAME ARRAY
C	DFLAG		FLAG TO INDICATE CHANGE IN DISPLAY
C
C INTERNAL
C	IOS		INPUT ERROR FLAG
C	DEL		SEG NUMBER TO DELETE
C	I		LOOP COUNTER
C
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------------
       SUBROUTINE DELSEG (NSEG,CRSEGN,CRSEGT,DFLAG)

       IMPLICIT NONE
C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

       CHARACTER*(*) CRSEGT(200)
       INTEGER NSEG,CRSEGN(200),DFLAG
C INTERNAL:
       INTEGER IOS,DEL,I
       INTEGER LINECT
C-----------------------------------------------------------------------

C ASK WHICH OVERLAY TO DELETE
    
 1000 CONTINUE
      LINECT = 0

      WRITE (6,100)
  100 FORMAT(/,' DELETE OVERLAY')
      
      DO 2000 I = 1,NSEG

        IF (CRSEGN(I) .NE. -1) THEN

          WRITE (6,200) I,CRSEGT(I)
  200     FORMAT (' ',I4,')  ',A20)
          LINECT = LINECT + 1

          IF ( LINECT .GE. 20 ) THEN
            LINECT = 0
            WRITE (6,210)
  210       FORMAT (/,' Press <RETURN> to Continue ',$)
            READ (5,*)
            WRITE (6,220)
  220       FORMAT (/)
          END IF

        END IF

 2000 CONTINUE

      WRITE (6,300)
  300 FORMAT ('  0)  QUIT')

      WRITE (6,310)
  310 FORMAT (/,' Selection : ',$)

      READ (5,*,IOSTAT = IOS) DEL

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (DEL .LT. 0 .OR. DEL .GT. NSEG) THEN
        CALL DISMSG('Error: Invalid overlay number.')
        GO TO 1000
      ELSE IF (DEL .EQ. 0) THEN
        GO TO 9999
      END IF

C DELETE OVERLAY FOR THE WORKSTATION AND WISS

      IF (CRSEGN(DEL) .EQ. -1) THEN
        CALL DISMSG('Error : Overlay has already been deleted.')
        GO TO 1000
      END IF

      CALL GDSG (DEL)

C MARK OVERLAY AS DELETED

      CRSEGN(DEL) = -1
      DFLAG = 1

      GO TO 1000
 9999 CONTINUE
      RETURN
      END


C-----------------------------------------------------------------------
C
C SUBROUTINE RMVALL
C
C PURPOSE
C	MARKS ALL OVERLAYS AS NODISPLAY - CRSEGN = 0
C
C VARIABLES
C
C INPUT
C	CRSEGT()	OVERLAY TEXT NAMES
C	NSEG		OVERLAY COUNT
C	DFLAG		DISPLAY FLAG SET IF ANY CHANGES OCCUR
C OUTPUT
C	CRSEGN()	OVERLAY STATUS FLAGS
C INTERNAL
C	IOS		READ STATEMENT I/O STATUS NUMBER
C       RMV		MENU NUMBER OF OVERLAY TO BE REMOVED
C       I		DO LOOP INDEX
C
C WRITTEN BY CRAIG K. FUJIMOTO
C-----------------------------------------------------------------------
      SUBROUTINE RMVALL (NSEG,CRSEGN,CRSEGT,DFLAG)

      IMPLICIT NONE
C INPUT:
      CHARACTER*(*) CRSEGT(*)
      INTEGER NSEG,CRSEGN(*),DFLAG
C INTERNAL:
      INTEGER IOS,I
      CHARACTER*1 ANS
C-----------------------------------------------------------------------

C VERIFY THAT ALL OVERLAYS ARE TO BE REMOVED
    
 1000 CONTINUE

      WRITE (6,100)
  100 FORMAT(/,' Are you sure you want to remove all overlays? [N] ',$)

      READ (5,110,IOSTAT=IOS) ANS
  110 FORMAT(A1)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error on input.')
        GO TO 1000
      ELSE IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') THEN
        GO TO 9999
      END IF

C THE ANSWER WAS 'Y' OR 'y':  YES.  
      DO 2000 I = 1,NSEG
     
        IF (CRSEGN(I) .EQ. 1) THEN

          CRSEGN(I) = 0
          DFLAG = 1
        
        END IF

 2000 CONTINUE

 9999 CONTINUE
      RETURN
      END
                   

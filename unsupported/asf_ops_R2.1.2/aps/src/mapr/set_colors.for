C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	set_colors.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------------
C SUBROUTINE SET_COLORS
C
C $Logfile:   QDISK:[BLD.MPS.MAPR.SRC]SET_COLORS.FOV  $
C
C PURPOSE
C	READ COLOR REPS FROM A FILE
C	COLORS WILL BE USED THROUGHOUT THE ENTIRE PROGRAM
C
C VARIABLES
C INPUT
C	WSID		WORKSTATION ID NUMBER
C OUTPUT
C	IOS		STATUS
C INTERNAL
C	NUM		INDEX NUMBER
C	RED,GRN,BLU	RED/GREEN/BLUE COMPONENTS
C	COMMENT		COMMENT TEXT FROM FILE
C
C 18 Jul 1990 17:38:20  2.0  DBMAN  
C $Date$ $Revision$ $Author$
C-----------------------------------------------------------------------
      SUBROUTINE SET_COLORS(WSID,IOS)
      IMPLICIT NONE

      character*100 SccsFileID
     -/'@(#)set_colors.for	5.1 98/01/08 APS/ASF\0'/

C INPUT:
c--port       INCLUDE 'SYS$LIBRARY:GKSDEFS.BND'

       INCLUDE 'GKS_ROOT:include/fgksenum.inc'

       INTEGER WSID
C OUTPUT
       INTEGER IOS
C INTERNAL
       REAL RED,GRN,BLU
       INTEGER NUM
       CHARACTER*80 COMMENT
c       character*100 buf
c        integer status
       integer bufsize
       character*100 buf2
C--    integer LASTC

       INTEGER*4 f77_getenv
       EXTERNAL  f77_getenv !$pragma c(f77_getenv)
       EXTERNAL  f77_aps_fullpath !$pragma c(f77_aps_fullpath)
       INTEGER*4 aps_fullpath
       EXTERNAL  aps_fullpath !$pragma c(aps_fullpath)

 

C OPEN COLOR FILE

       bufsize = 100
       CALL f77_aps_fullpath('APS_MAPPER','mapr_colors.dat',
     ?   buf2,bufsize)

       IF (bufsize .EQ. 0) THEN
           CALL DISMSG('Error finding COLOR file MAPR_COLORS.DAT.')
           IOS = 1
           GO TO 9999
       ENDIF
           
      OPEN (UNIT=10,FILE=buf2,
     1      IOSTAT=IOS,STATUS='OLD')

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error opening COLOR file mapr_colors.dat.')
        IOS = 1
        GO TO 9999
      END IF

 1000 CONTINUE

C READ IN COLOR FILE RECORD
      READ (10,110,END=5000,IOSTAT=IOS) NUM,RED,GRN,BLU,COMMENT
  110 FORMAT (I2,3(F10.5),A48)

      IF (IOS .NE. 0) THEN
        CALL DISMSG('Error reading COLOR file record.')
        IOS = 1
        GO TO 6000
      END IF

      IF (NUM .LT. 0 .OR. NUM .GT. 99 .OR.
     1    RED .LT. 0.0 .OR. RED .GT. 1.0 .OR.
     1    GRN .LT. 0.0 .OR. GRN .GT. 1.0 .OR.
     1    BLU .LT. 0.0 .OR. BLU .GT. 1.0) THEN
        CALL DISMSG('Error in COLOR file record.')
        IOS = 1
        GO TO 6000
      END IF

C SET COLOR FOR WORKSTATION
      CALL GSCR (WSID,NUM,RED,GRN,BLU)

      GO TO 1000

 5000 CONTINUE
      IOS = 0

C CLOSE THE COLOR FILE
 6000 CONTINUE
      CLOSE (10)

 9999 CONTINUE
      RETURN
      END

C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	times2r.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:	TIMES2R
*  Module Type: SUBROUTINE	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]TIMES2R.FOV  $
*  Purpose:	GIVEN A SAT AND TIME BRACKET, COMPUTES REV BRACKET
*		this rev bracket is to be used in a retrieve statement 
*		that also has more accurate time qualifiers.  
*		the purpose of the rev bracket is to speed the query;
*		the rev bracket is only accurate enough to include 
*		the desired records of the query and not too many others.
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	DBPROCESS* points to a structure containing info for Sybase.
*  IBATCH	INT	 = 1 IF A BATCH JOB
*  SAT		CH*2	SATELLITE NAME:  E1, J1, OR RS
*  ETIME1	REAL*8	EPHEMERIS TIME IN REAL JULIAN DAYS
*  ASFTIME1	CH*21	ASFTIME:  yyyy:ddd:hh:mm:ss.sss
*  ETIME2	REAL*8	EPHEMERIS TIME IN REAL JULIAN DAYS
*  ASFTIME2	CH*21	ASFTIME:  yyyy:ddd:hh:mm:ss.sss
*  Output Parameters:
*  IREV1	INT	REV NUMBER BEFORE TIME1.
*			IREV1 = 0 MEANS THAT THERE WAS NO NODE BEFORE
*			THE INPUT TIME1.  
*			IREV1 = 100,000 MEANS THAT THERE WAS NO NODE AFTER 
*			THE INPUT TIME1.  
*  IREV2	INT	REV NUMBER AFTER TIME2.  
*			IREV2 = 0 MEANS THAT THERE WAS NO NODE BEFORE
*			THE INPUT TIME2.  
*			IREV2 = 100,000 MEANS THAT THERE WAS NO NODE AFTER 
*			THE INPUT TIME2.  
*  IER		INT	=-1	THE INPUT ASFTIME1 WAS ILLEGAL.  
*  			=-2	THE INPUT ASFTIME2 WAS ILLEGAL.  
*  			=0	NORMAL CONDITION.
*			=4	NO RECORDS WERE FOUND.  
*			
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      SUBROUTINE TIMES2R(dbproc,IBATCH,SAT,ET1,ASFTIME1,ET2,ASFTIME2,
     ?    IREV1,IREV2,IER)
      character*100 SccsFileID
     -/'@(#)times2r.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      integer*4 dbproc
      CHARACTER*(*) SAT , ASFTIME1, ASFTIME2
      REAL*8 ET1, ET2
      INTEGER IREV1, IREV2, IER, IDUM, IBATCH
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*)' @TIMES2R:  SAT = ', SAT
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*)' @TIMES2R:  ET1 = ', ET1
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*)' @TIMES2R:  ASFTIME1 = ', ASFTIME1
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*)' @TIMES2R:  ET2 = ', ET2
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*)' @TIMES2R:  ASFTIME2 = ', ASFTIME2
      CALL TIME2REV(dbproc,SAT,ET1,ASFTIME1,IREV1,IDUM,IER)
      IF(IER .GE. 4) GO TO 8888
      IF(IER .LT. 0) GO TO 8001
      IF(IER .EQ. 3) IREV1 = 100000
      CALL TIME2REV(dbproc,SAT,ET2,ASFTIME2,IDUM, IREV2,IER)
      IF(IER .LT. 0) GO TO 8002
      IF(IER .EQ. 3) IREV2 = 100000
C
C---	SET THE BRACKETS IN CASE OF THE ZERO VALUE RETURNED:
      IF(IREV1 .LT. 0) IREV1 = 0
      IF(IREV2 .LE. 0) IREV2 = 100000
C---	NOW EXPAND THE REV BRACKET BY ONE TO BE ON THE SAFE SIDE.  
      IF(IREV1 .NE. 0 .AND. IREV1 .NE. 100000) IREV1 = IREV1 - 1
C---	NORMAL END.
      IER = 0
      GO TO 9999
 8001 CONTINUE
      IER = -1
      GO TO 9999
 8002 CONTINUE
      IER = -2
      GO TO 9999
 8888 CONTINUE
C---	NO PHASE RECORDS FOUND FOR THIS SATELLITE:
      WRITE(*,*)' TIMES2R:  ERROR'
      WRITE(*,*)' TIMES2R:  ERROR'
      WRITE(*,*)' TIMES2R:  ERROR'
      WRITE(*,*)
     ?'           NO RECORDS IN THE PHASE RELATION FOR THIS SATELLITE.'
      WRITE(*,*)' TIMES2R:  SAT = ', SAT
      WRITE(*,*)' TIMES2R:  NOW BOMBING THE PROGRAM.'
      CALL GCBOMB
 9999 CONTINUE
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' $TIME2R:  IREV1 = ', IREV1
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' $TIME2R:  IREV2 = ', IREV2
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' $TIME2R:  IER   = ', IER
      RETURN
      END

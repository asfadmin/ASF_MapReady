C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

********************************************************************
*  Name:	SSCVREVS2T
*  Module Type: SUBROUTINE 	Language: FORTRAN
*  Purpose:	GIVEN A SAT AND 2 REVS, DETERMINE TIME BRACKET.
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS pointer to Sybase session info structure.
*  IBATCH	INT	=1 IF THIS IS A BATCH RUN.
*  SAT		CH*2	SATELLITE NAME:  E1, J1, OR RS
*  IREV1	INT	FIRST REV IN REV BRACKET.
*  IREV2	INT	LAST REV IN REV BRACKET.
*  Output Parameters:
*  ET1		REAL*8	EPHEMERIS TIME OF ASFT1
*  ASFT1	CH*21	FIRST TIME IN RESULTANT TIME BRACKET.
*  ET2		REAL*8	EPHEMERIS TIME OF ASF2
*  ASFT2	CH*21	LAST TIME IN RESULTANT TIME BRACKET.
*  IER		INT	CONDITION CODE:
*  			= 0	NORMAL CONDITION.
*				normal means that both revs were within
*				a phase.  The revs could be in different 
*				phases.  
*
*			= -1	if IREV1 > IREV2.  = is o.k.
*			= >0	ANY OTHER ERROR.  
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
      SUBROUTINE SSCVREVS2T(dbproc, IBATCH,SAT,IREV1,IREV2,
     ?					ET1,ASFT1,ET2,ASFT2,IER)
      character*100 SccsFileID
     -/'@(#)sscvrevs2t.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      integer*4 dbproc
      CHARACTER *(*) SAT
      CHARACTER *(22) ASFT1, ASFT2
      INTEGER IREV1, IREV2, IER, IBATCH
      REAL*8 ET, ET1, ET2
      character*22 ASFT
      external sscvrev2time	!$pragma C(sscvrev2time)
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' @SSCVREVS2T:  SAT=', SAT(1:2),'  IREV1 = ', IREV1
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' @SSCVREVS2T:  IREV2 = ', IREV2
      ASFT1 = 'no value for time1   ' // char(0)
      ASFT2 = 'no value for time2   ' // char(0)
      ET1 = 0.123
      ET2 = 0.234
      IF(IREV1 .GT. IREV2) THEN
	IER = -1
	RETURN
      ENDIF
      IER = 0
      CALL sscvrev2time(dbproc,SAT,IREV1,ET1,ASFT1,ET,ASFT,IER)
      IF(IER .NE. 0) RETURN 
      CALL sscvrev2time(dbproc,SAT,IREV2,ET,ASFT,ET2,ASFT2,IER)
      IF(IER .NE. 0) RETURN 
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' $SSCVREVS2T:  ET1 = ', ET1,' ASFT1 = ', ASFT1(1:21)
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' $SSCVREVS2T:  ET2 = ', ET2,' ASFT2 = ', ASFT2(1:21)
      IF(IBATCH .EQ. 1)
     ?WRITE(*,*) ' $SSCVREVS2T:  IER = ', IER
      RETURN
      END

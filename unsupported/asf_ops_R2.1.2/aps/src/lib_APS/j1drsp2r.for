C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	j1drsp2r.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:	J1DRSP2R.FOR
*  Module Type: SUBROUTINE	Language: FORTRAN 
*  Purpose:	GIVEN A DATE AND J1 RSP, COMPUTE THE REV NUMBER.
*  Input Parameters:
*  Name         Type    Definition
*  ASFTIME	CH*21	Time in ASF-SIS format.  ONLY THE DATE IS USED.
*			JUST THE FIRST 8 CHARACTERS:  1990:245
*			but ASFTIME can be 21 characters long.  
*  IRSP		INT	RSP
*  Output Parameters:
*  IREV		INT	rev number corresponding to the RSP.
*  IER		INT	ERROR CODE:
*			-2= INPUT ERROR; BAD ASF TIME.  
*			-1= INPUT ERROR; THE RSP IS NOT WITHIN [1-CYREVS]
*  			0 = NORMAL CONDITION
*  			1 = THE REV DOES NOT OCCUR WITHIN 1 DAY OF THE INPUT
*			    TIME.
*			2 = THE INPUT TIME DOES NOT FALL WITHIN A PHASE.
*			3 = NO RECORDS WERE FOUND IN THE PHASE RELATION.
*  Variables:	CYREVS = THE NUMBER OF REVS IN ONE REPEAT CYCLE.
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
      SUBROUTINE J1DRSP2R(dbproc, ASFTIME,IRSP,IREV,IER)
      character*100 SccsFileID
     -/'@(#)j1drsp2r.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
	  integer dbproc
      CHARACTER*22 ASFTIME
      INTEGER IRSP, IREV, IER
      CHARACTER*21 ASFT, CHDUM
      REAL*8 ET0
CC    WRITE(*,*)' J1DRSP2R:  ASFTIME=', ASFTIME
CC    WRITE(*,*)'             IRSP   =', IRSP
      IREV = 0
      ASFT = ASFTIME(1:8) // ':00:00:00.000'
      ET0 = 0.0D0
CC write(*,1001) dbproc
CC1001 format(' J1DRSP2R.for:  dbproc = ', Z8)
      CALL J1RSP2RT(dbproc, ASFT,ET0,1,IRSP,0.0D0,IREV,CHDUM,IER)
      IF(IER .EQ. 2) THEN
C---	TRY AGAIN USING THE LAST SECOND OF THE DAY.  PERHAPS WE ARE AT THE 
C---	BEGINNING OF THE PHASE.  
		ASFT = ASFTIME(1:8) // ':23:59:59.999'
		ET0 = 0.0D0
        CALL J1RSP2RT(%VAL(dbproc),ASFT,ET0,-1,IRSP,0.0D0,IREV,
     ?		CHDUM,IER)
      ENDIF
 9999 CONTINUE
CC    WRITE(*,*)' $J1DRSP2R:  IREV = ', IREV
CC    WRITE(*,*)' $J1DRSP2R:  IER = ', IER
      RETURN
      END

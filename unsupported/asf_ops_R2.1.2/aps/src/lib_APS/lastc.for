C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	lastc.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

*************************************************************************
*  Name:	LASTC
*  Module Type: INT FUNCTION	Language: FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]LASTC.FOV  $
*  Purpose:	RETURNS COLUMN OF LAST CHARACTER IN THE CHAR STR.
*			this routine thus returns the length of the string 
*			in terms of C.  
*  			a null character terminates the character string.
*  Subroutines called:
*  Input Parameters:
*  Name         Type    Definition
*  XCHARS	CHAR*(*)INPUT CHARACTERS.
*  Output Parameters:
*  Name         Type    Definition
*  LASTC	INT*4	COLUMN NUMBER OF LAST NON-BLANK CHAR OR 1 IF A BLANK 
*			CHARACTER.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
      INTEGER FUNCTION LASTC(XCHARS)
      character*100 SccsFileID
     -/'@(#)lastc.for	5.1 98/01/08 APS/ASF\0'/

C---	RETURNS COLUMN OF LAST NON-BLANK CHARACTER IN XCHARS OR 1.
      IMPLICIT NONE
      INTEGER L, J, LC, LC2
      CHARACTER*(*) XCHARS
      CHARACTER*1 NULL
      NULL = CHAR(0)
      L = LEN(XCHARS)
	  LC = 1
      IF (L.LT.2) GO TO 9999

      DO 1000 J = 1, L
      LC = L - J + 1
      IF (XCHARS(LC:LC) .NE. ' ' .AND. 
     ?    XCHARS(LC:LC) .NE. NULL) GO TO 1999
 1000 CONTINUE
      LC = 1
 1999 CONTINUE

C--		check for an early termination of the string:  
      DO 2000 J = 0, L-1
      LC2 = J
C--		LC will be the column of the character previous to the NULL:
      IF (XCHARS(LC2+1:LC2+1) .EQ. NULL) GO TO 2999
 2000 CONTINUE
      LC2 = L
 2999 CONTINUE

      IF(LC2 .LT. LC) LC = LC2
 9999 CONTINUE
      LASTC = LC
      RETURN
      END


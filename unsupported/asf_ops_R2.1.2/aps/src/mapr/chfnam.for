C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	chfnam.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

      SUBROUTINE CHFNAM (FSPEC,IOST)

      character*100 SccsFileID
     -/'@(#)chfnam.for	5.1 98/01/08 APS/ASF\0'/

********************************************************************
*  Name: CHFNAM
*  Module Type: SUBROUTINE Language: FORTRAN 77
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]CHFNAM.FOV  $
*  Purpose: Checks file specification.  It does this by trying to open a 
*  new file using the file name.  If successful, the file is closed and 
*  deleted and a value of 0 is returned.  The routine searches for an unused 
*  FORTRAN logical unit number in order not to close a file used elsewhere in 
*  the calling program.
*  Functions called:
*  INQUIRE, OPEN
*  Input Parameters:
*  Name         Type    Definition
*  FSPEC	CHAR	FILE NAME SPECIFICATION
*  Output Parameters:
*  IER		INT	0	- IF LEGAL FILE SPECIFICATION.
*			30	BAD DIRECTORY NAME.
*			42	DEVICE DOESN'T EXIST.
*			43	ILLEGAL FILE SPECIFICATION.
*			143	BLANK FILE NAME.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*  7/27/94 Nadia Adhami PORT 2 UNIX/C : deleted "carriagecontrol" parameter
*
*********************************************************************/
      CHARACTER*(*) FSPEC
      INTEGER IUNIT, IOST
      LOGICAL TF
      IOST = 143
      IF(FSPEC .EQ. ' ' ) GO TO 9999
      DO 1000 I = 1, 119
      IUNIT = 120 - I
      INQUIRE (UNIT=IUNIT,OPENED=TF)
      IF(.NOT. TF) GO TO 2000
 1000 CONTINUE
      IOST = -1
      GO TO 9999
 2000 CONTINUE
      OPEN(UNIT=IUNIT,FILE=FSPEC,FORM='FORMATTED',
     ?     STATUS='NEW',IOSTAT=IOST)
c     ?     CARRIAGECONTROL='LIST',STATUS='NEW',IOSTAT=IOST)
      IF(IOST .EQ. 0 ) CLOSE(IUNIT,STATUS='DELETE')
 9999 CONTINUE
      RETURN
      END

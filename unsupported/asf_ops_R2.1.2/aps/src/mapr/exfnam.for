C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	exfnam.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

C-----------------------------------------------------------------
C                                                                   
C  SUBROUTINE EXFNAM
C
C  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]EXFNAM.FOV  $
C                                                                   
C  Purpose:
C	DETERMINE IF A FILE EXISTS
C          
C  Functions called:
C
C  Input Parameters:
C	Name		Definition
C	FNAME		FILENAME
C
C  Output Parameters:
C	IOST		RETURN STATUS 0=EXISTS,1=DOESN'T EXIST
C  
C  Locals :
C	FEXIST		RETURN FROM INQUIRE - LOGICAL
C
C  Date			Revision	Author
C  $Date$ $Revision$ $Author$
C                                                                   
C-----------------------------------------------------------------
       SUBROUTINE EXFNAM(FNAME,IOST)

       CHARACTER*(*) FNAME
       INTEGER IOST

       character*100 SccsFileID
     - /'@(#)exfnam.for	5.1 98/01/08 APS/ASF\0'/

       LOGICAL FEXIST

       INQUIRE(FILE=FNAME,EXIST=FEXIST)

       IF (FEXIST) THEN
         IOST = 0
       ELSE
         IOST = 1
       END IF

 9999  CONTINUE
       RETURN
       END

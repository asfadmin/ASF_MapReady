C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	catephm.for
C--
C--  Description:	a routine to read a binary ephemeris file and 
C               print formatted ascii output.  
C
C      returns IERR = 0 if no errors.  
C      returns IERR = system error number if an error occurred when the 
C                     file was read.  
C
C File Scope Functions:
C none
C External Variables Defined:
C none
C File Scope Variables:
C none
C ===========================================================================*/
C--	
C--  Notes:
C--
C-- ==========================================================================
      SUBROUTINE CATEPHM(EUNIT, IERR)
      IMPLICIT NONE
      integer EUNIT, IERR

      character*100 SccsFileID
     -/'@(#)catephm.for	5.1 98/01/08 APS/ASF\0'/

      REAL*8 X(12), EPOCH
	  character*1 nodeflag

      IERR = 0

      READ(EUNIT, err=8888, iostat=ierr)EPOCH
      print 10,' Epoch =', EPOCH
   10 format(A, F17.8)

    1 continue
      READ(EUNIT,END=1000,err=8888, iostat=ierr)nodeflag, X
	  print 11,nodeflag,'  X = ', X(1), X(2), X(3),
     ?                  '  V = ', X(4), X(5), X(6)
   11 format(A,A, 3F12.5, A, 3F9.5)
	  print 12,'   ET = ', X(7), '       REV = ', X(8), 
     ?       '     LASC =   ', X(9)
   12 format(A, F17.8, A, F8.1, 7x, A, F9.5)
	  print 13,'   SS LAT/LON = ', X(10), X(11), 'GHA = ', X(12)
   13 format(A, F9.5, F11.5, 22x, A, F12.6)
	  go to 1
 1000 continue
C---	end of file; normal end.
      IERR = 0
	  RETURN
 8888 continue
C---     ERROR reading file;  system error no. ', ierr
C---	return with the current value of IERR from the read statement:
	  RETURN
	  end

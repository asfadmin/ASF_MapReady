C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	writecheck.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

********************************************************************
*  Name:        writecheck.for
*  Module Type: SUBROUTINE      Language: FORTRAN
C  Purpose:     This subroutine checks a file to see if the needed 
*		permissions are given to the current process.  
*		The needed permissions are:  if the file does not exist,
*		this is O.K.  If it does exist, RW permission is needed.  
*		If the permissions are not there, info is printed into 
*		standard output.  
*  Functions called:
C               TOGEOD (RE,X,ALTD,TRIG)    DETERMINES THE GEODETIC ALTITUDE.
C               (INCLUDED IN THIS FILE)
*  Input Parameters:
C  filename     CH*(*)  name of the file to be checked. 
*  Name         Type    Definition
*  Output Parameters:
C  ier          int  	error code.  0 = OK; 1 = not OK.  
*  Modification History:
*  Date                 Revision        Author
*********************************************************************/

      subroutine writecheck(filename, ier)
      character*100 SccsFileID
     -/'@(#)writecheck.for	5.1 98/01/08 APS/ASF\0'/

      implicit none
      character *(*) filename
      integer ier
      character *150 ibuf
      integer iexist, rcode, system, lc, lastc
      external aps_access !$pragma c(aps_access)
      iexist = 0
      lc = lastc(filename)
      ibuf = filename(1:lastc(filename)) // char(0)
      call aps_access(ibuf, 'f', ier)
      if(ier .eq. 0) then
c---    the file exists; check the ability to delete.
        iexist = 1
        call aps_access(ibuf, 'rw', ier)
      else
c---    the file does not exist.  this is O.K.
        ier = 0
      endif
      if(ier .ne. 0) then
        print *,' '
        print *,'writecheck:  access to file is denied;  ',
     ?               'rw permission needed for:'
        print *, filename(1:lc)
        ibuf = 'ls -l ' // filename(1:lc) // char(0)
        rcode = system(ibuf)
      endif
      end

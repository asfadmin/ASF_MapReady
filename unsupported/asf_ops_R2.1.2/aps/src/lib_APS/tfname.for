C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	tfname.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

      subroutine tfname(fname, tname)
      character*100 SccsFileID
     -/'@(#)tfname.for	5.1 98/01/08 APS/ASF\0'/

* Written for porting of the MPS from the VAX to unix by Larry Stevens 3/1994
c---	this routine accepts a filename fname that can include 
c---	environment variables in it.  the routine will decode the variables
c---	until the full file name is decoded.  
c---	the environment variables must start with "MPS_", APS, "AOS" or "FOR"
c---
c---	example:  MPS_EPHMCMD/file.dat  
c---	after decoding MPS_EPHMCMD, it might become:
c---		  AOS_DATA/mps/data/ephmcmd/file.dat   
c---	then, after AOS_DATA is decoded, we might be left with:
c---		  /ua/dps/larry/work/mps/data/ephmcmd/file.dat   
c---	which is the complete file name.  
      implicit none
      character*(*) fname, tname
      character*150 new_name, var, dvar
      integer j, jcode
      call tfclean(fname, tname)
c---	find a variable to translate
c---	four levels will be enough.  
      do 1000 j = 1, 6
      call findvar(tname, var, jcode)
c---      print *, "tfname: tname = >", tname, "<"
c---      print *, "tfname: var = ", var
c---      print *, "tfname:  jcode = ", jcode
c---      print *, "   "
      if(jcode .eq. 0) go to 2000
c---	translate the variable.
      call getenv(var(1:jcode), dvar)  
      if(dvar .eq. ' ') go to 2000
c---      print *, " tfname: var = ", var
c---      print *, " tfname: after getenv: dvar = ", dvar
c---	substitute the variable to create the new name.
      call subvar(tname, jcode, dvar, new_name)
      tname = new_name
 1000 continue
 2000 continue
      return
      end

      subroutine findvar(name, var, jcode)
      implicit none
      character*(*) name, var
      integer jcode, j
c---      print *, "findvar: start: name = ", name
      jcode = 0
      var = ' '
      if    (name(1:4) .ne. 'MPS_' .and. name(1:4) .ne. 'AOS_'
     ? .and. name(1:3) .ne. 'APS'  .and. name(1:3) .ne. 'FOR') 
     ?   go to 9000
      do 1000 j = 1, LEN(name)
c---      print *, "findvar: j = ", j, '  ', name(j:j)
      jcode = j-1
      if ( name(j:j) .eq. '/' ) go to 2000
      if ( name(j:j) .eq. ' ' ) go to 2000
      if ( name(j:j) .eq. '^@' ) go to 2000
 1000 continue
      jcode = 0
      go to 9000
 2000 continue
      var = name(1:jcode)
 9000 continue
c---      print *, "findvar: end: var = ", var
c---      print *, "findvar: end: jcode = ", jcode
      return
      end

      subroutine subvar(name, last_vchar, dvar, new_name)
      implicit none
      character*(*) name, new_name, dvar
      integer last_vchar, ln, j 
      new_name = dvar
c--- 	get length of decoded variable.  
      do 1000 j = 1, LEN(dvar)
      ln = j-1
      if(dvar(j:j) .eq. ' ') go to 2000
 1000 continue
c---	end not found; error condition.  no substitution.  
      new_name = name
      go to 9000
 2000 continue
      new_name(1:ln) = dvar(1:ln)
      new_name(ln+1:) = name(last_vchar+1:)
 9000 continue
      return
      end
      
      subroutine tfclean(name, nameout)
      implicit none
      character *(*) name, nameout
      integer j, jlast, length1, length2
      length1 = LEN(name)
      length2 = LEN(nameout)
c---      print *,"tfclean: l1, l2 = ", length1, length2
      if(length1 .le. 0 ) return
      if(length2 .le. 0 ) return
      do 1000 j = 1, length1
      jlast = j-1
      nameout(j:j) = name(j:j)
      if(name(j:j) .eq. ' ') go to 2000
 1000 continue
c---	jlast indicates the position of the last character in name.
      jlast = jlast + 1
 2000 continue
      if(jlast .gt. length2) return
      do 3000 j = jlast+1, length2
      nameout(j:j) = ' '
 3000 continue
      return
      end

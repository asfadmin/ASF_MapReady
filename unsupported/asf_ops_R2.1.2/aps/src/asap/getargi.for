C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	getargi.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
      subroutine getargi(n, j, ncode)

      character*100 SccsFileID
     -/'@(#)getargi.for	5.1 98/01/08 APS/ASF\0'/

********************************************************************
* Written for porting of the MPS from the VAX to unix by Larry Stevens 3/1994
*  Name:  getargi.for
*  Module Type: SUBROUTINE	Language: FORTRAN 77
*  Purpose: read the nth argument from the command line, interpret it
*  as an integer, and put it in j.  
*  Functions called:
*  iargc, getarg, from Sun
*  Input Parameters:
*  Name         Type    Definition
*  n		integer	the argument number to get from the command line.
*  Output Parameters:
*  j		integer	the nth argument decoded as a real.  
*  ncode        integer return code.  = 0 if no error.  
*                                     = 1 if there are fewer than n 
*                                         arguments.
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
      implicit none
      integer n, j, iargc, ncode, ncount
      character*45 argbuf
c      -print *, "getargi start:  n = ", n
c---    get count of arguments, compare with input n.  
      ncount = iargc()
      if(n .gt. ncount) then
          j = 0
          ncode = 1
          return
      endif
c---      real command line argument number n
      call getarg(n, argbuf)
c      -print *, " arg n = ", argbuf
      decode (40, 1002, argbuf) j
 1002 format(i)
c      -print *, "getargi return j, ncode = ", j, " ", ncode
      return
      end

C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  gcbomb.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================


C-----------------------------------------------------------------
C SUBROUTINE GCBOMB
C
C PURPOSE
C	MPS SYSTEM ERROR BOMB ROUTINE
c  and now APF system error bomb routine.  Called when application 
c code wants to bomb the program.  This happens when an error is detected
c and the run must not continue.  The errors must attract attention 
c from a programmer immediately and be fixed.  
C 
C $LOGFILE$
C
C $DATE$ $REVISION$ $AUTHOR$
C-----------------------------------------------------------------
      SUBROUTINE GCBOMB 
      character*100 SccsFileID
     -/'@(#)gcbomb.for	5.1 98/01/08 APS/ASF\0'/

C---	CAUSES A PROGRAM BOMB.
      WRITE(*,*)'APS SYSTEM DETECTED ERROR.  ######################'
      WRITE(*,*)'APS SYSTEM DETECTED ERROR.  ######################'
      WRITE(*,*)'APS SYSTEM DETECTED ERROR.  ######################'
      print *,'You need to get a maintenance programmer to run '
      print *,'adb at this point using the core dumped to get  '
      print *,'a traceback or subroutine calling stack.        '
      call abort()
      END


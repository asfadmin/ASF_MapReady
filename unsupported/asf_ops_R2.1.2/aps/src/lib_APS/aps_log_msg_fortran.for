C--  Copyright (c)1995, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	aps_log_msg_fortran.for
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================

	  SUBROUTINE aps_log_msg_fortran( PROGNAME, LEVEL, MSG, SYSLOGFLAG, 
     ?                                PRINTFLAG )
      character*100 SccsFileID
     -/'@(#)aps_log_msg_fortran.for	5.1 98/01/08 APS/ASF\0'/

      IMPLICIT NONE
      CHARACTER*(*) PROGNAME
      INTEGER LEVEL, SYSLOGFLAG, PRINTFLAG
      CHARACTER*(*) MSG

      external aps_log_msg         !$pragma c(aps_log_msg)
	  INTEGER LASTC

      CALL aps_log_msg( PROGNAME(1:LASTC(PROGNAME)) // CHAR(0), 
     ?                  %val(LEVEL), MSG(1:LASTC(MSG)) // CHAR(0),
     ?                  %val(SYSLOGFLAG), %val(PRINTFLAG)  )

      RETURN
      END

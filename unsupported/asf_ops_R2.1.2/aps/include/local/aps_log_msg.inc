C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-******************************************************************
C-
C- File:        aps_log_msg.inc
C-
C- Description:  FORTRAN include file.  
C-              contains declarations for the following functions:
C-              aps_log_msg()
C-              Everything must correspond to  aps_log_msg.h
C-
C- Author:      L. Stevens
C-
C- Date:        Wed Feb  7 14:13:29 PST 1996
C-
C- Modified:
C-
C--   character*100 SccsFileID
C--  -/'@(#)aps_log_msg.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-************************************************************** */

	  EXTERNAL aps_log_msg_fortran
	  external aps_open_syslog  !$pragma c(aps_open_syslog)
	  external aps_close_syslog !$pragma c(aps_close_syslog)

      INTEGER DO_SYSLOG    / 1/
      INTEGER NO_SYSLOG    / 0/
C-              Must correspond to  aps_log_msg.h
      INTEGER DO_PRINT     / 1/
      INTEGER NO_PRINT     / 0/
C-              Must correspond to  aps_log_msg.h
      INTEGER APS_DEBUG    / 0/
      INTEGER APS_INFO     /-1/
C-              Must correspond to  aps_log_msg.h
      INTEGER APS_WARNING  /-2/
      INTEGER APS_ERROR    /-3/
C-              Must correspond to  aps_log_msg.h
      INTEGER APS_CRITICAL /-4/

      INTEGER APS_EXIT_OK    / 0/
C-              Must correspond to  aps_log_msg.h
      INTEGER APS_EXIT_ERROR /99/


/*==============================================================================
Filename:	check_status.h

Description:	
     This header file contains the CHECK_STATUS macro used the DCE
  client and server initialization modules.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	This was taken from "Guide to Writing DCE Applications" by
John Shirley, page 76, Example 3-12.

SCCS Info:
   @(#)check_status.h	1.2
==============================================================================*/

#ifndef _CHECKSTATUS_
#define _CHECKSTATUS_

#include <stdio.h>
#include <syslog.h>
#include <dce/dce_error.h> /* required to call dce_error_inq_text routine   */
#include <dce/pthread.h>   /* needed if application uses threads            */
/*
#include <dce/rpcexc.h>    / * needed if application uses exception handlers */

#define RESUME 0
#define ABORT  1

#define CHECK_STATUS(input_status, comment, action) \
{ \
   if(input_status != rpc_s_ok) { \
      dce_error_inq_text(input_status, error_string, &error_stat); \
      syslog(LOG_NOTICE, "CRITICAL: %s %s\n", comment, error_string); \
      if(action == ABORT) \
         exit(1); \
   } \
}

static int            error_stat;
static unsigned char  error_string[dce_c_error_string_len];

#endif /* _CHECKSTATUS_ */

/* End of File */

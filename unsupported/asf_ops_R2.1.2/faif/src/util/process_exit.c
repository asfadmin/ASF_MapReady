/*==============================================================================
Filename:	process_exit.c

Description:	
	This module contains the function pr_exit which reports the status
of a child process exit through syslog.

External Functions:
	None
	
Static Functions:
	process_exit
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <sys/types.h>
#include <sys/wait.h>
#include <syslog.h>
#include "faifdefs.h"

#ifdef __STDC__
void process_exit(int) ;
#else
void process_exit() ;
#endif


/*==============================================================================
Function:	void process_exit(int status)

Description:	
	This function simply records a message in the system log showing
the status of a forked process upon returning from execution.

Parameters:
	int status - the status returned by the forked process

Returns:	None
Creator:	Norbert Piega
Creation Date:	08/11/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
void
process_exit(int status)
#else
void
process_exit(status)
   int status ;
#endif
{
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   if (WIFEXITED(status))
   {
      sprintf(logmsg, 
	 "NOTICE: Normal child process termination, exit status = %d\n", 
	 WEXITSTATUS(status)) ;
      syslog(LOG_DEBUG, logmsg) ;
   }

   else if (WIFSIGNALED(status))
   {
      sprintf(logmsg, 
	 "WARNING, Abnormal child process termination, signal number = %d%s\n",
		WTERMSIG(status),
		WCOREDUMP(status) ? " (core file generated)" : "");
      syslog(LOG_DEBUG, logmsg) ;
   }

   else if (WIFSTOPPED(status))
   {
      sprintf(logmsg, 
	 "WARNING, child stopped, signal number = %d\n", WSTOPSIG(status));
      syslog(LOG_DEBUG, logmsg) ;
   }

} /* process_exit */

/* End of file */

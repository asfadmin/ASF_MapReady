/*==============================================================================
Filename:	utilmalloc.c

Description:	
	This module contains the "safe" malloc function.

External Functions:
	None
	
Static Functions:
	util_do_malloc
	
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

#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include "faifdefs.h"

#ifdef __STDC__
void *util_do_malloc(unsigned int) ;
#else
void *util_do_malloc() ;
#endif




/*==============================================================================
Function:	void * util_do_malloc(size)

Description:
	This is the same as malloc except it reports a fatal error when
malloc is unable to allocate the bytes requested.  When successful, a
pointer to the newly allocated space is returned.

Parameters:
	size - size of the space requested for allocation

Returns:	pointer to the newly allocated space
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
	If malloc is unable to allocate enough memory, an error is
reported via syslog.
==============================================================================*/
#ifdef __STDC__
void *
util_do_malloc(unsigned int size)
#else
void *
util_do_malloc(size)
   unsigned int size ;
#endif
{
   char	 *ptr ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   ptr = malloc(size) ;
   if (ptr == NULL)
   {
      sprintf(logmsg, "CRIICAL, Not enough memory to allocate %d bytes", size) ;
      syslog(LOG_ERR, logmsg) ; 
   }

   return(ptr) ;
} /* util_do_malloc */

/* End of File */

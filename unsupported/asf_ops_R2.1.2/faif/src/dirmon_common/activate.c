/*==============================================================================
Filename:	activate.c

Description:	
	This module contains the routing activation function "activate_route".

External Functions:
	activate_route
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFile[] = "activate.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)activate.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include "configrec.h"
#include "filerec.h"
#include "dapps_list.h"
#include "nmalloc.h"

#ifdef __STDC__
int activate_route(Config_Record *, int) ;
#else
int activate_route() ;
#endif

extern File_Record *check_FA_dir() ;
extern int          check_sendlist() ;



/*==============================================================================
Function:	activate_route(config)

Description:
	Routing activation.  The incoming files that appear in the 
reception directory are detected, identified and then routed to
the appropriate destination.

	The directory to monitor is checked for files via check_FA_dir.
File record structures are created for each file and are added into a
file record list which is returned by check_dir after obtaining the
last directory entry.

	The list obtained from check_dir is passed to the check_sendlist
function to do the routing procedure; check_sendlist transfers each file
represented in the list to its destination.  Appropriate updates to the
log file and incoming directory are also performed by check_sendlist.

	The status returned by this directory monitor activate function
depends on the status returned by each called function.

Parameters:
	Config_Record *config - pointer to record containing config
settings for the directory monitor

Returns:
	OK - Normal return
	ERROR - Error detected

Creator:	Norbert Piega	
Creation Date:	08/05/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
activate_route(Config_Record *config, int fa_code)
#else
int
activate_route(config, fa_code)
   Config_Record *config ;
   int fa_code ;
#endif
{
   int status = OK ;
   llist *filerec_llist = NULL ;

   /* Check if config record is ok
   */
   if (config == (Config_Record *) NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL Config record\n") ;
      return(ERROR) ;
   }

   /* Perform Check Directory
   */
   filerec_llist = (llist *)check_FA_dir(config->FA_receptdir, fa_code) ;
   if (filerec_llist == (llist *)NULL)
   {
      syslog(LOG_WARNING, "WARNING, No files in file list\n") ;
      return(OK) ;
   }

   /* Do Check Send List
   */
   status = check_sendlist(filerec_llist, config) ;
   if (status == ERROR)
      syslog(LOG_ERR, "WARNING, Check Send List error\n") ;

   /* Free structures
   */
   DEL_ALL(filerec_llist) ;

   return(status) ;

} /* activate_route */

/* End of File */

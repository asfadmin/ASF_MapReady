#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	db_open_APS_readonly.c

Description:	
	this  file provides support for routines opening and closing the 
	$APSDB database in a readonly mode in order to get needed data or to 
	compute results from db data.  

External Functions Defined:
	db_open_APS_readonly()
	
File Scope Functions:
	
External Variables Defined:
	DBPROCESS	*APS_readonly_dbproc ;

File Scope Variables:
	
Notes:
The name of the database comes from the environment variable APSDB.  

==============================================================================*/
#pragma ident	"@(#)db_open_APS_readonly.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_open_APS_readonly.c"

#include <stdlib.h>     /* for getenv()   */
#include <string.h>     /* for strlen()   */
#include "db_sybint.h"  /* for all of the SYBASE stuff, like DBPROCESS.  */

static char    	*db_name = NULL ;   /* note there is a dbname() from Sybase*/
static char    	user[] = "aps_reader" ;
static char    	password[] = "aps_reader" ;


/*==============================================================================
Function:       db_open_APS_readonly()

Description:    open a readonly database session.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul  7 09:51:09 PDT 1995

Notes:		
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int db_open_APS_readonly( DBPROCESS **APS_dbread_proc )
{

	int		return_code ;

	/* 
	-- check to see if the db_name is known.  if not, get 
	-- from the environment.  
	*/

	if ( db_name == NULL )
		db_name = getenv ( "APSDB" ) ;

	if ( db_name == NULL )
	{
		printf("%s(%d)  ERROR:  NO VALUE FOR ENVIRONMENT VARIABLE APSDB.  \n",
			__FILE__, __LINE__ ) ;
		return FALSE ;
	}

	*APS_dbread_proc = db_open( 
		db_name, "APS_readonly", user, password, 
		NULL, error_handler_exit, 
		&return_code ) ;

	if(return_code != 0)
	{
		printf("%s(%d)  ERROR OPENING APS DB SESSION WITH APS_readonly.\n",
			__FILE__, __LINE__ ) ;
		return FALSE ;
	}

	return TRUE ;

}

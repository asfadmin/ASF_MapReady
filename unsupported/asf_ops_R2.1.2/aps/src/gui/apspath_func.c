#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		apspath_func.c

Description:	contains functions for manipulating the results
				from the apspath functions in libaps

External Functions Defined:
				filename_from_pathname
				path_from_pathname
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)apspath_func.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.apspath_func.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Xm/Xm.h>

#include "dapps_defs.h"


/*==========================================================================
 * RETURNS:	a pointer to a new string containing the filename part of
 *			pathname (where pathname == [dir/]filename)
 *
 * NOTE: the caller is responsible for free'ing the return value.
 =========================================================================*/
char *
filename_from_pathname(pathname)
	char    *pathname ;
{
	char    *filename, *slash_search, *stored_filename ;

	/* Extract filename from pathname */
	for (filename = slash_search = pathname ; *slash_search != STREND ; )
		if (*slash_search++ == '/')
			filename = slash_search ;

										  
	/* Get storage, copy, and return pointer */
	stored_filename = (char *) malloc (strlen(filename) + 1) ;
	(void) strcpy(stored_filename, filename) ;
	return  stored_filename ;
}   /* filename_from_pathname() */


/*==========================================================================
 * RETURNS:	a pointer to the new string containing the path component of
 *			pathname (including the last slash)
 *			or NULL if there is no path component
 *
 * NOTE: the caller is responsible for free'ing the return value.
 =========================================================================*/
char *
path_from_pathname(pathname)
	char    *pathname ;
{
	char    *filename, *slash_search;
	char    *stored_path = NULL;
	int		pathLen;

	/* Find beginning of filename */
	for (filename = slash_search = pathname ; *slash_search != STREND ; )
		if (*slash_search++ == '/')
			filename = slash_search ;

	pathLen = filename - pathname;

	if (pathLen > 0)	/* there IS a path component */
	{
		/* Get storage, copy, and return pointer */
		stored_path = (char *) malloc (pathLen + 1);
		(void) strncpy(stored_path, pathname, pathLen) ;
		*(stored_path + (pathLen)) = STREND;
	}

	return  (stored_path) ;

}   /* path_from_pathname() */

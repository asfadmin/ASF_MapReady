#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		prcapsfile_main.c

Description:	contains code common to both the do_prcapsfile_main and
				get_prcapsfile_main processes.

External Functions Defined:
				get_active_permissions
	
File Scope Functions:
				input_error_exit
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)prcapsfile_main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.prcapsfile_main.c"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "dapps_defs.h"
#include "aps_defs.h"
#include "prcapsfile_main.h"

/*==============================================================================
	Function Declarations
==============================================================================*/
static void		input_error_exit( char * ) ;



/*==============================================================================
Function:       get_active_permissions

Description:    reads, from stdin, the active permissions and file types
				and creates the permission array for file processing.

Parameters:     callingCmd - the cmd (argv[0]) of the calling process
				permArray (output) - malloc'ed permission array
				numPerms (output) - the number of perms in the array

Returns:        a pointer to the permission array

Creator:        Teresa McKillop

Creation Date:  01/16/97

Notes:			*** EXITs *** if an error occurs
==============================================================================*/
PERMREC *
get_active_permissions( char *callingCmd, int *numPerms )
{
	PERMREC *permArray ;
	char	inBuf[BUFSIZ] ;	/* stdin buffer */
	char	*ptr ;
	int		i ;

	/*
	-- Get the active permission ids from stdin
	*/

	if (isatty( STDIN_FILENO ))
	{
		/* stdin is a terminal, can't run this from the cmd line */
		(void) fprintf( stderr,
				"%s: ** EXITING **, must be started from within the APS GUI\n",
				callingCmd ) ;
		exit (1) ;
	}

	/* get the # of permissions */
	if (fgets( inBuf, BUFSIZ, stdin ) == NULL)
		input_error_exit( callingCmd ) ;	/* EXITs */
	if ((ptr = strrchr( inBuf, NEWLINE )) != NULL)
		*ptr = STREND ;
	/* validate the input */
	for (ptr = inBuf ; isspace( *ptr ) ; ptr++ )
		;	/* deliberately empty */
	ptr = memmove( inBuf, ptr, strlen( ptr ) ) ;
	for ( ; *ptr != STREND ; ptr++)
	{
		if (!isdigit( *ptr ))
			input_error_exit( callingCmd ) ;	/* EXITs */
	}
	if (ptr == inBuf || (*numPerms = atoi( inBuf )) <= 0)
		input_error_exit( callingCmd ) ;	/* EXITs */
	
	permArray = (PERMREC *) malloc( *numPerms * sizeof( PERMREC ) ) ;
	/* read in the permissions */
	for (i = 0 ; i < *numPerms && fgets( inBuf, BUFSIZ, stdin ) != NULL ; i++)
	{
		char	*type ;
		char	*idStr ;

		if ((ptr = strrchr( inBuf, NEWLINE )) != NULL)
			*ptr = STREND ;
		/* get rid of trailing and leading blanks */
		for (ptr = inBuf+strlen( inBuf )-1 ; ptr >= inBuf && isspace( *ptr )
				; ptr-- )
			;	/* deliberately empty */
		*(++ptr) = STREND ;
		for (ptr = inBuf ; isspace( *ptr ) ; ptr++ )
			;	/* deliberately empty */
		if (strlen( ptr ) == 0)	/* empty string */
			input_error_exit( callingCmd ) ;	/* EXITs */
		/* TYPE */
		type = ptr ;
		while (!isspace( *ptr ))
			ptr++ ;
		*ptr = STREND ;
		/* ID */
		while (isspace( *++ptr ))
			;	/* deliberately empty */
		idStr = ptr ;
		while (isdigit( *ptr ))
			ptr++ ;
		/* store permission in permission array, if id is valid */
		if (*ptr == STREND && (permArray[i].id = atoi( idStr )) > 0)
			(void) strcpy( permArray[i].type, type ) ;
		else
			input_error_exit( callingCmd ) ;	/* EXITs */
	}
	if (i < *numPerms)
		input_error_exit( callingCmd ) ;	/* EXITs */

	return (permArray) ;
}


/*==============================================================================
Function:       input_error_exit

Description:    Prints error message to stderr and exits if the
				input stream contains incorrect data.

Parameters:     callingCmd - the cmd (argv[0]) of the calling process

Returns:        None

Creator:        Teresa McKillop

Creation Date:  01/08/97

Notes:		
==============================================================================*/
static void
input_error_exit( char *callingCmd )
{
	(void) fprintf( stderr,
			"%s: ** QUITTING **, incorrect input (stdin) data\n",
			callingCmd ) ;
	exit ( APS_EXIT_ERROR ) ;
}

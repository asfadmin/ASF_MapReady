#ifndef PRCAPSFILE_MAIN_H
#define PRCAPSFILE_MAIN_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	prcapsfile_main.h
Description:	
Creator:	Teresa McKillop
Notes:		
==============================================================================*/
#pragma ident	"@(#)prcapsfile_main.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.prcapsfile_main.h"

#include "apsfiledef.h"	/* needed for MAX_RPT_TYPE_LEN */

/*==============================================================================
**	Constants/Macros
==============================================================================*/

#define FP_MAX_CMD_SIZE		1024	/* max len. of child cmd string */
#define FP_MAX_FILE_LINE	1024	/* max len. of line in fp files' info file*/
#define FP_FILE_SEPARATORS	" 	"	/*separators bet. items in fp files' lines*/

#define READ_END			0		/* read end of a 'pipe' */
#define WRITE_END			1		/* write end of a 'pipe' */

#define CURR_DIR_STR		"./"	/* path value if would have been NULL */


/*==============================================================================
**	GLOBAL STRUCT DEFINITIONS
==============================================================================*/

/*
-- structure of info for each file (format for file records is
-- these elements, in the order given and space separated)
*/
typedef
	struct FP_FILE
	{
		char *filename;			/* filename portion only (no path elements) */
		char *path;				/* full dir path, incl. ending slash; or "./" */
		char *fileProcName;		/* file processor for ingesting this file */
		char *fileType;			/* mnemonic file type */
	}	FP_FILE;

/*
-- structure of active file processing permission records
*/
typedef
	struct PERMREC
	{
		int		id ;						/* permission id */
		char	type[MAX_RPT_TYPE_LEN + 1] ;	/* file type */
	} PERMREC ;


/*==============================================================================
    Function Prototypes
==============================================================================*/
PERMREC	*get_active_permissions( char *callingCmd, int *numPerms ) ;

#endif /* PRCAPSFILE_MAIN_H */

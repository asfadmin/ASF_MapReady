#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		apsfilefunc.c

Description:	contains functions used with the apsfile reports

External Functions Defined:
				create_aps_meta_file
				cb_make_REPORT_type_option_menu
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)apsfilefunc.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.apsfilefunc.c"

#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>

#include "timeconv.h"

#include "apsfiledef.h"
#include "gui_utils.h"

extern void popup_message() ;
extern char display_string[] ;


/*==============================================================================
Function:		create_aps_meta_file

Description:	Creates a metadata file to be sent when transfering
            	data files.

Parameters:

Returns:

Creator:		Ron Green

Creation Date:	12/19/1994

Notes:
==============================================================================*/
int create_aps_meta_file(
	REPORT *apsfile, char *filename, char *start, char *stop)
{

#define HEADER_LENGTH 50
#define METAMSGFILE_FILENAME_OFFSET 94

	FILE *fp ;
	int nchars ;
	char metamsgfile_header[50+1] ;
	char metamsgfile_record[44+1] ;
	char year[5], date[17] ;
	char msgfile[] = "XX.MSG" ;
	char *fname ;

    if (!tc_systime2yr_date(year, date))
    {
        gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
            "Unable to convert systime" ) ;
        return(FALSE) ;
    } ;


	(void) sprintf(metamsgfile_header, "%-5.4s%-17.16s%-3.2sACS APS",
		year, date, apsfile->metacode)  ;

	nchars = strlen(metamsgfile_header) ;
	(void) sprintf(metamsgfile_header, "%s%*s",
		metamsgfile_header, HEADER_LENGTH - nchars, blank_str) ;

	nchars = strlen(metamsgfile_header) ;

	(void) sprintf(metamsgfile_record, "%-5.4s%-17.16s%-5.4s%-17.16s",
        start, start+5, stop, stop+5) ;

	(void) sprintf(msgfile, "%s.%s", apsfile->metacode, "MSG") ;

	/* open the metadata file */
	if ((fp = fopen(msgfile, "w")) == NULL)
	{
		(void) sprintf( display_string,
			"Error opening Metadata MSG file: %s ", msgfile ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(FALSE) ;
	}

	/*
	-- ensure that only the actual file name is contained
	-- in the meta message file, not the full pathname
	*/
	fname = strrchr(filename, '/') ;
	if (fname)
		fname++ ;
	else
		fname = filename ;

	(void) fprintf(fp, "%s%s", metamsgfile_header, metamsgfile_record) ;
	(void) fprintf(fp, "%-14s\n", fname) ;

	(void) fclose(fp) ;

	return(TRUE) ;
}

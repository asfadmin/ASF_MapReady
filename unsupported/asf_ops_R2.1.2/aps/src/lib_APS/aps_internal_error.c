#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       aps_internal_error

Description:    Prints out the input arguments which are source file, 
line number, and a message.  

Parameters:     
	FILE	*output_file,
	char	*source_file,
	int		line_number,
	char	*error_message );

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Fri May 19 18:21:36 PDT 1995

Notes:		
	output_file is an argument for flexibility, to send to stdout or stderr...
==============================================================================*/
#pragma ident	"@(#)aps_internal_error.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.aps_internal_error.c"

#include <stdio.h>         /* for FILE, fprintf...    */

void aps_internal_error(
	FILE	*output_file,
	char	*source_file,
	int		line_number,
	char	*error_message )
{
	fprintf(output_file, 
		"APS INTERNAL ERROR AT SOURCE FILE:  %s  LINE:  %d\n%s\n",
		source_file, line_number, error_message ) ;
	return ;
}

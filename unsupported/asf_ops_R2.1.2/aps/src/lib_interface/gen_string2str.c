#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		gen_string2str

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
==============================================================================*/
#pragma ident	"@(#)gen_string2str.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.gen_string2str.c"

#include <stdlib.h>
#include <string.h>
#include "dapps_defs.h"  /* for TRUE, FALSE */

/*==============================================================================
Function:       gen_string2str

Description:    translate input string into a duplicate string

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *result_string      duplicate of string

Returns:        
Type          Name              Definition
int                             TRUE = successful string duplication.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Fri May 19 14:26:39 PDT 1995

Notes:      
==============================================================================*/
int gen_string2str(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string)
{
    strcpy(result_string, source_string);

#ifdef PRINT_DIAG
        printf("%s(%d):  gen_string2str() value = %s\n", 
            __FILE__, __LINE__, result_string ) ;
#endif

    if ( !strcmp(result_string,source_string) )
        return(TRUE);
    else
        return(FALSE);
}

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		gen_field_is_numeric.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
==============================================================================*/
#pragma ident	"@(#)gen_field_is_numeric.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.gen_field_is_numeric.c"
#include <string.h>
#include <ctype.h>       /* for isspace()   */
#include "dapps_defs.h"  /* for TRUE, FALSE */

/*==============================================================================
Function:       gen_field_is_numeric()

Description:    returns TRUE if the field is numeric.  leading 
                blanks are OK; imbedded or trailing blanks are not OK.  

Creator:        Lawrence Stevens

Creation Date:  Fri Jan  5 14:28:13 PST 1996

==============================================================================*/
int gen_field_is_numeric(
    char        *field,     /* input numeric field to test.    */
    int         length )    /* how many bytes we are to test.  */
{
    char    *field_end = field + (length - 1) ;

    while( field <= field_end )
    {
        /* 
        -- skip the starting 
        -- blanks:  
        */
        if ( ! isspace( *field )  )
        {
            /* 
            -- the first non-blank; this character 
            -- and all the rest are supposed to 
            -- be decimal digits.  
            */
            while( field <= field_end )
                if ( ! isdigit( *field++ )  )
                    return FALSE ;
        }

        field++ ;
    }

    return TRUE ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       decode_error_message

Description:    based on the error code value, it uses the 
				right macro to decode the string.  

Parameters:     error code.  < 0.  

Returns:        int

Creator:        Lawrence Stevens

Creation Date:  Fri Jun 16 14:38:57 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)decode_error_message.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.decode_error_message.c"

#include "dtkm_utilities.h"
#include "phase_utilities.h"
#include "string.h"            /* for strcpy()  */

int decode_error_message( 
	int error_code,       /* input error code < 0  */
	char *message )       /* output error message  */
{

	(void)strcpy(message, "" ) ;

	if ( error_code >= 0 )
		return ERROR_CODE_NOT_LT_ZERO ;

	if ( error_code > -1000 )
	{
		(void)strcpy(message, DTKM_ERROR_MESSAGE(error_code) ) ;
		return DECODE_ERROR_MESSAGE_OK ;
	}

	if ( error_code > -2000 )
	{
		(void)strcpy(message, PHASE_ERROR_MESSAGE(error_code) ) ;
		return DECODE_ERROR_MESSAGE_OK ;
	}

	return ERROR_CODE_UNKNOWN ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       asftime2rev

Description:    given a satellite asftime, compute rev number.

Parameters:     
int phase_asftime2rev(
	char		*sat,            input satellite
	char		*asftime,   	 input asftime                            
	int			*rev )  		 output rev that contains the input time 

Returns:        int
	>= 0 :  No error:
		PHASE_ASFTIME2REV_OK 

	< 0  :  Error:
		PHASE_BAD_ASFTIME 
		PHASE_ERROR_IN_PHASE_START_TIME 
		PHASE_INPUT_ASFTIME_NOT_WITHIN_INPUT_PHASE 

Creator:        Lawrence Stevens

Creation Date:  Wed Jul 12 19:12:48 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)asftime2rev.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.asftime2rev.c"

#include "phase_utilities.h"
#include "timeconv.h"
#include <string.h>

int asftime2rev(
	char		*sat,
	char		*asftime,   	/* input asftime                            */
	int			*rev )  		/* output rev that contains the input time  */
{

	int			return_code ;
	DB_RECORD	**phase_rec ;

	/* get correct phase record.  */
	return_code = asftime_2_phase( sat, asftime, &phase_rec ) ;
	if ( return_code < 0 )
		return return_code ;

	/* use the phase record in call to get the rev number.  */
	return_code = phase_asftime2rev( phase_rec, asftime, rev ) ;
	free_db_record(phase_rec) ;

	if ( return_code < 0 )
		return return_code ;

	return PHASE_ASFTIME2REV_OK ;

}

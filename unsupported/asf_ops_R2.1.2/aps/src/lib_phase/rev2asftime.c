#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       rev2asftime

Description:    given a satellite asftime, compute rev number.

Parameters:     

Returns:        int
	>= 0 :  No error:
		PHASE_REV2ASFTIME_OK 

	< 0  :  An error ocurred, diagnosed by a called routine.

Creator:        Lawrence Stevens

Creation Date:  Wed Jul 12 19:12:32 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)rev2asftime.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.rev2asftime.c"


#include "phase_utilities.h"
#include "timeconv.h"

int rev2asftime(
	char		*sat,
	int			rev,   				/* input rev */
	char        *asftime_start_rev, /* output ASF time for rev start.  */
	char        *asftime_end_rev )  /* output ASF time for rev end.  */
{

	int			return_code ;
	DB_RECORD	**phase_rec ;

	/* get correct phase record.  */
	return_code = rev_2_phase( sat, rev, &phase_rec ) ;
	if ( return_code < 0 )
		return return_code ;

	/* use the phase record in call to get the asftimes.  */
	return_code = phase_rev2asftime( phase_rec, rev, 
		asftime_start_rev, asftime_end_rev ) ;
	free_db_record(phase_rec) ;

	return return_code ;

}

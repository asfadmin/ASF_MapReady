#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_first_rev

Description:    computes first rev in phase from phase record.  

Parameters:     
int phase_first_rev( 
	 DB_RECORD   **phase_rec,    phase record which has the data to use. 
	 int		*first_rev_in_phase )

Returns:        
	>= 0:  No error:
		PHASE_FIRST_REV_OK 
	<  0:  Error:
		PHASE_POINTER_IS_NULL

Creator:        Lawrence Stevens

Creation Date:  Wed Jun 14 15:24:46 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_first_rev.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_first_rev.c"

#include "phase_utilities.h"

int phase_first_rev( 
	 DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
	 int		*first_rev_in_phase )
{
	if ( phase_rec == NULL )
		return PHASE_POINTER_IS_NULL ;

	*first_rev_in_phase 
		= CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] 
			- CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] + 1  ;

	return PHASE_FIRST_REV_OK ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_rev_in_phase

Description:    given a phase record, checks input rev vs valid values 

Parameters:     
int phase_rev_in_phase( 
	 DB_RECORD   **phase_rec,    phase record which has the data to use. 
	 int		rev )

Returns:        int
	TRUE   if the input rev is within the phase.  
	FALSE  if there is any problem whatsoever.  


Creator:        Lawrence Stevens

Creation Date:  Wed Jun 14 14:37:01 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_rev_in_phase.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_rev_in_phase.c"

#include "phase_utilities.h"
int phase_rev_in_phase( 
	 DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
	 int		rev )
{

	int		return_code ;
	int		first_rev_in_phase ;

	/* 
	-- check against the first rev in the phase. remember the -1 when 
	-- computing
	*/
	if ( phase_rec == NULL )
		return FALSE ;

	return_code = phase_first_rev( phase_rec, &first_rev_in_phase ) ;
	if( return_code < 0 )
		return FALSE ;

	if ( rev < first_rev_in_phase )
		return FALSE ;

	/* 
	-- check against the last rev in the phase. 
	*/
	if ( rev > CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] )
		return FALSE ;

	return TRUE ;
}

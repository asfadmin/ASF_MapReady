#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_rev2asftime

Description:    given a phase record and rev number, compute start 
				and end ASF 21-character times for this rev.  

Parameters:     
int phase_rev2asftime(
	DB_RECORD	**phase_rec,    	 phase record which has the data 
	int			rev ,  				 input rev                      
	char		*asftime_start_rev,	 output ASF time for rev start. 
	char		*asftime_end_rev )	 output ASF time for rev end.   

Returns:        int
	>= 0:  No error.
		PHASE_REV2ASFTIME_OK 

	< 0:   An error ocurred, diagnosed by a called routine.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun 14 15:12:09 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_rev2asftime.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_rev2asftime.c"


#include "phase_utilities.h"
#include "timeconv.h"
#include "string.h"              /* for strcpy() */

int phase_rev2asftime(
	DB_RECORD	**phase_rec,    	/* phase record which has the data */
	int			rev ,  				/* input rev                       */
	char		*asftime_start_rev,	/* output ASF time for rev start.  */
	char		*asftime_end_rev )	/* output ASF time for rev end.    */
{

	int			return_code ;

	double		et_start_rev ;
	double		et_end_rev ;

	/* initialize the output values to unusable.  */
	(void)strcpy(asftime_start_rev, "" ) ;
	(void)strcpy(asftime_end_rev, "" ) ;

	return_code = phase_rev2et( phase_rec, rev, &et_start_rev, &et_end_rev ) ; 
	if ( return_code < 0 )
		return return_code ;

	/* 
	-- since we returned from phase_rev2et() ok, then we 
	-- assume no errors:  
	*/
	if (!tc_et2asf(et_start_rev, asftime_start_rev ) )
		return PHASE_ERROR_ET2ASF ;
	if (!tc_et2asf(et_end_rev,   asftime_end_rev ) )
		return PHASE_ERROR_ET2ASF ;

	return PHASE_REV2ASFTIME_OK ;

}

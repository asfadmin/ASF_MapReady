#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_asftime2rev

Description:    given a phase record and asftime, compute rev number.

Parameters:     
int phase_asftime2rev(
	DB_RECORD	**phase_rec,     phase record which has the data to use.  
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

Creation Date:  Wed Jun 14 09:27:51 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_asftime2rev.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_asftime2rev.c"

#include "phase_utilities.h"
#include "timeconv.h"

int phase_asftime2rev(
	DB_RECORD	**phase_rec,    /* phase record which has the data to use.  */
	char		*asftime,   	/* input asftime                            */
	int			*rev )  		/* output rev that contains the input time  */
{

	double		et_input ;
	double		et_phase_start ;
	double		et_phase_end ;
	double		time_for_one_rev_days ;

	int			first_rev_in_phase ;
	int			revs_since_phase_start ;


	/* initialize the output value to unusable.  */
	*rev = -1 ;

	/* check the inputs.  */
	if (! tc_asf2et(asftime, &et_input) )
		return PHASE_BAD_ASFTIME ;

	/* 
	-- compute phase time bracket and then check input time     
	*/
	if (! tc_asf2et( CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], 
					 &et_phase_start  ) )
		return PHASE_ERROR_IN_PHASE_START_TIME ;

	time_for_one_rev_days = 
		(double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS]
		/ CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

	et_phase_end = et_phase_start 
		+ ( time_for_one_rev_days 
			* CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] ) ;

	if ( et_input < et_phase_start || et_input > et_phase_end )
		return PHASE_INPUT_ASFTIME_NOT_WITHIN_INPUT_PHASE ;

	/* 
	-- compute number of complete revs from phase start to 
	-- the input time.  use implicit integer truncation.  
	*/
	revs_since_phase_start = 
		( et_input - et_phase_start ) / time_for_one_rev_days ;

	/* 
	-- get first rev in the phase, add to revs_since_phase_start 
	-- to yield the output rev number.  
	*/
	/* 
	-- remember to add 1 to (last rev - number of revs) when 
	-- computing first rev number.  
	*/
	(void) phase_first_rev(phase_rec, &first_rev_in_phase ) ;

	*rev = first_rev_in_phase + revs_since_phase_start ;

	return PHASE_ASFTIME2REV_OK ;

}

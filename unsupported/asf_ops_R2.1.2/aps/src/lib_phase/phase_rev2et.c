#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_rev2et

Description:    given a phase record and rev number, compute start 
				and end ephemeris times for this rev.  

Parameters:     
int phase_rev2et(
	DB_RECORD	**phase_rec,     phase record which has the data to use. 
	int			rev ,  			 input rev                              
	double		*et_start_rev,	 output ephemeris time for rev start.  
	double		*et_end_rev )	 output ephemeris time for rev end.   

Returns:       int 
	> 0:  No error: PHASE_REV2ET_OK 
	< 0:  Errors:
			PHASE_REV_NUMBER_NOT_IN_PHASE
			PHASE_ERROR_IN_PHASE_START_TIME
			PHASE_POINTER_IS_NULL

Creator:        Lawrence Stevens

Creation Date:  Wed Jun 14 15:12:09 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_rev2et.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_rev2et.c"


#include "phase_utilities.h"
#include "timeconv.h"

int phase_rev2et(
	DB_RECORD	**phase_rec,    /* phase record which has the data to use.  */
	int			rev ,  			/* input rev                                */
	double		*et_start_rev,	/* output ephemeris time for rev start.     */
	double		*et_end_rev )	/* output ephemeris time for rev end.       */
{

	int			return_code ;

	double		et_phase_start ;
	double		delta_time_days ;
	double		time_for_one_rev_days ;

	int			first_rev_in_phase ;
	int			revs_since_phase_start ;

	/* initialize the output values to unusable.  */
	*et_start_rev = -1.1 ;
	*et_end_rev   = -1.1 ;

	/* check the inputs.  */
	if (phase_rec == NULL)
		return PHASE_POINTER_IS_NULL ;

	/* get first rev in phase, check for error   */
	return_code = phase_first_rev( phase_rec, &first_rev_in_phase ) ;
	if (return_code < 0 )
		return return_code ;

	if (!phase_rev_in_phase( phase_rec, rev ) )
		return PHASE_REV_NUMBER_NOT_IN_PHASE ;

	/*
	-- continue to gather info to use in computations.  
	-- first_rev_in_phase, revs_since_phase_start, 
	-- et_phase_start, time_for_one_rev_days
	*/

	revs_since_phase_start = rev - first_rev_in_phase ;

	/* start time of phase */
	if (!tc_asf2et( CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], 
					&et_phase_start  ) )
		return PHASE_ERROR_IN_PHASE_START_TIME ;

	time_for_one_rev_days = 
		(double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS]
		/ CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

	/* 
	-- compute delta time from phase start to start of 
	-- input rev.  
	*/
	delta_time_days = revs_since_phase_start * time_for_one_rev_days ;

	/* assign output argument values:  */
	*et_start_rev = et_phase_start + delta_time_days ;
	*et_end_rev = *et_start_rev + time_for_one_rev_days ;

	/* 
	-- actually, et_end_rev is the start time of the 
	-- next rev.  subtract 1/2 of a millisecond from the end time
	-- so that the end of one rev and the start of another rev 
	-- will be a non-zero but very small amount of time, 
	-- so that it will show up when converted to asftime 
	-- as a millisecond apart.  
	*/
	*et_end_rev -= (double) 0.5 / 24 / 60 / 60 / 1000 ;

	return PHASE_REV2ET_OK ;

}

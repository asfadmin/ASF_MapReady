#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       rev_2_phase()

Description:    given satellite and rev number, it returns the 
DB_RECORD corresponding to the phase that includes the rev.  

Parameters:     

Returns:        
	>= 0   :  Condition codes; the input time is compared to the 
			time brackets for each of the phases for the satellite:  
		PHASE_INPUT_REV_BEFORE_ALL_PHASES 
		PHASE_INPUT_REV_WITHIN_A_PHASE 
		PHASE_INPUT_REV_BETWEEN_PHASES 
		PHASE_INPUT_REV_AFTER_ALL_PHASES 

	< 0    :  Error codes:  
		DB_QUERY_FAILED
		NO_PHASE_RECS_FOUND
		BAD_PHASE_REC_START_TIME
		TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME
		TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS
		TWO_PHASE_RECS_OVERLAP_IN_TIME

Creator:        Lawrence Stevens

Creation Date:  Mon Jun 26 17:23:39 PDT 1995

Notes:		
Don't pre-fabricate a DB_RECORD for a the phase record to find.  
This routine CREATES a DB_RECORD whose pointer is returned to the 
calling routine.  

==============================================================================*/
#pragma ident	"@(#)rev_2_phase.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.rev_2_phase.c"



/* FOR SYBASE INTERFACES */
#include "db_sybint.h"       /* for APS sybase interface routines    */
#include "aps_db_table.h"    /* for APS DB tables sybase interface   */
#include "dapps_list.h"      /* for APS linked list macros           */
#include "dapps_defs.h"      /* for MIN_REV, MAX_REV                 */

/* FOR NASDA UTILITIES   */
#include "phase_utilities.h"

/* FOR TIME CONVERSION   */
#include "timeconv.h"

int rev_2_phase(       
	char			*sat,
	int				rev_input,            /* input rev for desired phase.  */
	DB_RECORD		***phase_rec_found )  /* pointer to a new DB_RECORD  */
{

	int			return_code ;

	DB_RECORD	**phase_rec_to_return ;
	DB_RECORD	**phase_rec ;
	llist		*phase_list = NULL ;
	cursor		phase_list_ptr ;

	/* 
	-- counters that start at zero; they help to establish 
	-- return code conditions. 
	*/
	int		rev_input_after_start_of_a_phase_count = 0 ;
	int		rev_input_before_end_of_a_phase_count = 0 ;
	int		rev_input_within_a_phase_count = 0 ;

	/* computed results from current phase record:  */
	int		first_rev ;
	double	et_phase_start ;

	/* check for rev_input between MIN_REV and MAX_REV   */
	if( rev_input < MIN_REV  )
		return PHASE_REV_2_PHASE_INPUT_REV_TOO_LOW ;

	if( rev_input > MAX_REV )
		return PHASE_REV_2_PHASE_INPUT_REV_TOO_HIGH ;


	return_code = phase_get_list(sat, &phase_list ) ;
	if ( return_code < 0 )
		return return_code ;

	/* check each record in order retrieved.  */
	phase_rec = (DB_RECORD **) FIRST(phase_list, phase_list_ptr) ;
	do 
	{
		return_code = phase_first_rev( phase_rec, &first_rev ) ;
		if ( return_code < 0 )
			return return_code ;

		if (!tc_asf2et( CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START],
			             &et_phase_start ) )
		{
			DEL_LIST( phase_list ) ;
			return PHASE_ERROR_IN_PHASE_START_TIME ;
		}

		/*
		-- in case of conditions where the input rev_input is before 
		-- all of the phases, or where the input rev_input is between 
		-- two phases, or where the input rev_input is after all phases,
		-- we need to return the right DB_RECORD for the phase.  
		-- for a start, we set some flags for the input rev_input 
		-- being before, during, or after the current phase.  
		*/

		if ( rev_input >= first_rev )
			rev_input_after_start_of_a_phase_count ++ ;

		/* 
		-- check to see if we haven't yet got to the desired record 
		-- and keep going if not; it is safe to skip this record now.
		*/
		if ( CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] < rev_input )
			continue ;

		if ( rev_input <= CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] )
		{
			rev_input_before_end_of_a_phase_count ++ ;
			if ( rev_input_before_end_of_a_phase_count == 1 )
			{
				/*
				-- input rev is within this phase record; this is the
				-- first such phase record.  
				--
				-- we always return the FIRST phase_rec in 
				-- which the input ephemeris time (rev_input) 
				-- is before the phase end time.  
				--
				-- (the only exception is when the rev_input is 
				-- after all of the phase ends.  In that case, 
				-- the last phase_rec is returned.  )
				*/
				phase_rec_to_return = phase_rec ;
			}
		}

		/* 
		-- check this phase record to see if it is the 
		-- desired record, i.e., the input rev_input is within 
		-- this phase.  
		*/
		if ( rev_input >= first_rev 
		&&   rev_input <= CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] )
			rev_input_within_a_phase_count ++ ;

	}  while (  (phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr))
			 != NULL ) ;

	/* 
	-- set up special storage for the phase record to be 
	-- returned.  set the return value of the phase_rec_found 
	-- parameter to this address.  
	*/
	*phase_rec_found =  new_table_record(APS_CDEFS(PHASE)) ;

	/* 
	-- now determine the correct condition code for where rev_input fits 
	-- relative to the phase time brackets:
	-- 	PHASE_INPUT_REV_BEFORE_ALL_PHASES
	-- 	PHASE_INPUT_REV_WITHIN_A_PHASE
	-- 	PHASE_INPUT_REV_AFTER_ALL_PHASES
	-- and, by elimination, PHASE_INPUT_REV_BETWEEN_PHASES.
	*/

	if ( rev_input_before_end_of_a_phase_count == 0 )
	{
		/* PHASE_INPUT_REV_AFTER_ALL_PHASES:  we return the last phase rec.  */
		phase_rec_to_return = (DB_RECORD **) LAST(phase_list, phase_list_ptr) ;

		/* copy the last record to the special phase record.  */
		return_code = db_copy_record ( 
			APS_CDEFS(PHASE), *phase_rec_found, phase_rec_to_return ) ;

		/* CLEAN UP */
		DEL_LIST( phase_list ) ;

		return PHASE_INPUT_REV_AFTER_ALL_PHASES ;
	}

	/*
	-- in all other cases, we return the "phase_rec_to_return" 
	-- record selected earlier.  
	*/

	/* copy the selected record to the special phase record.  */
	return_code = db_copy_record ( 
		APS_CDEFS(PHASE), *phase_rec_found, phase_rec_to_return ) ;

	/* CLEAN UP */
	DEL_LIST( phase_list ) ;

	if ( rev_input_after_start_of_a_phase_count == 0 )
		return PHASE_INPUT_REV_BEFORE_ALL_PHASES ;

	if ( rev_input_within_a_phase_count == 1 )
		return PHASE_INPUT_REV_WITHIN_A_PHASE ;

	if ( rev_input_within_a_phase_count > 1 )
		return PHASE_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS ;

	/* 
	-- by elimination of PHASE_INPUT_REV_BEFORE_ALL_PHASES, 
	-- PHASE_INPUT_REV_WITHIN_A_PHASE, and PHASE_INPUT_REV_AFTER_ALL_PHASES, 
	-- there is only one logical possibility left, 
	-- PHASE_INPUT_REV_BETWEEN_PHASES
	*/

	return PHASE_INPUT_REV_BETWEEN_PHASES ;

}

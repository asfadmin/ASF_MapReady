#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_check_bumpability.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_bumpability.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_bumpability.c"


/*==============================================================================
Function:       dtkm_check_bumpability()

Description:    determine if the dtk_proposal, by the various rules and 
				priorities, can bump every conflicting data-take off of 
				the antenna.  

Returns:        
	>= 0 :
			DTKM_CAN_BUMP_DTKS
			DTKM_CANNOT_BUMP_DTKS

	< 0 :   Errors.  

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 18:27:50 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include "timeconv.h"  /* for ASF_TIME_STR_LENGTH  */

#include <string.h>			/* for strcmp()  */

int dtkm_check_bumpability(
	DBPROCESS	*APS_dbproc, 
	DB_RECORD	**dtk_proposal, 
	llist		*dtk_conflicts )  /* input list of conflicts.  */
{
	int		highest_conflict_priority = 0 ;
	int		dtk_proposal_priority = 0 ;
	int		return_code ;
	char	earliest_submit_time[ASF_TIME_STR_LENGTH+1] ;

	/*
	-- STEP 8.2.1
	-- Determine the highest priority of the other satellites
	-- which conflict with the dtk proposal.
	*/
#ifdef PRINT_DIAG
	printf("%s(%d):  STEP 8.2.1 Determine the highest priority\n", 
		__FILE__, __LINE__ ) ;
#endif
	return_code = dtkm_get_highest_antenna_priority(APS_dbproc, dtk_conflicts,
			&highest_conflict_priority ) ;
	if ( return_code < 0 )
		return return_code ;

	return_code = dtkm_get_antenna_priority( APS_dbproc, dtk_proposal, 
		&dtk_proposal_priority ) ;
	if ( return_code < 0 )
		return return_code ;

	/*
	-- STEP 8.2.2
	-- Compare the priority of the dtk proposal satellite with
	-- the highest priority from the previous step.
	*/
#ifdef PRINT_DIAG
	printf("%s(%d):  STEP 8.2.2 Compare dtk proposal priority\n", 
		__FILE__, __LINE__ ) ;
#endif
	if ( dtk_proposal_priority < highest_conflict_priority ) 
	{
#ifdef PRINT_DIAG
		printf("%s(%d):  STEP 8.2.2.1  the data-take can bump.  \n", 
			__FILE__, __LINE__ ) ;
#endif
		/* dtk_proposal has a HIGHER priority.  */
		return DTKM_CAN_BUMP_DTKS ;
	}
	else if ( dtk_proposal_priority == highest_conflict_priority )
	{
		/* dtk_proposal has an EQUAL priority.  */
		return_code = dtkm_get_earliest_submit_time( dtk_conflicts, 
			earliest_submit_time ) ;
		if ( return_code < 0 )
			return return_code ;

		if ( strcmp(CAST_DTK_SUBMIT_TIME dtk_proposal[DTK_SUBMIT_TIME], 
					earliest_submit_time ) < 0 )
		{
#ifdef PRINT_DIAG
			printf("%s(%d):  STEP 8.2.2.2  the data-take can bump\n", 
				__FILE__, __LINE__ ) ;
#endif
			/* 
			-- the dtk proposal has an earlier "submit time"
			-- than all of the highest priority data-takes
			*/
			return DTKM_CAN_BUMP_DTKS ;
		}
			else return DTKM_CANNOT_BUMP_DTKS ;
			
	}
	else
	{
		/* dtk_proposal has a LOWER priority.  */
		return DTKM_CANNOT_BUMP_DTKS ;
	}

}

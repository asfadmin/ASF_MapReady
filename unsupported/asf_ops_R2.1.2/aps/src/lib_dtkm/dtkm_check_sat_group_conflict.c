#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_check_sat_group_conflict.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_sat_group_conflict.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_sat_group_conflict.c"


/*==============================================================================
Function:       dtkm_check_sat_group_conflict()

Description:    check for antenna conflict with a group of downlink
				data-takes all from the same satellite.  
				downlink data-takes from the same satellite are always 
				handled in planning as a single data-take for conflict 
				checking purposes.  
				NOTE:  the dtk_sat_group should be in the same pass.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Nov 13 09:35:05 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"

#include <string.h>		/* for strlen strcmp    */

int dtkm_check_sat_group_conflict(
	llist	*dtk_sat_group,     /* a list of downlink data-takes from a sat  */
	char	*antenna_strttime,  /* time bracket to use for dtk_proposal  */
	char	*antenna_stoptime,
	llist	*dtk_conflicts )    /* if conflicting, add to this list.  */
{

	int		return_code ;
	llist	*list_check ;

	/* this is the sat group time bracket initialized.  */
	char	sat_group_strttime[] = "2100:001:00:00:00.000" ;
	char	sat_group_stoptime[] = "1900:001:00:00:00.000" ;

	/* quick, superficial error checking  */
	if ( dtk_sat_group == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

	if ( NUMELTS( dtk_sat_group ) <= 0 )
		return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

	if ( strlen(antenna_strttime) != ASF_TIME_STR_LENGTH )
		return DTKM_ERROR_NO_STRTTIME ;

	if ( strlen(antenna_stoptime) != ASF_TIME_STR_LENGTH )
		return DTKM_ERROR_NO_STOPTIME ;

	if ( dtk_conflicts == NULL )
		return DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED ;

	/* 
	-- determine the sat group time bracket.  
	-- the dtk_sat_group MUST have something in it, 
	-- for this to have meaning for us.  
	-- already checked in this routine.  
	*/
	return_code = dtkm_time_bracket_dtk_list( dtk_sat_group, 
		sat_group_strttime, sat_group_stoptime ) ;

	if ( return_code < 0 )
		return return_code ;

	/* one more check:  */
	if ( strcmp(sat_group_strttime, sat_group_stoptime ) >= 0 )
		return DTKM_ERROR_EXPANDING_TIME_BRACKET ;

	/* 
	-- check for an overlap of sat_group time bracket and 
	-- antenna_time_bracket.  note the strange but efficient 
	-- comparisons between start and stop times.  
	*/
	if ( strcmp( sat_group_strttime, antenna_stoptime ) < 0 
	&&   strcmp( sat_group_stoptime, antenna_strttime ) > 0  )
	{
		/* 
		-- the time brackets overlap.  move these data-takes 
		-- out of the dtk_sat_group list and into 
		-- the dtk_conflicts list.  
		*/
		list_check = db_record_llist_move( dtk_conflicts, dtk_sat_group ) ;
		if ( list_check != dtk_conflicts )
			return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
	}

	return TRUE ;

}

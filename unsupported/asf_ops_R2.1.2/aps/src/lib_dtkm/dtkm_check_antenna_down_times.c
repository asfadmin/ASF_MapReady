#undef PRINT_DIAG
#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_check_antenna_down_times.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_antenna_down_times.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_antenna_down_times.c"



/*==============================================================================
Function:       dtkm_check_antenna_down_times()

Description:    checks to see if the antenna is down, preventing the 
				dtk proposal from getting onto the schedule.  
				it does this by retrieving the down times from the 
				database, if any.  

Returns:        TRUE, or an error message < 0

Creator:        Lawrence Stevens

Creation Date:  Sat Nov 11 16:31:01 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include "db_antenna_down_times.h"

#include <string.h>		/* for strlen()  */

int dtkm_check_antenna_down_times( 
	DBPROCESS	*APS_dbproc,
	DB_RECORD	**dtk_proposal,
	char 		*antenna_strttime, /* time bracket for which the antenna */
	char 		*antenna_stoptime, /* is needed.                         */
	llist		*antenna_down_times_list )
{
	llist		*retrievals = NULL ;
	llist		*list_check = NULL ;

	/* quick error checking.  */
	if ( dtk_proposal == NULL )
		return DTKM_ERROR_NULL_DTK_PROPOSAL ;

	if ( strlen(antenna_strttime) != ASF_TIME_STR_LENGTH ) 
		return DTKM_ERROR_NO_STRTTIME ;

	if ( strlen(antenna_stoptime) != ASF_TIME_STR_LENGTH ) 
		return DTKM_ERROR_NO_STOPTIME ;

	if ( antenna_down_times_list == NULL )
		return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_INITIALIZED ;

	if ( NUMELTS( antenna_down_times_list ) != 0 )
		return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY ;

	/*
	-- look in the antenna_down_times relation for a time 
	-- overlapping record for the dtk_proposal station, antenna.
	-- use the "same_sat" time bracket.  
	-- note the strange comparision between start and stop times; 
	-- this is the most direct way to make the overlapping test.  
	*/
	sprintf(where_clause, 
		"where %s = '%s' and %s = %d and %s < '%s' and %s > '%s'", 
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STATION_ID), 
				CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID], 
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_ANTENNA_ID), 
				CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID], 
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STRTTIME), 
				antenna_stoptime, 
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STOPTIME), 
				antenna_strttime ) ;

#ifdef PRINT_DIAG
	printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, where_clause) ;
#endif

	/* get records, sort by ANTENNA_DOWN_TIMES_STRTTIME */
	retrievals = db_get_records( APS_dbproc, 
		APS_TABLE(ANTENNA_DOWN_TIMES), where_clause, 
		APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STRTTIME),
		APS_CDEFS(ANTENNA_DOWN_TIMES), ALL_COLS) ;

	if ( retrievals == NULL )
		return DTKM_ERROR_DB_QUERY_FAILED ;

	/* move the retrievals to the output list, already initialized.  */
	list_check = db_record_llist_move( antenna_down_times_list, retrievals ) ;
	if ( list_check != antenna_down_times_list )
		return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
	DEL_LIST( retrievals ) ;

#ifdef PRINT_DIAG
	printf("%s(%d):  NUMELTS( antenna_down_times_list ) = %d\n", 
		__FILE__, __LINE__, NUMELTS( antenna_down_times_list )  ) ;
#endif


	return TRUE ;

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

#ifdef THIS_IS_OBSOLETE_CODE 

/*==============================================================================
Filename:	dtkm_get_antenna_steering_time.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_antenna_steering_time.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_antenna_steering_time.c"



/*==============================================================================
Function:       dtkm_get_antenna_steering_time()

Description:    gets the antenna steering time in seconds for the data-take, 
				based on station_id and antenna_id. 

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 18:55:52 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_antenna.h"
#include "db_dtk.h"

#include <string.h>		/* for strcmp()   */

int dtkm_get_antenna_steering_time( 
	DBPROCESS	*APS_dbproc,
	DB_RECORD	**dtk_proposal,
	int 		*antenna_steering_time_sec ) 
{
	static llist	*antenna_list = NULL ;
	DB_RECORD		**antenna_rec = NULL ;
	cursor			antenna_list_ptr ;

	/* 
	-- initialize to a value that will cause really bad 
	-- problems soon for a routine that does not check 
	-- the return code to see if there is an error.  
	*/
	*antenna_steering_time_sec = -100000 ;

	if ( antenna_list == NULL )
	{
		/* 
		-- retain this whole small relation in memory for 
		-- efficiency.  
		*/
		antenna_list = db_get_records(APS_dbproc, 
			APS_TABLE(ANTENNA), NULL, NULL, 
			APS_CDEFS(ANTENNA), ALL_COLS) ;
		if ( antenna_list == NULL )
			return DTKM_ERROR_DB_QUERY_FAILED ;
		if ( NUMELTS( antenna_list ) == 0 )
		{
			DEL_LIST( antenna_list ) ;
			antenna_list = NULL ;
			return DTKM_ERROR_NO_RECORDS_IN_ANTENNA_RELATION ;
		}
	}

	/* check each record:  */
	for (
		antenna_rec = (DB_RECORD **) FIRST(antenna_list, antenna_list_ptr ) ;
		antenna_rec ; 
		antenna_rec = (DB_RECORD **) NEXT(antenna_list, antenna_list_ptr ) 
		)
	{
		/* match on station_id and antenna_id */
		if ( strcmp(CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID] ,
					CAST_ANTENNA_STATION_ID
					antenna_rec[ANTENNA_STATION_ID] ) == 0 
		&&  CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID]
			== CAST_ANTENNA_ANTENNA_ID antenna_rec[ANTENNA_ANTENNA_ID] 
		   )
		{
			/* a match on STATION_ID, ANTENNA_ID   */
			*antenna_steering_time_sec = 
				CAST_ANTENNA_STEERING_TIME_SEC 
				antenna_rec[ANTENNA_STEERING_TIME_SEC] ;
			return TRUE ;
		}
	}
	return DTKM_ERROR_ANTENNA_STEERING_TIME_SEC_NOT_FOUND_FOR_DTK_PROPOSAL ;
}
#endif

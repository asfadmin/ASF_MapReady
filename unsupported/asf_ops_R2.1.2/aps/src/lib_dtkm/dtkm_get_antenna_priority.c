#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_get_antenna_priority.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_antenna_priority.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_antenna_priority.c"



/*==============================================================================
Function:       dtkm_get_antenna_priority()

Description:    gets the antenna priority for the data-take, based 
				on satellite, station_id, and antenna_id. 

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 18:55:52 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_antenna_priority.h"
#include "db_dtk.h"
#include <string.h>		/* for strcmp()    */

int dtkm_get_antenna_priority( 
	DBPROCESS	*APS_dbproc,
	DB_RECORD	**dtk_proposal,
	int			*priority )
{
	static llist	*antenna_priority_list = NULL ;
	DB_RECORD		**antenna_priority_rec = NULL ;
	cursor			antenna_priority_list_ptr ;

	*priority = 0 ;

	if ( CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID] == 0 )
		return DTKM_ERROR_ANTENNA_ID_IS_ZERO_NO_PRIORITY ;

	if ( antenna_priority_list == NULL )
	{
		antenna_priority_list = db_get_records(APS_dbproc, 
			APS_TABLE(ANTENNA_PRIORITY), NULL, NULL, 
			APS_CDEFS(ANTENNA_PRIORITY), ALL_COLS) ;
		if ( antenna_priority_list == NULL )
			return DTKM_ERROR_DB_QUERY_FAILED ;
		if ( NUMELTS( antenna_priority_list ) == 0 )
		{
			DEL_LIST( antenna_priority_list ) ;
			antenna_priority_list = NULL ;
			return DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PRIORITY_RELATION ;
		}
	}

	for (
		antenna_priority_rec = (DB_RECORD **) FIRST(antenna_priority_list, 
									antenna_priority_list_ptr ) ;
		antenna_priority_rec ; 
		antenna_priority_rec = (DB_RECORD **) NEXT(antenna_priority_list, 
									antenna_priority_list_ptr ) 
		)
	{
		/* match on station_id, antenna_id, and satellite */
		if ( 
			strcmp(CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID] ,
					CAST_ANTENNA_PRIORITY_STATION_ID 
					antenna_priority_rec[ANTENNA_PRIORITY_STATION_ID] )
			== 0 
		&&  CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID]
			== CAST_ANTENNA_PRIORITY_ANTENNA_ID 
				antenna_priority_rec[ANTENNA_PRIORITY_ANTENNA_ID] 
		&&	strcmp(CAST_DTK_SAT dtk_proposal[DTK_SAT] ,
					CAST_ANTENNA_PRIORITY_SAT 
					antenna_priority_rec[ANTENNA_PRIORITY_SAT] )
			== 0 
		   )
		{
			/* a match on STATION_ID, ANTENNA_ID, SAT  */
			*priority = CAST_ANTENNA_PRIORITY_PRIORITY 
						antenna_priority_rec[ANTENNA_PRIORITY_PRIORITY] ;
			return TRUE ;
		}
	}
	return DTKM_ERROR_ANTENNA_PRIORITY_NOT_FOUND_FOR_DTK_PROPOSAL ;
}

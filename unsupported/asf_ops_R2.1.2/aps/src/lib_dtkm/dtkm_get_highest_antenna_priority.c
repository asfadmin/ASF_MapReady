#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_get_highest_antenna_priority.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_highest_antenna_priority.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_highest_antenna_priority.c"



/*==============================================================================
Function:       dtkm_get_highest_antenna_priority()

Description:    find the highest antenna priority for data-takes 
				in a list.  
				priority 1 is tops; priority 2 is lower, etc.
				list of data-takes are all in the SAME station_id, antenna_id.

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 19:20:27 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"		/* for CAST_DTK_ANTENNA_ID, etc.  */

#include <string.h>		/* for strcpy strcmp   */

int dtkm_get_highest_antenna_priority(
	DBPROCESS	*APS_dbproc,
	llist		*dtk_list,
	int			*highest_antenna_priority )
{
	int 		priority ;
	int			return_code ;
	char		station_id[4] ;
	int			antenna_id ;

	DB_RECORD	**dtk_rec ;
	cursor		dtk_list_ptr ;

	*highest_antenna_priority = 1000 ;

	if ( dtk_list == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
	if ( NUMELTS( dtk_list ) == 0 )
		return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

	/* set up to check for a mixed list of data-takes.  */
	dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
	antenna_id = CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;
	strcpy( station_id, CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) ;

	/* check each record.  */
	for ( 	dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
		  	dtk_rec ;
			dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)   
		)
	{
		if ( strcmp( station_id, CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] )
			!= 0 
		||   antenna_id != CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] )
			return DTKM_ERROR_DTK_LIST_IS_MIXED_WITH_DIFFERENT_ANTENNAS ;

		return_code = dtkm_get_antenna_priority(APS_dbproc, dtk_rec,
				&priority ) ;
		if ( return_code < 0 )
			return return_code ;

		/* priority 1 is tops; priority 2 is lower, etc.  */
		if ( priority < *highest_antenna_priority )
			*highest_antenna_priority = priority ;
	}

	return TRUE ;

}

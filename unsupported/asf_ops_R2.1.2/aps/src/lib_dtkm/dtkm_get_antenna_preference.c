#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_get_antenna_preference.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_antenna_preference.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_antenna_preference.c"



/*==============================================================================
Function:       dtkm_get_antenna_preference()

Description:    gets the antenna preference for the data-take, based 
				on satellite, station_id, and antenna_id. 

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 18:55:52 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_antenna_pref.h"
#include "db_dtk.h"
#include <string.h>

int dtkm_get_antenna_preference( 
	DBPROCESS	*APS_dbproc,
	DB_RECORD	**dtk_proposal,
	int			*preference )
{
	static llist	*antenna_pref_list = NULL ;
	DB_RECORD		**antenna_pref_rec = NULL ;
	cursor			antenna_pref_list_ptr ;

	*preference = 0 ;

	if ( antenna_pref_list == NULL )
	{
		antenna_pref_list = db_get_records(APS_dbproc, 
			APS_TABLE(ANTENNA_PREF), NULL, NULL, 
			APS_CDEFS(ANTENNA_PREF), ALL_COLS) ;
		if ( antenna_pref_list == NULL )
			return DTKM_ERROR_DB_QUERY_FAILED ;
		if ( NUMELTS( antenna_pref_list ) == 0 )
		{
			DEL_LIST( antenna_pref_list ) ;
			antenna_pref_list = NULL ;
			return DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PREF_RELATION ;
		}
	}

	for (
		antenna_pref_rec = (DB_RECORD **) FIRST(antenna_pref_list, 
									antenna_pref_list_ptr ) ;
		antenna_pref_rec ; 
		antenna_pref_rec = (DB_RECORD **) NEXT(antenna_pref_list, 
									antenna_pref_list_ptr ) 
		)
	{
		/* match on station_id, antenna_id, and satellite */
		if ( 
			strcmp(CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID] ,
					CAST_ANTENNA_PREF_STATION_ID 
					antenna_pref_rec[ANTENNA_PREF_STATION_ID] )
			== 0 
		&&  CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID]
			== CAST_ANTENNA_PREF_ANTENNA_ID 
				antenna_pref_rec[ANTENNA_PREF_ANTENNA_ID] 
		&&	strcmp(CAST_DTK_SAT dtk_proposal[DTK_SAT] ,
					CAST_ANTENNA_PREF_SAT 
					antenna_pref_rec[ANTENNA_PREF_SAT] )
			== 0 
		   )
		{
			/* a match on STATION_ID, ANTENNA_ID, SAT  */
			*preference = CAST_ANTENNA_PREF_PREFERENCE 
						antenna_pref_rec[ANTENNA_PREF_PREFERENCE] ;
			return TRUE ;
		}
	}
	return DTKM_ERROR_ANTENNA_PREFERENCE_NOT_FOUND_FOR_DTK_PROPOSAL ;
}

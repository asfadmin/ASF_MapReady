#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif
#ifdef THIS_IS_OBSOLETE_CODE
/*==============================================================================
Filename:	dtkm_get_minmax_aoslos.c

Description:	

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_minmax_aoslos.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_minmax_aoslos.c"



/*==============================================================================
Function:       dtkm_get_minmax_aoslos()

Description:    get the min aos and max los times from the input list.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 14 16:47:36 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

int dtkm_get_minmax_aoslos(
	llist	*dtk_list,
	char	*min_aos_time,
	char	*max_los_time )
{
	DB_RECORD		**dtk_rec = NULL ;
	cursor			dtk_list_ptr ;

	/* quick error checking.  */
	if ( dtk_list == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

	if ( NUMELTS( dtk_list ) <= 0 )
		return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

	/* 
	-- start with rediculous values.  
	*/
	strcpy(min_aos_time, "2199:001:01:01:01.000" ) ;
	strcpy(max_los_time, "1950:001:01:01:01.000" ) ;

	for (
		dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
		dtk_rec ;
		dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)
		)
	{
		if (strcmp( min_aos_time, CAST_DTK_AOS_TIME dtk_rec[DTK_AOS_TIME] ) > 0)
			strcpy( min_aos_time, CAST_DTK_AOS_TIME dtk_rec[DTK_AOS_TIME] ) ;

		if (strcmp( max_los_time, CAST_DTK_LOS_TIME dtk_rec[DTK_LOS_TIME] ) < 0)
			strcpy( max_los_time, CAST_DTK_LOS_TIME dtk_rec[DTK_LOS_TIME] ) ;
	}

	return TRUE ;
}
#endif

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_get_earliest_submit_time.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_earliest_submit_time.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_earliest_submit_time.c"



/*==============================================================================
Function:       dtkm_get_earliest_submit_time()

Description:    goes through a dtk list, finding the earliest submit time.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Nov 13 14:31:36 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

#include <string.h>		/* for strcpy strcmp */

int dtkm_get_earliest_submit_time(
	llist	*dtk_list, 
	char	*earliest_submit_time )
{

	cursor		dtk_list_ptr ;
	DB_RECORD	**dtk_rec ;

	if ( dtk_list == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

	if ( NUMELTS( dtk_list ) <= 0 )
		return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

	/* initialize the output to insure updating on first record.     */
	strcpy( earliest_submit_time, "2100:001:00:00:00.000" ) ;

	for (
		dtk_rec = (DB_RECORD **) FIRST( dtk_list, dtk_list_ptr ) ;
		dtk_rec ;
		dtk_rec = (DB_RECORD **) NEXT( dtk_list, dtk_list_ptr ) 
		)
	{
		if ( strcmp(CAST_DTK_SUBMIT_TIME dtk_rec[DTK_SUBMIT_TIME],
					earliest_submit_time ) < 0 )
		{
			/* update the submit time; we found an earlier time.  */
			strcpy( earliest_submit_time, 
				CAST_DTK_SUBMIT_TIME dtk_rec[DTK_SUBMIT_TIME] ) ;
		}

	}
	return TRUE ;
}

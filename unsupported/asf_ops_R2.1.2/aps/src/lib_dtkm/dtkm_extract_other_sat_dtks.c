#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_extract_other_sat_dtks.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_extract_other_sat_dtks.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_extract_other_sat_dtks.c"



/*==============================================================================
Function:       dtkm_extract_other_sat_dtks()

Description:    reads from one list, selecting every data-take from a 
				satellite different from the one in the dtk_proposal.  
				selected ones are duplicated into the other_sat_dtks
				list.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Tue Dec  5 18:20:01 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>

int dtkm_extract_other_sat_dtks(
	DB_RECORD	**dtk_proposal,
	llist		*dtk_list,
	llist		*other_sat_dtks )
{

	cursor		dtk_list_ptr ;
	DB_RECORD   **dtk_rec ;
	llist		*list_check = NULL ;

	for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
			dtk_rec != NULL ;
			dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
		)
	{
		/* 
		-- process the current dtk_rec right here.  
		*/
		if ( strcmp( CAST_DTK_SAT dtk_proposal[DTK_SAT], 
					 CAST_DTK_SAT dtk_rec[DTK_SAT] ) != 0 )
		{
			/* this data-take sat is different.  */
			list_check = dtkm_duplicate_dtk_into_list( dtk_rec, 
				other_sat_dtks ) ;
			if ( list_check != other_sat_dtks ) 
				return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
		}
	}

	return TRUE ;

}

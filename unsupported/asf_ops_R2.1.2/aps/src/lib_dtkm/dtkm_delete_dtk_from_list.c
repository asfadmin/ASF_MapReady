#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_delete_dtk_from_list.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_delete_dtk_from_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_delete_dtk_from_list.c"

#include "dtkm.h"            
#include "db_dtk.h"            /* for dtk table             */

#include <string.h>		/* for strcmp strcpy		*/


/*==============================================================================
Function:		dtkm_delete_dtk_from_list

Description:  
	searches a list for a data-take.  Deletes all occurrences of it 
	from the list, based on sat/sensor/rev/dtkid.  

NOTE:
	if not found, it's OK.  

	a match is determined by sat, sensor, rev, dtkid being all the same.  

Parameters:		
	DB_RECORD **dtk_rec,         delete this rec from list, if found in  
	llist     *dtk_list          this list.                             

Returns:
	void

Creator:		Lawrence Stevens

Creation Date:	Tue Nov 28 17:03:37 PST 1995

Notes:		

==============================================================================*/
void dtkm_delete_dtk_from_list(
	DB_RECORD **dtk_rec,      /* delete this rec from list, if found in  */
	llist     *dtk_list  )    /* this list.                              */
{
	cursor    dtk_list_ptr ;
	DB_RECORD **dtk_list_rec = NULL ;

	/* 
	-- loop thru each dtk record in the list.
	*/

	if ( NUMELTS( dtk_list ) <=  0 )
		return ;


	for (
		dtk_list_rec = (DB_RECORD **) FIRST( dtk_list, dtk_list_ptr) ;
		dtk_list_rec != NULL ;
		dtk_list_rec = (DB_RECORD **) NEXT( dtk_list, dtk_list_ptr)
		)
	{
		if ( strcmp( CAST_DTK_SAT      dtk_rec[DTK_SAT],
		             CAST_DTK_SAT dtk_list_rec[DTK_SAT] ) == 0 
		&&   strcmp( CAST_DTK_SENSOR      dtk_rec[DTK_SENSOR],
		             CAST_DTK_SENSOR dtk_list_rec[DTK_SENSOR] ) == 0 
		&&   CAST_DTK_REV      dtk_rec[DTK_REV]
		  == CAST_DTK_REV dtk_list_rec[DTK_REV] 
		&&   CAST_DTK_DTKID      dtk_rec[DTK_DTKID]
		  == CAST_DTK_DTKID dtk_list_rec[DTK_DTKID]     )
		{
			/* 
			-- the input dtk_rec matches the dtk_list_rec.  
			-- now delete the dtk_list_rec from the list.
			-- also frees the memory.  
			*/
			DEL_AT_CURSOR( dtk_list, dtk_list_ptr ) ;
		}
	}

	return ;
}

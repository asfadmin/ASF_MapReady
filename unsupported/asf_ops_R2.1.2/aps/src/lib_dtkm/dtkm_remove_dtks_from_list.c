#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_remove_dtks_from_list.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_remove_dtks_from_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_remove_dtks_from_list.c"

#include "dtkm.h"            

/* FOR DATABASE TABLES        */
#include "db_dtk.h"            /* for dtk table             */

#include <string.h>		/* for strcmp strcpy		*/


/*==============================================================================
Function:       dtkm_remove_dtks_from_list()

Description:    if a data-take is found in the first list, 
				remove it from the second list if it is in there, 
				too.  
Returns:        

Creator:        Lawrence Stevens

Creation Date:  Wed Nov 15 17:59:23 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_remove_dtks_from_list( 
	llist	*dtk_list,
	llist	*remove_from_this_dtk_list )
{
	DB_RECORD	**dtk_rec ;
	cursor		dtk_list_ptr ;
	if ( dtk_list == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

	if ( remove_from_this_dtk_list == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

	if ( NUMELTS( dtk_list ) == 0 )
		return TRUE ;

	if ( NUMELTS( remove_from_this_dtk_list ) == 0 )
		return TRUE ;

	for ( dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
		  dtk_rec ;
		  dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr) )
	{
		dtkm_delete_dtk_from_list( dtk_rec, remove_from_this_dtk_list ) ;
	}

	return TRUE ;

}

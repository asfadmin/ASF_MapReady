#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		dtkm_copy_dtks_value.c

Description:    reads from one list, selecting every data-take with
				the same Value.  
				selected ones are DUPLICATED into the output dtk list.  

Creator:        Lawrence Stevens

Creation Date:  Tue Dec  5 18:20:01 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#pragma ident	"@(#)dtkm_copy_dtks_value.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_copy_dtks_value.c"

#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>

int dtkm_copy_dtks_value(
    int         col_number,     /* column to look at, e.g. DTK_STRTTIME       */
    void        *value_ptr,     /* &(field value); make sure it's long enough */
	llist		*dtk_list,
	llist		*copied_dtk_list )
{

	cursor		dtk_list_ptr ;
	DB_RECORD   **dtk_rec ;
	llist		*list_check = NULL ;

	/* error checking.  */
    if ( dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
 
	if ( col_number >= NUM_DTK_COLS )
		return DTKM_ERROR_COL_NUMBER_GT_NUM_DTK_COLS ;
 
    if ( value_ptr == NULL )
        return DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE ;
 
    if ( copied_dtk_list == NULL )
        return DTKM_ERROR_NULL_OUTPUT_DTK_LIST ;

	for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
			dtk_rec != NULL ;
			dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
		)
	{
		/*
        -- compare the value in the argument to the
        -- value in the DB_RECORD:
        */
        if (memcmp( dtk_rec[col_number], value_ptr, APS_SIZE(DTK, col_number) )
            == 0 )
        {
            /*
            -- the values are equal; copy
            -- this record; 
            */
			list_check = dtkm_duplicate_dtk_into_list( dtk_rec, 
				copied_dtk_list ) ;
			if ( list_check != copied_dtk_list ) 
				return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
		}
	} /* end of loop on dtk_list.  */

	return TRUE ;

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_extract_first_sat.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_extract_first_sat.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_extract_first_sat.c"



/*==============================================================================
Function:       dtkm_extract_first_sat()

Description:    accept a dtk_list of DB_RECORDs.  remove every data-take 
				involving the first satellite in the list; append each to 
				the dtk_sat_list.  

Returns:        int
			== TRUE if OK
			< 0 if error.  

Creator:        Lawrence Stevens

Creation Date:  Mon Nov 13 08:50:48 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>		/* for strcpy() strcmp() */

int dtkm_extract_first_sat(
	llist	*dtk_retrievals,
	llist	*dtk_sat_list )
{
	char		first_satellite[20] ;
	cursor		dtk_retrievals_ptr ;
	cursor		next_dtk_retrievals_ptr ;
	DB_RECORD	**dtk_rec ;
	DB_RECORD	**next_dtk_rec ;
	llist		*list_check ;

    /* quick error checking.  */
    if (dtk_retrievals == NULL)
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_retrievals) == 0 )
        return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;
 
    if (dtk_sat_list == NULL)
        return DTKM_ERROR_DTK_SAT_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_sat_list) != 0 )
        return DTKM_ERROR_DTK_SAT_LIST_NOT_EMPTY ;
 
	/* determine the first satellite.  */
	dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, dtk_retrievals_ptr);
	strcpy(first_satellite, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;

    /*
    -- check each record in the dtk_retrievals list, move it to
    -- dtk_sat_list if the satellite is the same as first_satellite.
    */
    dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, dtk_retrievals_ptr);
    while ( dtk_rec )
    {
        /*
        -- test the dtk_rec vs first_satellite for equality.  
        */
        if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], first_satellite ) == 0 )
		{
			/* 
			-- move this record from the dtk_retrievals list to the 
			-- dtk_sat_list list.  
			*/
            /*
            -- capture/save the next record from the
            -- list for processing.
            */
            next_dtk_retrievals_ptr = dtk_retrievals_ptr ;
            next_dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals,
                    next_dtk_retrievals_ptr);
 
            /*
            -- move the dtk_rec to the dtk_similars list.
            */
            list_check = move_db_record2new_llist( dtk_sat_list, dtk_rec,
                dtk_retrievals, dtk_retrievals_ptr ) ;
            if ( list_check != dtk_sat_list )
                return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;
 
            /* now set up for the next iteration.  */
            dtk_rec = next_dtk_rec ;
            dtk_retrievals_ptr = next_dtk_retrievals_ptr ;
 
        }
		else
			dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, dtk_retrievals_ptr) ;
		
	}
	return TRUE ;

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_duplicate_dtk_into_list.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_duplicate_dtk_into_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_duplicate_dtk_into_list.c"



/*==============================================================================
Function:       dtkm_duplicate_record_into_list()

Description:    duplicates a dtk DB_RECORD and places it into the list. 
				if the data-take (sat/sensor/rev/dtkid) is already there, 
				first delete it from the list.  
				This is used mainly to accumulate changes in a single 
				linked list without having a single data-take appear 
				many times as it is first accepted, bumped, and 
				accepted again.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 17 08:53:29 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

#include <string.h>

llist *dtkm_duplicate_dtk_into_list( 
	DB_RECORD	**dtk_rec,
	llist		*dtk_list )
{
	cursor    	dtk_list_ptr ;
	DB_RECORD 	**dtk_list_rec = NULL ;
	DB_RECORD	**copy_of_dtk_rec = NULL ;
	int			return_code ;

	/* quick, minimal error checking.  */

	if ( dtk_rec == NULL || dtk_list == NULL )
		return NULL ;

	/* 
	--	This routine is used mainly to accumulate changes in a single 
	--	linked list without having a single data-take appear 
	--	many times as it is first accepted, bumped, and 
	--	accepted again.  
	*/

	/*
	-- if the dtk is ALREADY in the list, copy the data 
	-- onto the existing record in the list.   
	-- if the dtk is NOT already in the list, APPEND a 
	-- newly allocated copy of the record.  
	*/
    for (
        dtk_list_rec = (DB_RECORD **) FIRST( dtk_list, dtk_list_ptr) ;
        dtk_list_rec != NULL ;
        dtk_list_rec = (DB_RECORD **) NEXT( dtk_list, dtk_list_ptr)
        )
    {
		return_code = dtkm_2dtks_concur( dtk_rec, dtk_list_rec ) ;
		if ( return_code < 0 )
			return NULL ; 

		if ( return_code == TRUE )
		{
            /*
            -- the input dtk_rec matches the dtk_list_rec.
            -- now just copy the data from the dtk_rec into
			-- the dtk_list_rec.  
			-- use the existing storage, don't make 
			-- a new record.  
            */
			db_copy_record( APS_CDEFS(DTK), dtk_list_rec, dtk_rec ) ;
			/* now RETURN.  */
            return dtk_list ;
        }
	}

	/*
	-- an existing record was not found  
	-- after a search of the whole list.  
	-- allocate new storage for the copy, and 
	-- APPEND it to the list.  
	*/
	copy_of_dtk_rec = new_table_record(APS_CDEFS(DTK)) ;
	db_copy_record( APS_CDEFS(DTK), copy_of_dtk_rec, dtk_rec ) ;

	APPEND( dtk_list, copy_of_dtk_rec, free_db_record, copy_of_dtk_rec ) ;

	return dtk_list ;

}

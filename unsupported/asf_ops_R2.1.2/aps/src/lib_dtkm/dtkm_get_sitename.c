#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_get_sitename.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_sitename.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_sitename.c"



/*==============================================================================
Function:       

Description:	given an input data-take DB_RECORD, fills in the 
				sitename if dtk.darid != 0 and if the dar is in the database.  
				NOTE:  if the dar is not found, no action 
				is taken.  

Creator:        Lawrence Stevens

Creation Date:  Fri Dec 15 19:36:56 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include "db_dar.h"

#include <string.h>

int dtkm_get_sitename(
	DB_RECORD	**input_dtk,
	DB_RECORD	**result_dtk )
{
	llist		*dar_list = NULL ;
	DB_RECORD	**dar_rec ;
	cursor		dar_list_ptr ;

	/* quick error check */
    if ( input_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

	db_copy_record( APS_CDEFS(DTK), result_dtk, input_dtk ) ; 

	if ( CAST_DTK_DARID result_dtk[DTK_DARID] <= 0 )
		return TRUE ;

	/* we can try to find the dar.  */
	sprintf( where_clause, "where %s = %ld ",
		APS_COL(DAR, DAR_DARID), CAST_DTK_DARID result_dtk[DTK_DARID] ) ;

	dar_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DAR),
		where_clause, NULL, APS_CDEFS(DAR), ALL_COLS) ;
	if (dar_list == NULL)
		return DTKM_ERROR_DB_QUERY_FAILED ;

	if ( NUMELTS( dar_list ) <= 0 )
	{
		DEL_LIST( dar_list ) ;
		return TRUE ;
	}

	/* 
	-- a match on darid.  
	-- copy the sitename.  
	*/
	dar_rec = (DB_RECORD **) FIRST(dar_list, dar_list_ptr) ;
	strcpy( CAST_DTK_SITENAME result_dtk[DTK_SITENAME], 
		CAST_DAR_SITENAME dar_rec[DAR_SITENAME] ) ;

	DEL_LIST( dar_list ) ;

	return TRUE ;

}

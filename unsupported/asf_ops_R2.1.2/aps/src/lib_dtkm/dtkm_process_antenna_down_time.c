#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_process_antenna_down_time.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_process_antenna_down_time.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_process_antenna_down_time.c"



/*==============================================================================
Function:       dtkm_process_antenna_down_time()

Description:    Accepts a new 
				dtkm_process_antenna_down_times DB_RECORD
				and inserts it into the database.  
				Then this routines retrieves all of the affected 
				data-takes and submits them to 
				dtkm_process_dtk_proposal_list() in an attempt to 
				get them onto another antenna.  

Creator:        Lawrence Stevens

Creation Date:  Mon Dec 11 15:42:32 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_antenna_down_times.h"
#include "db_dtk.h"

#include <string.h>

int dtkm_process_antenna_down_time(
	DBPROCESS   *APS_dbproc,               /* Sybase db process              */
	DB_RECORD	**antenna_down_times_rec,  /* new antenna down time.         */
	llist       *dtks_updated,  /* changed data-takes (NULL if unwanted).  */
    FILE		*report_fp )    /* pointer for output report file, or NULL.  */
{
	int			return_code ;
	int			nrecs_inserted ;

	llist		*dtk_list = NULL ;
	llist		*accepted_dtks = NULL ;
	llist		*rejected_dtks = NULL ;
	llist		*CON_dtks = NULL ;
	llist		*deleted_dtks = NULL ;
	llist		*error_dtks = NULL ;
	llist		*REJ_omission_dtks = NULL ;
	llist		*other_sat_dtks = NULL ;
	llist		*same_sat_dtks = NULL ;
	llist		*dtk_updates = NULL ;

	/* brief error checking.  */
	if ( antenna_down_times_rec == NULL )
		return DTKM_ERROR_NULL_ANTENNA_DOWN_TIMES_REC ;

	if ( dtks_updated != NULL )
	{
		if ( NUMELTS( dtks_updated ) != 0 )
			return DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY ;
		dtk_updates = dtks_updated ;
	}

	/*
	-- first, append the new antenna_down_times_rec to the 
	-- database.  
	*/
	nrecs_inserted = db_insert_single_record(APS_dbproc,
		antenna_down_times_rec, APS_TABLE(ANTENNA_DOWN_TIMES), 
		APS_CDEFS(ANTENNA_DOWN_TIMES) ) ;

	if ( nrecs_inserted != 1 )
		return DTKM_ERROR_ANTENNA_DOWN_TIMES_REC_NOT_INSERTED ;

	/*
	-- retrieve the affected data-takes from the 
	-- database.  
	-- OVERLAP the times of the antenna_down_times_rec, 
	-- MATCH the station_id and antenna_id.  
	*/
	sprintf( where_clause,
		"where %s < '%s' and %s > '%s' and %s = '%s' and %s = %hd ",
		APS_COL(DTK, DTK_STRTTIME), 
		CAST_ANTENNA_DOWN_TIMES_STOPTIME 
			antenna_down_times_rec[ANTENNA_DOWN_TIMES_STOPTIME],  

		APS_COL(DTK, DTK_STOPTIME), 
		CAST_ANTENNA_DOWN_TIMES_STRTTIME 
			antenna_down_times_rec[ANTENNA_DOWN_TIMES_STRTTIME], 

		APS_COL(DTK, DTK_STATION_ID),
		CAST_ANTENNA_DOWN_TIMES_STATION_ID 
			antenna_down_times_rec[ANTENNA_DOWN_TIMES_STATION_ID], 

		APS_COL(DTK, DTK_ANTENNA_ID),
		CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID 
			antenna_down_times_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID] ) ;

	/*
	-- add to the where_clause the dtk status values to retrieve.  
	-- QUE, SUB, PLN, SCH
	*/
	sprintf( where_clause, 
		"%s and ( %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' ) ", 
			where_clause, 
			APS_COL(DTK, DTK_DTKSTAT), "QUE", 
			APS_COL(DTK, DTK_DTKSTAT), "SUB", 
			APS_COL(DTK, DTK_DTKSTAT), "PLN", 
			APS_COL(DTK, DTK_DTKSTAT), "SCH" ) ;

#ifdef PRINT_DIAG
	printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, where_clause) ;
#endif

	strcpy( orderby_cols, APS_COL(DTK, DTK_STOPTIME) ) ;

	dtk_list = db_get_records(APS_dbproc, APS_TABLE(DTK),
		where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

	if ( dtk_list == NULL )
		return DTKM_ERROR_DB_QUERY_FAILED ;

	if ( NUMELTS( dtk_list ) == 0 )
		return TRUE ;

	/*
	-- Now process the affected data-takes.
	*/
	/*
	-- first, update them in the database to CON status.  
	*/
	if ( dtks_updated == NULL )
		dtk_updates = create_dyn_llist() ;
		
	return_code = dtkm_update_dtks_field( APS_dbproc, 
		dtk_list, DTK_DTKSTAT, "CON", dtk_updates ) ;
	if ( return_code < 0 )
	{
		DEL_LIST( dtk_list ) ;
		if ( dtks_updated == NULL )
			DEL_LIST( dtk_updates ) ;
		return return_code ;
	}

	accepted_dtks     = create_dyn_llist() ;
	rejected_dtks     = create_dyn_llist() ;
	CON_dtks          = create_dyn_llist() ;
	deleted_dtks      = create_dyn_llist() ;
	error_dtks        = create_dyn_llist() ;
	REJ_omission_dtks = create_dyn_llist() ;
	other_sat_dtks    = create_dyn_llist() ;
	same_sat_dtks     = create_dyn_llist() ;

	/* 
	-- the dtk_list list will be processed 
	-- as a CON roundup.  
	*/
	return_code = dtkm_process_dtk_proposal_list( APS_dbproc, dtk_list,
		accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks, error_dtks,
		REJ_omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates,
		report_fp ) ;

	DEL_LIST( dtk_list ) ;
	DEL_LIST( accepted_dtks ) ;
	DEL_LIST( rejected_dtks ) ;
	DEL_LIST( CON_dtks      ) ;
	DEL_LIST( deleted_dtks  ) ;
	DEL_LIST( error_dtks    ) ;
	DEL_LIST( REJ_omission_dtks ) ;
	DEL_LIST( other_sat_dtks ) ;
	DEL_LIST( same_sat_dtks  ) ;
	if ( dtks_updated == NULL )
		DEL_LIST( dtk_updates ) ;

	if ( return_code < 0 )
		return return_code ;

	return TRUE ;

}

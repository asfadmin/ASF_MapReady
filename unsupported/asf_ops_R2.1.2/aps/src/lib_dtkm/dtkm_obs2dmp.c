THIS SOURCE FILE IS NOW OBSOLETE.  REPLACED BY dtkm_obs2dl.c
Tue Mar 18 15:43:05 PST 1997


#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_obs2dmp.c

Description:    given an observation (recording) data-take, find the tape 
                dump data-take that does its downlinking.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_obs2dmp.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_obs2dmp.c"


#include "dtkm.h"
#include <db_dtk.h>
#include <db_dtk_obs_dmp.h>

static char     DUMP_CODE[] = "DMP" ;


/*==============================================================================
Function:       dtkm_get_dmp_dtk()

Description:    use the dtk_obs_dmp relation to retrieve the tape 
                dump data-take that downlinks the input observation 
                (recording) data-take.  

Creator:        Lawrence Stevens

Creation Date:  Tue Mar 19 19:51:04 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_get_dmp_dtk( 
    DB_RECORD   **obs_dtk,   /* input observation data-take.                 */
    DB_RECORD   **dmp_dtk )  /* output tape dump data-take that downlinks it */
{
    cursor      dtk_obs_dmp_ptr ;
    llist       *dtk_obs_dmp_list = NULL ;
    DB_RECORD   **dtk_obs_dmp_rec = NULL ;

    cursor      dtk_list_ptr ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec = NULL ;

    llist       *list_check = NULL ;

    sprintf( where_clause, 
        "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d ",
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_SAT), 
                CAST_DTK_SAT obs_dtk[DTK_SAT],
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_SENSOR_OBS), 
             CAST_DTK_SENSOR obs_dtk[DTK_SENSOR],
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_REV_OBS), 
                CAST_DTK_REV obs_dtk[DTK_REV],
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_DTKID_OBS), 
              CAST_DTK_DTKID obs_dtk[DTK_DTKID] ) ;

    dtk_obs_dmp_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK_OBS_DMP), where_clause, NULL, 
        APS_CDEFS(DTK_OBS_DMP), ALL_COLS) ;
    if ( dtk_obs_dmp_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS( dtk_obs_dmp_list ) == 0 )
    {
        DEL_LIST( dtk_obs_dmp_list ) ;
        return DTKM_DMP_REC_NOT_FOUND ;
    }

    if ( NUMELTS( dtk_obs_dmp_list ) != 1 )
    {
        DEL_LIST( dtk_obs_dmp_list ) ;
        return DTKM_ERROR_DUPLICATE_DTK_KEYS ;
    }

    /* 
    -- we have the one tape dump 
    -- record.  now get the dtk record data.  
    -- if not DEL status:
    */
    dtk_obs_dmp_rec = (DB_RECORD **) FIRST(dtk_obs_dmp_list, dtk_obs_dmp_ptr) ;
    sprintf( where_clause,
        "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d and %s != '%s'",
        APS_COL(DTK, DTK_SAT), 
        CAST_DTK_OBS_DMP_SAT dtk_obs_dmp_rec[DTK_OBS_DMP_SAT],
        APS_COL(DTK, DTK_SENSOR), 
        "DMP", 
        APS_COL(DTK, DTK_REV), 
        CAST_DTK_OBS_DMP_REV_DMP dtk_obs_dmp_rec[DTK_OBS_DMP_REV_DMP],
        APS_COL(DTK, DTK_DTKID), 
        CAST_DTK_OBS_DMP_DTKID_DMP dtk_obs_dmp_rec[DTK_OBS_DMP_DTKID_DMP],
        APS_COL(DTK, DTK_DTKSTAT), "DEL" ) ;

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;

    if ( dtk_list == NULL )
    {
        DEL_LIST( dtk_obs_dmp_list ) ;
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS( dtk_list ) == 0 )
    {
        /* 
        -- the promise of the record being in the dtk 
        -- relation is not kept.  Processing can go on  
        -- without the tape dump record.  
        -- A tape dump data-take could have been deleted 
        -- and without deleting the corresponding record 
        -- in the dtk_obs_dmp relation.  
        -- Should this be an error?  TBD
        -- right now, this is OK, just not found.  
        */
        DEL_LIST( dtk_list ) ;
        return DTKM_DMP_REC_NOT_FOUND ;
    }


    if ( NUMELTS( dtk_list ) > 1 )
    {
        DEL_LIST( dtk_list ) ;
        DEL_LIST( dtk_obs_dmp_list ) ;
        return DTKM_ERROR_DUPLICATE_DTK_KEYS ;
    }

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

    /* 
    -- copy the values of this first and only data-take 
    -- into the output record, dmp_dtk 
    */
    db_copy_record ( APS_CDEFS(DTK), dmp_dtk, dtk_rec ) ;

    DEL_LIST( dtk_list ) ;
    DEL_LIST( dtk_obs_dmp_list ) ;

    return DTKM_DMP_REC_FOUND ;

}


/*==============================================================================
Function:       dtkm_update_link_obs2dmp()

Description:    given an observation (recording) find its dump data-take 
                which downlinks its data, linking by the field 
                dtk.fa_schedule_link.  

                When it is found, append a new record to the 
				dtk_obs_dmp relation, now that the link between 
				the 2 data-takes has been established. 

Creator:        Miguel Siu      

Creation Date:  Fri Mar 29 10:15:28 PST 1996

Notes:      
	We always check to make sure that the dtk_obs_dmp needs the new record.
	a successful return value for this routine is DTKM_DMP_REC_FOUND
==============================================================================*/
int dtkm_update_link_obs2dmp( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **obs_dtk ) /* input observation (recording) data-take.      */
{

    cursor      dtk_list_ptr ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec  = NULL ;
    DB_RECORD   **dummy_dmp_dtk  = NULL ;

    DB_RECORD   **dtk_obs_dmp_rec = NULL ;

    llist       *list_check  = NULL ;

    int         nrecs_inserted ;
	int			return_code ;

    /* 
	-- first try with link_obs_dmp relation: 
	-- if an entry already exists, exit successfully with DTKM_DMP_REC_FOUND
	*/
    return_code = dtkm_get_dmp_dtk( obs_dtk, dummy_dmp_dtk ) ;
    if ( return_code < 0 )
	{
        return return_code ;
	}
	else if ( return_code == DTKM_DMP_REC_FOUND )
	{
        return DTKM_DMP_REC_FOUND ;
	}
	/* 
	else
		the return code is DTKM_DMP_REC_NOT_FOUND, no action.
	*/


    /* 
    -- use the single link field DTK_FA_SCHEDULE_LINK 
    -- to get the dmp dtk if not DEL status:
    */
    sprintf( where_clause, 
        "where %s = '%s' and %s = '%s' and %s = '%s' and %s != '%s'",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT obs_dtk[DTK_SAT],
        APS_COL(DTK, DTK_SENSOR), "DMP",
        APS_COL(DTK, DTK_FA_SCHEDULE_LINK), 
            CAST_DTK_FA_SCHEDULE_LINK obs_dtk[DTK_FA_SCHEDULE_LINK],
        APS_COL(DTK, DTK_DTKSTAT), "DEL" ) ;

    dtk_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS(dtk_list) == 0 )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_DMP_REC_NOT_FOUND ;
    }

    if ( NUMELTS(dtk_list) != 1 )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_ERROR_GT_1_DMP_LINKED_TO_OBS_DTK ;
    }

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);

    /*
    -- now create a new db record in the dtk_obs_dmp relation for the 
    -- record in the dtk_list list.  
    */

    /* first, make a new empty dtk_obs_dmp DB_RECORD to use:  */
    dtk_obs_dmp_rec =  new_table_record(APS_CDEFS(DTK_OBS_DMP)) ;

    /* 
    -- now fill each data field in the 
    -- dtk_obs_dmp DB_RECORD: 
    */
    /* 
    -- first the DMP sat, rev, dtkid, and station_id fields 
    -- filled from the retrieved dtk_rec, which is the tape dump dtk.
    */
    strcpy( CAST_DTK_OBS_DMP_SAT dtk_obs_dmp_rec[DTK_OBS_DMP_SAT], 
                    CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
    CAST_DTK_OBS_DMP_REV_DMP dtk_obs_dmp_rec[DTK_OBS_DMP_REV_DMP] = 
            CAST_DTK_REV dtk_rec[DTK_REV] ;
    CAST_DTK_OBS_DMP_DTKID_DMP dtk_obs_dmp_rec[DTK_OBS_DMP_DTKID_DMP] = 
            CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;
    strcpy(        CAST_DTK_OBS_DMP_STATION_ID_DMP 
        dtk_obs_dmp_rec[DTK_OBS_DMP_STATION_ID_DMP], 
                           CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) ;

    /* 
    -- Now the OBS sensor, rev, and dtkid fields 
    -- filled from the input obs_dtk, which is the observation:
    */
    strcpy( CAST_DTK_OBS_DMP_SENSOR_OBS dtk_obs_dmp_rec[DTK_OBS_DMP_SENSOR_OBS],
                    CAST_DTK_SENSOR obs_dtk[DTK_SENSOR] ) ;
    CAST_DTK_OBS_DMP_REV_OBS dtk_obs_dmp_rec[DTK_OBS_DMP_REV_OBS] = 
            CAST_DTK_REV obs_dtk[DTK_REV] ;
    CAST_DTK_OBS_DMP_DTKID_OBS dtk_obs_dmp_rec[DTK_OBS_DMP_DTKID_OBS] = 
            CAST_DTK_DTKID obs_dtk[DTK_DTKID] ;

    /* 
    -- the fields are filled in; now insert this 
    -- record into the db:
    */
    nrecs_inserted = db_insert_single_record(APS_dbproc,
        dtk_obs_dmp_rec, APS_TABLE(DTK_OBS_DMP), APS_CDEFS(DTK_OBS_DMP) ) ;

    if ( nrecs_inserted != 1 )
    {
        DEL_LIST( dtk_list ) ;
        free_db_record( dtk_obs_dmp_rec ) ;
        return DTKM_ERROR_dtk_obs_dmp_rec_NOT_INSERTED ;
    }

    /* dtk_obs_dmp_rec is no longer needed: */
    free_db_record( dtk_obs_dmp_rec ) ;
    DEL_LIST( dtk_list ) ;

    return DTKM_DMP_REC_FOUND ;

}


/*==============================================================================
Function:       dtkm_link_dmp_dtk()

Description:    given an observation (recording) find its dump data-take 
                which downlinks its data, linking by the field 
                dtk.fa_schedule_link.  
                when it is found, put its field values into the output 
                dmp_dtk record.  

                We DO NOT append a new record to the dtk_obs_dmp relation, 
                even thought the link between the 2 data-takes has been 
                established.  This is because the recording datatake may
				still be missing a valid dtkid value (because it has not
				been inserted in database yet).

Creator:        Lawrence Stevens

Creation Date:  Tue Mar 19 20:11:54 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_link_dmp_dtk( 
    DBPROCESS       *APS_dbproc, 
    DB_RECORD   **obs_dtk,  /* input observation (recording) data-take.      */
    DB_RECORD   **dmp_dtk ) /* output tape dump data-take which downlinks it */
{

    cursor      dtk_list_ptr ;
    llist       *dtk_list ;
    DB_RECORD   **dtk_rec ;

    DB_RECORD   **dtk_obs_dmp_rec ;

    llist       *list_check ;

    int         nrecs_inserted ;

    /* 
    -- use the single link field DTK_FA_SCHEDULE_LINK 
    -- to get the dmp dtk if not DEL status:
    */
    sprintf( where_clause, 
        "where %s = '%s' and %s = '%s' and %s = '%s' and %s != '%s'",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT obs_dtk[DTK_SAT],
        APS_COL(DTK, DTK_SENSOR), "DMP",
        APS_COL(DTK, DTK_FA_SCHEDULE_LINK), 
            CAST_DTK_FA_SCHEDULE_LINK obs_dtk[DTK_FA_SCHEDULE_LINK],
        APS_COL(DTK, DTK_DTKSTAT), "DEL" ) ;

    dtk_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS(dtk_list) == 0 )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_DMP_REC_NOT_FOUND ;
    }

    if ( NUMELTS(dtk_list) != 1 )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_ERROR_GT_1_DMP_LINKED_TO_OBS_DTK ;
    }

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);


    /* 
    -- the tape dump record was found; now copy all the field
    -- values from the retrieved record into the output dmp_dtk:  
    */
    db_copy_record ( APS_CDEFS(DTK), dmp_dtk, dtk_rec ) ;

    DEL_LIST( dtk_list ) ;
    return DTKM_DMP_REC_FOUND ;

}


/*==============================================================================
Function:       dtkm_obs2dmp()

Description:    given an observation (recording) data-take, find the  
                tape dump dtk rec which does its downlinking.  

Creator:        Lawrence Stevens

Creation Date:  Tue Mar 19 20:31:49 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int
dtkm_obs2dmp(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **obs_dtk,   /* input observation (recording) data-take.     */
    DB_RECORD   **dmp_dtk )  /* output tape dump which downlinks it.         */
{
    int     return_code ;

    /* quick error checking.  */
    if ( obs_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( dmp_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    if ( dtkm_is_a_downlink( obs_dtk) == TRUE )
        return DTKM_ERROR_DTK_IS_A_DOWNLINK ;

    /*
    -- 1.  first try using the link_obs_dmp relation.  
    -- 2.  if that gets nothing, try using the dtk.fa_schedule_link field.  
    --     NOTE: 
	--		even if this works, we do NOT add a new entry for each 
	--		found recording.  This is because the recording may not yet have
	--		a valid dtkid value.
    -- 3.  if both tries get nothing, then there were no recordings found.  
    --     probably the recordings will be proposed later.  Not an error.  
    */

    /* 
    -- use the sat field to check to see if 
    -- anything found. 
    */
    strcpy( CAST_DTK_SAT dmp_dtk[DTK_SAT], "" ) ;

    /* first try with link_obs_dmp relation: */
    return_code = dtkm_get_dmp_dtk( obs_dtk, dmp_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* see if we found something.  */
    if ( return_code == DTKM_DMP_REC_FOUND )
        return DTKM_DMP_REC_FOUND ;

    /* 
	-- nothing found.  now try with dtk.fa_schedule_link  
	-- NOTE: remember that there is no insertion into the link relation
	--       (dtk_obs_dmp) here, because the recording may not yet have
	--		 a valid dtkid value
	*/
    return_code = dtkm_link_dmp_dtk( APS_dbproc, obs_dtk, dmp_dtk ) ;
    /* 
    --  NOTE:  return the return code from 
    --  dtkm_link_dmp_dtk().  the return code could be 
    --  < 0 ERROR, or DTKM_DMP_REC_FOUND, 
    --  or DTKM_DMP_REC_NOT_FOUND.
    */
    return return_code ;

}

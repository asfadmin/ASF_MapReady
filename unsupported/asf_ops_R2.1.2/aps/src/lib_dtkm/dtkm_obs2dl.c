#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_obs2dl.c

Description:    given an observation (recording or real-time observation) 
                data-take, find the downlink data-take that does its 
                downlinking.  

Notes:          

==============================================================================*/
#pragma ident   "@(#)dtkm_obs2dl.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_obs2dl.c"


#include "dtkm.h"
#include <db_dtk.h>
#include <db_dl2obs.h>



/*==============================================================================
Function:       dtkm_get_dl_dtk()

Description:    use the dl2obs relation to retrieve the downlink
                data-take that downlinks the input observation 
                (recording or real-time observation) data-take.  

Creator:        Lawrence Stevens

Creation Date:  Sun Mar  9 13:53:47 PST 1997

Notes:      
==============================================================================*/
int dtkm_get_dl_dtk( 
    DB_RECORD   **obs_dtk,  /* input observation data-take.                 */
    DB_RECORD   **dl_dtk )  /* output downlink data-take that downlinks it  */
{
    cursor      dl2obs_ptr ;
    llist       *dl2obs_list = NULL ;
    DB_RECORD   **dl2obs_rec = NULL ;

    cursor      dtk_list_ptr ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec = NULL ;

    if ( (int) CAST_DTK_DTKID obs_dtk[DTK_DTKID] == 0 )
        return DTKM_DOWNLINK_REC_NOT_FOUND ;

    sprintf( where_clause, 
        "where %s = '%s' and %s = %ld and %s = %d ",
        APS_COL(DL2OBS, DL2OBS_SAT), 
                CAST_DTK_SAT obs_dtk[DTK_SAT],
        APS_COL(DL2OBS, DL2OBS_REV_OBS), 
                CAST_DTK_REV obs_dtk[DTK_REV],
        APS_COL(DL2OBS, DL2OBS_DTKID_OBS), 
              CAST_DTK_DTKID obs_dtk[DTK_DTKID] ) ;

    dl2obs_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DL2OBS), where_clause, NULL, 
        APS_CDEFS(DL2OBS), ALL_COLS) ;
    if ( dl2obs_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    if ( NUMELTS( dl2obs_list ) == 0 )
    {
        DEL_LIST( dl2obs_list ) ;
        return DTKM_DOWNLINK_REC_NOT_FOUND ;
    }

    if ( NUMELTS( dl2obs_list ) != 1 )
    {
        DEL_LIST( dl2obs_list ) ;
        return DTKM_ERROR_DUPLICATE_DTK_KEYS ;
    }

    /* 
    -- we have the one downlink record.  now get 
    -- the downlink dtk record data.  
    -- if not DEL (or CMB) status:
    */
    dl2obs_rec = (DB_RECORD **) FIRST(dl2obs_list, dl2obs_ptr) ;
    sprintf( where_clause,
"where %s = '%s' and %s = %ld and %s = %d and %s != '%s' and %s != '%s'",
        APS_COL(DTK, DTK_SAT), 
            CAST_DL2OBS_SAT dl2obs_rec[DL2OBS_SAT],
        APS_COL(DTK, DTK_REV), 
            CAST_DL2OBS_REV_DL dl2obs_rec[DL2OBS_REV_DL],
        APS_COL(DTK, DTK_DTKID), 
            CAST_DL2OBS_DTKID_DL dl2obs_rec[DL2OBS_DTKID_DL],
        APS_COL(DTK, DTK_DTKSTAT), "DEL",
        APS_COL(DTK, DTK_DTKSTAT), "CMB" ) ;

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        DEL_LIST( dl2obs_list ) ;
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }
    if ( NUMELTS( dtk_list ) == 0 )
    {
        /* 
        -- the promise of the record being in the dtk 
        -- relation is not kept.  Processing can go on  
        -- without the downlink record.  
        -- A downlink data-take could have been deleted 
        -- and without deleting the corresponding record 
        -- in the dl2obs relation.  
        -- Should this be an error?  TBD
        -- right now, this is OK, just not found.  
        */
        DEL_LIST( dtk_list ) ;
        return DTKM_DOWNLINK_REC_NOT_FOUND ;
    }

    if ( NUMELTS( dtk_list ) > 1 )
    {
        DEL_LIST( dtk_list ) ;
        DEL_LIST( dl2obs_list ) ;
        return DTKM_ERROR_DUPLICATE_DTK_KEYS ;
    }

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

    /* 
    -- copy the values of this first and only data-take 
    -- into the output record, dl_dtk 
    */
    db_copy_record ( APS_CDEFS(DTK), dl_dtk, dtk_rec ) ;

    DEL_LIST( dtk_list ) ;
    DEL_LIST( dl2obs_list ) ;

    return DTKM_DOWNLINK_REC_FOUND ;

}

/*==============================================================================
Function:       dtkm_link_dl_dtk()

Description:    given an observation (recording) find its dump data-take 
                which downlinks its data, linking by the field 
                dtk.fa_schedule_link.  
                when it is found, put its field values into the output 
                dl_dtk record.  

                We DO NOT append a new record to the dl2obs relation, 
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
int dtkm_link_dl_dtk( 
    DB_RECORD   **obs_dtk, /* input observation (recording or realtime obs) */
    DB_RECORD   **dl_dtk ) /* output downlink data-take which downlinks it  */
{

    cursor      dtk_list_ptr ;
    llist       *dtk_list ;
    DB_RECORD   **dtk_rec ;

    /* 
    -- use the single link field DTK_FA_SCHEDULE_LINK 
    -- to get the downlink dtk if not DEL status:
    */
    if( dtkm_is_a_recording( obs_dtk ) == TRUE )
    {
        /* 
        -- observation is a recording.  
        -- look for its tape dump downlink.  
        -- just look in the current obs rev and the next 100 revs for it, 
        -- this is about 1 week forward.  a satellite 
        -- won't hold onto a recording for a week.  
        -- don't retrieve any DEL or CMB data-take.  
        */
        sprintf( where_clause, 
"where %s = '%s' and %s = '%s' and %s >= %ld and %s <= %ld and %s = '%s' and %s != '%s' and %s != '%s'",
            APS_COL(DTK, DTK_SAT),    CAST_DTK_SAT obs_dtk[DTK_SAT],
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,

            APS_COL(DTK, DTK_REV),    CAST_DTK_REV obs_dtk[DTK_REV],
            APS_COL(DTK, DTK_REV), (( CAST_DTK_REV obs_dtk[DTK_REV] ) + 100 ),

            APS_COL(DTK, DTK_FA_SCHEDULE_LINK), 
                CAST_DTK_FA_SCHEDULE_LINK obs_dtk[DTK_FA_SCHEDULE_LINK],
            APS_COL(DTK, DTK_DTKSTAT), "DEL",
            APS_COL(DTK, DTK_DTKSTAT), "CMB" ) ;
    }
    if( dtkm_is_a_realtime_observation( obs_dtk ) == TRUE )
    {
        /* 
        -- observation is a realtime observation.  look for 
        -- a realtime downlink.  
        -- look in the same rev for a realtime downlink.  
        */
        sprintf( where_clause, 
"where %s = '%s' and %s = '%s' and %s = %ld and %s = '%s' and %s != '%s' and %s != '%s'",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT obs_dtk[DTK_SAT],
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,

            APS_COL(DTK, DTK_REV), CAST_DTK_REV obs_dtk[DTK_REV],

            APS_COL(DTK, DTK_FA_SCHEDULE_LINK), 
                CAST_DTK_FA_SCHEDULE_LINK obs_dtk[DTK_FA_SCHEDULE_LINK],
            APS_COL(DTK, DTK_DTKSTAT), "DEL",
            APS_COL(DTK, DTK_DTKSTAT), "CMB" ) ;
    }

    dtk_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS(dtk_list) == 0 )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_DOWNLINK_REC_NOT_FOUND ;
    }

    if ( NUMELTS(dtk_list) != 1 )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_ERROR_GT_1_DOWNLINK_LINKED_TO_OBS_DTK ;
    }

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);

    /* 
    -- the downlink record was found; now copy all the field
    -- values from the retrieved record into the output dl_dtk:  
    */
    db_copy_record ( APS_CDEFS(DTK), dl_dtk, dtk_rec ) ;

    DEL_LIST( dtk_list ) ;
    return DTKM_DOWNLINK_REC_FOUND ;

}



/*==============================================================================
Function:       dtkm_update_link_obs2dl()

Description:    given an observation (recording or real-time observation) 
                find its downlink data-take which downlinks its data, 
                linking by the field dtk.fa_schedule_link.  

                When it is found, append a new record to the 
                dl2obs relation, now that the link between 
                the 2 data-takes has been established. 

Creator:        Miguel Siu      

Creation Date:  Fri Mar 29 10:15:28 PST 1996

Notes:      
    We always check to make sure that the dl2obs needs the new record.
    a successful return value for this routine is DTKM_DOWNLINK_REC_FOUND
==============================================================================*/
int dtkm_update_link_obs2dl( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **obs_dtk ) /* input observation (recording) data-take.      */
{

    DB_RECORD   **dtk_rec  = NULL ;
    DB_RECORD   **dummy_dl_dtk  = NULL ;

    DB_RECORD   **dl2obs_rec = NULL ;

    int         nrecs_inserted ;
    int         return_code ;

    /* 
    -- first try with dl2obs relation: 
    -- if an entry already exists, exit successfully 
    */
    /* initialize the output record:  */
    dummy_dl_dtk = new_table_record(APS_CDEFS(DTK)) ;
    return_code = dtkm_get_dl_dtk( obs_dtk, dummy_dl_dtk ) ;
    free_db_record( dummy_dl_dtk );
    if ( return_code < 0 )
    {
        return return_code ;
    }
    else if ( return_code == DTKM_DOWNLINK_REC_FOUND )
    {
        return return_code ;
    }
    /* 
    -- else
    --      the return code is DTKM_DOWNLINK_REC_NOT_FOUND, no action.
    */

    /*
    -- use the single link field DTK_FA_SCHEDULE_LINK 
    */
    /* initialize the output record:  */
    dtk_rec = new_table_record(APS_CDEFS(DTK)) ;
    return_code = dtkm_link_dl_dtk( obs_dtk, dtk_rec ) ;
    if( return_code < 0 )
    {
        free_db_record( dtk_rec ) ;
        return return_code ;
    }
    if( return_code  == DTKM_DOWNLINK_REC_NOT_FOUND )
    {
        free_db_record( dtk_rec ) ;
        return return_code ;
    }

    /* 
    -- the downlink record (dtk_rec) was found, 
    -- using DTK_FA_SCHEDULE_LINK.  
    */

    /* first, make a new empty dl2obs DB_RECORD to use:  */
    dl2obs_rec =  new_table_record(APS_CDEFS(DL2OBS)) ;

    /* 
    -- now fill each data field in the 
    -- dl2obs DB_RECORD: 
    */
    /* 
    -- first the DOWNLINK sat, rev, dtkid, and station_id fields 
    -- filled from the retrieved dtk_rec, which is the downlink dtk.
    */
    strcpy( CAST_DL2OBS_SAT dl2obs_rec[DL2OBS_SAT], 
        CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
    CAST_DL2OBS_REV_DL dl2obs_rec[DL2OBS_REV_DL] = 
        CAST_DTK_REV dtk_rec[DTK_REV] ;
    CAST_DL2OBS_DTKID_DL dl2obs_rec[DL2OBS_DTKID_DL] = 
        CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;

    /* dtk_rec no longer needed.  */
    free_db_record( dtk_rec ) ;

    /* 
    -- Now the OBS sensor, rev, and dtkid fields 
    -- filled from the input obs_dtk, which is the observation:
    */
    CAST_DL2OBS_REV_OBS dl2obs_rec[DL2OBS_REV_OBS] = 
        CAST_DTK_REV obs_dtk[DTK_REV] ;
    CAST_DL2OBS_DTKID_OBS dl2obs_rec[DL2OBS_DTKID_OBS] = 
        CAST_DTK_DTKID obs_dtk[DTK_DTKID] ;

    /* 
    -- the fields are filled in; now insert this 
    -- record into the db:
    */
    nrecs_inserted = db_insert_single_record(APS_dbproc,
        dl2obs_rec, APS_TABLE(DL2OBS), APS_CDEFS(DL2OBS) ) ;

    if ( nrecs_inserted != 1 )
    {
        free_db_record( dl2obs_rec ) ;
        return DTKM_ERROR_dl2obs_rec_NOT_INSERTED ;
    }

    /* dl2obs_rec is no longer needed: */
    free_db_record( dl2obs_rec ) ;

    return DTKM_DOWNLINK_REC_FOUND ;

}


/*==============================================================================
Function:       dtkm_obs2dl()

Description:    given an observation (recording or realtime observation) 
                data-take, find the  downlink dtk rec which does its 
                downlinking.  

Creator:        Lawrence Stevens

Creation Date:  Tue Mar 19 20:31:49 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int
dtkm_obs2dl(
    DB_RECORD   **obs_dtk,  /* input observation (recording or realtime obs) */
    DB_RECORD   **dl_dtk )  /* output downlink which downlinks it.           */
{
    int     return_code ;

    /* quick error checking.  */
    if ( obs_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( dl_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    if ( dtkm_is_a_downlink( obs_dtk) == TRUE )
        return DTKM_ERROR_DTK_IS_A_DOWNLINK ;

    /*
    -- 1.  first try using the link_obs_dl relation.  
    -- 2.  if that gets nothing, try using the dtk.fa_schedule_link field.  
    --     NOTE: 
    --      even if this works, we do NOT add a new entry for each 
    --      found recording.  This is because the recording may not yet have
    --      a valid dtkid value.
    -- 3.  if both tries get nothing, then there were no recordings found.  
    --     probably the recordings will be proposed later.  Not an error.  
    */

    /* 
    -- use the sat field to check to see if 
    -- anything found.  We set it to a null string.  
    -- if it stays null, then nothing found.  if it 
    -- gets filled in, then the function found a data-take:  
    */
    strcpy( CAST_DTK_SAT dl_dtk[DTK_SAT], "" ) ;

    /* first try with link_obs_dl relation: */
    return_code = dtkm_get_dl_dtk( obs_dtk, dl_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* see if we found something.  */
    if ( return_code == DTKM_DOWNLINK_REC_FOUND )
        return DTKM_DOWNLINK_REC_FOUND ;

    /* 
    -- nothing found.  now try with dtk.fa_schedule_link  
    -- NOTE: remember that there is no insertion into the link relation
    --       (dl2obs) here, because the recording may not yet have
    --       a valid dtkid value
    */
    return_code = dtkm_link_dl_dtk( obs_dtk, dl_dtk ) ;

    /* 
    --  NOTE:  return the return code from 
    --  dtkm_link_dl_dtk().  the return code could be 
    --  < 0 ERROR, or DTKM_DOWNLINK_REC_FOUND, 
    --  or DTKM_DOWNLINK_REC_NOT_FOUND.
    */
    return return_code ;

}


/*==============================================================================
Function:       dtkm_obs2dl_list()

Description:    given a list of observations, find their downlinks and 
                put them all into the output list.  if the input list 
                contains a downlink, it is skipped and processing continues.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 11 16:34:23 PST 1997

==============================================================================*/
int
dtkm_obs2dl_list(
    llist       *obs_dtk_list,     /* input observations list.  */
    llist       *dl_dtk_list   )   /* output downlinks list.    */
{
    DB_RECORD   **obs_dtk_rec ;
    DB_RECORD   **dl_dtk_rec ;
    cursor      obs_dtk_list_ptr ;
    cursor      dl_dtk_list_ptr ;
    int         return_code ;

    /* quick error checking.  */
    if ( obs_dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
    if ( dl_dtk_list == NULL )
        return DTKM_ERROR_NULL_OUTPUT_DTK_LIST ;
    if( NUMELTS( dl_dtk_list ) > 0 )
        return DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY ;

    /* check for no work to do.  */
    if( NUMELTS( obs_dtk_list ) <= 0 )
        return TRUE ;

    /* this memory is used for the next downlink.  */
    dl_dtk_rec = new_table_record(APS_CDEFS(DTK)) ;

    for (obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, obs_dtk_list_ptr);
         obs_dtk_rec ;
         obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, obs_dtk_list_ptr)
        )
    {
        /* process the current obs_dtk_rec right here.  */
        return_code = dtkm_obs2dl( obs_dtk_rec, dl_dtk_rec ) ;
        if( return_code == DTKM_ERROR_DTK_IS_A_DOWNLINK )
            continue ;
        if( return_code < 0 )
            return return_code ;
        if ( return_code == DTKM_DOWNLINK_REC_FOUND )
        {
            /* 
            -- save this downlink into the list 
            -- and get more memory for the next downlink.  
            */
            APPEND( dl_dtk_list, dl_dtk_rec, free_db_record, dl_dtk_rec) ;
            dl_dtk_rec = new_table_record(APS_CDEFS(DTK)) ;
        }
        continue ;
    }

    /*
    -- all done with the list.  now free the memory 
    -- that was saved for the next downlink, but 
    -- never used.
    */
    free_db_record( dl_dtk_rec ) ;

    return TRUE ;

}

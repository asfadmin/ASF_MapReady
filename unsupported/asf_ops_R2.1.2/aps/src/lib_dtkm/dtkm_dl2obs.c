#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_dl2obs.c

Description:    given a downlink record, get the observation dtks that it is 
                downlinking.  

    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_dl2obs.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_dl2obs.c"


#include "dtkm.h"
#include <aps_defs.h>    /* for DTKM_ACTID_RECORDING_OBSERVATION_CODE  */
#include <db_dtk.h>
#include <db_dl2obs.h>


/*==============================================================================
Function:       dtkm_get_obs_dtks()

Description:    use the dl2obs relation to retrieve a list 
                of data-takes that are downlinked by the input realtime 
                or downlink data-take.

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 18 17:09:53 PST 1996

Notes:      
==============================================================================*/
int dtkm_get_obs_dtks( 
    DB_RECORD   **dl_dtk,   /* input downlink downlink dtk record.         */
    llist       *obs_dtks ) /* output list of corresponding observation dtks.*/
{
    cursor      dl2obs_ptr ;
    llist       *dl2obs_list = NULL ;
    DB_RECORD   **dl2obs_rec ;

    cursor      dtk_list_ptr ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec ;

    cursor      obs_dtks_ptr ;
    llist       *list_check = NULL ;

    /* if dtkid == 0, no recs will be found.  */
    if( (int) CAST_DTK_DTKID dl_dtk[DTK_DTKID]  == 0  )
        return TRUE ;

    /* 
    -- get all entries in dl2obs relation for the input 
    -- downlink data-take:  
    */
    sprintf( where_clause, 
        "where %s = '%s' and %s = %ld and %s = %d ",
        APS_COL(DL2OBS, DL2OBS_SAT), 
                                CAST_DTK_SAT dl_dtk[DTK_SAT],
        APS_COL(DL2OBS, DL2OBS_REV_DL), 
                                CAST_DTK_REV dl_dtk[DTK_REV],
        APS_COL(DL2OBS, DL2OBS_DTKID_DL), 
                                CAST_DTK_DTKID dl_dtk[DTK_DTKID]  ) ;
    strcpy( orderby_cols, APS_COL(DL2OBS, DL2OBS_DTKID_OBS ) ) ;

    dl2obs_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DL2OBS), where_clause, orderby_cols, 
        APS_CDEFS(DL2OBS), ALL_COLS) ;

    if ( dl2obs_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS( dl2obs_list ) == 0 )
    {
        DEL_LIST( dl2obs_list ) ;
        return TRUE ;
    }

    for (   dl2obs_rec = (DB_RECORD **) 
                FIRST(dl2obs_list, dl2obs_ptr);
            dl2obs_rec ;
            dl2obs_rec = (DB_RECORD **) 
                NEXT(dl2obs_list, dl2obs_ptr)
        )
    {
        /* 
        -- get each of the observation dtk(s) from the dtk relation 
        -- and append it to the output list obs_dtks if not DEL status
        -- and not CMB status.  
        -- use the COMPLETE PRIMARY KEY sat/sensor/rev/dtkid
        -- to retrieve the dtk record.  
        */
        sprintf( where_clause,
"where %s = '%s' and %s = %ld and %s = %d and %s != '%s' and %s != '%s'",
            APS_COL(DTK, DTK_SAT), 
                CAST_DL2OBS_SAT dl2obs_rec[DL2OBS_SAT],
            APS_COL(DTK, DTK_REV), 
                CAST_DL2OBS_REV_OBS dl2obs_rec[DL2OBS_REV_OBS],
            APS_COL(DTK, DTK_DTKID), 
                CAST_DL2OBS_DTKID_OBS dl2obs_rec[DL2OBS_DTKID_OBS],
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
            DEL_LIST( dtk_list ) ;
            continue ;
        }

        if ( NUMELTS( dtk_list ) > 1 )
        {
            DEL_LIST( dtk_list ) ;
            DEL_LIST( dl2obs_list ) ;
            return DTKM_ERROR_DUPLICATE_DTK_KEYS ;
        }

        dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

        /*  
        -- if this is a DL (our input dtk), then delete it from this list.  
        -- actually, this record should NEVER BE A DOWNLINK RECORD.  
        -- if this happens, the system is NOT WORKING.  Error in code:
        */
        if( dtkm_is_a_downlink( dtk_rec ) == TRUE ) 
        {
            /* 
            -- this dtk is either a realtime downlink or 
            -- a tape dump downlink
            */
            DEL_AT_CURSOR( obs_dtks, obs_dtks_ptr ) ;
            DEL_LIST( dtk_list ) ;

            fprintf(stderr, "%s(%d):  Error in code.\n", __FILE__, __LINE__ ) ;
            return DTKM_ERROR_IN_CODE_SEE_STDERR_FOR_CODE_LOCATION ;

        }
        else
        {
            /* 
            -- move this first and only data-take out of 
            -- dtk_list and into obs_dtks 
            */
            list_check = move_db_record2new_llist( obs_dtks, dtk_rec, 
                dtk_list, dtk_list_ptr);
            if ( list_check != obs_dtks )
                return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;

            DEL_LIST( dtk_list ) ;

            /* now continue with the next record.  */
        }

    }

    DEL_LIST( dl2obs_list ) ;
    return TRUE ;

}


/*==============================================================================
Function:       dtkm_link_obs_dtks()

Description:    given a downlink data-take, find its observation data-takes, 
                linking by the field dtk.fa_schedule_link.  
                When they are found, put them into the output obs_dtks list.  

                NOTE:
                We will NOT append new records into the link relation 
                (dl2obs), even though the link between the downlink datatake
                and its observation datatakes has been established.  This is 
                because the downlink datatake could still be missing a valid
                dtkid value (because the datatake has not been inserted into
                the database yet).

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 18 19:32:49 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_link_obs_dtks( 
    DB_RECORD   **dl_dtk,   /* input downlink dtk record.         */
    llist       *obs_dtks ) /* output list of corresponding observation dtks. */
{

    llist       *dtk_list = NULL ;

    llist       *list_check = NULL ;

    /*
    -- if this datatake has a DTK.fa_schedule_link value of NULL or blanks,
    -- then we know it came from a REQUEST file, and that there are no
    -- observation to link to.
    -- So ignore the current datatake proposal
    */
    if (*(CAST_DTK_FA_SCHEDULE_LINK dl_dtk[DTK_FA_SCHEDULE_LINK][0]) == '\0'
    ||  *(CAST_DTK_FA_SCHEDULE_LINK dl_dtk[DTK_FA_SCHEDULE_LINK][0]) == ' ' )
        return (TRUE) ;

    /*
    -- get all observations which correspond to the input downlink record
    -- and which do not have a status of "DEL"
    -- and which do not have a status of "CMB"
    -- and the actid identifies it as an observation
    -- ie: the actid is like "RE...." (recording) if downlink is a Tape Dump
    -- and the actid is like "RO...." (realtime observation) if downlink 
    --                                 is a real time downlink
    */
    if( dtkm_is_a_tape_dump( dl_dtk ) == TRUE ) 
    {
        /* 
        -- input downlink record is a TAPE DUMP.  
        -- find recordings:  look in the previous 100 revs, 
        -- about 1 week.  
        */
        sprintf( where_clause,
"where %s = '%s' and %s >= %ld and %s <= %ld and %s = '%s' and %s != '%s' and %s != '%s' and %s like '%s%%'",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dl_dtk[DTK_SAT],

            APS_COL(DTK, DTK_REV), (  (CAST_DTK_REV dl_dtk[DTK_REV]) - 100 ) ,
            APS_COL(DTK, DTK_REV), CAST_DTK_REV dl_dtk[DTK_REV],

            APS_COL(DTK, DTK_FA_SCHEDULE_LINK),
                CAST_DTK_FA_SCHEDULE_LINK dl_dtk[DTK_FA_SCHEDULE_LINK],
            APS_COL(DTK, DTK_DTKSTAT), "DEL" ,   
            APS_COL(DTK, DTK_DTKSTAT), "CMB" ,   
            APS_COL(DTK, DTK_ACTID), DTKM_ACTID_RECORDING_OBSERVATION_CODE );
    }
    else if( dtkm_is_a_realtime_downlink( dl_dtk ) == TRUE ) 
    {
        /* 
        -- input downlink record is a REAL TIME downlink.  
        -- look in the SAME REV for the observations.  
        */
        sprintf( where_clause,
"where %s = '%s' and %s = %ld and %s = '%s' and %s != '%s' and %s != '%s' and %s like '%s%%'",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dl_dtk[DTK_SAT],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV dl_dtk[DTK_REV],
            APS_COL(DTK, DTK_FA_SCHEDULE_LINK),
                CAST_DTK_FA_SCHEDULE_LINK dl_dtk[DTK_FA_SCHEDULE_LINK],
            APS_COL(DTK, DTK_DTKSTAT), "DEL" ,   
            APS_COL(DTK, DTK_DTKSTAT), "CMB" ,   
            APS_COL(DTK, DTK_ACTID), DTKM_ACTID_REALTIME_OBSERVATION_CODE );
    }
    else
    {
        fprintf(stderr, "%s(%d):  Error in source code.\n", __FILE__, __LINE__);
        return DTKM_ERROR_IN_CODE_SEE_STDERR_FOR_CODE_LOCATION ;
    }

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }
    if ( NUMELTS( dtk_list ) == 0 )
    {
        /* 
        -- There are no observations, 
        -- no entries need be made to dl2obs 
        */
        DEL_LIST( dtk_list ) ;
        return TRUE ;
    }

    /* 
    -- observation records were found; now move them all 
    -- out of the dtk_list and into the output obs_dtks list:  
    */
    list_check = db_record_llist_move( obs_dtks, dtk_list ) ;
    if ( list_check != obs_dtks )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
    }

    DEL_LIST( dtk_list ) ;
    return TRUE ;

}


/*==============================================================================
Function:       dtkm_update_link_dl2obs()

Description:    given an downlink, find its observation data-takes
                which downlinks its data, linking by the field 
                dtk.fa_schedule_link.  

                When they are found, append  new records to the 
                dl2obs relation, now that the link between 
                the downlink, and observation data-takes has been established. 

Creator:        Miguel Siu      

Creation Date:  Fri Mar 29 10:15:28 PST 1996

Notes:      
    We always check to make sure that the dl2obs needs the new records.

    Successful return value for this routine is TRUE.
==============================================================================*/
int dtkm_update_link_dl2obs( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **dl_dtk ) /* input downlink data-take.      */
{

    DB_RECORD   **dtk_rec = NULL ;

    llist       *obs_dtks = NULL ;
    cursor      obs_dtks_ptr ;

    llist       *dl_obs_dtk_list = NULL ;
    cursor      dl_obs_dtk_list_ptr ;
    DB_RECORD   **dl_obs_dtk_rec = NULL ;

    DB_RECORD   **dl2obs_insert = NULL ;
    DB_RECORD   **dtk_unlinked_rec = NULL ;


    int         nrecs_inserted ;
    int         nrecs ;
    int         return_code ;

    /* initialize linked list to obtain data-takes.  */
    dl_obs_dtk_list = create_dyn_llist() ;

    /* 
    -- now use (only) fa_schedule_link value 
    -- to find observations for this downlink: 
    */
    return_code = dtkm_link_obs_dtks( dl_dtk, dl_obs_dtk_list ) ;
    if ( return_code < 0 )
        return return_code ;
    if ( NUMELTS( dl_obs_dtk_list ) == 0 )
    {
        /* 
        -- There are no observation dtks, 
        -- no new entries can be made to dl2obs relation.  
        */
        DEL_LIST( dl_obs_dtk_list ) ;
        return TRUE ;
    }

    /*
    -- create output list obs_dtks.  
    -- we will later create a new db record in the 
    -- dl2obs relation for each record in this obs_dtks list.  
    */
    obs_dtks = create_dyn_llist() ;


    for (   dl_obs_dtk_rec = (DB_RECORD **) 
                FIRST(dl_obs_dtk_list, dl_obs_dtk_list_ptr);
            dl_obs_dtk_rec ;
            dl_obs_dtk_rec = (DB_RECORD **) 
                NEXT(dl_obs_dtk_list, dl_obs_dtk_list_ptr)
        )
    {

        /* 
        -- if this observation dtk has no
        -- dl2obs entry, include it in obs_dtks list 
        -- to make an entry.  
        */
        sprintf( where_clause,
            "where %s = '%s' and %s = %ld and %s = %d ",
            APS_COL(DL2OBS, DL2OBS_SAT),
                             CAST_DTK_SAT    dl_obs_dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_OBS),
                             CAST_DTK_REV    dl_obs_dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_OBS),
                             CAST_DTK_DTKID  dl_obs_dtk_rec[DTK_DTKID] ) ;

        nrecs = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(DL2OBS), where_clause ) ;
        if (nrecs < 0 )
        {
            /* error.  */
            DEL_LIST( obs_dtks ) ;
            DEL_LIST( dl_obs_dtk_list ) ;
            free_db_record( dl_obs_dtk_rec) ;
            return DTKM_ERROR_DB_QUERY_FAILED ;
        }
        if (nrecs != 0 )
        {
            /* 
            -- there is an entry in dl2obs already, 
            -- skip current record.
            */
            continue ;
        }

        /* 
        -- there is NO entry in dl2obs already, 
        -- move current observation dtk into obs_dtks list
        */
        dtk_unlinked_rec = (DB_RECORD **) 
            UNLINK_AT_CURSOR(dl_obs_dtk_list, dl_obs_dtk_list_ptr) ;
        APPEND (obs_dtks, dtk_unlinked_rec, free_db_record, dtk_unlinked_rec) ;

        /* now continue with the next record.  */

    }
    DEL_LIST( dl_obs_dtk_list ) ;

    /*
    -- now create a new db record in the dl2obs relation for each 
    -- record in the obs_dtks list.  
    */
    dl2obs_insert = new_table_record(APS_CDEFS(DL2OBS)) ;
    for (   dtk_rec = (DB_RECORD **) FIRST(obs_dtks, obs_dtks_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(obs_dtks, obs_dtks_ptr)
        )
    {
        /* 
        -- now fill each data field in the 
        -- dl2obs DB_RECORD: 
        */
        /* 
        -- first the OBS sat, sensor, rev, dtkid fields 
        -- filled from the dtk_rec, which is the observation.
        */
        strcpy( CAST_DL2OBS_SAT dl2obs_insert[DL2OBS_SAT], 
            CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
        CAST_DL2OBS_REV_OBS dl2obs_insert[DL2OBS_REV_OBS] = 
                CAST_DTK_REV dtk_rec[DTK_REV] ;
        CAST_DL2OBS_DTKID_OBS dl2obs_insert[DL2OBS_DTKID_OBS] = 
                CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;

        /* 
        -- Now the downlink sensor, rev, dtkid, and station_id fields 
        -- filled from the dl_dtk, which is the downlink:
        */
        CAST_DL2OBS_REV_DL dl2obs_insert[DL2OBS_REV_DL] = 
                CAST_DTK_REV dl_dtk[DTK_REV] ;
        CAST_DL2OBS_DTKID_DL dl2obs_insert[DL2OBS_DTKID_DL] = 
                CAST_DTK_DTKID dl_dtk[DTK_DTKID] ;

        /* 
        -- the fields are filled in; now insert this 
        -- record into the db:
        */
        nrecs_inserted = db_insert_single_record(APS_dbproc,
            dl2obs_insert, APS_TABLE(DL2OBS), APS_CDEFS(DL2OBS) ) ;

        if ( nrecs_inserted != 1 )
        {
            DEL_LIST( obs_dtks ) ;
            free_db_record( dtk_rec ) ;
            free_db_record( dl2obs_insert ) ;
            return DTKM_ERROR_dl2obs_rec_NOT_INSERTED ;
        }
    }
    /* dl2obs_insert and obs_dtks no longer needed: */
    free_db_record( dl2obs_insert ) ;
    DEL_LIST( obs_dtks ) ;


    return DTKM_DOWNLINK_REC_FOUND ;
}


/*==============================================================================
Function:       dtkm_dl2obs()

Description:    given a downlink dtk rec, get the observations being 
                downlinked on it.  

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 18 16:33:59 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int
dtkm_dl2obs(
    DB_RECORD   **dl_dtk,   /* input downlink dtk record.         */
    llist       *obs_dtks ) /* output list of corresponding observation dtks. */
{
    int     return_code ;

    /* quick error checking.  */
    if ( dl_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( obs_dtks == NULL )
        return DTKM_ERROR_NULL_OUTPUT_DTK_LIST ;

    if ( NUMELTS( obs_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY ;

    if(  !( dtkm_is_a_downlink(dl_dtk)==TRUE )  ) 
        return DTKM_ERROR_DTK_IS_NOT_A_DOWNLINK ;
    /*
    -- 1.  first try using the link dl2obs relation.  
    -- 2.  if that gets nothing, try using the dtk.fa_schedule_link field.  
    --     if this works, then add a new entry for each found observation.  
    -- 3.  if both tries get nothing, then there were no observations found.  
    --     probably the observations will be proposed later.  Not an error.  
    */

    /* first try with link dl2obs relation: */
    return_code = dtkm_get_obs_dtks( dl_dtk, obs_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    /* see if we got something.  */
    if ( NUMELTS( obs_dtks ) > 0 )
        return TRUE ;

    /* nothing.  now try with dtk.fa_schedule_link  */
    return_code = dtkm_link_obs_dtks( dl_dtk, obs_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_count_obs_dtks4SHAQ()

Description:    given an input realtime downlink, count the realtime 
                observations that are downlinked by it, using 
                only the fa_schedule_id field.  
                Used in SHAQ file processing, not in regular lib_dtkm 
                processing.  

Creator:        Lawrence Stevens

Creation Date:  Wed Mar 26 13:12:12 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_count_obs_dtks4SHAQ( 
    DB_RECORD   **dl_dtk )  /* input downlink downlink dtk proposal.    */
{
    llist       *obs_dtks = NULL ;
    int         return_code ;
    int         count ;

    if( dl_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    obs_dtks = create_dyn_llist() ;

    /* find and count links using fa_schedule_link  */
    return_code = dtkm_link_obs_dtks( dl_dtk, obs_dtks ) ;
    count = NUMELTS( obs_dtks ) ;
    DEL_LIST( obs_dtks ) ;
    if( return_code < 0 )
        return return_code ;

    return count ;
}

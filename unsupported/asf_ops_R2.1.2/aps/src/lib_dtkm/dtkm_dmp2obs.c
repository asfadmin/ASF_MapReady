THIS FILE IS OBSOLETE.  REPLACED BY dtkm_dl2obs.c


Tue Mar 18 14:58:09 PST 1997


#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_dmp2obs.c

Description:    given a tape dump record, get the dtks that it is 
                downlinking.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_dmp2obs.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_dmp2obs.c"


#include "dtkm.h"
#include <db_dtk.h>
#include <db_dtk_obs_dmp.h>

static char     DUMP_CODE[] = "DMP" ;


/*==============================================================================
Function:       dtkm_get_obs_dtks()

Description:    use the dtk_obs_dmp relation to retrieve a list 
                of data-takes that are downlinked by the input dmp data-take.

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 18 17:09:53 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_get_obs_dtks( 
    DB_RECORD   **dmp_dtk,   /* input tape dump downlink dtk record.         */
    llist       *obs_dtks )  /* output list of corresponding recording dtks. */
{
    cursor      dtk_dmp_obs_ptr ;
    llist       *dtk_dmp_obs_list = NULL ;
    DB_RECORD   **dtk_dmp_obs_rec ;

    cursor      dtk_list_ptr ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec ;

    cursor      obs_dtks_ptr ;
    llist       *list_check = NULL ;

    sprintf( where_clause, 
        "where %s = '%s' and %s = %ld and %s = %d and %s = '%s'",
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_SAT), 
                                CAST_DTK_SAT dmp_dtk[DTK_SAT],
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_REV_DMP), 
                                CAST_DTK_REV dmp_dtk[DTK_REV],
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_DTKID_DMP), 
                                CAST_DTK_DTKID dmp_dtk[DTK_DTKID],
        APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_STATION_ID_DMP), 
                                CAST_DTK_STATION_ID dmp_dtk[DTK_STATION_ID] ) ;

    dtk_dmp_obs_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK_OBS_DMP), where_clause, NULL, 
        APS_CDEFS(DTK_OBS_DMP), ALL_COLS) ;

    if ( dtk_dmp_obs_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS( dtk_dmp_obs_list ) == 0 )
    {
        DEL_LIST( dtk_dmp_obs_list ) ;
        return TRUE ;
    }

    for (   dtk_dmp_obs_rec = (DB_RECORD **) 
                FIRST(dtk_dmp_obs_list, dtk_dmp_obs_ptr);
            dtk_dmp_obs_rec ;
            dtk_dmp_obs_rec = (DB_RECORD **) 
                NEXT(dtk_dmp_obs_list, dtk_dmp_obs_ptr)
        )
    {
        /* 
        -- get each of the recording dtk(s) from the dtk relation 
        -- and append it to the output list obs_dtks if not DEL status
		-- and the actid identifies it as a recording observation
		-- (ie: the actid is like RE%% ) 
		-- 
		-- NOTE: This is currently equivalent to not INV status.
        */
        sprintf( where_clause,
"where %s = '%s' and %s = '%s' and %s = %ld and %s = %d and %s != '%s' and %s like '%s'",
            APS_COL(DTK, DTK_SAT), 
            CAST_DTK_OBS_DMP_SAT dtk_dmp_obs_rec[DTK_OBS_DMP_SAT],
            APS_COL(DTK, DTK_SENSOR), 
            CAST_DTK_OBS_DMP_SENSOR_OBS dtk_dmp_obs_rec[DTK_OBS_DMP_SENSOR_OBS],
            APS_COL(DTK, DTK_REV), 
            CAST_DTK_OBS_DMP_REV_OBS dtk_dmp_obs_rec[DTK_OBS_DMP_REV_OBS],
            APS_COL(DTK, DTK_DTKID), 
            CAST_DTK_OBS_DMP_DTKID_OBS dtk_dmp_obs_rec[DTK_OBS_DMP_DTKID_OBS],
            APS_COL(DTK, DTK_DTKSTAT), "DEL", 
            APS_COL(DTK, DTK_ACTID), "RE%%");

        dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;

        if ( dtk_list == NULL )
        {
            DEL_LIST( dtk_dmp_obs_list ) ;
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
            DEL_LIST( dtk_dmp_obs_list ) ;
            return DTKM_ERROR_DUPLICATE_DTK_KEYS ;
        }

        dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

        /*  
		-- if this is a DMP (our input dtk), then delete it from this list.  
		*/
        if ( strcmp(CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], DUMP_CODE ) == 0 )
        {
            DEL_AT_CURSOR( obs_dtks, obs_dtks_ptr ) ;
        	DEL_LIST( dtk_list ) ;

			/* now continue with the next record.  */
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

    DEL_LIST( dtk_dmp_obs_list ) ;
    return TRUE ;

}


/*==============================================================================
Function:       dtkm_update_link_dmp2obs()

Description:    given an DMP find its recording data-takes
                which downlinks its data, linking by the field 
                dtk.fa_schedule_link.  

                When they are found, append  new records to the 
				dtk_obs_dmp relation, now that the link between 
				the DMP and recording data-takes has been established. 

Creator:        Miguel Siu      

Creation Date:  Fri Mar 29 10:15:28 PST 1996

Notes:      
	We always check to make sure that the dtk_obs_dmp needs the new records.

	Successful return value for this routine is TRUE.
==============================================================================*/
int dtkm_update_link_dmp2obs( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **dmp_dtk ) /* input observation (recording) data-take.      */
{

    llist       *dtk_list = NULL ;
    cursor      dtk_list_ptr ;
    DB_RECORD   **dtk_rec = NULL ;

    llist		*obs_dtks = NULL ;
    cursor      obs_dtks_ptr ;

    llist		*dtk_dmp_obs_list = NULL ;
    cursor      dtk_dmp_obs_ptr ;
    DB_RECORD   **dtk_dmp_obs_rec = NULL ;

    llist       *list_check = NULL ;
    DB_RECORD   **dtk_obs_dmp_insert = NULL ;
    DB_RECORD   **dtk_unlinked_rec = NULL ;


    int         nrecs_inserted ;
	int			return_code ;

	/*
	-- if this datatake has a DTK.fa_schedule_link value of NULL or blanks,
	-- then we know it came from a REQUEST file, and that there are no
	-- observation/recordings to link to.
	-- So ignore the current datatake proposal
	*/
	if (*(CAST_DTK_FA_SCHEDULE_LINK dmp_dtk[DTK_FA_SCHEDULE_LINK][0]) == '\0'
	||  *(CAST_DTK_FA_SCHEDULE_LINK dmp_dtk[DTK_FA_SCHEDULE_LINK][0]) == ' ' )
		return (TRUE) ;

	/*
	-- get all recordings which correspond to the input DMP datatake record
	-- Then, ignore any of the recordings which have a status "DEL"
	-- and the actid identifies it as a recording observation
	-- (ie: the actid is like RE%% ) 
	-- 
	-- NOTE: This is currently equivalent to not INV status.
	*/
    sprintf( where_clause,
        "where %s = '%s' and %s = '%s' and %s != '%s' and %s like '%s'",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dmp_dtk[DTK_SAT],
        APS_COL(DTK, DTK_FA_SCHEDULE_LINK),
            CAST_DTK_FA_SCHEDULE_LINK dmp_dtk[DTK_FA_SCHEDULE_LINK],
        APS_COL(DTK, DTK_DTKSTAT), "DEL" ,   
        APS_COL(DTK, DTK_ACTID), "RE%%");

	dtk_dmp_obs_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
		APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;

    if ( dtk_dmp_obs_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS( dtk_dmp_obs_list ) == 0 )
    {
		/* 
		-- There are no recordings, 
		-- no entries need be made to dtk_obs_dmp 
		*/
        DEL_LIST( dtk_dmp_obs_list ) ;
        return TRUE ;
    }

	/*
	-- create output list obs_dtks
	*/
	obs_dtks = create_dyn_llist() ;


    for (   dtk_dmp_obs_rec = (DB_RECORD **) 
                FIRST(dtk_dmp_obs_list, dtk_dmp_obs_ptr);
            dtk_dmp_obs_rec ;
            dtk_dmp_obs_rec = (DB_RECORD **) 
                NEXT(dtk_dmp_obs_list, dtk_dmp_obs_ptr)
        )
    {
        /*  
		-- if this is a DMP (our input dtk), then ignore it.
		*/
        if ( strcmp(CAST_DTK_SENSOR 
				dtk_dmp_obs_rec[DTK_SENSOR], DUMP_CODE ) == 0 )
        {
            continue ;
        }

        /* 
        -- if the current dtk has a dtk_obs_dmp entry, do not 
		-- include it in obs_dtks
        */
		sprintf( where_clause,
			"where %s = '%s' and %s = '%s' and %s = %ld and %s = %d ",
			APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_SAT),
					CAST_DTK_SAT dtk_dmp_obs_rec[DTK_SAT],
			APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_SENSOR_OBS),
				 CAST_DTK_SENSOR dtk_dmp_obs_rec[DTK_SENSOR],
			APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_REV_OBS),
					CAST_DTK_REV dtk_dmp_obs_rec[DTK_REV],
			APS_COL(DTK_OBS_DMP, DTK_OBS_DMP_DTKID_OBS),
				  CAST_DTK_DTKID dtk_dmp_obs_rec[DTK_DTKID] ) ;

        dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
            APS_TABLE(DTK_OBS_DMP), where_clause, NULL, 
			APS_CDEFS(DTK_OBS_DMP), ALL_COLS) ;
        if ( dtk_list == NULL )
        {
			DEL_LIST( obs_dtks ) ;
            DEL_LIST( dtk_dmp_obs_list ) ;
			free_db_record( dtk_dmp_obs_rec) ;
            return DTKM_ERROR_DB_QUERY_FAILED ;
        }

        if ( NUMELTS( dtk_list ) != 0 )
        {
			/* 
			-- there is an entry in dtk_obs_dmp already, skip current record.
			*/
            DEL_LIST( dtk_list ) ;
            continue ;
        }


        /* 
        -- move current recording into obs_dtks
        */
		dtk_unlinked_rec = (DB_RECORD **) 
			UNLINK_AT_CURSOR(dtk_dmp_obs_list, dtk_dmp_obs_ptr) ;
		APPEND (obs_dtks, dtk_unlinked_rec, free_db_record, dtk_unlinked_rec) ;

        DEL_LIST( dtk_list ) ;

        /* now continue with the next record.  */

    }

    DEL_LIST( dtk_dmp_obs_list ) ;


    /*
    -- now create a new db record in the dtk_dmp_obs relation for each 
    -- record in the obs_dtks list.  
    */
    dtk_obs_dmp_insert = new_table_record(APS_CDEFS(DTK_OBS_DMP)) ;

    for (   dtk_rec = (DB_RECORD **) FIRST(obs_dtks, obs_dtks_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(obs_dtks, obs_dtks_ptr)
        )
    {
        /* 
        -- now fill each data field in the 
        -- dtk_dmp_obs DB_RECORD: 
        */
        /* 
        -- first the OBS sat, sensor, rev, dtkid fields 
        -- filled from the dtk_rec, which is the observation.
        */
        strcpy( CAST_DTK_OBS_DMP_SAT dtk_obs_dmp_insert[DTK_OBS_DMP_SAT], 
            CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
        strcpy( CAST_DTK_OBS_DMP_SENSOR_OBS 
			dtk_obs_dmp_insert[DTK_OBS_DMP_SENSOR_OBS],
            CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) ;
        CAST_DTK_OBS_DMP_REV_OBS dtk_obs_dmp_insert[DTK_OBS_DMP_REV_OBS] = 
                CAST_DTK_REV dtk_rec[DTK_REV] ;
        CAST_DTK_OBS_DMP_DTKID_OBS dtk_obs_dmp_insert[DTK_OBS_DMP_DTKID_OBS] = 
                CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;

        /* 
        -- Now the DMP rev, dtkid, and station_id fields 
        -- filled from the dmp_dtk, which is the tape dump:
        */
        CAST_DTK_OBS_DMP_REV_DMP dtk_obs_dmp_insert[DTK_OBS_DMP_REV_DMP] = 
                CAST_DTK_REV dmp_dtk[DTK_REV] ;
        CAST_DTK_OBS_DMP_DTKID_DMP dtk_obs_dmp_insert[DTK_OBS_DMP_DTKID_DMP] = 
                CAST_DTK_DTKID dmp_dtk[DTK_DTKID] ;
        strcpy( CAST_DTK_OBS_DMP_STATION_ID_DMP 
                    dtk_obs_dmp_insert[DTK_OBS_DMP_STATION_ID_DMP], 
                CAST_DTK_STATION_ID dmp_dtk[DTK_STATION_ID] ) ;

        /* 
        -- the fields are filled in; now insert this 
        -- record into the db:
        */
        nrecs_inserted = db_insert_single_record(APS_dbproc,
            dtk_obs_dmp_insert, APS_TABLE(DTK_OBS_DMP), APS_CDEFS(DTK_OBS_DMP) ) ;

        if ( nrecs_inserted != 1 )
        {
            DEL_LIST( obs_dtks ) ;
			free_db_record( dtk_rec ) ;
            free_db_record( dtk_obs_dmp_insert ) ;
            return DTKM_ERROR_dtk_dmp_obs_rec_NOT_INSERTED ;
        }
    }
    /* dtk_dmp_obs_rec is no longer needed: */
    free_db_record( dtk_obs_dmp_insert ) ;
	DEL_LIST( obs_dtks ) ;


    return DTKM_DMP_REC_FOUND ;
}


/*==============================================================================
Function:       dtkm_link_obs_dtks()

Description:    given a dump data-take, find its recording data-takes, 
                linking by the field dtk.fa_schedule_link.  
                When they are found, put them into the output obs_dtks list.  

				NOTE:
				We will NOT append new records into the link relation 
				(dtk_obs_dmp), even though the link between the dump datatake
				and its recording datatakes has been established.  This is 
				because the dump datatake could still be missing a valid
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
    DBPROCESS       *APS_dbproc, 
    DB_RECORD   **dmp_dtk,   /* input tape dump downlink dtk record.         */
    llist       *obs_dtks )  /* output list of corresponding recording dtks. */
{

    cursor      dtk_list_ptr ;
    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec = NULL ;

    DB_RECORD   **dtk_dmp_obs_rec = NULL ;

    llist       *list_check = NULL ;

    int         nrecs_inserted ;

	/*
	-- if this datatake has a DTK.fa_schedule_link value of NULL or blanks,
	-- then we know it came from a REQUEST file, and that there are no
	-- observation/recordings to link to.
	-- So ignore the current datatake proposal
	*/
	if (*(CAST_DTK_FA_SCHEDULE_LINK dmp_dtk[DTK_FA_SCHEDULE_LINK][0]) == '\0'
	||  *(CAST_DTK_FA_SCHEDULE_LINK dmp_dtk[DTK_FA_SCHEDULE_LINK][0]) == ' ' )
		return (TRUE) ;

    /* 
    -- use the single link field DTK_FA_SCHEDULE_LINK 
    -- to get the obs dtks if not DEL status
	-- and the actid identifies it as a recording observation
	-- (ie: the actid is like RE%% ) 
	-- 
	-- NOTE: This is currently equivalent to not INV status.
    */
    sprintf( where_clause, 
        "where %s = '%s' and %s = '%s' and %s != '%s' and %s like '%s'",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dmp_dtk[DTK_SAT],
        APS_COL(DTK, DTK_FA_SCHEDULE_LINK), 
            CAST_DTK_FA_SCHEDULE_LINK dmp_dtk[DTK_FA_SCHEDULE_LINK],
            APS_COL(DTK, DTK_DTKSTAT), "DEL",
            APS_COL(DTK, DTK_ACTID), "RE%%");

    dtk_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        return DTKM_ERROR_DB_QUERY_FAILED ;
    }

    if ( NUMELTS(dtk_list) == 0 )
    {
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
Function:       dtkm_dmp2obs()

Description:    given a tape dump dtk rec, get the recordings being 
                downlinked on it.  

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 18 16:33:59 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int
dtkm_dmp2obs(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dmp_dtk,   /* input tape dump downlink dtk record.         */
    llist       *obs_dtks )  /* output list of corresponding recording dtks. */
{
    int     return_code ;

    /* quick error checking.  */
    if ( dmp_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( obs_dtks == NULL )
        return DTKM_ERROR_NULL_OUTPUT_DTK_LIST ;

    if ( NUMELTS( obs_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY ;

    if ( strcmp( CAST_DTK_SENSOR dmp_dtk[DTK_SENSOR], DUMP_CODE ) != 0 )
        return DTKM_ERROR_DTK_IS_NOT_DMP ;

    /*
    -- 1.  first try using the link dtk_obs_dmp relation.  
    -- 2.  if that gets nothing, try using the dtk.fa_schedule_link field.  
    --     if this works, then add a new entry for each found recording.  
    -- 3.  if both tries get nothing, then there were no recordings found.  
    --     probably the recordings will be proposed later.  Not an error.  
    */

    /* first try with link dtk_obs_dmp relation: */
    return_code = dtkm_get_obs_dtks( dmp_dtk, obs_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    /* see if we got something.  */
    if ( NUMELTS( obs_dtks ) > 0 )
        return TRUE ;

    /* nothing.  now try with dtk.fa_schedule_link  */
    return_code = dtkm_link_obs_dtks( APS_dbproc, dmp_dtk, obs_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    return TRUE ;

}

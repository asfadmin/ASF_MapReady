#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_update_dtk_record.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dtk_record.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dtk_record.c"



/*==============================================================================
Function:       dtkm_update_dtk_record()

Description:    updates EVERY field in an existing data-take record in 
                the dtk relation from a dtk DB_RECORD.  
                to use this routine, be sure to start with the existing
                values in a DB_RECORD and update the desired values in that 
                DB_RECORD, then call this routine to update the database.  

Returns:
    int
    = 0   OK:

    < 0   ERROR:


Creator:        Lawrence Stevens

Creation Date:  Mon Oct 30 13:00:27 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h" 
#include "db_dl2obs.h" 
#include "timeconv.h"   /* for tc_systime2yyyycddd()  */

#include <string.h>     /* for strcpy  */

int dtkm_update_dtk_record( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **input_dtk,    /* rec to update.  */
    DB_RECORD   **result_dtk,   /* result data-take; can be = input_dtk    */
    llist       *dtk_updates )  /* update duplicated into list */
{

    llist           *list_check = NULL ;

    DB_RECORD       **dtk_rec ;
    llist           *dtk_list = NULL ;
    cursor          dtk_list_ptr ;

    char            dtkdate[ASF_TIME_STR_LENGTH+1] ;
    int             nrecs ;
    int             nrecs_retrieved ;
    int             return_code ;

    char            dtk_fields_to_set[2048] ;
    char            add_field_to_set[128] ;

#ifdef PRINT_DIAG
    printf("%s(%d):  starting dtkm_update_dtk_record() \n", __FILE__, __LINE__);
#endif
    if ( input_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( result_dtk  == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    if ( dtk_updates  == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    return_code = db_copy_record( APS_CDEFS(DTK), result_dtk, input_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* 
    -- the record to update must already exist in the database.  
    -- it will be identified by either dtkid or fadtkid.  
    -- must see which:
    */
    if ( CAST_DTK_DTKID result_dtk[DTK_DTKID] == 0 )
    {
        /* 
        -- NOTE:  if you run purecov, there will not be coverage 
        -- here because the updating of DTKID using FADTKID 
        -- is also done elsewhere in processing.  this piece of code 
        -- has been rendered redundant by modifications.  
        */
        /*
        -- we expect to identify an existing record in the
        -- dtk relation and to find out its dtkid, to complete
        -- the primary key.
        */
        sprintf(where_clause,
            "where %s = '%s' and %s = %ld and %s = '%s'",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT result_dtk[DTK_SAT],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV result_dtk[DTK_REV],
            APS_COL(DTK,DTK_FADTKID),
                                CAST_DTK_FADTKID result_dtk[DTK_FADTKID]);
 
        /* must clean up existing_dtk_list later.  */
        dtk_list = db_get_records(APS_dbproc, APS_TABLE(DTK),
            where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
        if (dtk_list == NULL)
            return DTKM_ERROR_DB_QUERY_FAILED ;
 
        nrecs_retrieved = NUMELTS( dtk_list ) ;
        if ( nrecs_retrieved == 1 )
        {
            /*
            -- normal condition.  
            -- now we can obtain the correct dtkid to use to delete
            -- the correct record from the datbase.
            -- copy the dtkid to complete the primary key of
            -- the record we are updating.
            */

            dtk_rec = (DB_RECORD **) FIRST( dtk_list, dtk_list_ptr);
            CAST_DTK_DTKID result_dtk[DTK_DTKID] =
                CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;
        }
        else if ( nrecs_retrieved == 0 )
        {
            /*
            -- ERROR:
            -- there is no record that matches this record; 
            */
            DEL_LIST( dtk_list ) ;
            return DTKM_ERROR_ATTEMPT_TO_UPDATE_NONEXISTING_DTK ;
        }
        else
        {
            /*
            -- ERROR:  
            -- there is more than one record that matches this record; 
            */
            DEL_LIST( dtk_list ) ;
            return DTKM_ERROR_CANNOT_IDENTIFY_DTK_TO_UPDATE ;
        }
        /* clean up the list used.  */
        DEL_LIST( dtk_list ) ;
    }

    /*
    -- get today's date:  yyyy:ddd:hh:mm:ss.sss
    */
    return_code = tc_systime2asf(dtkdate);

    /*
    -- dtkdate, which is today's date.  
    -- the rest of the values come from the input dtk DB_RECORD
    */
    /* 
    -- set up the result data-take; write the new values.    
    */
    strcpy(CAST_DTK_DTKDATE result_dtk[DTK_DTKDATE], dtkdate ) ;

    /* 
    -- at this point, the result_dtk is prepared.  
    */

    /*
    -- now get ready for a call to db_update_records().  
    -- first, set up the where_clause to identify the 
    -- record to the database by its primary keys.  
    */
    sprintf(where_clause,
        "where %s = '%s' and %s = %ld and %s = %d",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT result_dtk[DTK_SAT],
        APS_COL(DTK, DTK_REV), CAST_DTK_REV result_dtk[DTK_REV],
        APS_COL(DTK,DTK_DTKID), CAST_DTK_DTKID result_dtk[DTK_DTKID] ) ;

    /*
    -- now, we need to set up dtk_fields_to_set[] which will be a 
    -- long string like this:
    -- dtk.actid = 'ROBNAS', dtk.strttime = "1996:001:00:00:00.000", 
    -- In other words, a field = value, field = value, ...
    -- kind of a string.  SUPER long for the dtk relation, 
    -- since there are 33 fields in the relation 
    -- now (Thu Mar 27 17:41:00 PST 1997)
    -- Since this field is so long, we put the first field = value, 
    -- into dtk_fields_to_set[] 
    -- then we sprintf() to add_field_to_set[] for the second 
    -- field, but this time we strcat() it to dtk_fields_to_set[].
    -- then we continue on until every one of the data fields 
    -- is set up in dtk_fields_to_set[].  
    */

    /*   2 sensor             char              3   */
    sprintf(dtk_fields_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_SENSOR), CAST_DTK_SENSOR result_dtk[DTK_SENSOR] ) ;

    /*   5 fadtkid            char             20   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_FADTKID), CAST_DTK_FADTKID result_dtk[DTK_FADTKID] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*   6 darid              DaridType         4   */
    sprintf(add_field_to_set, "%s = %ld, \n", 
        APS_COL(DTK, DTK_DARID), CAST_DTK_DARID result_dtk[DTK_DARID] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*   7 actid              ActidType         6   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_ACTID), CAST_DTK_ACTID result_dtk[DTK_ACTID] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*   8 ascdsc             AscdscType        1   */
    sprintf(add_field_to_set, "%s = '%c', \n", 
        APS_COL(DTK, DTK_ASCDSC), CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*   9 strttime           AsftimeType      21   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_STRTTIME), 
        CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  10 stoptime           AsftimeType      21   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_STOPTIME), 
        CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  11 strtlat            Latitude4Type     4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_STRTLAT), 
        CAST_DTK_STRTLAT result_dtk[DTK_STRTLAT] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  12 stoplat            Latitude4Type     4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_STOPLAT), 
        CAST_DTK_STOPLAT result_dtk[DTK_STOPLAT] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  13 nrlat1             Latitude4Type     4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_NRLAT1), 
        CAST_DTK_NRLAT1 result_dtk[DTK_NRLAT1] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  14 nrlon1             Longitude4Type    4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_NRLON1), 
        CAST_DTK_NRLON1 result_dtk[DTK_NRLON1] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  15 farlat1            Latitude4Type     4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_FARLAT1), 
        CAST_DTK_FARLAT1 result_dtk[DTK_FARLAT1] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  16 farlon1            Longitude4Type    4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_FARLON1), 
        CAST_DTK_FARLON1 result_dtk[DTK_FARLON1] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  17 nrlat2             Latitude4Type     4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_NRLAT2), 
        CAST_DTK_NRLAT2 result_dtk[DTK_NRLAT2] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  18 nrlon2             Longitude4Type    4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_NRLON2), 
        CAST_DTK_NRLON2 result_dtk[DTK_NRLON2] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  19 farlat2            Latitude4Type     4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_FARLAT2), 
        CAST_DTK_FARLAT2 result_dtk[DTK_FARLAT2] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  20 farlon2            Longitude4Type    4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_FARLON2), 
        CAST_DTK_FARLON2 result_dtk[DTK_FARLON2] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  21 lookangl           real              4   */
    sprintf(add_field_to_set, "%s = %.7f, \n", 
        APS_COL(DTK, DTK_LOOKANGL), 
        CAST_DTK_LOOKANGL result_dtk[DTK_LOOKANGL] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  22 dtkstat            DtkstatType       3   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_DTKSTAT), 
        CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  23 proposed_dtkstat   DtkstatType       3   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_PROPOSED_DTKSTAT), 
        CAST_DTK_PROPOSED_DTKSTAT result_dtk[DTK_PROPOSED_DTKSTAT] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  24 transid            TransidType       2   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_TRANSID), 
        CAST_DTK_TRANSID result_dtk[DTK_TRANSID] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  25 sitename           SitenameType     32   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_SITENAME), 
        CAST_DTK_SITENAME result_dtk[DTK_SITENAME] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  26 notes              varchar          40   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_NOTES), 
        CAST_DTK_NOTES result_dtk[DTK_NOTES] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  27 dtkdate            AsftimeType      21   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_DTKDATE), CAST_DTK_DTKDATE result_dtk[DTK_DTKDATE] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  28 station_id         StationidType     3   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_STATION_ID), 
        CAST_DTK_STATION_ID result_dtk[DTK_STATION_ID] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  29 fa_schedule_link   char             20   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_FA_SCHEDULE_LINK), 
        CAST_DTK_FA_SCHEDULE_LINK result_dtk[DTK_FA_SCHEDULE_LINK] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  30 planner_quicklook  YesNoType         1   */
    sprintf(add_field_to_set, "%s = '%c', \n", 
        APS_COL(DTK, DTK_PLANNER_QUICKLOOK), 
        CAST_DTK_PLANNER_QUICKLOOK result_dtk[DTK_PLANNER_QUICKLOOK] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  31 science_quicklook  YesNoType         1   */
    sprintf(add_field_to_set, "%s = '%c', \n", 
        APS_COL(DTK, DTK_SCIENCE_QUICKLOOK), 
        CAST_DTK_SCIENCE_QUICKLOOK result_dtk[DTK_SCIENCE_QUICKLOOK] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  32 submit_time        AsftimeType      21   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_SUBMIT_TIME), 
        CAST_DTK_SUBMIT_TIME result_dtk[DTK_SUBMIT_TIME] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  33 antenna_id         Antenna_idType    2   */
    sprintf(add_field_to_set, "%s = %d, \n", 
        APS_COL(DTK, DTK_ANTENNA_ID), 
        CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /* these fields can be updated only by FA:      */
    /*  34 fa_strttime        AsftimeType      21   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_FA_STRTTIME), 
        CAST_DTK_FA_STRTTIME result_dtk[DTK_FA_STRTTIME] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  35 fa_stoptime        AsftimeType      21   */
    sprintf(add_field_to_set, "%s = '%s', \n", 
        APS_COL(DTK, DTK_FA_STOPTIME), 
        CAST_DTK_FA_STOPTIME result_dtk[DTK_FA_STOPTIME] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  36 fa_duration_min    real              4   */
    sprintf(add_field_to_set, "%s = %.7f \n", 
        APS_COL(DTK, DTK_FA_DURATION_MIN), 
        CAST_DTK_FA_DURATION_MIN result_dtk[DTK_FA_DURATION_MIN] ) ;
    strcat( dtk_fields_to_set, add_field_to_set ) ;

    /*  37 asf_reduction_min  real              4   */
    if ( tc_validate_asf_datetime( 
            CAST_DTK_FA_STRTTIME result_dtk[DTK_FA_STRTTIME] ) == TRUE 
    &&   tc_validate_asf_datetime( 
            CAST_DTK_FA_STOPTIME result_dtk[DTK_FA_STOPTIME] ) == TRUE  )
    {
        /* 
        -- OK. the fa_time fields are OK.  
        -- maintain the asf_reduction_min field.  
        -- this means to calculate the new value (in minutes) 
        -- in case there was a change to the strt- or stoptimes
        -- and therefore a change to the asf reduction.  
        -- note the comma (,) pre-pended to this optional update.  
        */
        return_code = dtkm_set_asf_reduction_min( result_dtk, result_dtk ) ;
        sprintf(add_field_to_set, ", %s = %.5f \n", 
            APS_COL(DTK, DTK_ASF_REDUCTION_MIN), 
            CAST_DTK_ASF_REDUCTION_MIN result_dtk[DTK_ASF_REDUCTION_MIN] ) ;
        strcat( dtk_fields_to_set, add_field_to_set ) ;
    }

    nrecs = db_update_records(APS_dbproc, APS_TABLE(DTK), 
        dtk_fields_to_set, where_clause);
    if ( nrecs != 1 )
        return DTKM_ERROR_DTK_NOT_UPDATED ;

    /*
    -- If this data-take is an observation and the 
    -- status is DEL or CMB, then we must remove the entry
    -- to it in the dl2obs relations.  This is so the status 
    -- cannot get changed back from DEL to something 
    -- else when the status of the downlink is changed.  
    -- This is the difference between REJ and DEL (and CMB).  
    -- A REJ data-take can come back into the schedule.  
    */
    if( strcmp("DEL", CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) == 0 
    ||  strcmp("CMB", CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) == 0 )
    {
        return_code = dtkm_delete_dl2obs_recs( APS_dbproc, result_dtk,
            dtk_updates ) ;
        if( return_code < 0 )
                return return_code ; 
    }

    /*
    -- save a duplicate of the update into the
    -- dtk_updates list 
    */
    list_check = dtkm_duplicate_dtk_into_list( result_dtk,
        dtk_updates ) ;
    if ( list_check != dtk_updates )
        return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;

    return DTKM_DTK_UPDATED_OK ;

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

#undef PRINT_DIAG

/*==============================================================================
Filename:   dtkm_update_dtk_darid.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dtk_darid.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dtk_darid.c"


/*==============================================================================
Function:       dtkm_update_dtk_darid()

Description:    updates current darid value to a new value,
                which is determined by querying the database and
                selecting any observation dtks which overlap 
                the input observation dtk.

                This routine will work for input observation dtk ONLY.
                Currently, downlink dtk may retain a darid value of 0,
                that is why we exit if a downlink is detected.

                When there are several observation dtk overlapping the
                input observation dtk, we use the following formula to
                determine which observation dtk has a greater overlap:

                                     (overlap in seconds)
                % overlap =  ------------------------------------               
                              (length of input observation dtk)

Returns:        
                TRUE.       The darid value was changed.
                FALSE.      The darid value was not changed.
                < 0         An error ocurred.

Creator:        Miguel Siu

Creation Date:  Wed Apr 30 09:20:07 PDT 1997

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>     /* for strcmp strcpy  */
#include <timeconv.h>   /* for tc_et_ASF_datetime_diff */

int dtkm_update_dtk_darid(
    DB_RECORD   **dtk_proposal)     /* input observation dtk */
{
    int         largest_pct_darid ; /* darid for overlapping observation dtk */
    double      overlap_pct ;       /* percentage of overlap */
    double      largest_overlap_pct ;
    double      dtk_proposal_diff ;
    double      overlap_dtk_diff ;
    cursor      retrieval_ptr ;
    DB_RECORD   **overlap_dtk_rec ;
    llist       *dtk_retrievals = NULL ;

    char        delta_strttime[] = "yyyy:ddd:hh:mm:ss.ccc" ;
    char        delta_stoptime[] = "yyyy:ddd:hh:mm:ss.ccc" ;


    /* 
    -- quick error checking.  
    -- The dtk_proposal should be a recording or a realtime observation,
    -- but never, never should it be a realtime downlink or tapedump
    -- The dtk_proposal should have a darid value = 0; we don't want to
    -- replace a darid value which already exists.
    */
    if ( strcmp(CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR],
            DTKM_SENSOR_REALTIME_DOWNLINK_CODE) == 0 )
        return FALSE ;
    if ( strcmp(CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR],
            DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE) == 0 )
        return FALSE ;
    if ( CAST_DTK_DARID dtk_proposal[DTK_DARID] != 0 )
        return FALSE ;
    if (tc_et_ASF_datetime_diff(CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME],
                                CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME],
                                &dtk_proposal_diff) != TRUE)
        return DTKM_ERROR_COMPUTING_TIME_DIFF;

    /*
    -- get all overlaps for input observation dtk
    -- We don't match on rev because we are fetching overlap observation dtks;
    -- an observation dtk could conceivably be in a different rev and still
    -- overlap the input observation dtk
    -- So we will look at the current rev +- 1, to quicken the search.  
    -- actually, ovrlapping observations should always be in the 
    -- same rev, but this bracket search is both more robust and still fast.  
    */
    sprintf(where_clause,
"where %s = '%s' and \
%s >= %ld and %s <= %ld and \
%s != '%s' and %s < '%s' and %s > '%s' and (%s like '%s%%' or %s like '%s%%') ",
        APS_COL(DTK, DTK_SAT),      CAST_DTK_SAT dtk_proposal[DTK_SAT],
        APS_COL(DTK, DTK_REV),     ( CAST_DTK_REV dtk_proposal[DTK_REV] ) - 1,
        APS_COL(DTK, DTK_REV),     ( CAST_DTK_REV dtk_proposal[DTK_REV] ) + 1,
        APS_COL(DTK, DTK_DTKSTAT),  "DEL",
        APS_COL(DTK, DTK_STRTTIME), CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME],
        APS_COL(DTK, DTK_STOPTIME), CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME],
        APS_COL(DTK, DTK_ACTID),    DTKM_ACTID_REALTIME_OBSERVATION_CODE, 
        APS_COL(DTK, DTK_ACTID),    DTKM_ACTID_RECORDING_OBSERVATION_CODE ) ;

    dtk_retrievals = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS ) ;
    if ( dtk_retrievals == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    largest_pct_darid = 0 ; /* this should acquire a non-zero value */
    largest_overlap_pct = 0.0 ;

    for ( overlap_dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, retrieval_ptr);
          overlap_dtk_rec != NULL ;
          overlap_dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, retrieval_ptr) )
    {
        /*
        -- Don't examine any overlapping observations that have a darid = 0 ;
        -- they won't be useful in updating our input observation dtk.
        */
        if( CAST_DTK_DARID overlap_dtk_rec[DTK_DARID] == 0 )
            continue ;

        /*
        -- Compute the overlap by comparing the input observation dtk 
        -- with the overlapping observation dtk;
        -- subtract the largest starttime from the smallest stoptime to
        -- get the overlap time.
        */

        if (strcmp(CAST_DTK_STRTTIME    dtk_proposal[DTK_STRTTIME],
                   CAST_DTK_STRTTIME overlap_dtk_rec[DTK_STRTTIME]) < 0)
            /*
            -- overlap_dtk_rec has latest DTK_STRTTIME
            */
            strcpy(delta_strttime, 
                    CAST_DTK_STRTTIME overlap_dtk_rec[DTK_STRTTIME]) ;
        else 
            /* 
            -- dtk_proposal has latest DTK_STRTTIME
            */
            strcpy(delta_strttime,
                    CAST_DTK_STRTTIME    dtk_proposal[DTK_STRTTIME]) ;
            

        if (strcmp(CAST_DTK_STOPTIME    dtk_proposal[DTK_STOPTIME],
                   CAST_DTK_STOPTIME overlap_dtk_rec[DTK_STOPTIME]) < 0)
            /*
            -- dtk_proposal has earliest DTK_STOPTIME
            */
            strcpy(delta_stoptime, 
                   CAST_DTK_STOPTIME    dtk_proposal[DTK_STOPTIME]) ;
        else 
            /* 
            -- overlap_dtk_rec has earliest DTK_STOPTIME
            */
            strcpy(delta_stoptime,
                   CAST_DTK_STOPTIME overlap_dtk_rec[DTK_STOPTIME]) ;
            

        if ( tc_et_ASF_datetime_diff(delta_strttime, delta_stoptime, 
                                            &overlap_dtk_diff) != TRUE)
        {
            DEL_LIST( dtk_retrievals ) ;
            return DTKM_ERROR_COMPUTING_TIME_DIFF;
        }

        /*
        -- Determine the percentage of overlap.  The input observation dtk
        -- will inherit the darid value from the overlapping observation dtk
        -- with the LARGEST percentage overlap.
        */
        overlap_pct = overlap_dtk_diff / dtk_proposal_diff ;

#ifdef PRINT_DIAG
        printf ("FOR the following overlap_dtk_rec, this is percentage\n") ;
        dtkm_print( stdout, overlap_dtk_rec ) ;
        printf ("PERCENTAGE: %f\n", overlap_pct) ;
#endif

        if (overlap_pct > largest_overlap_pct)
        {
            largest_overlap_pct = overlap_pct ;
            largest_pct_darid = CAST_DTK_DARID overlap_dtk_rec[DTK_DARID] ;
        }   
    } /* end for  .. process dtk_retrievals list.  */
    DEL_LIST( dtk_retrievals ) ;

    /*
    -- If we could not find a darid value (ie: largest_pct_darid = 0)
    -- we don't do an update, and we return FALSE
    */
    if( largest_pct_darid == 0 )
        return FALSE ;
    else
    {
        CAST_DTK_DARID dtk_proposal[DTK_DARID] = largest_pct_darid ;
        return TRUE ;
    }

}

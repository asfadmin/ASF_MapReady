#undef PRINT_DIAG
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_determine_station_mask.c

Description:    find out if a data-take is within a planning mask.  check 
                ASF and MCM, if they are allowed.  Offer a time bracket 
                for the data-take that is guaranteed to be within the 
                station mask; the data-take might not fit all the way.  

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_determine_station_mask.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_determine_station_mask.c"

#include <dtkm_utilities.h>
#include <aps_db_table.h>
#include <db_station.h>
#include <db_dtk.h>
#include <string.h>



/*==============================================================================
Function:   dtkm_determine_station_mask

Description:    determines if a data-take takes place while the satellite is
within a station mask.  Checks for each mask in the station relation.  

Parameters:     
    DB_RECORD   **proposed_dtk      data-take to be checked.  
    int         secs,               criterion for minimum data-take.
                                    if the data-take is shorter than this, 
                                    or if the data-take is in the mask less
                                    time than this, the routine will report
                                    that the data-take is not in the mask - 
                                    the data-take is not approved as a real-
                                    time data-take.  

    char        *reduced_strttime,  if the dtk isn't all within the mask,
    char        *reduced_stoptime   reduced times are offered.  The reduced
                                    times indicate the times during the 
                                    proposed data-take that the satellite 
                                    is in the ASF mask and, are given even 
                                    when the entire data-take is in 
                                    the ASF mask.  

    char        *station_id         if the data-take is within a mask, 
                                    the mask station_id is given as output.  

Returns:        
    > 0 
        DTKM_DTK_HAS_TIME_IN_MASK   

    < 0
        DTK_RECORD_NULL
        DTKM_ERROR_DB_QUERY_FAILED
        DTK_ERROR_STRTTIME
        DTK_ERROR_STOPTIME
        DTK_HAS_NO_TIME_IN_MASK
        DTK_NO_MASKINOUT_RECS_FOUND   (possibly because there was no pass, i.e.,
                     the satellite did not enter the mask on that
                     rev.)
        DTK_MASKINOUT_ERR_MUST_RUN_CNOMCOV
                     an enter and exit even were not both found
                     for this satellite/rev.  Or more than these
                     records were found.  In any case, the 
                     the problem is solved by re-running 
                     coverage for that rev.  
                     This condition can arise when the planner 
                     inadvertantly runs create nominal coverage
                     with a time period that ends in the middle
                     of a pass, (when the satellite is in the
                     ASF mask) and the proposed data-take is 
                     in that rev.  

Creator:        Lawrence Stevens

Creation Date:  03/06/1995

Notes:      
==============================================================================*/

int dtkm_determine_station_mask ( 
    DB_RECORD   **proposed_dtk, 
    int         secs,               /* input min time to make it worthwile    */
    char        *reduced_strttime,  /* output start time for dtk within mask  */
    char        *reduced_stoptime,  /* output stop time for dtk within mask   */
                                    /* this could reduce the dtk a bit.       */
    char        *station_id )       /* output station id, if any, to take dtk */
{

    DB_RECORD   **station_rec ;
    llist       *station_list = NULL ;
    cursor      station_list_ptr ;

    int         return_code;

    if ( proposed_dtk == NULL ) 
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    strcpy(reduced_strttime, "" ) ;
    strcpy(reduced_stoptime, "" ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  proposed data-take = \n", __FILE__, __LINE__ ) ;
    dtkm_print(stdout,proposed_dtk) ;
#endif

    /* 
    -- obtain a list of the stations, used by the satellite,
    -- from the station relation:  
    */

    sprintf(where_clause, "where %s = '%s'", 
        APS_COL(STATION, STATION_SAT), CAST_DTK_SAT proposed_dtk[DTK_SAT] ) ;

    sprintf(orderby_cols, "%s", APS_COL(STATION, STATION_STATIONID) ) ;

    station_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(STATION), where_clause, orderby_cols, 
        APS_CDEFS(STATION), STATION_STATIONID, END_COLS) ;

    if ( station_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    if ( NUMELTS( station_list ) <= 0 )
    {
        DEL_LIST( station_list ) ;
        return DTKM_ERROR_NO_STATION_REC_FOR_DTK_SAT ;
    }

    for(
        station_rec = (DB_RECORD **) FIRST(station_list, station_list_ptr) ;
        station_rec ;
        station_rec = (DB_RECORD **) NEXT(station_list, station_list_ptr) 
        )
    {

#ifdef PRINT_DIAG
        printf("%s(%d):  checking stationid %3s\n", __FILE__, __LINE__, 
            CAST_STATION_STATIONID station_rec[STATION_STATIONID] ) ;
#endif

        strcpy(station_id, 
            CAST_STATION_STATIONID station_rec[STATION_STATIONID] ) ;

        return_code = dtkm_in_station_mask ( station_id,
            proposed_dtk, secs, reduced_strttime, reduced_stoptime ) ;

#ifdef PRINT_DIAG
        printf("%s(%d):  return from dtkm_in_station_mask:  return_code = %d\n",
            __FILE__, __LINE__, return_code ) ;
#endif

        if ( return_code == DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV )
        {
            /* 
            -- NOTE:  sometimes, a satellite will miss a ground station 
            -- and not have a pass.  also, the planner may have forgotten 
            -- to run coverage for that time.  
            -- Here, we need to check maybe more than one mask, and 
            -- perhaps there is no pass for a ground station, but there 
            -- is a pass for another ground station.  Therefore, we 
            -- want to keep on going to the next ground station.  
            -- Therefore, for us, there is no time in the mask for 
            -- this data-take.  
            -- So we set this code here in order to continue 
            -- processing here in a normal way:
            */
            return_code = DTKM_DTK_HAS_NO_TIME_IN_MASK ;
        }

        if ( return_code < 0 ) 
            return return_code ;

        if ( return_code == DTKM_DTK_HAS_TIME_IN_MASK )
        {
            DEL_LIST( station_list ) ;
            return DTKM_DTK_HAS_TIME_IN_MASK ;
        }
        if ( return_code != DTKM_DTK_HAS_NO_TIME_IN_MASK )
            return DTKM_ERROR_UNEXPECTED_RETURN_CODE_FROM_dtkm_in_station_mask ;
#ifdef PRINT_DIAG
        else
        {
            /* no time in this mask.  try another in the loop.  */
            printf( "%s(%d):  no time in mask; try another station_id.\n",
                __FILE__, __LINE__ ) ;
        }
#endif
    }

    /* no time in a mask found.  set return value for station_id to null */
    strcpy(station_id, "" ) ;

#ifdef PRINT_DIAG
    printf( "%s(%d):  no time in any mask.\n", __FILE__, __LINE__ ) ;
#endif

    DEL_LIST( station_list ) ;
    return DTKM_DTK_HAS_NO_TIME_IN_MASK ;
}

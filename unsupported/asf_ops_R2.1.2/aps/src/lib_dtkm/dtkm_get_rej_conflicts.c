#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_get_rej_conflicts.c

External Functions Defined:  dtkm_get_rej_conflicts()
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_get_rej_conflicts.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_rej_conflicts.c"


/*==============================================================================
Function:       dtkm_get_rej_conflicts()

Description:    retrieves data-takes from the DB and puts them into a linked 
                list.  The data-takes relate to the input data-take:
                1.  the data-takes are downlink data-takes on another antenna.
                2.  The data-takes overlap the antenna-time-padded input 
                    data-take time bracket.  
                Since these data-takes could have conflicted with the 
                newly-rejected data-take, we call them conflicts here.  
                They may be moved to the newly vacated antenna, if they 
                have preference and priority.  

                So we retrieve data-takes that "conflict" with a rejected 
                data-take.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov  5 18:12:17 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>         /* for dtk table                  */
#include <timeconv.h>       /* for tc_time_pad()              */

int dtkm_get_rej_conflicts( 
    DB_RECORD   **rej_dtk,        /* input dtk that was rejected.          */
    llist       *dtk_conflicts )  /* output data-takes to be re-submitted  */
{
    int     antenna_dtk_padding_time_sec  = 0 ;

    llist   *dtk_list = NULL ;
    llist   *dtk_llist_check = NULL ;
    char    antenna_strttime[ASF_TIME_STR_LENGTH+1] ;
    char    antenna_stoptime[ASF_TIME_STR_LENGTH+1] ;

    int     return_code ;

    /*
    -- quick parameter error checking
    */
    if ( rej_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_conflicts == NULL )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_conflicts ) != 0 )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY ;

    /*
    -- this is a REJ data-take proposal.  check to make sure 
    -- that this data-take was actually on a real antenna. 
    -- don't assume.  If the antenna id is <= 0 this simply 
    -- means that there is no work to do here; an antenna 
    -- was not freed up by the REJ/DEL action; there are no 
    -- data-takes that need to move at this time.  
    */
    if( (int) CAST_DTK_ANTENNA_ID rej_dtk[DTK_ANTENNA_ID] <= 0 )
        return TRUE ;

    /*
    --  Pad the input dtk time bracket with the sum of the 
    --  various times required for its (former) antenna to set 
    --  up, get ready, and to stand down.
    --  This antenna time bracket is what is used when checking
    --  other-satellite data-takes for conflicts.
    --
    --  This time bracket is formed only for the purpose of
    --  comparisons with data-takes from DIFFERENT SATELLITES.
    */
    return_code = dtkm_get_antenna_dtk_padding_time( 
        DB_SYBINT_USE_APS_READER_DBPROC, 
        rej_dtk, &antenna_dtk_padding_time_sec ) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    -- convert to minutes and pad the rej_dtk time bracket to
    -- get the antenna time bracket, to be used in the retrieve.
    -- tc_time_pad() needs a padding in terms of minutes.
    */
    if ( !tc_time_pad( 
        CAST_DTK_STRTTIME rej_dtk[DTK_STRTTIME], 
        CAST_DTK_STOPTIME rej_dtk[DTK_STOPTIME],
        (float) ( antenna_dtk_padding_time_sec/60.0),
        antenna_strttime, antenna_stoptime ) )
    {
        return DTKM_ERROR_PADDING_ANTENNA_TIME_BRACKET ;
    }

    /*
    -- Retrieve data-take records with:
    -- 1.  other satellites
    -- 2.  downlinking sensor  (= DTKM_SENSOR_REALTIME_DOWNLINK_CODE or
    --                            DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE  )
    -- 3.  any antenna id value ( because CON is retrieved. )
    -- 4.  time bracket that overlaps the antenna time bracket. 
    -- 5.  status is CON, QUE, SUB, PLN, or SCH
    --
    -- Note the comparison between a start and a stop time, when 
    -- trying to retrieve data-takes that overlap the antenna 
    -- time bracket.  This is intentional, and assumes that 
    -- start time < stop time.  
    */

    sprintf(where_clause, 
"where %s != '%s' and \
( %s = '%s' or %s = '%s' ) and \
%s < '%s' and %s > '%s' and \
( %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' )",

        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT rej_dtk[DTK_SAT],

        APS_COL(DTK, DTK_SENSOR), 
            DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
        APS_COL(DTK, DTK_SENSOR), 
            DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,

        APS_COL(DTK, DTK_STRTTIME), antenna_stoptime,
        APS_COL(DTK, DTK_STOPTIME), antenna_strttime, 
        APS_COL(DTK, DTK_DTKSTAT), "CON",
        APS_COL(DTK, DTK_DTKSTAT), "QUE",
        APS_COL(DTK, DTK_DTKSTAT), "SUB",
        APS_COL(DTK, DTK_DTKSTAT), "PLN",
        APS_COL(DTK, DTK_DTKSTAT), "SCH" ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, where_clause) ;
#endif

    dtk_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;
    if ( NUMELTS(dtk_list) <= 0 ) 
    {
        /* 
        -- no records found.  no work to do.  
        -- free allocated mem and return.  
        */
        DEL_LIST( dtk_list ) ;
        return TRUE ;
    }

    /* 
    -- there are records in dtk_list.  
    -- move them all to dtk_conflicts, the output list, which 
    -- already exists.  
    */
    dtk_llist_check = db_record_llist_move( dtk_conflicts, dtk_list ) ;
    if( dtk_llist_check != dtk_conflicts )
    {
        DEL_LIST( dtk_list ) ;
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
    }

    /* 
    -- no more work to do.  
    -- free allocated mem and return.  
    */
    DEL_LIST( dtk_list ) ;
    return TRUE ;

}

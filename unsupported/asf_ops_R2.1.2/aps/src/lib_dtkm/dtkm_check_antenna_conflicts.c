#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_check_antenna_conflicts.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_antenna_conflicts.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_antenna_conflicts.c"


/*==============================================================================
Function:       dtkm_check_antenna_conflicts()

Description:    collect conflicting other satellite data-takes.   
                these would be other downlinking data-takes at the 
                same station, antenna.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 19:31:30 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include <string.h>     /* for strlen strcat  */
#include <stdlib.h>     /* for free()         */

#include "dtkm.h"
#include "db_dtk.h"
#include "timeconv.h"


int dtkm_check_antenna_conflicts(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    char        *antenna_strttime,  /* time bracket to use for time overlaps. */
    char        *antenna_stoptime, 
    llist       *dtk_conflicts )    /* output list of conflicting data-takes. */
{
    int     return_code ;
    char    retrieval_strttime[ASF_TIME_STR_LENGTH+1] ;
    char    retrieval_stoptime[ASF_TIME_STR_LENGTH+1] ;
    char    dtkstat_where_clause[100] ;
    llist   *new_dtk_list = NULL ;
    llist   *dtk_sat_group = NULL ;

    /* quick error checking.  */
    if ( dtk_proposal == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
    if ( strlen( antenna_strttime ) != ASF_TIME_STR_LENGTH )
        return DTKM_ERROR_NO_STRTTIME ;
    if ( strlen( antenna_stoptime ) != ASF_TIME_STR_LENGTH )
        return DTKM_ERROR_NO_STOPTIME ;

    if ( dtk_conflicts == NULL )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_conflicts ) != 0 )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY ;

    /* 
    -- pad the antenna time bracket by 20 minutes on either side 
    -- to yield the retrieval time bracket.  this is what we will 
    -- use to find all possible relevant other satellite data-takes.  
    -- convert to minutes and pad the same_sat time bracket to 
    -- get the antenna time bracket.  
    */
    if ( !tc_time_pad( antenna_strttime, antenna_stoptime, 
        (float) ( 20.0), 
        retrieval_strttime, retrieval_stoptime ) )
        return DTKM_ERROR_PADDING_ANTENNA_TIME_BRACKET  ;

    /* 
    -- the antenna dtk conflicts must be found by first obtaining 
    -- the possibly relevant other satellite data-takes, grouping 
    -- them by satellite, and then processing them one satellite 
    -- group at a time.  
    -- note the dtkstat qualification.  
    */
    /*
    -- note the strange comparison between start and stop times.  
    -- this is the most direct way to retrieve the time overlaps.
    */
    sprintf(where_clause, 
    "where %s != '%s' and %s = '%s' and %s = %d and %s < '%s' and %s > '%s'",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dtk_proposal[DTK_SAT],
        APS_COL(DTK, DTK_STATION_ID),
            CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID],
        APS_COL(DTK, DTK_ANTENNA_ID),
            CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID],
        APS_COL(DTK, DTK_STRTTIME), retrieval_stoptime,
        APS_COL(DTK, DTK_STOPTIME), retrieval_strttime ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, where_clause) ;
#endif

    /* 
    -- this gets the dtkstat part of the where_clause 
    -- according to the dtkstat field in the dtk_proposal.  
    */
    return_code = dtkm_get_dtkstat_where_clause(dtk_proposal, 
        dtkstat_where_clause ) ;
    if ( return_code < 0 )
        return return_code ; 
    strcat(where_clause, dtkstat_where_clause ) ;

    sprintf(orderby_cols, "%s, %s",
        APS_COL(DTK, DTK_SAT), APS_COL(DTK, DTK_STRTTIME) ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, where_clause) ;
    printf("%s(%d):  orderby_cols = \n%s\n", __FILE__, __LINE__, orderby_cols) ;
#endif

    new_dtk_list = db_get_records( APS_dbproc, APS_TABLE(DTK),
        where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

    if ( new_dtk_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    while( NUMELTS(new_dtk_list) > 0 )
    {
        /* 
        -- conflicts found.  group them according to satellite 
        -- and process one group at a time.  
        -- this is done by moving them one group at a time out of 
        -- the new_dtk_list into the dtk_sat_group;
        */
        if ( dtk_sat_group == NULL )
            dtk_sat_group = create_dyn_llist() ;
        return_code = dtkm_extract_first_sat( new_dtk_list, dtk_sat_group ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( new_dtk_list ) ;
            DEL_LIST( dtk_sat_group ) ;
            return return_code ;
        }
        /* 
        -- if there are conflicts, move the conflicts from the 
        -- dtk_sat_group list to the dtk_conflicts list. 
        -- use the antenna time bracket as the time bracket to 
        -- check for the dtk_proposal.  
        */
        return_code = dtkm_check_sat_group_conflict( 
            dtk_sat_group, antenna_strttime, antenna_stoptime, 
            dtk_conflicts ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( new_dtk_list ) ;
            DEL_LIST( dtk_sat_group ) ;
            return return_code ;
        }
        /* 
        -- destroy all the members of dtk_sat_group, if any, 
        -- for the next group.  
        */
        DEL_ALL( dtk_sat_group ) ;

    }  /* end of   while( NUMELTS(new_dtk_list) > 0 )   */

    /* clean up and return.  */
    if ( dtk_sat_group != NULL )
        DEL_LIST(dtk_sat_group) ;

    DEL_LIST( new_dtk_list ) ;
    return TRUE ;
}

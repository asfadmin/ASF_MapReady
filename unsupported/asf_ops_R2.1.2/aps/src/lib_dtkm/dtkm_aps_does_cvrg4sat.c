#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_aps_does_cvrg4sat.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_aps_does_cvrg4sat.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_aps_does_cvrg4sat.c"



/*==============================================================================
Function:       dtkm_aps_does_cvrg4sat()

Description:    return TRUE if the APS does coverage for the 
                satellite, FALSE if not, < 0 if ERROR.  

Creator:        Lawrence Stevens

Creation Date:  Fri Dec 15 17:45:15 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include "db_satsensor.h"

#include <string.h>

int dtkm_aps_does_cvrg4sat( DB_RECORD   **dtk_rec )
{
    struct Sat_Y_N
    {
        char    sat[3] ;
        char    y_n ;
    } ;

#define MAX_POSSIBLE_SATS  50

    int                     j ;
    int                     nrecs ;

    static struct Sat_Y_N   cvrg_allowed[MAX_POSSIBLE_SATS+1] = {0} ;
/*
    {
        { "XX", (char) 0 },
    };
*/
    /* 
    -- using above cvrg_allowed[] to store satellite results 
    -- to reduce db retrievals.  
    */

    /* 
    -- move thru loop;  2 ways to STOP:  
    -- 1.  sat matches - strcmp() == 0
    -- 2.  y_n == 0 (end of the list)
    */
    for( 
        j = 0 ; 
        (   strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], cvrg_allowed[j].sat ) 
            && cvrg_allowed[j].y_n ) ;
        j++ 
        )
    {
    }
    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], cvrg_allowed[j].sat ) == 0 )
    {
        /* 
        -- a previous result that we can re-use:  
        -- handle at the end.  
        */
        /* make a harmless NULL statement to remove a lint warning: */
        nrecs = 0 ;
    }
    else if ( j >= ( MAX_POSSIBLE_SATS - 1 ) )
        return DTKM_ERROR_MUST_INCREASE_MAX_POSSIBLE_SATS ;
    else if ( (int) cvrg_allowed[j].y_n != 0 )
        return DTKM_ERROR_IN_CODE_dtkm_aps_does_cvrg4sat ;
    else
    {
        /* 
        -- a new entry is needed.  
        -- we reached the end of the list.  
        -- find out about this satellite.  
        */
        sprintf( where_clause, "where %s = '%s' and %s = 'Y' ",
            APS_COL(SATSENSOR, SATSENSOR_SAT),
                CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL(SATSENSOR, SATSENSOR_CVRG_ALLOWED) ) ;

        nrecs = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(SATSENSOR), where_clause ) ;
        if ( nrecs < 0 )
            return DTKM_ERROR_DB_QUERY_FAILED ;
        else if ( nrecs == 0 )
        {
            /* 
            -- the APS does not do ANY coverage.  
            -- for this satellite.  
            */
            cvrg_allowed[j].y_n = 'N' ;
        }
        else
        {
            /* it does SOME coverage.  */
            cvrg_allowed[j].y_n = 'Y' ;
        }

        /* Now complete this added entry with the satellite:   */
        strcpy( cvrg_allowed[j].sat, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
    }

    /* 
    -- now use the result.  
    */
    if ( cvrg_allowed[j].y_n == 'Y' )
        return TRUE ;
    if ( cvrg_allowed[j].y_n == 'N' )
        return FALSE ;

    return DTKM_ERROR_IN_CODE_dtkm_aps_does_cvrg4sat ;

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_get_cvrg_points.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_get_cvrg_points.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_cvrg_points.c"



/*==============================================================================
Function:       dtkm_get_cvrg_points

Description:    uses the dtk fields to retrieve cvrg points 
from the cvrg relation and insert them into the data-take in the
form of a DB_RECORD.  

Parameters:
    DB_RECORD   **proposed_dtk  
    DB_RECORD   **result_dtk  

Returns:
    int
        DTKM_GET_CVRG_POINTS_OK


    < 0 ERROR:
        DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE

Creator:    Lawrence Stevens

Creation Date:  05/09/1995

Notes:

==============================================================================*/

#include "dtkm.h"
#include "db_dtk.h"

int dtkm_get_cvrg_points( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **proposed_dtk, /* proposed data-take    */
    DB_RECORD   **result_dtk )  /* result data-take    */
{
    int     return_code ;
    char    ascdsc[2] ;
    char    ascdsc_1 ;
    char    ascdsc_2 ;
    int     rev_number_for_strttime ;
    char    strttime_for_rev[ASF_TIME_STR_LENGTH+1] ;
    char    stoptime_for_rev[ASF_TIME_STR_LENGTH+1] ;

#ifdef PRINT_DIAG
    PRINT_DIAG("dtkm_get_cvrg_points:  \n%s", 
        CAST_DTK_SAT proposed_dtk[DTK_SAT] ) ;
    PRINT_DIAG("/%s", 
        CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR] ) ;
    PRINT_DIAG("/%05d", 
        CAST_DTK_REV proposed_dtk[DTK_REV] ) ;
    PRINT_DIAG("   %s", 
        CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME] ) ;
    PRINT_DIAG(" %s\n", 
        CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME] ) ;
    db_print_record(proposed_dtk, APS_CDEFS(DTK) ) ;
#endif

    /*
    -- copy the proposed data-take to the result data-take.
    -- if any values are changed, such as the coverage points,
    -- they will be in the result_dtk, not the proposed_dtk.
    */
    return_code = db_copy_record ( APS_CDEFS(DTK), result_dtk, proposed_dtk ) ;


    /*
    -- first check that the start and stop times are within
    -- the input rev.  
    */
    if ( strcmp( CAST_DTK_SAT result_dtk[DTK_SAT], "A1" ) == 0 
    &&   dtkm_is_a_recording( result_dtk ) )
    {
        /*
        -- We do not need to do any checking of the start or stop time
        -- for A1 recordings.  Because of their low bit-rate sensors,
        -- long observations are sometimes encountered.
        -- Note that we do not reconcile the starttime with the rev,
        -- since it was this very same starttime that was used to 
        -- calculate the rev.
        */
        /* add a harmless statement to remove a lint warning:  */
        return_code = 0 ;
    }
    else
    {
        return_code = dtkm_check_rev_asftimes( 
            result_dtk, &rev_number_for_strttime, 
            strttime_for_rev, stoptime_for_rev ) ;

        if ( !return_code )
            return DTKM_ERROR_TIMES_NOT_WITHIN_REV ;
    }


    /*
    -- accommodate the called routine with the ascdsc being, 
    -- in that routine, a character string:
    */

    return_code =  get_cvg( APS_dbproc, 
                CAST_DTK_SAT      result_dtk[DTK_SAT] ,
                CAST_DTK_SENSOR   result_dtk[DTK_SENSOR],
                CAST_DTK_REV      result_dtk[DTK_REV],
                CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME],
                &CAST_DTK_STRTLAT result_dtk[DTK_STRTLAT],
                &CAST_DTK_NRLAT1  result_dtk[DTK_NRLAT1],
                &CAST_DTK_NRLON1  result_dtk[DTK_NRLON1],
                &CAST_DTK_FARLAT1 result_dtk[DTK_FARLAT1],
                &CAST_DTK_FARLON1 result_dtk[DTK_FARLON1],
                ascdsc  ) ;

    if ( return_code != 0 )
        return DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE ;

    ascdsc_1 = ascdsc[0] ;

    return_code =  get_cvg( APS_dbproc, 
                CAST_DTK_SAT      result_dtk[DTK_SAT] ,
                CAST_DTK_SENSOR   result_dtk[DTK_SENSOR],
                CAST_DTK_REV      result_dtk[DTK_REV],
                CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME],
                &CAST_DTK_STOPLAT result_dtk[DTK_STOPLAT],
                &CAST_DTK_NRLAT2  result_dtk[DTK_NRLAT2],
                &CAST_DTK_NRLON2  result_dtk[DTK_NRLON2],
                &CAST_DTK_FARLAT2 result_dtk[DTK_FARLAT2],
                &CAST_DTK_FARLON2 result_dtk[DTK_FARLON2],
                ascdsc  ) ;

    if ( return_code != 0 )
        return DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE ;

    ascdsc_2 = ascdsc[0] ;

    if (ascdsc_1 != 'S')
        CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] = ascdsc_1 ;
    else
        CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] = ascdsc_2 ;
        
#ifdef PRINT_DIAG
    PRINT_DIAG(" dtkm_get_cvrg_points:   results:  \n" ) ;
    db_print_record(result_dtk, APS_CDEFS(DTK) ) ;
#endif

    return DTKM_GET_CVRG_POINTS_OK ;

}


#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_set_asf_reduction_min.c

Description:    calculate the time reduction.  

==============================================================================*/
#pragma ident   "@(#)dtkm_set_asf_reduction_min.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_set_asf_reduction_min.c"


/*==============================================================================
Function:       dtkm_set_asf_reduction_min()

Description:    calculate the ASF time reduction in minutes from 
                dtk time fields.  

Creator:        Lawrence Stevens

Creation Date:  Fri Jan 16 12:55:52 PST 1998

==============================================================================*/
#include <timeconv.h>   /* for tc_et_ASF_datetime_diff()  etc.     */
#include "dtkm.h"
#include <db_dtk.h>     /* for dtk relation.  */
 
int dtkm_set_asf_reduction_min(
    /* both dtk pointers can be the same:  */
    DB_RECORD   **downlink_dtk_rec,   /* input (downlink) dtk  rec         */
    DB_RECORD   **result_dtk )        /* result dtk record.                */
{
    int     return_code ;
    double  dtk_duration_days ;
    double  fa_duration_days ;
    double  asf_reduction_minutes ;

    /* error checking.  */
    if( downlink_dtk_rec == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
    if( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    (void) db_copy_record( APS_CDEFS(DTK), result_dtk, downlink_dtk_rec ) ;

    /* 
    -- if the fa_times are not kept, then set value to 0 and return.  
    */
    if ( tc_validate_asf_datetime( 
            CAST_DTK_FA_STRTTIME result_dtk[DTK_FA_STRTTIME] ) != TRUE 
    ||   tc_validate_asf_datetime( 
            CAST_DTK_FA_STOPTIME result_dtk[DTK_FA_STOPTIME] ) != TRUE )
    {
        CAST_DTK_ASF_REDUCTION_MIN result_dtk[DTK_ASF_REDUCTION_MIN] = 0.0 ;
        return TRUE ;
    }

    /*
    -- OK.  times are kept.  compute reduction.  
    */

    return_code = tc_et_ASF_datetime_diff( 
        CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME],
        CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME],
        &dtk_duration_days ) ;
    if( return_code != TRUE )
        return return_code ;

    return_code = tc_et_ASF_datetime_diff( 
        CAST_DTK_FA_STRTTIME result_dtk[DTK_FA_STRTTIME],
        CAST_DTK_FA_STOPTIME result_dtk[DTK_FA_STOPTIME],
        &fa_duration_days ) ;
    if( return_code != TRUE )
        return return_code ;

    asf_reduction_minutes = 
        24.0 * 60.0 * ( fa_duration_days - dtk_duration_days ) ;
    
    CAST_DTK_ASF_REDUCTION_MIN result_dtk[DTK_ASF_REDUCTION_MIN] 
        = asf_reduction_minutes ;

    return TRUE ;

}

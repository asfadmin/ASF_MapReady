#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_get_mask_times.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)dtkm_get_mask_times.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_mask_times.c"

#include <string.h>
#include <timeconv.h>
#include <db_maskinout.h>
#include "dtkm.h"


/*==============================================================================
Function:       dtkm_get_mask_times()

Description:    retrieve mask in and out times from the maskinout relation

Creator:        Lawrence Stevens

Creation Date:  Thu Jan 29 19:12:45 PST 1998

Notes:          
==============================================================================*/
int dtkm_get_mask_times(
    char    *sat,
    int     rev,
    char    *station_id,
    char    *asftime_mask_entry,
    char    *asftime_mask_exit,
    double  *et_mask_entry,
    double  *et_mask_exit ) 
{

    DB_RECORD   **maskinout_rec ;
    llist       *maskinout_list ;
    cursor      maskinout_list_ptr ;

    int         in_rec_count = 0 ;
    int         out_rec_count = 0 ;

    int         return_code ;

    sprintf(where_clause, "where %s = '%s' and %s = '%s' and %s = %ld",
        APS_COL(MASKINOUT, MASKINOUT_STATIONID), station_id,
        APS_COL(MASKINOUT, MASKINOUT_SAT),  sat, 
        APS_COL(MASKINOUT, MASKINOUT_REV),  rev ) ;
 
    maskinout_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(MASKINOUT),
        where_clause, NULL, APS_CDEFS(MASKINOUT), ALL_COLS ) ;
 
    if ( maskinout_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;
 
    if ( NUMELTS( maskinout_list ) == 0 )
    {
        DEL_LIST( maskinout_list ) ;
        return DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV ;
    }
 
    if ( NUMELTS( maskinout_list ) != 2 )
    {
        DEL_LIST( maskinout_list ) ;
        return DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK;
    }

    for (
        maskinout_rec =
            (DB_RECORD **) FIRST(maskinout_list, maskinout_list_ptr) ;
        maskinout_rec ;
        maskinout_rec = (DB_RECORD **)NEXT(maskinout_list, maskinout_list_ptr)
        )
    {
        if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT],
                       "IN", 2 ) == 0 )
        {
            in_rec_count ++ ;
            *et_mask_entry 
                = CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] ;
        }
        else if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT],
                            "OUT", 3 ) == 0 )
        {
            out_rec_count ++ ;
            *et_mask_exit 
                = CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] ;
        }
    }
 
    /*
    -- we are now done with the maskinout list.
    -- we use et_inmask, et_outmask for the times from the records.
    */
    DEL_LIST( maskinout_list ) ;

    /*
    -- we should have ONE in and ONE out
    -- record from the relation.
    -- because of the above code.
    */
    if ( in_rec_count != 1 || out_rec_count != 1 )
    {
        /*
        -- ERROR in maskinout relation; 1 IN and 1 OUT rec expected
        */
        return DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK;
    }

    /* O.K.  we have both records as expected.  */
    return_code = tc_et2asf( *et_mask_entry, asftime_mask_entry ) ; 
    if( return_code != TRUE )
    {
        printf("%s(%d):  %s\n", __FILE__, __LINE__, 
            DTKM_ERROR_MESSAGE(DTKM_ERROR_IN_TIME_CONVERSION)  ) ;
        return DTKM_ERROR_IN_TIME_CONVERSION ;
    }
    return_code = tc_et2asf( *et_mask_exit, asftime_mask_exit ) ; 
    if( return_code != TRUE )
    {
        printf("%s(%d):  %s\n", __FILE__, __LINE__, 
            DTKM_ERROR_MESSAGE(DTKM_ERROR_IN_TIME_CONVERSION)  ) ;
        return DTKM_ERROR_IN_TIME_CONVERSION ;
    }

    return TRUE ;

}

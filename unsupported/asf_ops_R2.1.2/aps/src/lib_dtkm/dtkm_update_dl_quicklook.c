#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_update_dl_quicklook.c

Description:    check/update quicklook values for a downlink in DB.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dl_quicklook.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dl_quicklook.c"

#include "dtkm.h"
#include <db_dtk.h>   /*  for CAST_DTK_PLANNER_QUICKLOOK etc.  */
#include <timeconv.h>   /*  for tc_systime2asf()   etc.  */

/*==============================================================================
Function:       dtkm_update_dl_quicklook

Description:    check/update quicklook values for a downlink in DB

Creator:        Lawrence Stevens

Creation Date:  Thu Aug 14 09:53:51 PDT 1997

==============================================================================*/
int dtkm_update_dl_quicklook( 
    DBPROCESS   *APS_dbproc, 
    char        dl_planner_quicklook, /* input desired value:  'Y' or 'N' */
    char        dl_science_quicklook, /* input desired value:  'Y' or 'N' */
    DB_RECORD   **downlink_rec,       /* dtk rec to update.               */
    llist       *dtk_updates )        /* output list of updates.          */
{   
    int     nrecs ;
    char    fields_to_set_buffer[100] ;
    llist   *list_check = NULL ;

    char    now_asftime[ASF_TIME_STR_LENGTH+1] ;

    /* quick error check  */
    if( dl_planner_quicklook != 'Y' && dl_planner_quicklook != 'N' )
    {
        fprintf(stderr, "%s(%d):  %s\n", __FILE__, __LINE__, 
            DTKM_ERROR_MESSAGE(DTKM_ERROR_PLANNER_QL_ARG_VALUE_NOT_Y_OR_N) ) ;
        return DTKM_ERROR_PLANNER_QL_ARG_VALUE_NOT_Y_OR_N ;
    }
    if( dl_science_quicklook != 'Y' && dl_science_quicklook != 'N' )
    {
        fprintf(stderr, "%s(%d):  %s\n", __FILE__, __LINE__, 
            DTKM_ERROR_MESSAGE(DTKM_ERROR_SCIENCE_QL_ARG_VALUE_NOT_Y_OR_N) ) ;
        return DTKM_ERROR_SCIENCE_QL_ARG_VALUE_NOT_Y_OR_N ;
    }
    if( downlink_rec == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
    if( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    /*
    -- if necessary, update quicklook values in
    -- the downlink.
    -- initialize 'fields_to_set' now to "", check later.
    */
    strcpy( fields_to_set, "" ) ;

    if ( CAST_DTK_PLANNER_QUICKLOOK downlink_rec[DTK_PLANNER_QUICKLOOK]
         !=  dl_planner_quicklook  )
    {
        CAST_DTK_PLANNER_QUICKLOOK downlink_rec[DTK_PLANNER_QUICKLOOK]
         =  dl_planner_quicklook  ;

        /* change to planner quick look needed.  */
        sprintf( fields_to_set_buffer, "%s = '%c'",
            APS_COL(DTK, DTK_PLANNER_QUICKLOOK), dl_planner_quicklook ) ;
        strcat( fields_to_set, fields_to_set_buffer ) ;
    }

    if ( CAST_DTK_SCIENCE_QUICKLOOK downlink_rec[DTK_SCIENCE_QUICKLOOK]
         !=  dl_science_quicklook  )
    {
        /* change to science quick look needed.    */
        CAST_DTK_SCIENCE_QUICKLOOK downlink_rec[DTK_SCIENCE_QUICKLOOK]
            =  dl_science_quicklook  ;

        /*
        -- (check to add comma between fields)
        */
        if( strcmp( fields_to_set, "" ) != 0 )
            strcat( fields_to_set, ", " ) ;

        sprintf( fields_to_set_buffer, "%s = '%c'",
            APS_COL(DTK, DTK_SCIENCE_QUICKLOOK), dl_science_quicklook ) ;
        strcat( fields_to_set, fields_to_set_buffer ) ;
    }
    if( strlen( fields_to_set ) > 0 )
    {
        /* there is a change.  perform the update.  */
        sprintf(where_clause, "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT downlink_rec[DTK_SAT],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV downlink_rec[DTK_REV],
            APS_COL(DTK, DTK_DTKID), CAST_DTK_DTKID downlink_rec[DTK_DTKID] ) ;

        /*
        -- and update the dtkdate, too.
        -- add comma between fields
        */
        tc_systime2asf( now_asftime ) ;
        sprintf( fields_to_set_buffer, ", %s = '%s'",
            APS_COL(DTK, DTK_DTKDATE), now_asftime ) ;
        strcat( fields_to_set, fields_to_set_buffer ) ;
        nrecs = db_update_records(APS_dbproc,
            APS_TABLE(DTK), fields_to_set, where_clause) ;
        if ( nrecs < 0 )
            return DTKM_ERROR_DB_UPDATE_FAILED_ON_DTK_RELATION ;

        /* update of downlink is done.  */
        /*
        -- important:  report this change into dtk_updates
        -- list.
        */
        list_check = dtkm_duplicate_dtk_into_list(
            downlink_rec, dtk_updates ) ;
        if ( list_check != dtk_updates )
            return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;

    }  /* endif  fields to set.   */

    return TRUE ;

}

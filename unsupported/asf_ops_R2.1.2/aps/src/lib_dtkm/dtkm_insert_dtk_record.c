#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_insert_dtk_record.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_insert_dtk_record.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_insert_dtk_record.c"



/*==============================================================================
Function:       dtkm_insert_dtk_record()

Description:    inserts a new data-take record into the dtk relation 
                from a dtk DB_RECORD.  If dtkid == 0 in the DB_RECORD, 
                this routine creates one.  The dtkdate value is created 
                here as well.  

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
#include <aps_get_sequence_number.h>   /* for db_get_new_dtkid()  */
#include "db_dtk.h"

#include "timeconv.h"       /* for tc_systime2yyyycddd()   */

#include <string.h>         /* for strcpy()  */

int dtkm_insert_dtk_record( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **input_dtk,    /* rec to insert.  */
    DB_RECORD   **result_dtk ,  /* result data-take    */
    llist       *dtk_updates )  /* update duplicated into list */
{

    llist           *list_check = NULL ;

    int             new_dtkid ;    /* dtk id for the new record.  */
    char            dtkdate[ASF_TIME_STR_LENGTH+1] ;
    int             nrecs_inserted ;
    int             return_code ;

#ifdef PRINT_DIAG
    printf("%s(%d):  starting dtkm_insert_dtk_record() \n", __FILE__, __LINE__);
    db_print_record(input_dtk, APS_CDEFS(DTK) ) ;
#endif

    if ( input_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( result_dtk  == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    if ( dtk_updates  == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    if( (int) CAST_DTK_DTKID input_dtk[DTK_DTKID] != 0 )
        return DTKM_ERROR_DTK_INSERT_REQUEST_DTK_HAS_DTKID_NE_ZERO ;

    return_code = db_copy_record( APS_CDEFS(DTK), result_dtk, input_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* 
    -- Perhaps the calling routine has already determined the 
    -- desired dtkid.  
    -- Thu May  8 16:58:38 PDT 1997  NO LONGER ALLOWED WITH THE 
    -- new dtkid generators.  
    -- 
    */
    if ( CAST_DTK_DTKID result_dtk[DTK_DTKID] == 0 )
    {
        /* 
        -- Not determined.  Need a new dtkid to complete 
        -- the primary key.  
        */
        return_code = db_get_new_dtkid( APS_dbproc, 
            CAST_DTK_SAT result_dtk[DTK_SAT],
            CAST_DTK_REV result_dtk[DTK_REV] ) ;
        if( return_code < 0 )
        {
            fprintf(stderr, "%s(%d):  Error getting new dtkid:\n %s\n",
                __FILE__, __LINE__, APS_NEW_DTKID_ERROR_MESSAGE( return_code ));
            return DTKM_ERROR_GETTING_NEW_DTKID ;
        }

        /* OK.  dtkid is the return_code  */
        new_dtkid = return_code ;

        CAST_DTK_DTKID result_dtk[DTK_DTKID] = new_dtkid ;

#ifdef PRINT_DIAG
        PRINT_DIAG("dtkm_insert_dtk_record:  new_dtkid = %d\n", new_dtkid ) ;
#endif

    }
    else
    {
        /* Error.  Dtkid != 0 for creation of dtk.  */
        return DTKM_ERROR_DTK_INSERT_REQUEST_DTK_HAS_DTKID_NE_ZERO ;
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

    nrecs_inserted = db_insert_single_record(APS_dbproc,
        result_dtk, APS_TABLE(DTK), APS_CDEFS(DTK) ) ;

    if ( nrecs_inserted != 1 )
        return DTKM_ERROR_DTK_NOT_INSERTED ;

    /*
    -- save a duplicate of the update into the 
    -- dtk_updates list; don't allow a program 
    -- to forget to accumulate the list of changes.  
    */
    list_check = dtkm_duplicate_dtk_into_list( result_dtk,
        dtk_updates ) ;
    if ( list_check != dtk_updates )
        return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;

    return DTKM_DTK_INSERTED_OK ;

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_r1_update_sensor.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_r1_update_sensor.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_r1_update_sensor.c"



/*==============================================================================
Function:       dtkm_r1_update_sensor()

Description:    updates the sensor field from SAR to the 
                appropriate value (SS1, SS2, etc.) if there 
                is a non-SAR value found in the list.  
                This routine is here just because the Radarsat 
                request and schedule files do not include 
                sensor and mode in them.  therefore, R1 SAR is 
                assumed.  If the planner later updates them in
                the database, we wish to continue that value.   
                if CSA sends another file and refers to that 
                data-take, in dtk_similars, updated by the planner.  

                However, if the proposal and a concurring data-take 
                have the same dtkid, then don't do anything here.  Allow 
                the update to occur.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  2 21:16:36 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>     /* for strcmp strcpy  */

int dtkm_r1_update_sensor(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    DB_RECORD   **result_dtk,       /* can be the same memory. */
    llist       *dtk_concurs, 
    llist       *dtk_similars, 
    llist       *dtk_updates ) 
{
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **new_dtk_rec ;
    cursor      current_list_ptr ;

    llist       *current_list = NULL ;

    int         j, k ;
    int         return_code ;
    char        init_sensor[10] ; /* only 4 bytes are needed; be safe  */

    /* quick error checking.  */
    if ( dtk_proposal == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    if (dtk_concurs == NULL)
        return DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED ;
 
    if (dtk_similars == NULL)
        return DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED ;

    if ( strcmp( CAST_DTK_SAT dtk_proposal[DTK_SAT], "R1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_R1_FOR_dtkm_r1_update_sensor ;

    /* finished error checking.  */

    /*  
    -- save the initial value of init_sensor  
    -- for later comparison.  don't rely on dtk_proposal
    -- having different memory from result_dtk
    */
    strcpy( init_sensor, CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR] ) ;

    db_copy_record( APS_CDEFS(DTK), result_dtk, dtk_proposal ) ;

    /* 
    -- no change unless the sensor starts with 'S'.  
    -- Either 
    -- 1.  the current dtk is already set with a good value, and we update 
    --     the concurring and similar dtk values from SAR to the correct 
    --     value.  
    -- or
    -- 2.  the current dtk has a value of SAR and we update
    --     it from the concurring or similar dtks.  
    */
    if( init_sensor[0] != 'S' )
    {
        /* 
        -- dtkm_r1_update_sensor() should not have been called, but 
        -- we are "soft" here.  
        -- when using debugger one time, I saw that an RDL dtk 
        -- got into this routine.  No damage due to the 
        -- protective "if" statements below, but we want 
        -- to do a better job than that.  
        */
        return TRUE ;    
    }

    /* sensor does start with 'S'  */

    /* 
    -- we may to run through the loops for 
    -- dtk_concurs and dtk_similars twice.  
    -- this is because the dtk proposal and all of the 
    -- dtk_concurs and dtk_similars could be SAR except 
    -- for the very last one, which would update the 
    -- dtk proposal.  In this case, we would want to run 
    -- the loop again, so that the dtk proposal could 
    -- update the earlier members in the lists.  
    -- so if the dtk proposal gets updated, we do want to 
    -- run the loop again.  if not, break.  
    */
    for ( j = 0 ; j < 2 ; j++ )
    {
        /* 
        -- run through the dtk_concurs list and 
        -- the dtk_similars list.  
        */
        for ( k = 0 ; k < 2 ; k++ )
        {
            if ( k == 0 )
                current_list = dtk_concurs ;
            if ( k == 1 )
                current_list = dtk_similars ;

            for (dtk_rec = (DB_RECORD **) FIRST(current_list, current_list_ptr);
                 dtk_rec != NULL ;
                 dtk_rec = (DB_RECORD **) NEXT(current_list, current_list_ptr)  
                )
            {
                if ( strcmp(CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                            CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0 )
                {
                    continue ;
                }
                if (    CAST_DTK_DTKID result_dtk[DTK_DTKID] 
                     == CAST_DTK_DTKID    dtk_rec[DTK_DTKID]  ) 
                {
                    /* 
                    -- the sat, rev, and dtkid are the same.  
                    -- the planner is updating the sensor of 
                    -- this record.  Do nothing here; allow this 
                    -- dtk record to get updated later.  
                    */
                    continue ;
                }
                /* 
                -- the data-take sensor in the list differs from the 
                -- sensor in the input data-take.  update the 
                -- one with the SAR value, using the other data-take
                -- sensor value:  
                */
                if ( strcmp(CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                            "SAR" ) == 0 )
                {
                    /* update the sensor of the dtk proposal.  */
                    strcpy( CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                        CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) ;
                }
                if ( strcmp(CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                            "SAR" ) != 0 )
                {
                    continue ;
                }
                /* 
                -- UPDATE the current_list dtk, which is already in 
                -- the database.  
                */
                /* 
                -- allocate new storage to save changes in 
                -- members of lists:
                */
                new_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
                db_copy_record( APS_CDEFS(DTK), new_dtk_rec, dtk_rec ) ;
                return_code = dtkm_update_dtk_sensor( APS_dbproc, 
                    dtk_rec, CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                    new_dtk_rec, dtk_updates ) ;
                if ( return_code < 0 )
                {
                    free_db_record( new_dtk_rec ) ;
                    return return_code ;
                }
                /* 
                -- update the list entry with 
                -- the new version: 
                -- remove the old and append the new.  
                -- don't free new_dtk_rec in this routine.  
                */
                DEL_AT_CURSOR( current_list, current_list_ptr ) ;
                APPEND( current_list, new_dtk_rec, free_db_record, new_dtk_rec);

                /* 
                -- if we just updated the CONCURRING data-take, 
                -- we need to copy the dtkid value into the dtk 
                -- proposal result to keep the connection  
                -- between them:  
                */
                if ( current_list == dtk_concurs )
                    CAST_DTK_DTKID result_dtk[DTK_DTKID] = 
                        CAST_DTK_DTKID new_dtk_rec[DTK_DTKID] ;
            }   /* end for loop on list  */
        }  /* end list loop.  */
        /* 
        -- if the sensor field was updated, run thru 
        -- the loops for dtk_concurs and dtk_similars again.  
        */
        if ( strcmp(CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                    init_sensor ) == 0 )
            break ;
    }

    return TRUE ;

}

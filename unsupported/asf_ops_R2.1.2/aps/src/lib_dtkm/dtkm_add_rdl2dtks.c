#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_add_rdl2dtks.c

Description:    

External Functions Defined:
GLOBAL Function:    dtkm_dtk_consolidation_by_rev   
GLOBAL Function:    dtkm_add_rdl2obs
    
File Scope Functions:

External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)dtkm_add_rdl2dtks.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_add_rdl2dtks.c"

#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>

/*==============================================================================
Function:       dtkm_dtk_consolidation_by_rev
 
Description:    Condenses a list of dtk DB_RECORDS by rev value;
                That is, all datatakes which have the same rev will be
                merged into a single datatake, which will have the earliest
                start time in the rev, and the latest stop time in the rev.
 
Parameters:
 
Returns:        Always TRUE.  If the incoming list is empty, there is no
                action performed; return is still TRUE.
 
Creator:        Miguel Siu
 
Creation Date:  Sat Sep  7 12:18:47 PDT 1996
 
Notes:
==============================================================================*/
int                 /* Ignore LINT warning: declared global, could be static */
dtkm_dtk_consolidation_by_rev( llist      *input_dtk_list)
{
    cursor        dtk_rec_ptr ;
    cursor        loop_rec_ptr ;
    DB_RECORD     **dtk_rec ;
    DB_RECORD     **loop_rec ;
    DB_RECORD     **dtk_unlinked_rec = NULL ;
 
    int           current_rev ;
    llist         *long_dtk_list ;
 
    /*
    -- Move input_dtk_list into long_dtk_list; input_dtk_list is now empty.
    --         _________________       ________________
    --        /_input_dtk_list_/ ---> /_long_dtk_list_/
    */
    long_dtk_list = create_dyn_llist();
    if ( NUMELTS(input_dtk_list)  > 0 )
        db_record_llist_move (long_dtk_list, input_dtk_list) ;
 
    /*
    -- Condense long_dtk_list to retain only one dtk_proposal per rev
    --         ________________               _________________
    --        /_long_dtk_list_/ condenses to /_input_dtk_list_/
    */
    if ( NUMELTS(long_dtk_list) > 0 )
    {
        current_rev = 0 ;
        for (
            dtk_rec = (DB_RECORD **)FIRST(long_dtk_list, dtk_rec_ptr) ;
            dtk_rec ;
            dtk_rec = (DB_RECORD **)NEXT( long_dtk_list, dtk_rec_ptr)
            )
        {
            if (CAST_DTK_REV dtk_rec[DTK_REV] == current_rev)
                continue ;

            /* this is a new, different rev:  */
            current_rev = CAST_DTK_REV dtk_rec[DTK_REV] ;
            dtk_unlinked_rec = (DB_RECORD **) UNLINK_AT_CURSOR(
                        long_dtk_list, dtk_rec_ptr ) ;
            APPEND(input_dtk_list, dtk_unlinked_rec,
                            free_db_record, dtk_unlinked_rec) ;
        }
    }
 
    /*
    -- For each dtk in the input_dtk_list, find any entries in
    -- long_dtk_list which match in rev.  Search through the matches
    -- for the earliest start time and latest stoptime;
    -- update the entry in input_dtk_list with these values.
    */
    for (
        loop_rec = (DB_RECORD **)FIRST(input_dtk_list, loop_rec_ptr) ;
        loop_rec ;
        loop_rec = (DB_RECORD **)NEXT( input_dtk_list, loop_rec_ptr)
        )
    {
        current_rev = CAST_DTK_REV loop_rec[DTK_REV] ;
        for (
            dtk_rec = (DB_RECORD **)FIRST(long_dtk_list, dtk_rec_ptr) ;
            dtk_rec ;
            dtk_rec = (DB_RECORD **)NEXT( long_dtk_list, dtk_rec_ptr)
            )
        {
            if (CAST_DTK_REV dtk_rec[DTK_REV] != current_rev)
                continue ;

            /*  rev == current rev.  */

            if ( strcmp(CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
                        CAST_DTK_STRTTIME loop_rec[DTK_STRTTIME]) < 0 )
            {
                /*
                -- the start time in the long_dtk_list dtk_rec is earlier
                -- than the input_dtk_list loop_rec starttime.
                -- Therefore, we can update the
                -- DTK_STRTTIME in the input_dtk_list loop_rec.
                */
                memmove( CAST_DTK_STRTTIME loop_rec[DTK_STRTTIME],
                    CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
                    ASF_TIME_STR_LENGTH ) ;
                *((CAST_DTK_STRTTIME loop_rec[DTK_STRTTIME]) +
                    ASF_TIME_STR_LENGTH) = '\0' ;
            }

            if ( strcmp(CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
                        CAST_DTK_STOPTIME loop_rec[DTK_STOPTIME]) > 0 )
            {
                /*
                -- the stop time in the long_dtk_list dtk_rec
                -- is later than the input_dtk_list loop_rec stoptime.
                -- Therefore, we can update the
                -- DTK_STOPTIME in the input_dtk_list loop_rec.
                */
                memmove( CAST_DTK_STOPTIME loop_rec[DTK_STOPTIME],
                    CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
                    ASF_TIME_STR_LENGTH ) ;
                *((CAST_DTK_STOPTIME loop_rec[DTK_STOPTIME]) +
                    ASF_TIME_STR_LENGTH) = '\0' ;
            }
        }
    }
 
    DEL_LIST( long_dtk_list ) ;

    return(TRUE) ;
 
}/* end of dtkm_dtk_consolidation_by_rev() */

/*==============================================================================
Function:       dtkm_add_rdl2obs

Description:    This routine will take a list of realtime observation 
                datatake proposals and create a single realtime downlink 
                record for each rev in the list.

Returns:        
    TRUE.       Successful operation.

    Otherwise, < 0 :
                DTKM_ERROR_INPUT_DTK_LIST_EMPTY
                DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY
                DTKM_ERROR_DUPLICATING_DTK_LIST
                DTKM_ERROR_SETTING_SENSOR_AND_ACTID_VALUES

Creator:        Miguel Siu

Creation Date:  Fri Mar 21 15:45:49 PST 1997

Notes:          
    This routine has global usage, so we cannot declare it 'static int'
    This will cause a lint warning; please ignore it.
==============================================================================*/
int dtkm_add_rdl2obs(
    llist       *input_dtk_obs,
    llist       *output_dtk_rdl)
{
    int     return_code ;

    DB_RECORD   **dtk_rec ;
    DB_RECORD   **first_dtk_rec ;
    cursor      dtk_rec_ptr ;
	char		zero_one_byte = 0 ;

    /*
    -- error checking
    */
    if ( NUMELTS( input_dtk_obs ) == 0 )
        return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

    if ( NUMELTS( output_dtk_rdl ) != 0 )
        return DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY ;

    /* 
    -- this function does not support every satellite.  
    -- check for a wrong satellite:  
    */
    first_dtk_rec = (DB_RECORD **) FIRST(input_dtk_obs, dtk_rec_ptr);

    if( strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "E1" ) != 0 
    &&  strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "E2" ) != 0 
    &&  strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "J1" ) != 0 
    &&  strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "R1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_SUPPORTED_BY_FUNCTION ;


    for (   dtk_rec = (DB_RECORD **) FIRST(input_dtk_obs, dtk_rec_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(input_dtk_obs, dtk_rec_ptr)  
        )
    {
        /*
        -- check for different satellites in the list.  
        */
        if( strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], 
                    CAST_DTK_SAT       dtk_rec[DTK_SAT] )  )
            return DTKM_ERROR_DIFFERENT_SATS_IN_INPUT_DTK_LLIST ;

        /*
        -- check for something other than a real-time observation 
        -- data-take in the list.  
        */
        if( dtkm_is_a_realtime_observation( dtk_rec ) != TRUE )
            return DTKM_ERROR_INPUT_LLIST_MUST_BE_ALL_REALTIME_OBSERVATIONS ;
    }

    /*
    -- we have proved:
    -- 1.  All data-takes in this list are the same satellite.
    -- 2.  The satellite is supported by this routine.  
    -- 3.  Every data-take in the list is a realtime observation.  
    */

    /*
    -- place a copy of the single-satellite realtime observation 
    -- records into a list which will eventually become the 
    -- downlink records
	-- NOTE:  the dtkid must be 0 for each new RDL dtk.  
    */
    if ( db_record_llist_copy(
            APS_CDEFS(DTK), output_dtk_rdl, input_dtk_obs) == NULL)
        return DTKM_ERROR_DUPLICATING_DTK_LIST ;

    /*
    -- consolidate the future realtime downlinks by rev.
    -- This operation always returns TRUE value, so don't check return_code
    */
    return_code = dtkm_dtk_consolidation_by_rev(output_dtk_rdl) ;

    /*
    -- Create the realtime downlinks by updating the SENSOR and ACTID values.
    -- The following is not a typo: we assign the value of
    -- DTKM_SENSOR_REALTIME_DOWNLINK_CODE (ie:"RDL") to both
    -- the realtime downlink sensor and realtime downlink activity id.
    -- NOTE:  the DTK_ACTID value must be 6 characters, so 
    --        dtkm_default_values() is called a few lines down, 
    --        in order to complete this field.  
	-- NOTE:  the dtkid must be 0 for each new RDL dtk.  
    */
    if ( set_db_record_values(APS_CDEFS(DTK),
            DTK_SENSOR, DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
            output_dtk_rdl) == NULL )
        return DTKM_ERROR_SETTING_SENSOR_AND_ACTID_VALUES ;

    if ( set_db_record_values(APS_CDEFS(DTK),
            DTK_ACTID, DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
            output_dtk_rdl) == NULL   )
        return DTKM_ERROR_SETTING_SENSOR_AND_ACTID_VALUES ;

	/* the first 2 worked, probably this one, too.  */
    (void) set_db_record_values(APS_CDEFS(DTK),
            DTK_DTKID, &zero_one_byte,
            output_dtk_rdl) ;

    /* 
    -- before calling dtkm_default_values(), we need to 
    -- "" the DTK_TRANSID value.
    -- Now, when dtkm_default_values is called, it 
    -- will write the only correct value for the 
    -- supported satellites.  
    -- also, it will complete the DTK_ACTID 6-character field.  
    */
    if ( set_db_record_values(APS_CDEFS(DTK),
            DTK_TRANSID, "", output_dtk_rdl) == NULL   )
        return DTKM_ERROR_SETTING_VALUES_IN_LLIST ;

    /*
    -- call dtkm_default_values() for all the created downlinks,
    */
    for (
        dtk_rec = (DB_RECORD **)FIRST(output_dtk_rdl, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **)NEXT( output_dtk_rdl, dtk_rec_ptr)
        )
    {
        return_code = dtkm_default_values( dtk_rec, dtk_rec ) ;
        if( return_code < 0 )
            return return_code ;
    }

    /*
    -- normal exit
    */
    return (TRUE) ;
}


/*==============================================================================
Function:       dtkm_add_rdl2dtks()

Description:    look at the realtime observation data-takes in the 
                input dtk list and create real-time downlink data-takes 
                to downlink them, and put them into the output 
                realtime downlink list.  
                Allowed only for E1, E2, J1, and R1 data-takes.  

Creator:        Lawrence Stevens

Creation Date:  Fri Mar 21 18:34:42 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_add_rdl2dtks( 
    llist       *input_dtks,        /* input list of data-takes              */
    llist       *output_dtk_rdl )   /* output list of RDL data-takes, if any */
{
    int         return_code ;

    DB_RECORD   **dtk_rec ;
    cursor      input_dtks_ptr ;

    llist       *temp_rtobs_list ;
    llist       *llist_check = NULL ;

    /*
    -- error checking
    */
    if ( input_dtks == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    if ( output_dtk_rdl == NULL )
        return DTKM_ERROR_NULL_OUTPUT_DTK_LIST ;

    if ( NUMELTS( input_dtks ) == 0 )
        return TRUE ;

    if ( NUMELTS( output_dtk_rdl ) != 0 )
        return DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY ;

    dtk_rec = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr);
    if( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E1" ) != 0 
    &&  strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E2" ) != 0 
    &&  strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "J1" ) != 0 
    &&  strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_SUPPORTED_BY_FUNCTION ;

    /*
    -- 1.  duplicate the realtime observation data-takes from 
    --     input_dtks into the temp_rtobs_list.  
    -- 2.  call dtkm_add_rdl2obs() to create realtime downlinks 
    --     for them into the output_dtk_rdl list.  
    -- 3.  clean up.  free the storage.  
    */

    temp_rtobs_list = create_dyn_llist() ;
    for (   dtk_rec = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(input_dtks, input_dtks_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        if ( dtkm_is_a_realtime_observation( dtk_rec ) != TRUE )
            continue ;

        /* 
        -- a real time observation.  
        -- duplicate into list.  
        */
        llist_check = dtkm_duplicate_dtk_into_list( dtk_rec, 
            temp_rtobs_list ) ;
        if( llist_check != temp_rtobs_list ) 
        {
            DEL_LIST( temp_rtobs_list ) ;
            return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
        }
    }

    if( NUMELTS( temp_rtobs_list ) <= 0 )
    {
        /* done.  no data-takes to create.  */
        DEL_LIST( temp_rtobs_list ) ;
        return TRUE ;
    }

    return_code = dtkm_add_rdl2obs( temp_rtobs_list, output_dtk_rdl ) ;
    DEL_LIST( temp_rtobs_list ) ;
    if( return_code < 0 )
        return return_code ;

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_consolidate_tapedumps()

Description:    accepts a list of data-takes.  extracts the 
                tape dump downlinks from the list, then 
                consolidates them to one per rev number per 
                ground station.  Then puts the tape dump 
                downlinks back into the list before returning.  
                Input list has data-takes from only one satellite 

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 24 16:53:26 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int dtkm_consolidate_tapedumps( llist *input_dtks )
{

    llist       *llist_check = NULL ;
    llist       *tape_dump_dtks = NULL ;
    cursor      input_dtks_ptr ;
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **first_dtk_rec ;

    /*
    -- error checking. 
    */
    if ( input_dtks == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    if ( NUMELTS( input_dtks ) == 0 )
        return TRUE ;

    first_dtk_rec = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr);
    if( strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "E1" ) != 0 
    &&  strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "E2" ) != 0 
    &&  strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "J1" ) != 0 
    &&  strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], "R1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_SUPPORTED_BY_FUNCTION ;

    for (   dtk_rec = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr );
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(input_dtks, input_dtks_ptr )  
        )
    {
        /*
        -- check for different satellites in the list.  
        */
        if( strcmp( CAST_DTK_SAT first_dtk_rec[DTK_SAT], 
                    CAST_DTK_SAT       dtk_rec[DTK_SAT] )  )
            return DTKM_ERROR_DIFFERENT_SATS_IN_INPUT_DTK_LLIST ;
    }

    /*
    -- we have proved:
    -- 1.  All data-takes in this list are the same satellite.
    -- 2.  The satellite is supported by this routine.  
    */

    /* remove tape dump downlinks into one list.  */
    tape_dump_dtks = create_dyn_llist() ;
    llist_check = move_db_record_matches2llist(
        APS_CDEFS(DTK), DTK_SENSOR, DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,
        tape_dump_dtks, input_dtks ) ;
    if ( llist_check != tape_dump_dtks )
    {
        DEL_LIST( tape_dump_dtks ) ;
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
    }

    if( NUMELTS( tape_dump_dtks ) <= 0 )
    {
        DEL_LIST( tape_dump_dtks ) ;
        return TRUE ;
    }

    /*
    -- consolidate the tapedump downlinks by rev.
    -- This operation always returns TRUE value, so don't check return_code
    */
    (void) dtkm_dtk_consolidation_by_rev(tape_dump_dtks) ;

    /* move the tape dump downlinks back into the input list.  */
    llist_check = db_record_llist_move( input_dtks, tape_dump_dtks ) ;
    if ( llist_check != input_dtks )
    {
        DEL_LIST( tape_dump_dtks ) ;
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
    }

    DEL_LIST( tape_dump_dtks ) ;
    return TRUE ;

}

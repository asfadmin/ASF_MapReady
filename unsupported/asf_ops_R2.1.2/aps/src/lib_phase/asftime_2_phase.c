#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       asftime_2_phase()

Description:    given asftime, it returns the DB_RECORD corresponding 
to the phase that includes the time.  

Parameters:     
int asftime_2_phase(       
    char            *sat,
    char            *asftime,              input asftime for desired phase. 
    DB_RECORD       ***phase_rec_found )   pointer to a new DB_RECORD 

Returns:        
    >= 0   :  Condition codes; the input time is compared to the 
            time brackets for each of the phases for the satellite:  
        PHASE_INPUT_TIME_BEFORE_ALL_PHASES  the first phase rec is returned.
        PHASE_INPUT_TIME_WITHIN_A_PHASE     the desired phase rec is returned.
        PHASE_INPUT_TIME_BETWEEN_PHASES     the next phase rec is returned.
        PHASE_INPUT_TIME_AFTER_ALL_PHASES   the last phase rec is returned.

    < 0    :  Error codes:  
        ASFTIME_2_PHASE_INPUT_ETIME_TOO_LOW 
        ASFTIME_2_PHASE_INPUT_ETIME_TOO_HIGH
        DB_QUERY_FAILED
        NO_PHASE_RECS_FOUND
        BAD_PHASE_REC_START_TIME
        TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME
        TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS
        TWO_PHASE_RECS_OVERLAP_IN_TIME

Creator:        Lawrence Stevens

Creation Date:  Tue Jun  6 16:07:57 PDT 1995

Notes:      
Don't pre-allocate a DB_RECORD for the desired phase record; 
this routine CREATES a DB_RECORD whose pointer is returned to the 
calling routine.  

==============================================================================*/
#pragma ident   "@(#)asftime_2_phase.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.asftime_2_phase.c"

/*
#define PRINT_DIAG   printf("%s(%d):  ", __FILE__, __LINE__ ) ; printf( 
*/


/* FOR SYBASE INTERFACES */
#include <string.h>          /* for strcmp() etc.      */
#include "db_sybint.h"       /* for APS sybase interface routines    */
#include "aps_db_table.h"    /* for APS DB tables sybase interface   */
#include "dapps_list.h"      /* for APS linked list macros           */

/* FOR NASDA UTILITIES   */
#include "phase_utilities.h"

/* FOR TIME CONVERSION   */
#include "timeconv.h"
#include "dapps_defs.h"       /* for min, max ephemeris times.  */

int asftime_2_phase(       
    char            *sat,
    char            *asftime,         /* input asftime for desired phase.  */
    DB_RECORD       ***phase_rec_found )  /* pointer to a new DB_RECORD  */
{

    /* static variables to retain for the next call:  */
    static DB_RECORD    **previous_phase_rec = NULL ;
    static char         previous_sat[3] ;
    static char         previous_phase_start[22] ;
    static char         previous_phase_end[22] ;

    int         return_code ;

    DB_RECORD   **phase_rec_to_return ;
    DB_RECORD   **phase_rec ;
    llist       *phase_list = NULL ;
    cursor      phase_list_ptr ;

    double      et_input ;     /* input time in ephemeris time.  */

    /* counters that start at zero; they help to establish conditions:  */
    int     et_input_after_start_of_a_phase_count = 0 ;
    int     et_input_before_end_of_a_phase_count = 0 ;
    int     et_input_within_a_phase_count = 0 ;

    /* computed results from current phase record:  */
    int     first_rev ;
    double  et_phase_start ;
    double  et_phase_end ;
    double  days_per_rev ;

    if (!tc_asf2et(asftime, &et_input ) )
        return PHASE_BAD_ASFTIME ;

    if ( previous_phase_rec != NULL )
    {
        if ( strcmp( previous_sat, sat ) == 0 
        &&   strcmp( previous_phase_start, asftime ) <= 0 
        &&   strcmp( asftime, previous_phase_end   ) <= 0 )
        {
            /* 
            -- the input sat is the same; 
            -- the input asftime is within the previous phase.  
            -- make a new db record for the output and 
            -- copy the previous phase record to this output 
            -- phase record.  
            -- we have saved the processing time involved in 
            -- a retrieve from the database.  
            */
            *phase_rec_found =  new_table_record(APS_CDEFS(PHASE)) ;
            db_copy_record ( APS_CDEFS(PHASE), 
                *phase_rec_found, previous_phase_rec ) ;
            return PHASE_INPUT_TIME_WITHIN_A_PHASE ;
        }
    }
    /* check for et_input between 1/1/1985 and 1/1/2100   */
    if( et_input < EPH_MIN_TIME )
        return PHASE_ASFTIME_2_PHASE_INPUT_TIME_TOO_LOW ;

    if( et_input > EPH_MAX_TIME )
        return PHASE_ASFTIME_2_PHASE_INPUT_TIME_TOO_HIGH ;

    /* 
    -- get the phase recs for the given satellite; 
    -- pass the address of the phase_list so that its value
    -- will be set right.  phase_get_list will allocate a list 
    -- of DB_RECORDS.  
    */
    return_code = phase_get_list(sat, &phase_list ) ;
    if ( return_code < 0 )
        return return_code ;

    /* 
    -- check each record in order retrieved to determine the right 
    -- phase record.  
    */
    phase_rec = (DB_RECORD **) FIRST(phase_list, phase_list_ptr) ;
    do 
    {
        return_code = phase_first_rev( phase_rec, &first_rev ) ;

        return_code = tc_asf2et(
            CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START],
            &et_phase_start ) ;

        /* 
        -- compute nodal period, the float days_per_rev, 
        -- and end of phase.  this is for time overlap checking 
        -- and to determine if this is our desired record.  
        */
        days_per_rev = 
              (double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] 
            / (double) CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

        et_phase_end = et_phase_start + 
            days_per_rev 
            * CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] ;

        /*
        -- we need to return the right DB_RECORD for the phase.  
        -- for a start, we set some flags for the input et_input 
        -- being before, during, or after the current phase.  
        */

        if ( et_input >= et_phase_start )
            et_input_after_start_of_a_phase_count ++ ;

        /* 
        -- check to see if we haven't yet got to the desired record 
        -- and keep going if not; it might be safe to skip this record now.
        */
        if ( et_phase_end < et_input )
            continue ;

        if ( et_input < et_phase_end )
        {
            et_input_before_end_of_a_phase_count ++ ;
            if ( et_input_before_end_of_a_phase_count == 1 )
            {
                /*
                -- we always return the FIRST phase_rec in 
                -- which the input ephemeris time (et_input) 
                -- is before the phase end time.  
                --
                -- (the only exception is when the et_input is 
                -- after all of the phase ends.  In that case, 
                -- the last phase_rec is returned.  )
                */
                phase_rec_to_return = phase_rec ;
            }
        }

        /* 
        -- check this phase record to see if it is the 
        -- desired record, i.e., the input et_input is within 
        -- this phase.  
        */
        if ( et_input >= et_phase_start && et_input < et_phase_end )
            et_input_within_a_phase_count ++ ;

    }  while (  (phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr))
             != NULL ) ;

    /* 
    -- CREATE DB_RECORD for output phase record.  
    -- set the return value of the phase_rec_found 
    -- parameter to this address.  
    */
    *phase_rec_found =  new_table_record(APS_CDEFS(PHASE)) ;

    /* 
    -- now determine the correct condition code for where et_input fits 
    -- relative to the phase time brackets:
    --  PHASE_INPUT_TIME_BEFORE_ALL_PHASES
    --  PHASE_INPUT_TIME_WITHIN_A_PHASE
    --  PHASE_INPUT_TIME_AFTER_ALL_PHASES
    -- and, by elimination, PHASE_INPUT_TIME_BETWEEN_PHASES.
    */

    if ( et_input_before_end_of_a_phase_count == 0 )
    {
        /* PHASE_INPUT_TIME_AFTER_ALL_PHASES:  we return the last phase rec.  */
        phase_rec_to_return = (DB_RECORD **) LAST(phase_list, phase_list_ptr) ;

        /* copy the last record to the special phase record.  */
        return_code = db_copy_record ( 
            APS_CDEFS(PHASE), *phase_rec_found, phase_rec_to_return ) ;

        /* CLEAN UP */
        DEL_LIST( phase_list ) ;

        return PHASE_INPUT_TIME_AFTER_ALL_PHASES ;
    }

    /*
    -- in all other cases, we return the "phase_rec_to_return" 
    -- record selected earlier.  
    */

    /* copy the selected record to the special phase record.  */
    return_code = db_copy_record ( 
        APS_CDEFS(PHASE), *phase_rec_found, phase_rec_to_return ) ;

    /* save info on this record to save time on next call. */
    if ( previous_phase_rec == NULL )
    {
        /* 
        -- must CREATE DB_RECORD to make storage to 
        -- save the current record the first time.  
        -- we don't want this record to get freed and lost
        -- by the calling routine.  
        */
        previous_phase_rec =  new_table_record(APS_CDEFS(PHASE)) ;
    }
    /* 
    -- copy the selected record to the previous phase record. 
    -- also record the phase sat and time bracket for easy comparison 
    -- on next call. 
    */
    return_code = db_copy_record ( 
        APS_CDEFS(PHASE), previous_phase_rec, phase_rec_to_return ) ;
    (void)strcpy( previous_sat, 
        CAST_PHASE_SAT previous_phase_rec[PHASE_SAT] ) ;
    (void)strcpy( previous_phase_start, 
        CAST_PHASE_PHASE_START previous_phase_rec[PHASE_PHASE_START] ) ;
    return_code = tc_asf2et(
            CAST_PHASE_PHASE_START previous_phase_rec[PHASE_PHASE_START],
            &et_phase_start ) ;

    /* 
    -- compute nodal period, the float days_per_rev, 
    -- and end of phase.  this is for time overlap checking 
    -- and to determine if this is our desired record.  
    */
    days_per_rev = 
          (double) CAST_PHASE_CYCLE_DAYS previous_phase_rec[PHASE_CYCLE_DAYS] 
        / (double) CAST_PHASE_CYCLE_REVS previous_phase_rec[PHASE_CYCLE_REVS] ;

    et_phase_end = et_phase_start + 
            days_per_rev 
            * CAST_PHASE_PHASE_ORBITS previous_phase_rec[PHASE_PHASE_ORBITS] ;

    tc_et2asf( et_phase_end, previous_phase_end ) ;

    /* CLEAN UP */
    DEL_LIST( phase_list ) ;

    if ( et_input_after_start_of_a_phase_count == 0 )
        return PHASE_INPUT_TIME_BEFORE_ALL_PHASES ;

    if ( et_input_within_a_phase_count == 1 )
        return PHASE_INPUT_TIME_WITHIN_A_PHASE ;

    if ( et_input_within_a_phase_count > 1 )
        return PHASE_TWO_PHASE_RECS_OVERLAP_IN_TIME ;
    /* 
    -- by elimination of PHASE_INPUT_TIME_BEFORE_ALL_PHASES, 
    -- PHASE_INPUT_TIME_WITHIN_A_PHASE, and PHASE_INPUT_TIME_AFTER_ALL_PHASES, 
    -- there is only one logical possibility left, 
    -- PHASE_INPUT_TIME_BETWEEN_PHASES
    */

    return PHASE_INPUT_TIME_BETWEEN_PHASES ;

}

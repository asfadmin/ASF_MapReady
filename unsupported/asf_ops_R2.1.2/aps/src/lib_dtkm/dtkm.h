#ifndef DTKM_H
#define DTKM_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   
Description:    
Creator:    Lawrence Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)dtkm.h	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm.h"


/* USED FREQUENTLY   */
#include <string.h>        /* for strcpy() etc.  */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for NULL, TRUE, FALSE, MIN_REV and others  */ 
#include <values.h>         /* for MAXINT and others */
#include "aps_defs.h"       /* for APS_MAX_DTKID     */
#include <aps_db_table.h>       /* for APS DB tables sybase interface   */
#include "dtkm_utilities.h"     /* for the dtkm - library definitions   */
#include <ODLconversions.h>     /* for gen_rev2trackstart() etc.        */

struct Best_Antenna
{
    int     antenna_id ;
    int     preference ;
    llist   *dtk_conflicts ;
} ;


/*
-- "PRIVATE"  FUNCTION DECLARATIONS for libdtkm.a
*/

int activities_conflict(
    int         activity_id_1,
    int         activity_id_2 ) ;

int dtkm_activities_conflict(
    DB_RECORD   **proposed_dtk,     /* compare the proposal with the db dtk */
    DB_RECORD   **db_dtk ) ;

int dtkm_activity_id( DB_RECORD   **dtk_rec) ;    /* input data-take  */

int dtkm_set_asf_reduction_min( 
    DB_RECORD **dtk_proposal, /* readonly input DB_RECORD                     */
    DB_RECORD **result_dtk ) ;/* this pointer can be the same as dtk_proposal */

int dtkm_check_antenna_conflicts(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    char        *antenna_strttime,  /* time bracket to use for time overlaps. */
    char        *antenna_stoptime,
    llist       *dtk_conflicts ) ;  /* output list of conflicting data-takes. */

int dtkm_check_antenna_down_times(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    char        *antenna_strttime,
    char        *antenna_stoptime,
    llist       *antenna_down_times_list ) ;

int dtkm_check_bumpability(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    llist       *dtk_conflicts ) ;

int dtkm_check_dtkstat( DB_RECORD   **dtk_proposal ) ;

int dtkm_check_off_antenna(
    int     antenna_id,             /* input antenna_id                     */
    int     *antenna_list ) ;       /* zero-terminated list of antenna_id's */

int dtkm_check_other_sat_conflicts(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    char        *antenna_strttime,  /* time bracket to use for time overlaps. */
    char        *antenna_stoptime,
    llist       *dtk_conflicts ) ;  /* output list of conflicting data-takes. */

int dtkm_check_same_sat_conflicts(
    DB_RECORD       **dtk_proposal, /* input dtk proposal  */
    llist           *dtk_concurs,   /* output lists.  */
    llist           *dtk_similars,
    llist           *dtk_conflicts,
    llist           *dtk_parallels,
    llist           *dtk_same_pass,
    DB_RECORD       **result_dtk ) ;  /* dtk_proposal might change.  */

int dtkm_check_sat_equipment(
    DB_RECORD   **dtk_proposal,   /* check to see if this data-take uses
                                  any satellite equipment that is down
                                  during the time bracket of the data-take.  */
    llist      *dtk_sat_down_times ) ;/* holds satellite down time indicators */

int dtkm_check_sat_group_conflict(
    llist   *dtk_sat_group,     /* a list of downlink data-takes from a sat  */
    char    *antenna_strttime,  /* time bracket to use for dtk_proposal  */
    char    *antenna_stoptime,
    llist   *dtk_conflicts ) ;  /* if conflicting, add to this list.  */

int dtkm_check_station_id_antenna_id ( DB_RECORD   **dtk_rec ) ;

int dtkm_combine_dtks(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    llist       *dtk_concurs,    /* data-takes that match the proposal       */
    llist       *dtk_similars,   /* list of data-takes that the proposed     */
                                 /* data-take is similar to                  */
    DB_RECORD   **result_dtk,   /* the resulting dtk_proposal  */
    llist       *dtk_updates ); /* this routine adds the changed dtks to list */

int dtkm_compare_best_antenna(
    DBPROCESS               *APS_dbproc,
    DB_RECORD               **dtk_proposal,
    llist                   *dtk_conflicts,
    struct Best_Antenna     *p ) ;

int dtkm_create_antenna_list(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    int         **new_antenna_list ) ; /* address of new list of antennas.  */

int dtkm_create_same_sat_time_bracket(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_similars,  /* dtks overlapping, similar function.  */
    llist       *dtk_parallels, /* dtks overlapping, different function.*/
    llist       *dtk_same_pass, /* dtks with same downlink, not overlapping.  */
    char        *ss_strttime,   /* earliest start time of all data-takes.    */
    char        *ss_stoptime ); /* latest stop time of all data-takes.    */

int dtkm_delete_dl2obs_recs(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_record,  /* delete dl2obs associated with this dtk.    */
    llist       *dtk_updates );/* if dtk updates are necessitated, put note  */

int dtkm_delete_dtk_record(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_record ) ;

int dtkm_dl2obs(
    DB_RECORD   **dl_dtk,    /* input realtime downlink dtk record.          */
    llist       *obs_dtks ); /* output list of corresponding recording dtks. */

int dtkm_2dtks_concur(
    DB_RECORD   **dtk_rec1,
    DB_RECORD   **dtk_rec2 ) ;
/* returns TRUE, FALSE, and < 0 errors.  */

int dtkm_extract_first_sat(
    llist   *dtk_retrievals,
    llist   *dtk_sat_list ) ;

int dtkm_extract_other_sat_dtks(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_list,
    llist       *other_sat_dtks ) ;

int dtkm_extract_same_sat_dtks(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_list,
    llist       *same_sat_dtks ) ;

int dtkm_find_antenna_for_dtk(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    llist       *dtk_similars,
    llist       *dtk_parallels,
    llist       *dtk_same_pass,
    llist       *antenna_down_times,
    llist       *dtk_conflicts,
    DB_RECORD   **result_dtk ) ;

int dtkm_fix_dl2obs(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **downlink_dtk_rec,    /* input downlink dtk.    */
 
            /*
            -- note:  either or both llist pointers could be NULL,
            -- with the meaning (here) that there are no observations
            -- in the input list.
            */
    llist       *correct_obs_list,     /* input correct obs list */
    llist       *dl2obs_obs_list ) ;   /* input obs list according to dl2obs */

int dtkm_get_antenna_preference(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    int         *preference ) ;

int dtkm_get_antenna_priority(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    int         *priority ) ;

int dtkm_get_antenna_dtk_padding_time(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_proposal,
    int         *antenna_dtk_padding_time_sec ) ;

int dtkm_get_dl_dtk(
    DB_RECORD   **obs_dtk,   /* input observation data-take.                 */
    DB_RECORD   **dl_dtk ) ; /* output downlink linked by dl2obs             */

int dtkm_get_dtkstat_where_clause(
    DB_RECORD   **dtk_proposal,
    char        *dtkstat_where_clause ) ;

int dtkm_get_earliest_submit_time(
    llist   *dtk_list,
    char    *earliest_submit_time ) ;

int dtkm_get_highest_antenna_priority(
    DBPROCESS   *APS_dbproc,
    llist       *dtk_list,
    int         *highest_antenna_priority ) ;

int dtkm_get_obs_dtks(
    DB_RECORD   **dl_dtk,   /* input downlink downlink dtk record.         */
    llist       *obs_dtks );/* output observation dtks linked by dl2obs    */

int dtkm_get_quicklook_values(
    llist   *obs_dtk_list,          /* input list of observations            */
    char    *dl_planner_quicklook,  /* output 1-char planner QL for downlink */
    char    *dl_science_quicklook) ;/* output 1-char science QL for downlink */

int dtkm_get_rej_conflicts(
    DB_RECORD   **rej_dtk,        /* input dtk that was rejected.          */
    llist       *dtk_conflicts ); /* output data-takes to be re-submitted  */

int dtkm_insert_dl2obs_rec(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **obs_dtk_rec,       /* input REC or ROB observation dtk    */
    DB_RECORD   **downlink_dtk_rec) ;/* input downlnk data-take             */

int dtkm_insert_dtk_record(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **input_dtk,       /* rec to insert.  */
    DB_RECORD   **result_dtk,      /* result data-take    */
    llist       *dtk_updates ) ;   /* update duplicated into list if != NULL */

int dtkm_link_dl_dtk(
    DB_RECORD   **obs_dtk,   /* input observation (recording or realtime obs) */
    DB_RECORD   **dl_dtk ) ; /* output downlink linked by fa_schedule_id.     */

int dtkm_link_obs_dtks(
    DB_RECORD   **dl_dtk,    /* input downlink dtk record.                    */
    llist       *obs_dtks ); /* output obs dtks linked by fa_schedule_id.     */

int dtkm_obs2dl(
    DB_RECORD   **obs_dtk,   /* input observation (recording) data-take.     */
    DB_RECORD   **dl_dtk ) ; /* output downlink which downlinks it.          */

/* private routine to handle a single data-take, not bumps.  */
int dtkm_process_dtk(
    DBPROCESS   *APS_dbproc,     /* Sybase db process   */
    DB_RECORD   **input_dtk_rec,  /* data-take proposal.              */
    DB_RECORD   **result_dtk,     /* data-take with changed info              */
    DB_RECORD   **parent_dtk,     /* downlink for the recording dtk proposal  */
    llist       *dtk_sat_down_times, /* list of conflicting sat down times */
    llist       *antenna_down_times,/* list of antenna down times encountered */
    llist       *dtk_concurs,    /* data-takes that match the proposal       */
    llist       *dtk_similars,   /* list of data-takes that the proposed     */
                                 /* data-take is similar to                  */
    llist       *dtk_conflicts,  /* list of conflicting data-takes           */
    llist       *dtk_parallels,  /* list of parallel data-takes              */
    llist       *dtk_same_pass,  /* list of same_pass data-takes             */
    llist       *dtk_updates,    /* list of updated data-takes, not proposal */
    llist       *dtk_bumps ) ;   /* list of bumped data-takes to process.    */

int dtkm_r1_update_sensor(
    DBPROCESS   *APS_dbproc,     /* Sybase db process   */
    DB_RECORD   **dtk_proposal,
    DB_RECORD   **result_dtk,
    llist       *dtk_concurs,
    llist       *dtk_similars,
    llist       *dtk_updates ) ;

int dtkm_reduce_accepted_dtks(
    llist       *accepted_dtks, /*  input/output list of accepted dtks       */
    llist       *input_dtks ) ; /*  input list - look for REJ/CON/DEL dtks.  */

int dtkm_remove_observations( llist *dtk_list ) ;

int dtkm_sat_has_recorder( DB_RECORD **dtk_rec ) ;

int dtkm_sat_sensor_is_low_bit_rate( char *sat, char *sensor ) ;

int dtkm_select_next_untried_antenna(
    int     *antenna_list,              /* read this list      */
    int     *next_untried_antenna ) ;   /* return this member  */

int dtkm_set_fa_schedule_link_from_list(
    llist       *dtk_list,           /* look in this list for a value.       */
    DB_RECORD   **output_dtk_rec );  /* write the value found in this dtk.   */

int dtkm_set_proposed_dtkstat(
    DB_RECORD **dtk_proposal, /* readonly input DB_RECORD                     */
    DB_RECORD **result_dtk ) ;/* this pointer can be the same as dtk_proposal */

int dtkm_set_dtkstat_from_dl(
    DB_RECORD **obs_proposal, /* observation dtk readonly input DB_RECORD     */
    DB_RECORD **result_dtk,   /* this pointer can be the same as dtk_proposal */
    DB_RECORD **downlink_dtk);/* downlink for the observation dtk_proposal    */

int dtkm_time_bracket_dtk_list(
    llist       *dtk_list,
    char        *strttime,
    char        *stoptime ) ;

int dtkm_update_best_antenna(
    DBPROCESS           *APS_dbproc,
    DB_RECORD           **dtk_proposal,
    llist               *dtk_conflicts,
    struct Best_Antenna *p ) ;

int dtkm_update_dl2obs_4dtk(          /* updates dl2obs on a dtk change.   */
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **before_dtk_rec,     /* dtk before it was changed         */
    DB_RECORD   **after_dtk_rec  ) ;  /* dtk after it was changed.         */

int dtkm_update_dtk(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **input_dtk,      /* DB_RECORD to update dtk relation */
    DB_RECORD   **result_dtk,     /* resulting updated data-take      */
    llist       *dtk_updates ) ;  /* update duplicated into list if != NULL */

int dtkm_update_dtk_record(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **input_dtk,      /* rec to update.  */
    DB_RECORD   **result_dtk,     /* result data-take; can be = input_dtk   */
    llist       *dtk_updates ) ;  /* update duplicated into list if != NULL */

int dtkm_update_dtk_sensor(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **input_dtk,
    char        *sensor,
    DB_RECORD   **result_dtk,
    llist       *dtk_updates ) ;

int dtkm_update_link_dl2obs(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dl_dtk ) ;      /* input downlink data-take.      */

int dtkm_update_obs_dtkstat(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dl_dtk,  
    llist       *dtk_updates ) ;

int dtkm_update_obs_times_from_rdl(
    DBPROCESS       *APS_dbproc,    /* Sybase db process info struct          */
    DB_RECORD       **dl_dtk_rec,   /* input downlink data-take record        */
    llist           *dtk_updates ); /* append any changed dtks to this list.  */

int dtkm_update_link_obs2dl(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **obs_dtk );/* executes the update of dl2obs link in db     */

int dtkm_update_dl_quicklook(
    DBPROCESS   *APS_dbproc,
    char        dl_planner_quicklook, /* input desired value:  'Y' or 'N' */
    char        dl_science_quicklook, /* input desired value:  'Y' or 'N' */
    DB_RECORD   **downlink_rec,       /* dtk rec to update.               */
    llist       *dtk_updates ) ;      /* output list of updates.          */

#endif  /* DTKM_H */

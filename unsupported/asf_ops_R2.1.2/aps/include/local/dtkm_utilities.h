#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   dtkm_utilities.h
Description:    
Creator:    unknown
Notes:      
==============================================================================*/
#pragma ident   "@(#)dtkm_utilities.h	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.dtkm_utilities.h"

#ifndef _DTKM_UTILITIES_H_

#define _DTKM_UTILITIES_H_

#include <db_sybint.h>


/* 
-- use these to tell something about the dtk_rec:  
-- they return TRUE if true, and FALSE if false, and 
-- < 0 if the pointer is null or another error is found decode using 
-- DTKM_ERROR_MESSAGE() macro.  
*/
int dtkm_is_a_downlink( DB_RECORD **dtk_rec ) ;
int dtkm_is_a_realtime_downlink( DB_RECORD **dtk_rec ) ;
int dtkm_is_a_realtime_observation( DB_RECORD **dtk_rec ) ;
int dtkm_is_a_recording( DB_RECORD **dtk_rec ) ;
int dtkm_is_a_tape_dump( DB_RECORD **dtk_rec ) ;
int dtkm_is_an_observation( DB_RECORD **dtk_rec ) ;
int dtkm_is_in_antarctic_mode( DB_RECORD **dtk_rec ) ;

/* used to compare with dtk.sensor to identify realtime/tape dump downlink:   */
#define DTKM_SENSOR_REALTIME_DOWNLINK_CODE       "RDL"
#define DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE       "DMP"
/* used to compare with dtk.actid to identify realtime/recording observations */
#define DTKM_ACTID_REALTIME_OBSERVATION_CODE     "ROB"
#define DTKM_ACTID_RECORDING_OBSERVATION_CODE    "REC"
 


/*
--  Contents:
--      Extern Global Declarations
--      Function prototypes
--      Error codes
*/


/*
--  External Global Variables for libdtkm.a
*/

extern char dtk_string[];


/*
--  "PUBLIC" FUNCTION PROTOTYPES for libdtkm.a
*/
/* libdtkm.a   */

int dtkm_consolidate_tapedumps( llist *input_dtks ) ;

int dtkm_add_rdl2dtks( 
    llist       *input_dtks,        /* input list of data-takes              */
    llist       *output_dtk_rdl ) ; /* output list of RDL data-takes, if any */
int dtkm_add_rdl2obs( llist *input_dtk_obs, llist *output_dtk_rdl) ;

int dtkm_dtk_consolidation_by_rev( llist      *input_dtk_list) ;

int dtkm_antenna_down_times_rec2str( DB_RECORD **antenna_down_times_record ) ;

int dtkm_blank_values( 
    DB_RECORD **dtk_proposal,
    DB_RECORD **result_dtk ) ;


/*
-- dtkm_determine_station_mask() can return these condition codes >= 0:
-- DTKM_DTK_HAS_TIME_IN_MASK
-- DTKM_DTK_HAS_NO_TIME_IN_MASK
*/ 
int dtkm_determine_station_mask (
    DB_RECORD   **proposed_dtk,     /* input dtk proposal                     */
    int         secs,               /* input min time to make it worthwile    */
    char        *reduced_strttime,  /* output start time for dtk within mask  */
    char        *reduced_stoptime,  /* output stop time for dtk within mask   */
                                    /* this could reduce the dtk a bit.       */
    char        *station_id ) ;     /* output station id, if any, to take dtk */


int dtkm_get_firstlast_aoslos( 
    llist   *dtk_list,
    char    *first_aos_time, 
    char    *last_los_time ) ;

int dtkm_get_firstlast_track(
    llist   *dtk_list,            /* list of dtks.                        */
    char    *first_track_start,   /* output track start of rec 1          */
    char    *last_track_end ) ;   /* output track end of last rec in list */

int dtkm_get_mask_times(
    char    *sat,
    int     rev,
    char    *station_id,
    char    *asftime_mask_entry,   /* output entry in ASF time        */
    char    *asftime_mask_exit,    /* output exit in ASF time         */
    double  *et_mask_entry,        /* output entry in ephemeris time  */
    double  *et_mask_exit ) ;      /* output exit in ephemeris time   */

int dtkm_get_minmax_aoslos( 
    llist   *dtk_list,
    char    *min_aos_time, 
    char    *max_los_time ) ;

int dtkm_get_sitename( 
    DB_RECORD **dtk_proposal,
    DB_RECORD **result_dtk ) ;

int dtkm_aps_does_cvrg4sat( DB_RECORD **dtk_rec ) ;
/* returns TRUE, FALSE, error < 0    */

int dtkm_check_rev_asftimes (
    DB_RECORD   **dtk_rec,
    int         *rev_number_for_strttime,   /* output rev for strttime   */
    char        *strttime_for_rev,          /* output strttime for rev   */
    char        *stoptime_for_rev  ) ;      /* output stoptime for rev   */

int dtkm_check_station_id (
    char        *station_id,
    char        *sat  ) ;

int dtkm_check_transid( DB_RECORD   **proposed_dtk ) ;

int dtkm_check_values(
    DB_RECORD   **proposed_dtk , /* proposed data-take  */
    DB_RECORD   **result_dtk ) ; /* result data-take    */

int dtkm_copy_dtks_value(
    int         col_number,     /* column to update, e.g. DTK_STRTTIME */
    void        *value_ptr,     /* &(field value); MAKE SURE it's the same
                                    byte length as the value of the column 
                                    as in aps_db_table.c in libapsdb.a */
    llist       *dtk_list,
    llist       *copied_dtk_list ) ;

int dtkm_count_obs_dtks4SHAQ( DB_RECORD **dl_dtk ); 
    /* input SHAQ downlink dtk record.*/

int dtkm_default_values(
    DB_RECORD   **dtk_rec ,
    DB_RECORD   **result_rec ) ;

int dtkm_delete_dtk(            /* returns TRUE if OK.                   */
    DBPROCESS   *APS_dbproc,    /* Sybase process id.                    */
                                /* identify dtk by sat/sensor/rev/dtkid: */
    char        *sat,           /* input satellite:  E1, E2, R1, etc.    */
    char        *sensor,        /* input sensor:  SAR, ST1, RDL, DMP...  */
    int         rev,            /* input rev number                      */
    int         dtkid ) ;       /* input dtkid                           */

int dtkm_delete_dtk_record(     /* delete this dtk from the database.    */
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_record ) ;

void dtkm_delete_dtk_from_list(
    DB_RECORD **dtk_rec,      /* delete this rec from list, if found in  */
    llist     *dtk_list  ) ;  /* this list.                              */

int dtkm_dl2obs(
    DB_RECORD   **dl_dtk,   /* input downlink dtk record.         */
    llist       *obs_dtks );/* output list of corresponding observation dtks. */

llist *dtkm_duplicate_dtk_into_list(
    DB_RECORD   **dtk_rec,
    llist       *dtk_list ) ;

int get_cvg(
    DBPROCESS   *APS_dbproc,    /* Sybase process id.   */
    char        *sat,       /* input satellite.     */
    char        *sen,       /* input sensor         */
    int         rev,        /* input rev number     */
    char        *asft,      /* input asf time for desired cvrg points.  */
    float       *sublat,    /* sublat point from cvrg relation, interpolated. */
    float       *nrlat,     /* nrlat point from cvrg relation, interpolated. */
    float       *nrlon,     /* nrlon point from cvrg relation, interpolated. */
    float       *frlat,     /* frlat point from cvrg relation, interpolated. */
    float       *frlon,     /* frlon point from cvrg relation, interpolated. */
    char        *ascdsc ) ; /* ascdsc point from cvrg relation, interpolated. */

int dtkm_get_cvrg_points(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **proposed_dtk, /* proposed data-take    */
    DB_RECORD   **result_dtk ) ;/* result data-take    */


int dtkm_in_station_mask( 
    char        *station_id,
    DB_RECORD   **proposed_dtk,
    int         secs,
    char        *reduced_strttime,
    char        *reduced_stoptime ) ;
/* positive, non-error return codes to check for:  */
#define DTKM_DTK_HAS_NO_TIME_IN_MASK      1
#define DTKM_DTK_HAS_TIME_IN_MASK         2

int dtkm_obs2dl_list(
    llist       *obs_dtk_list,     /* input observations list.  */
    llist       *dl_dtk_list   ) ; /* output downlinks list.    */

int dtkm_print(
    FILE *fp,
    DB_RECORD **dtk_rec ) ;

int dtkm_print_antenna_down_times(
    FILE *fp,
    DB_RECORD **antenna_down_times_rec ) ;

int dtkm_print_antenna_down_times_list(
    FILE    *fp,
    llist   *antenna_down_times_list ) ;

int dtkm_print_list(FILE *fp, llist *dtk_list ) ;

int dtkm_process_antenna_down_time(
    DBPROCESS   *APS_dbproc,               /* Sybase db process              */
    DB_RECORD   **antenna_down_times_rec,  /* new antenna down time.         */
    llist       *dtks_updated,  /* changed data-takes (NULL if unwanted).    */
    FILE        *report_fp ) ;  /* output report file (NULL if unwanted).    */

/* handles this dtk_proposal and bumps.  */
int dtkm_process_dtk_proposal(
    DBPROCESS   *APS_dbproc,     /* Sybase db process   */
    DB_RECORD   **dtk_proposal,  /* input data-take proposal.          */
    DB_RECORD   **result_dtk ,   /* data-take proposal with updated info     */
    DB_RECORD   **parent_dtk ,   /* downlink for a recording dtk proposal    */
    llist       *sat_down_times,/* list of conflicting satellite down times */
    llist       *antenna_down_times, /* antenna down times encountered       */
    llist       *dtk_concurs,   /* concurs found processing original proposal */
    llist       *dtk_similars,  /* similars found processing original.  */
    llist       *dtk_conflicts, /* list of conflicting data-takes      */
    llist       *dtk_updates ) ; /* other data-takes updated                 */

/* 
-- this macro is used to disable "rejection by omission"
-- by using it as the value for REJ_omission_dtks in the function call
*/ 
#define DTKM_DO_NOT_REJ_BY_OMISSION  ( llist* ) -3
int dtkm_process_dtk_proposal_list(
    DBPROCESS   *APS_dbproc,    /* Sybase db process                        */
    llist       *input_dtks,    /* list of input data-take records          */
    llist       *accepted_dtks, /* output list of accepted data-takes       */
    llist       *rejected_dtks, /* output list of rejected data-takes       */
    llist       *CON_dtks,      /* output list of CON data-takes            */
    llist       *deleted_dtks,  /* output list data-takes deleted on request. */
    llist       *error_dtks,    /* output list data-takes with errors.      */
    llist       *REJ_omission_dtks, /* db dtks omitted by FA schedule       */
    llist       *other_sat_dtks,/* other satellite db dtks affected         */
    llist       *same_sat_dtks, /* same sat other db dtks affected          */
    llist       *dtk_updates,   /* complete list of final updates.          */
    FILE        *report_fp ) ;  /* pointer for output report file.          */

int dtkm_rec2str( DB_RECORD **dtk_rec ) ;

int dtkm_remove_dtks_from_list(
    llist   *dtk_list,
    llist   *remove_from_this_dtk_list ) ;

int dtkm_sat_has_recorder( DB_RECORD **dtk_rec ) ;

int dtkm_update_dtks_field(
    DBPROCESS   *APS_dbproc,
    llist       *dtk_list,      /*  list of data-takes to update in db
                                    ALL fields in these records will update
                                    the database unless the value already
                                    matches.     */
    int         col_number,     /* column to set, e.g. DTK_STRTTIME     */
    void        *value_ptr,     /* &(field value); make sure it's long enough */
    llist       *dtks_changed ) ;

int dtkm_update_dtk_darid(
    DB_RECORD   **dtk_proposal) ;


/*
--  START ERROR CODES AND MESSAGES
--
--  These error codes and corresponding messages are
--  implemented here and in dtkm_decode_error.c
--  The code v. message correspondances must be the
--  same here as there.
--
-- NOTE:  these error codes must be < 0.  this is because the
-- calling routines check for the return code < 0 to do
-- error handling.
--
*/
 
/* a macro to decode the error code into a string:  */
extern char *dtkm_error_message[] ;
 
#define DTKM_ERROR_MESSAGE( code ) \
    dtkm_error_message[ -(code) ]


/* ERROR CODES from dtkm_process_dtk()  */
#define DTKM_ERROR_ACTIVITY_GT_1_RECORD                                    -1
#define DTKM_ERROR_ACTIVITY_ILLEGAL                                        -2
#define DTKM_ERROR_ACTIVITY_NOT_FOUND                                      -3
#define DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY                       -4
#define DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_INITIALIZED                 -5
#define DTKM_ERROR_ANTENNA_ID_IS_ZERO_NO_PRIORITY                          -6
#define DTKM_ERROR_ANTENNA_ID_LE_ZERO                                      -7
#define DTKM_ERROR_ANTENNA_NOT_FOUND_IN_LIST                               -8
#define DTKM_ERROR_ANTENNA_PREFERENCE_NOT_FOUND_FOR_DTK_PROPOSAL           -9
#define DTKM_ERROR_ANTENNA_PRIORITY_NOT_FOUND_FOR_DTK_PROPOSAL            -10
#define DTKM_ERROR_ANTENNA_REC_NOT_FOUND_FOR_DTK_PROPOSAL                 -11
#define DTKM_ERROR_APS_READER_SESSION_COULD_NOT_BE_OPENED                 -12
#define DTKM_ERROR_ATTEMPTING_TO_DELETE_DTK                               -13
#define DTKM_ERROR_ATTEMPT_TO_DELETE_NONEXISTING_DTK                      -14
#define DTKM_ERROR_ATTEMPT_TO_UPDATE_NONEXISTING_DTK                      -15
#define DTKM_ERROR_ATTEMPT_TO_UPDATE_PRIMARY_KEY                          -16
#define DTKM_ERROR_BAD_ACTID_VALUE                                        -17
#define DTKM_ERROR_BAD_ASCDSC_VALUE                                       -18
#define DTKM_ERROR_BAD_DTKSTAT_VALUE                                      -19
#define DTKM_ERROR_BAD_FARLAT1_VALUE                                      -20
#define DTKM_ERROR_BAD_FARLAT2_VALUE                                      -21
#define DTKM_ERROR_BAD_FARLON1_VALUE                                      -22
#define DTKM_ERROR_BAD_FARLON2_VALUE                                      -23
#define DTKM_ERROR_BAD_NRLAT1_VALUE                                       -24
#define DTKM_ERROR_BAD_NRLAT2_VALU                                        -25
#define DTKM_ERROR_BAD_NRLON1_VALUE                                       -26
#define DTKM_ERROR_BAD_NRLON2_VALUE                                       -27
#define DTKM_ERROR_BAD_REV_VALUE                                          -28
#define DTKM_ERROR_BAD_SAT_SENSOR_VALUES                                  -29
#define DTKM_ERROR_BAD_SAT_STATION_ID_VALUE                               -30
#define DTKM_ERROR_BAD_SCHEDULE_ID_FOR_R1_DTK                             -31
#define DTKM_ERROR_BAD_SENSOR_VALUE                                       -32
#define DTKM_ERROR_BAD_STOPLAT_VALUE                                      -33
#define DTKM_ERROR_BAD_STOPTIME_VALUE                                     -34
#define DTKM_ERROR_BAD_STRTLAT_VALUE                                      -35
#define DTKM_ERROR_BAD_STRTTIME_VALUE                                     -36
#define DTKM_ERROR_BAD_TRANSID_VALUE                                      -37
#define DTKM_ERROR_BUMPED_DTK_WAS_DELETED_OR_REJECTED                     -38
#define DTKM_ERROR_BUMPS_LIST_NOT_EMPTY                                   -39
#define DTKM_ERROR_BUMPS_LIST_NOT_INITIALIZED                             -40
#define DTKM_ERROR_CANNOT_IDENTIFY_DTK_TO_UPDATE                          -41
#define DTKM_ERROR_COMPUTING_TIME_DIFF                                    -42
#define DTKM_ERROR_CONCURS_LIST_NOT_EMPTY                                 -43
#define DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED                           -44
#define DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY                               -45
#define DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED                         -46
#define DTKM_ERROR_DAR_NOT_FOUND_FROM_DTK_DARID                           -47
#define DTKM_ERROR_DB_QUERY_FAILED                                        -48
#define DTKM_ERROR_DELETING_DTK_RECORD_WHEN_CHANGING_SENSOR               -49
#define DTKM_ERROR_DELETION_DURING_UPDATE                                 -50
#define DTKM_ERROR_DTKID_LT_ZERO                                          -51
#define DTKM_ERROR_DTKID_TOO_BIG                                          -52
#define DTKM_ERROR_DTK_CONCURS_LIST_HAS_NEITHER_0_NOR_1_MEMBERS           -53
#define DTKM_ERROR_DTK_LIST_IS_MIXED_WITH_DIFFERENT_ANTENNAS              -54
#define DTKM_ERROR_DTK_NOT_INSERTED                                       -55
#define DTKM_ERROR_DTK_NOT_INSERTED_DURING_UPDATE                         -56
#define DTKM_ERROR_DTK_SAT_LIST_NOT_EMPTY                                 -57
#define DTKM_ERROR_DTK_SAT_LIST_NOT_INITIALIZED                           -58
#define DTKM_ERROR_DTK_SHOULD_BE_DEL_REJ_INV_QUE_SUB_REQ_PLN_OR_SCH_STATUS -59
#define DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY                             -60
#define DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED                       -61
#define DTKM_ERROR_EXPANDING_TIME_BRACKET                                 -62
#define DTKM_ERROR_FARLAT1_AND_FARLAT2_EQUAL_ZERO                         -63
#define DTKM_ERROR_FARLON1_AND_FARLON2_EQUAL_ZERO                         -64
#define DTKM_ERROR_FIELD_ACTID_NOT_SET                                    -65
#define DTKM_ERROR_GT_1_DTK_DELETED                                       -66
#define DTKM_ERROR_GT_1_DTK_DELETED_DURING_UPDATE                         -67
#define DTKM_ERROR_GT_1_DTK_REC_WITH_SAME_SAT_SENSOR_REV_DTKID            -68
#define DTKM_ERROR_GT_ONE_ANTENNA_USED_ON_SAME_PASS                       -69
#define DTKM_ERROR_ILLEGAL_CONF_STATUS_IN_ACTIV_CONF_RELATION             -70
#define DTKM_ERROR_INPUT_ANTENNA_LIST_NOT_INITIALIZED                     -71
#define DTKM_ERROR_INPUT_DTK_LIST_EMPTY                                   -72
#define DTKM_ERROR_INPUT_DTK_LIST_NOT_EMPTY                               -73
#define DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED                         -74
#define DTKM_ERROR_INPUT_DTK_WITH_DTKID_LT_ZERO                           -75
#define DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE                           -76
#define DTKM_ERROR_INSERTING_DTK_WITH_EXISTING_PRIMARY_KEY                -77
#define DTKM_ERROR_IN_CODE_DTK_CREATED_BUMPS                              -78
#define DTKM_ERROR_IN_CODE_IN_dtkm_check_same_sat_conflicts               -79
#define DTKM_ERROR_IN_CODE_dtkm_activities_conflict                       -80
#define DTKM_ERROR_IN_CODE_dtkm_extract_conflicts_and_parallels_else      -81
#define DTKM_ERROR_IN_CODE_dtkm_extract_conflicts_and_parallels_if        -82
#define DTKM_ERROR_IN_CODE_dtkm_j1_equipment_status_check                 -83
#define DTKM_ERROR_IN_CREATING_SAME_SAT_TIME_BRACKET                      -84
#define DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST                    -85
#define DTKM_ERROR_IN_MAKING_RETRIEVE_TIME_BRACKET                        -86
#define DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST                        -87
#define DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST                            -88
#define DTKM_ERROR_LIST_NOT_INITIALIZED                                   -89
#define DTKM_ERROR_LOWEST_DTKID_NOT_FOUND                                 -90
#define DTKM_ERROR_LT_ZERO_ELEMENTS_IN_LIST                               -91
#define DTKM_ERROR_NE_1_DARS_FOUND_FROM_DTK_DARID                         -92
#define DTKM_ERROR_NO_ANTENNAS_FOUND_IN_ANTENNA_PREF_TABLE                -93
#define DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE               -94
#define DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PREF_RELATION                    -95
#define DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PRIORITY_RELATION                -96
#define DTKM_ERROR_NO_RECORDS_IN_ANTENNA_RELATION                         -97
#define DTKM_ERROR_NO_RECS_IN_ACTIVITIES_RELATION                         -98
#define DTKM_ERROR_NO_STOPTIME                                            -99
#define DTKM_ERROR_NO_STRTTIME                                           -100
#define DTKM_ERROR_NRLAT1_AND_NRLAT2_EQUAL_ZERO                          -101
#define DTKM_ERROR_NRLON1_AND_NRLON2_EQUAL_ZERO                          -102
#define DTKM_ERROR_NULL_ANTENNA_LIST_PTR                                 -103
#define DTKM_ERROR_NULL_DTK_2B_COMBINED                                  -104
#define DTKM_ERROR_NULL_DTK_PROPOSAL                                     -105
#define DTKM_ERROR_NULL_DTK_RESULT_RECORD                                -106
#define DTKM_ERROR_NULL_OUTPUT_DTK_LIST                                  -107
#define DTKM_ERROR_NULL_PROPOSED_DTK_SEG                                 -108
#define DTKM_ERROR_NULL_RECORD                                           -109
#define DTKM_ERROR_OUTPUT_ACCEPTED_DTK_LIST_IS_NOT_EMPTY                 -110
#define DTKM_ERROR_OUTPUT_ACCEPTED_DTK_LIST_NOT_INITIALIZED              -111
#define DTKM_ERROR_OUTPUT_CON_DTK_LIST_IS_NOT_EMPTY                      -112
#define DTKM_ERROR_OUTPUT_CON_DTK_LIST_NOT_INITIALIZED                   -113
#define DTKM_ERROR_OUTPUT_DELETED_DTK_LIST_IS_NOT_EMPTY                  -114
#define DTKM_ERROR_OUTPUT_DELETED_DTK_LIST_NOT_INITIALIZED               -115
#define DTKM_ERROR_OUTPUT_ERROR_DTK_LIST_IS_NOT_EMPTY                    -116
#define DTKM_ERROR_OUTPUT_ERROR_DTK_LIST_NOT_INITIALIZED                 -117
#define DTKM_ERROR_OUTPUT_OMISSION_DTK_LIST_IS_NOT_EMPTY                 -118
#define DTKM_ERROR_OUTPUT_OMISSION_DTK_LIST_NOT_INITIALIZED              -119
#define DTKM_ERROR_OUTPUT_OTHER_SAT_DTKS_LIST_IS_NOT_EMPTY               -120
#define DTKM_ERROR_OUTPUT_OTHER_SAT_DTKS_LIST_NOT_INITIALIZED            -121
#define DTKM_ERROR_OUTPUT_REJECTED_DTK_LIST_IS_NOT_EMPTY                 -122
#define DTKM_ERROR_OUTPUT_REJECTED_DTK_LIST_NOT_INITIALIZED              -123
#define DTKM_ERROR_OUTPUT_SAME_SAT_DTKS_LIST_IS_NOT_EMPTY                -124
#define DTKM_ERROR_OUTPUT_SAME_SAT_DTKS_LIST_NOT_INITIALIZED             -125
#define DTKM_ERROR_PADDING_ANTENNA_TIME_BRACKET                          -126
#define DTKM_ERROR_PADDING_SAME_SAT_TIME_BRACKET                         -127
#define DTKM_ERROR_PARALLELS_LIST_NOT_EMPTY                              -128
#define DTKM_ERROR_PARALLELS_LIST_NOT_INITIALIZED                        -129
#define DTKM_ERROR_PROCESSING_BUMPED_DTKS                                -130
#define DTKM_ERROR_OBSERVATION_DTK_HAS_ANTENNA_ID_NE_ZERO                -131
#define DTKM_ERROR_SAME_PASS_LIST_NOT_EMPTY                              -132
#define DTKM_ERROR_SAME_PASS_LIST_NOT_INITIALIZED                        -133
#define DTKM_ERROR_SAT_DOES_NOT_HAVE_A_RECORDER                          -134
#define DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY                         -135
#define DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_INITIALIZED                   -136
#define DTKM_ERROR_SAT_NOT_A1                                            -137
#define DTKM_ERROR_SAT_NOT_E1                                            -138
#define DTKM_ERROR_SAT_NOT_E2                                            -139
#define DTKM_ERROR_SAT_NOT_J1                                            -140
#define DTKM_ERROR_SAT_NOT_R1                                            -141
#define DTKM_ERROR_SAT_NOT_R1_FOR_dtkm_r1_update_sensor                  -142
#define DTKM_ERROR_SENSOR_AND_ACTID_DONT_MATCH                           -143
#define DTKM_ERROR_SIMILARS_LIST_IS_EMPTY                                -144
#define DTKM_ERROR_SIMILARS_LIST_NOT_EMPTY                               -145
#define DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED                         -146
#define DTKM_ERROR_STATUS_IS_NEITHER_SCH_NOR_PLN_WHEN_COMBINING_DTKS     -147
#define DTKM_ERROR_STATUS_NOT_QUE_PLN_REQ_OR_SCH                         -148
#define DTKM_ERROR_STOPTIME_LE_STRTTIME                                  -149
#define DTKM_ERROR_STRTLAT_AND_STOPLAT_EQUAL_ZERO                        -150
#define DTKM_ERROR_TIMES_NOT_WITHIN_REV                                  -151
#define DTKM_ERROR_UNKNOWN_NORMAL_RETURN_CODE_FROM_DTKM_PROCESS_DTK      -152
#define DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_check_bumpability       -153
#define DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_update_best_antenna     -154
#define DTKM_ERROR_UNKNOWN_SAT                                           -155
#define DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_transid                     -156
#define DTKM_ERROR_UNLINKING_RECORD                                      -157
#define DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV                        -158
#define DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK -159
#define DTKM_ERROR_CONVERTING_ET2ASF                                     -160
#define DTKM_ERROR_REC_NEITHER_IN_NOR_OUT_IN_MASKINOUT                   -161
#define DTKM_ERROR_NE_1_INMASK_EVENT_IN_REV                              -162
#define DTKM_ERROR_NULL_ANTENNA_DOWN_TIMES_REC                           -163
#define DTKM_ERROR_ANTENNA_DOWN_TIMES_REC_NOT_INSERTED                   -164
#define DTKM_ERROR_IN_MASKINOUT_RELATION                                 -165
#define DTKM_ERROR_DOWNLINK_DTK_HAS_NO_TIME_IN_MASK                      -166
#define DTKM_ERROR_DOWNLINK_DTK_NOT_ENTIRELY_WITHIN_STATION_PASS         -167
#define DTKM_ERROR_UNEXPECTED_RETURN_CODE_FROM_dtkm_in_station_mask      -168
#define DTKM_ERROR_MUST_INCREASE_MAX_POSSIBLE_SATS                       -169
#define DTKM_ERROR_IN_CODE_dtkm_aps_does_cvrg4sat                        -170
#define DTKM_ERROR_COL_NUMBER_GT_NUM_DTK_COLS                            -171
#define DTKM_ERROR_BAD_SAT_STATION_ID_ANTENNA_ID_COMBINATION             -172

#define DTKM_ERROR_avaliable_for_a_new_code                              -173

#define DTKM_ERROR_BAD_ACTID_AGENCY_VALUE                                -174
#define DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_agency                      -175
#define DTKM_ERROR_NO_INCLUSION_PERIOD_FOR_SATELLITE                     -176
#define DTKM_ERROR_DUPLICATE_DTK_KEYS                                    -177
#define DTKM_ERROR_OUTPUT_DTK_LIST_IS_NOT_EMPTY                          -178
#define DTKM_ERROR_dl2obs_rec_NOT_INSERTED                               -179
#define DTKM_ERROR_DTK_IS_NOT_A_DOWNLINK                                 -180
#define DTKM_ERROR_GT_1_DOWNLINK_LINKED_TO_OBS_DTK                       -181
#define DTKM_ERROR_DTK_IS_A_DOWNLINK                                     -182
#define DTKM_ERROR_INSERTING_DTK__DTKID_NOT_UNIQUE_TO_SAT_REV            -183
#define DTKM_ERROR_NO_STATION_REC_FOR_DTK_SAT                            -184
#define DTKM_ERROR_NULL_DTK_DOWNLINK_RECORD                              -185
#define DTKM_ERROR_BAD_SUBMIT_TIME_VALUE                                 -186
#define DTKM_ERROR_A1_REALTIME_SENSOR_DTKSTAT_NOT_INV                    -187
#define DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD                 -188
#define DTKM_ERROR_SETTING_ZERO_VALUE                                    -189
#define DTKM_ERROR_DB_UPDATE_FAILED_ON_DL2OBS_RELATION                   -190
#define DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION                   -191
#define DTKM_ERROR_INPUT_DTK_IS_NOT_RDL                                  -192
#define DTKM_ERROR_NO_SATSENSOR_RECORDS_FOUND                            -193
#define DTKM_ERROR_IN_CODE_SEE_STDERR_FOR_CODE_LOCATION                  -194
#define DTKM_ERROR_DUPLICATING_DTK_LIST                                  -195
#define DTKM_ERROR_SETTING_SENSOR_AND_ACTID_VALUES                       -196
#define DTKM_ERROR_SETTING_VALUES_IN_LLIST                               -197
#define DTKM_ERROR_SAT_NOT_SUPPORTED_BY_FUNCTION                         -198
#define DTKM_ERROR_DIFFERENT_SATS_IN_INPUT_DTK_LLIST                     -199
#define DTKM_ERROR_INPUT_LLIST_MUST_BE_ALL_REALTIME_OBSERVATIONS         -200
#define DTKM_ERROR_DUPLICATING_DTK_RECORD                                -201
#define DTKM_ERROR_DTK_NOT_UPDATED                                       -202
#define DTKM_ERROR_DETERMINING_PHASE_FOR_DTK                             -203
#define DTKM_ERROR_COPYING_DTK_RECORDS_TO_LLIST                          -204
#define DTKM_ERROR_BAD_SAT_VALUE                                         -205
#define DTKM_ERROR_BAD_DTKID_VALUE                                       -206
#define DTKM_ERROR_DTK_INSERT_REQUEST_DTK_HAS_DTKID_NE_ZERO              -207
#define DTKM_ERROR_DTK_PROPOSAL_DTKID_GT_ZERO_AND_NOT_IN_DB              -208
#define DTKM_ERROR_GETTING_NEW_DTKID                                     -209
#define DTKM_ERROR_DTK_IS_NEITHER_OBSERVATION_NOR_DOWNLINK               -210
#define DTKM_ERROR_DB_UPDATE_FAILED_ON_DTK_RELATION                      -211
#define DTKM_ERROR_INCREASE_CHECK_LIST_SIZE_in_dtkm_fix_dl2obs           -212
#define DTKM_ERROR_PLANNER_QL_ARG_VALUE_NOT_Y_OR_N                       -213
#define DTKM_ERROR_SCIENCE_QL_ARG_VALUE_NOT_Y_OR_N                       -214
#define DTKM_ERROR_IN_TIME_CONVERSION                                    -215


/* NOTE:  */
/*****************************************************************************
*                                                                            *
*         You MUST add a line in src/lib_dtkm/dtkm_error_message.c           *
*         whenever you add a new error code.                                 *
*                                                                            *
*****************************************************************************/


/* NORMAL CODES from dtkm_create_antenna_list.c:  */
#define DTKM_CREATE_ANTENNA_LIST_OK                              1
#define DTKM_CHECK_OFF_ANTENNA_OK                                1
#define DTKM_SELECT_NEXT_UNTRIED_ANTENNA_OK                      1
#define DTKM_NO_MORE_UNTRIED_ANTENNAS                            0

/* NORMAL CODES from dtkm_default_values.c:  */
#define DTKM_DEFAULT_VALUES_OK                                   1

/* NORMAL CODES from dtkm_get_cvrg_points.c: must be 0  */
#define DTKM_GET_CVRG_POINTS_OK                                  0

/* NORMAL CODES from dtkm_check_values.c  */
#define DTKM_REC_OK                                              1
#define DTKM_FIELD_OK                                            1

/* NORMAL CODES from dtkm_insert_dtk_record()  */
#define DTKM_DTK_INSERTED_OK                                     1

/* NORMAL CODES from dtkm_update_dtk_record()  */
#define DTKM_DTK_UPDATED_OK                                      1

/* NORMAL CODES from dtkm_update_dtks_field()  */
#define DTKM_UPDATE_DTKS_FIELD_OK                                1

/*
-- dtkm_check_sat_equipment.c return codes:
*/
#define DTKM_SAT_EQUIPMENT_NOT_OK               2
#define DTKM_SAT_EQUIPMENT_OK                   1
 
/*
-- activity conflict status codes.
-- NOTE:  these values must agree with the database values
-- in the activ_conf relation.
*/
#define DTKM_ACTIVITIES_DO_NOT_CONFLICT         0
#define DTKM_ACTIVITIES_SHOULD_BE_COMBINED      1
#define DTKM_ACTIVITIES_CONFLICT                2
#define DTKM_ACTIVITIES_IDENTICAL               3

/*
-- Non-error return codes from dtkm_obs2dl() 
*/
#define DTKM_DOWNLINK_REC_NOT_FOUND                 1
#define DTKM_DOWNLINK_REC_FOUND                     0

    /*
    -- data-take conflict status codes.  these are the final
    -- status values when two data-takes are being compared.
    -- the actual dtk record dtkid or fadtkid values were
    -- compared to deterimine the SIMILAR vs CONCUR status.
    -- these values have nothing to do with any database values
    -- in the activ_conf relation.
    */
 
#define DTKS_PARALLEL                      4
#define DTKS_SHOULD_BE_COMBINED            5
#define DTKS_CONFLICT                      6
#define DTKS_CONCUR                        7
#define DTKS_SIMILAR                       8

/* NORMAL CODES from dtkm_check_bumpability()  */
#define DTKM_CAN_BUMP_DTKS                                1
#define DTKM_CANNOT_BUMP_DTKS                             2

/* NORMAL CODES from dtkm_compare_best_antenna()  */
#define DTKM_NEW_BEST_ANTENNA                             1
#define DTKM_NO_NEW_BEST_ANTENNA                          2

/* NORMAL CODES from dtkm_process_dtk():  */
#define DTKM_DTK_DELETED_OK                                      1 
#define DTKM_DTK_PROPOSAL_REJ                                    2 
#define DTKM_DTK_PROPOSAL_CONFLICT                               3 
#define DTKM_DTK_ACCEPTED                                        4 
#define DTKM_DTK_ACCEPTED_INHERITANCE                            5 

/* NORMAL CODES from dtkm_find_antenna_for_dtk():  */
#define DTKM_ANTENNA_FOUND                                       1 
#define DTKM_NO_AVAILABLE_ANTENNA                                2 

#endif      /*  _DTKM_UTILITIES_H_    */

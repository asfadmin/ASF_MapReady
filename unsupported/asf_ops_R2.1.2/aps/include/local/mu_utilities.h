#ifndef _MU_UTILITIES_H_
#define _MU_UTILITIES_H_

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   mu_utilities.h
Description:    
Creator:    Larry Stevens
Notes:      
==============================================================================*/
#pragma ident	"@(#)mu_utilities.h	5.3 98/03/10 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.mu_utilities.h"

#include <db_sybint.h>

/*
--  Contents:
--      Global Constants
--      Extern Global Declarations
--      Function prototypes
--      Error codes
*/



/*
--  Global Constants for libmu.a
*/
/* Empty (ie, placeholder) parameters; Note: NULL is used for "char *" */
#define MU_EMPTY_INT_PARAM      0

/****************************************************************
*                                                               *
*  List of valid Multi-User Station Id values; use them when    *
*  calling the public routines.                                 *
*                                                               *
****************************************************************/
#define MU_ASF_STATIONID        "ASF"   /* ASF station id */
#define MU_MCM_STATIONID        "MCM"   /* MCM station id */
#define MU_ALL_STATIONID        "ALL"   /* all/any station id(s) */

/****************************************************************
*                                                               *
*  List of valid Multi-User Activity types; use them when       *
*  calling the public routines.                                 *
*                                                               *
*  NOTE: for any new, deleted, or modified definition names     *
*  mu_validate_activity_id.c may need to be changed also.       *
*                                                               *
****************************************************************/
#define MU_DAR_ACTIVITY_TYPE            "DAR"
#define MU_SINGLE_ACTIVITY_TYPE         "single"
#define MU_PLANNING_ACTIVITY_TYPE       "planning"



/****************************************************************
*                                                               *
*          DAR activities                                       *
*                                                               *
*  List of valid DAR mu_activity_id values;                     *
*  use them when calling the public routines:                   *
*                                                               *
*  NOTE: for any new, deleted, or modified definition names     *
*  mu_validate_activity_id.c may need to be changed also.       *
*                                                               *
****************************************************************/
#define MU_CREATE_SITE_COV_FOR_DAR                "CreateSiteCovForDAR"
#define MU_SELECT_SITE_COV_FOR_DAR                "SelectSiteCovForDAR"
#define MU_EDIT_OR_DELETE_DAR                     "EditOrDeleteDAR"
#define MU_CREATE_DATATAKES_FROM_DAR              "CreateDataTakesFrmDAR"
#define MU_DELETE_DAR                             "DeleteDAR"
 


/****************************************************************
*                                                               *
*          single activities                                    *
*                                                               *
*  NOTE: for any new, deleted, or modified definition names     *
*  mu_validate_activity_id.c may need to be changed also.       *
*                                                               *
****************************************************************/
/*
-- List of valid SINGLE mu_activity_id values;
-- use them when calling the public routines
-- except when it is more convenient to insert
-- a satellite value in the middle of a string
-- when forming the mu_activity_id value, rather
-- that using a case statement vs satellite value.
*/
    /* create files  */
#define MU_AWOS     "AWOS_ADDM" /* AWOS & ADDM are created at the same time.*/
#define MU_MWOS     "MWOS_MDDM" /* MWOS & MDDM are created at the same time.*/
#define MU_AREQ     "AREQ"
#define MU_ADDM     "AWOS_ADDM" /* AWOS & ADDM are created at the same time.*/
#define MU_MDDM     "MWOS_MDDM" /* MWOS & MDDM are created at the same time.*/
#define MU_AE1E     "AE1E"
#define MU_AE2E     "AE2E"
#define MU_AJ1E     "AJ1E"
#define MU_AA1E     "AA1E"
#define MU_AR1E     "AR1E"
#define MU_ME1E     "ME1E"
#define MU_ME2E     "ME2E"
#define MU_MR1E     "MR1E"
#define MU_REQW     "REQW"
#define MU_REQQ     "REQQ"
#define MU_REUG     "StationDownTimes"     /* for ESA     */
#define MU_CRAR     "StationDownTimes"     /* for CSA     */
#define MU_MSGF     "StationDownTimes"     /* for NASDA   */
 
    /* process files  */
#define MU_ARES       "ARES"
#define MU_MPSG       "MPSG"
#define MU_SHAQ       "SHAQ"
#define MU_SHAQP      "SHAQP"
#define MU_REQR_STGS  "REQR_STGS"
#define MU_OPL1       "OPL1"
#define MU_REQM_msge  "REQM_msge"
#define MU_REQA       "REQA"
#define MU_MSGN       "MSGN"
#define MU_OPLN       "OPLN"
#define MU_CRRA       "CRRA"
#define MU_CRSA       "CRSA"
#define MU_CRRM       "CRRM"
#define MU_CRSM       "CRSM"
 
#define MU_DAR_STATISTICS          "DAR_Statistics"
#define MU_FRAME_GENERATION        "FrameGeneration"
#define MU_APS_STATISTICS          "APS_Statistics"
 
/*
-- To create a valid create nominal orbit mu_activity_id, you
-- can use the following #define macro by concatenating
-- Satellite, Phase same, and MU_CREATE_NOMINAL_ORBIT into
-- a single string to indicate mu_activity_type:
--
-- Coding example:
--    strcpy( mu_activity_type, sat ) ;
--    strcat( mu_activity_type, phase_name ) ;
--    strcat( mu_activity_type, MU_CREATE_NOMINAL_ORBIT ) ;
--
-- The result will look like this:  "E1ACreateNominalOrbit"
*/
#define MU_CREATE_NOMINAL_ORBIT    "CreateNominalOrbit"
 
#define MU_STATION_DOWN_TIMES           "StationDownTimes"
 


/****************************************************************
*                                                               *
*          planning activities                                  *
*                                                               *
*  NOTE: for any new, deleted, or modified definition names     *
*  mu_validate_activity_id.c may need to be changed also.       *
*                                                               *
****************************************************************/
/*
-- List of valid PLANNING mu_activity_id values;
-- use them when calling the public routines:
*/
#define MU_CREATE_NOMINAL_COVERAGE             "CreateNominlCoverage"
#define MU_CREATE_NOMINAL_MASKINOUT            "CreateNominlMaskInOut"
#define MU_CREATE_EDIT_OR_DELETE_DTK           "CreateEditOrDeleteDTK"
 
/*
-- these planning activities are already defined
-- in the single activities above:
*/
    /* create files  */
/*  #define MU_AWOS     "AWOS_ADDM"        */
/*  #define MU_MWOS     "MWOS_MDDM"        */
/*  #define MU_AREQ     "AREQ"        */
/*  #define MU_ADDM     "AWOS_ADDM"        */
/*  #define MU_MDDM     "MWOS_MDDM"        */
/*  #define MU_REQW     "REQW"        */
/*  #define MU_REQQ     "REQQ"        */
    /* process files  */
/*  #define MU_ARES                                "ARES"        */
/*  #define MU_MPSG                                "MPSG"        */
/*  #define MU_SHAQ                                "SHAQ"        */
/*  #define MU_SHAQP                               "SHAQP"       */
/*  #define MU_REQR_STGS                           "REQR_STGS"   */
/*  #define MU_OPL1                                "OPL1"        */
/*  #define MU_REQM_msge                           "REQM_msge"   */
/*  #define MU_REQA                                "REQA"        */
/*  #define MU_OPLN                                "OPLN"        */
/*  #define MU_CRRA                                "CRRA"        */
/*  #define MU_CRSA                                "CRSA"        */
/*  #define MU_CRRM                                "CRRM"        */
/*  #define MU_CRSM                                "CRSM"        */
 
/*
-- this planning activity is already defined above in the DAR list:
*/
/*  #define MU_CREATE_DATATAKES_FROM_DAR           "CreateDataTakesFrmDAR" */
 
#define MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES   "CrtEditOrDelAntDnTime"
#define MU_CON_ROUNDUP                         "CON_Roundup"
/****************************************************************
*                                                               *
*          End of planning activities                           *
*                                                               *
****************************************************************/
 


/****************************************************************
*                                                               *
*   "PUBLIC" FUNCTION PROTOTYPES for libmu.a                    *
*                                                               *
****************************************************************/
/* 
-- gets/validates permission for 
-- executable, does retry logic.  
-- returns permission_id if granted or validated
-- returns -1 if no permission granted or validated
*/
int mu_get_permission(
    char        *progname,           /*  name of executable, for syslog()    */
    DBPROCESS   *APS_dbproc,         /*  Sybase process pointer              */
    int         permission_id,       /*  if != 0, verify this permission.    */
    char        *mu_activity_type,   /*  Multi-user activity type.           */
    char        *mu_activity_id,     /*  Multi-user activity id.             */

                                     /*  use NULL or 0 value of not used:    */
    char        *strttime,           /*  start time of planning activity     */
    char        *stoptime,           /*  end time of planning activity       */
    char        *station_id,         /*  station id of planning activity.    */
    int         darid,               /*  DAR id of dar activity.             */
    int         n_retries,           /*  retry logic                         */
    int         n_seconds_retry ) ;  /*  retry logic                         */

/*
-- mu_permission_retrieve() returns the count of all 3 lists of
-- active permissions or 0 if none.
-- It returns a negative number if there is an error.  You can print the
-- error:  printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
*/
int mu_permission_retrieve(
    DBPROCESS   *dbproc,       /*  input sybase session pointer              */
    llist       *planning_activity_list,
                                /* output linked list for planning activity
                                   permissions. filled only if valid planning
                                   permissions exist.
                                   calling routine must allocate the list
                                   and it must have no members at input.    */
 
    llist       *dar_activity_list,
                                /* output linked list for DAR activity
                                   permissions. filled only if valid DAR
                                   permissions exist.
 
                                   calling routine must allocate the list
                                   and it must have no members at input.    */
 
    llist       *single_activity_list ) ;
                                /* output linked list for single activity
                                   permissions. filled only if valid single
                                   permissions exist.
                                   calling routine must allocate the list
                                   and it must have no members at input.    */
 
/*
-- mu_permission_request() returns the new permission id or 0 if none.
-- It returns a negative number if there is an error.  You can print the
-- error:  printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
*/
int mu_permission_request(
    DBPROCESS   *APS_dbproc,   /*  input sybase session pointer              */
    int         permission_id, /*  input permission id, usually = 0          */
    char        *mu_activity_id,/*  input activity for which permission is
                                   requested.                                */
    char        *activity_type,/*  input activity type for which permission is
                                   requested.  values = "planning", "DAR",
                                   or "single"                               */
    llist       *blocking_permission_list,
                                /* output linked list for blocking permissions.
                                   filled only if permission is denied.  The
                                   calling routine must allocate the list
                                   and it must have no members at input.     */
 
    /* Other parameters.   use NULL, "", or a 0 value if not used.  */
    char        *strttime,  /* for planning activities, input start time ASF  */
    char        *stoptime,  /* for planning activities, input stop time ASF   */
    char        *station_id,/* for planning activities, input station ID, "ASF",
                                  "MCM", or "ALL"                            */
    int         darid ) ;   /* for DAR activities, the darid of the DAR.     */
 
int mu_permission_terminate(
    DBPROCESS   *APS_dbproc,   /*  input sybase session pointer               */
    int         permission_id, /*  input permission id to be terminated       */
    char        *activity_id,  /*  input activity for which permission is
                                   terminated.                                */
    char        *activity_type);/* input activity type for which permission is
                                   terminated.  values = "planning", "DAR",
                                   or "single"                                */
/*
-- mu_permission_validate() returns the validated permission id or 0 if
-- not valid.
-- It returns a negative number if there is an error.  You can print the
-- error:  printf("%s\n", MU_ERROR_MESSAGE(return_code) ) ;
*/
int mu_permission_validate(
    int     permission_id,   /*  input permission id to be validated       */
    char    *activity_id,    /*  input activity for which permission is
                                 validated.                                */
    char    *activity_type );/*  input activity type for which permission is
                                 validated.  values = "planning", "DAR",
                                 or "single"                               */
 

/*
--  START ERROR CODES AND MESSAGES
--
--  These error codes and corresponding messages are
--  implemented here and in mu_error_message.c
--  The code v. message correspondances must be the
--  same here as there.
--
-- NOTE:  these error codes must be < 0.  this is because the
-- calling routines check for the return code < 0 to do
-- error handling.
--
*/
 

/* a macro to decode the error codes into a string:
-- use like this:
--
-- if( return_code < 0 )
--     printf("%s\n", MU_ERROR_MESSAGE( return_code ) ) ;
*/
extern char *mu_error_message[] ;
#define MU_ERROR_MESSAGE( code ) \
    mu_error_message[ -(code) ]
/*****************************************************************************
*                                                                            *
*         You MUST add a line in src/lib_mu/mu_error_message.c               *
*         whenever you add a new error code.                                 *
*                                                                            *
*****************************************************************************/

/* ERROR CODES from mu_ Routines()  */
#define MU_DB_ERROR_DELETING_FROM_ACTIVE_DAR_ACTIVITIES                     -1
#define MU_DB_ERROR_DELETING_FROM_ACTIVE_PLANNING_ACTIVITIES                -2
#define MU_DB_ERROR_DELETING_FROM_ACTIVE_SINGLE_ACTIVITIES                  -3
#define MU_DB_ERROR_DURING_PHASE_TABLE_COUNTING                             -4
#define MU_DB_ERROR_DURING_SATSENSOR_TABLE_COUNTING                         -5
#define MU_DB_ERROR_DURING_SYSPROCESSES_COUNTING                            -6
#define MU_DB_ERROR_DURING_SYSPROCESSES_RETRIEVE                            -7
#define MU_DB_ERROR_INSERTING_DAR_ACTIVITY                                  -8
#define MU_DB_ERROR_INSERTING_PLANNING_ACTIVITY                             -9
#define MU_DB_ERROR_INSERTING_SINGLE_ACTIVITY                              -10
#define MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_DAR_ACTIVITIES_TABLE            -11
#define MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_PLANNING_ACTIVITIES_TABLE       -12
#define MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_SINGLE_ACTIVITIES_TABLE         -13
#define MU_DB_ERROR_RETRIEVING_FROM_PERMISSION_COUNTER_TABLE               -14
#define MU_DB_ERROR_STATION_RELATION_QUERY_FAILED                          -15
#define MU_DB_ERROR_UPDATING_MULTI_USER_WAIT                               -16
#define MU_DB_ERROR_UPDATING_PERM_COUNTER_REC                              -17
#define MU_ERROR_COULD_NOT_FIND_SYSPROCESSES_INFO_FOR_CURRENT_PROCESS      -18
#define MU_ERROR_COULD_NOT_GET_USERID                                      -19
#define MU_ERROR_DATA_CORRUPTION_DURING_VALIDATION                         -20
#define MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_DAR_ACTIVITIES                -21
#define MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_PLANNING_ACTIVITIES           -22
#define MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_SINGLE_ACTIVITIES             -23
#define MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_DAR_ACTIVITIES                 -24
#define MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_PLANNING_ACTIVITIES            -25
#define MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_SINGLE_ACTIVITIES              -26
#define MU_ERROR_DURING_BEGIN_TRANSACTION                                  -27
#define MU_ERROR_DURING_COMMIT_TRANSACTION                                 -28
#define MU_ERROR_DURING_ROLLBACK_TRANSACTION                               -29
#define MU_ERROR_FROM_FUNCTION_tc_time_pad                                 -30
#define MU_ERROR_INPUT_ACTIVITY_ID_IS_LE_3_CHARS_LONG                      -31
#define MU_ERROR_INPUT_ACTIVITY_ID_IS_NULL_PTR                             -32
#define MU_ERROR_INPUT_ACTIVITY_TYPE_HAS_ILLEGAL_VALUE                     -33
#define MU_ERROR_INPUT_ACTIVITY_TYPE_IS_LT_3_CHARS_LONG                    -34
#define MU_ERROR_INPUT_ACTIVITY_TYPE_IS_NULL_PTR                           -35
#define MU_ERROR_INPUT_BLOCKING_PERMISSION_LIST_IS_NOT_EMPTY               -36
#define MU_ERROR_INPUT_BLOCKING_PERMISSION_LIST_IS_NULL_PTR                -37
#define MU_ERROR_INPUT_DARID_IS_LE_ZERO_FOR_DAR_ACTIVITY                   -38
#define MU_ERROR_INPUT_DAR_ACTIVITY_ID_HAS_ILLEGAL_VALUE                   -39
#define MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NOT_EMPTY                      -40
#define MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NULL_PTR                       -41
#define MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT                 -42
#define MU_ERROR_INPUT_DBPROC_IS_NULL                                      -43
#define MU_ERROR_INPUT_HOSTNAME_POINTER_IS_NULL                            -44
#define MU_ERROR_INPUT_HOSTNAME_STRING_ZERO_LENGTH                         -45
#define MU_ERROR_INPUT_HOSTPROCESS_POINTER_IS_NULL                         -46
#define MU_ERROR_INPUT_HOSTPROCESS_STRING_ZERO_LENGTH                      -47
#define MU_ERROR_INPUT_KPID_IS_ZERO                                        -48
#define MU_ERROR_INPUT_NOMINAL_ORBIT_ACTIVITY_ID_HAS_NON_COVERAGEABLE_SAT  -49
#define MU_ERROR_INPUT_NOMINAL_ORBIT_ACTIVITY_ID_HAS_UNKNOWN_SAT_PHASE     -50
#define MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO                     -51
#define MU_ERROR_INPUT_PERMISSION_ID_IS_ZERO                               -52
#define MU_ERROR_INPUT_PLANNING_ACTIVITY_ID_HAS_ILLEGAL_VALUE              -53
#define MU_ERROR_INPUT_PLANNING_ACTIVITY_LIST_IS_NOT_EMPTY                 -54
#define MU_ERROR_INPUT_PLANNING_ACTIVITY_LIST_IS_NULL_PTR                  -55
#define MU_ERROR_INPUT_PROGNAME_POINTER_IS_NULL                            -56
#define MU_ERROR_INPUT_PROGNAME_STRING_ZERO_LENGTH                         -57
#define MU_ERROR_INPUT_SINGLE_ACTIVITY_ID_HAS_ILLEGAL_VALUE                -58
#define MU_ERROR_INPUT_SINGLE_ACTIVITY_LIST_IS_NOT_EMPTY                   -59
#define MU_ERROR_INPUT_SINGLE_ACTIVITY_LIST_IS_NULL_PTR                    -60
#define MU_ERROR_INPUT_STATION_ID_IS_NULL                                  -61
#define MU_ERROR_INPUT_STATION_ID_VALUE_IS_ILLEGAL                         -62
#define MU_ERROR_INPUT_STOPTIME_IS_NOT_A_VALID_ASFTIME                     -63
#define MU_ERROR_INPUT_STOPTIME_IS_NULL_FOR_PLANNING_ACTIVITY              -64
#define MU_ERROR_INPUT_STRTTIME_IS_NOT_A_VALID_ASFTIME                     -65
#define MU_ERROR_INPUT_STRTTIME_IS_NOT_BEFORE_STOPTIME                     -66
#define MU_ERROR_INPUT_STRTTIME_IS_NULL_FOR_PLANNING_ACTIVITY              -67
#define MU_ERROR_IN_MOVING_RECS_TO_LLIST                                   -68
#define MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE                               -69
#define MU_ERROR_MORE_THAN_ONE_MULTI_USER_WAIT_REC_UPDATED                 -70
#define MU_ERROR_MORE_THAN_ONE_PERMISSION_COUNTER_REC_FOUND_IN_TABLE       -71
#define MU_ERROR_NOT_ONLY_ONE_PERM_COUNTER_REC_UPDATED                     -72
#define MU_ERROR_NO_MULTI_USER_WAIT_REC_UPDATED                            -73
#define MU_ERROR_NO_PERMISSION_COUNTER_RECS_FOUND_IN_TABLE                 -74
#define MU_ERROR_PADDED_STOPTIME_IS_NULL_FOR_PLANNING_ACTIVITY             -75
#define MU_ERROR_PADDED_STRTTIME_IS_NULL_FOR_PLANNING_ACTIVITY             -76
#define MU_ERROR_UNLINKING_PERM_RECORD                                     -77
#define MU_ERROR_NO_SYSPROCESSES_PROG_NAME_CHECK_DBOPEN_CALL_2ND_ARGUMENT  -78
 
/* NOTE:  */
/*****************************************************************************
*                                                                            *
*      You MUST add a corresponding line in                                  *
*      src/lib_mu/mu_error_message.c  whenever you add a new error code.     *
*                                                                            *
*****************************************************************************/
 
#endif      /*  _MU_UTILITIES_H_    */

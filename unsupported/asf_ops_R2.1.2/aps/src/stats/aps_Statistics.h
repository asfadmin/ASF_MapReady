#ifndef APS_STATISTICS_H
#define APS_STATISTICS_H

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:       aps_Statistics.h
Description:    
Creator:        Lawrence Stevens
Notes:          
==============================================================================*/
#pragma ident   "@(#)aps_Statistics.h	1.3 98/03/23 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.aps_Statistics.h"

#include <db_sybint.h>    /*  for APS sybase interface routines, DBPROCESS  */
                          /*  some of the IMS includes also use this.       */

#include <stdio.h>        /* for FILE, used by ims_msg.h 
                             but file not included in that h-file.          */
#include <string.h>            /* for strcpy()                      */
/* IMS SETUP FILES:    */
#include <cstypes.h>      /* for CS_SMALLINT, etc. (a sybase type)    */
#include <ims_const.h>    /* for IMS_COL30_LEN, etc.                  */
#include <ims_msg.h>      /* for typedef IMS_MSG_STRUCT               */
#include <ims_archive.h>  /* for typedef IMS_CLNT_EVENT               */

/*  Frequently used .h files in this directory:  */
#include <dtkm_utilities.h>    /* for dtkm_print()                  */
#include <db_stats_calls.h>    /* for CAST_STATS_CALLS_SAT etc.     */
#include <db_dtk.h>            /* for CAST_DTK_SAT etc.             */
#include <timeconv.h>          /* for tc_systime2asf() etc.         */
#include <dapps_defs.h>        /* for ASF_TIME_STR_LENGTH           */
#include <aps_db_table.h>      /* for STATS_CALLS, DTK etc.         */

/*
-- Holds APS stats info required to interface with the IMS.  
*/
typedef struct {
    char username[IMS_NAME_LEN+1] ;
    char password[IMS_NAME_LEN+1] ;
    char accountId[IMS_NAME_LEN+1] ;
    char sourceDir[200] ;
    char programName[IMS_PROGRAM_LEN+1] ;
    char catSrvName[IMS_NAME_LEN+1] ;
    char catDbName[IMS_NAME_LEN+1] ;
    int  ims_call_flag ;
    FILE *logfile_ptr ;   /* not used by IMS, here for convenience of APS.  */
} APS_STATS_IMS_INFO ;


/*
-- Structure to put values in, when making a PMF
-- some fields are optional, and will be "".  
*/
typedef struct {
    char type[4] ;       /* AOS, DLK, MSK, or SAR   */
    char condition[4] ;  /* CAN, RED, SCH, or PLN   */
    char sat[3] ;     
    int  rev ;         
    int  dtkid ;
    char sensor[4] ;         
    int  antenna_id ;
    char activity_id[4] ;         
    char asftime_now[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_aos[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_los[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_aos_fa[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_los_fa[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_mask_entry[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_mask_exit[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_on[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_off[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_on_fa[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_off_fa[ASF_TIME_STR_LENGTH+1] ;         
    char asftime_planned[ASF_TIME_STR_LENGTH+1] ;         
    int  downlink_rev ;
    int  downlink_dtkid ;
} APS_STATS_PMF_VALUES ;


/* application function prototypes  */

int IMS_interface(                          /* interfaces with IMS.      */
    APS_STATS_IMS_INFO      *ims_info,      /* input info, for IMS.      */
    APS_STATS_PMF_VALUES    *pmf_values ) ; /* input info, for IMS.      */

int aps_Statistics(
    DBPROCESS   *APS_dbproc,
    APS_STATS_IMS_INFO *aps_stats_ims_info,
    int         keep_flag,        /* TRUE:  keep all old stats_calls recs */
    char        *today_dtkdate,   /* input.  "yyyy:ddd", use as today.    */
    int         n_lookback_days,  /* no. of days to look back for dtks.   */
    int         *IMS_error_count, /* output count of IMS errors.          */
    int         *IMS_fatal_count);/* output count of IMS fatal erros.     */

void print_ims_event( FILE *fp, IMS_CLNT_EVENT *ims_event ) ;

int stats_call_reported( DB_RECORD **stats_calls_rec ) ;

int stats_compute_aos_los( 
    DB_RECORD   **dtk_rec,
    char        *condition,     /* SCHEDULED, REDUCED, ORIGINAL, or CANCELLED */
    char        *asftime_aos,   /* output AOS time         */
    char        *asftime_los ); /* output LOS time         */

int stats_compute_mask_times( 
    char        *sat,       
    int         rev,  
    char        *asftime_mask_entry,    /* output mask entry time   */
    char        *asftime_mask_exit ) ;  /* output mask exit time    */

int stats_init_pmf_values( APS_STATS_PMF_VALUES *pmf_values ) ;

int stats_report_all_masks( 
    DBPROCESS       *APS_dbproc,
    llist           *dtk_list,      /* downlink list, sorted by sat, rev.  */
    char            *condition,     /* SCHEDULED or CANCELLED              */
    APS_STATS_IMS_INFO   *aps_stats_ims_info,
    int             *call_count ) ;

int stats_report_all_aos_los( 
    DBPROCESS       *APS_dbproc,
    llist           *dtk_list,      /* downlink list, sorted by sat, rev.  */
    char            *condition,
    APS_STATS_IMS_INFO   *aps_stats_ims_info,
    int             *call_count ) ;

int stats_report_all_downlinks( 
    DBPROCESS       *APS_dbproc,
    llist           *dtk_list,      /* downlink list, sorted by sat, rev.  */
    char            *condition,
    APS_STATS_IMS_INFO   *aps_stats_ims_info,
    int             *call_count ) ;

int stats_dtk_is_sch( DB_RECORD **dtk_rec ) ;     /* returns TRUE/FALSE */
int stats_dtk_is_cancelled(DB_RECORD **dtk_rec) ; /* returns TRUE/FALSE */
int stats_dtk_is_reduced( DB_RECORD **dtk_rec ) ; /* returns TRUE/FALSE */

int stats_print_ims_info( APS_STATS_IMS_INFO  *aps_stats_ims_info ) ;

int stats_report_sar_pln( 
    DBPROCESS       *APS_dbproc,
    DB_RECORD       **dtk_rec,
    APS_STATS_IMS_INFO   *aps_stats_ims_info ) ;


/* a MACRO to decode the error code into a string:  */
extern char *stats_error_message[] ;
 
#define STATS_ERROR_MESSAGE( code ) \
    stats_error_message[ -(code) ]

/* ERROR CONDITIONS:   */
#define STATS_ERROR_CONDITION_ARG                                     -1
#define STATS_ERROR_STATS_CALLS_REC_NOT_INSERTED                      -2
#define STATS_ERROR_DB_ERROR_INSERTING_STATS_CALLS_REC                -3
#define STATS_ERROR_GETTING_MASK_TIMES                                -4
#define STATS_ERROR_DB_ERROR_COUNTING_STATS_CALLS_RECS                -5
#define STATS_ERROR_DB_ERROR_IN_APSDB_QUERY                           -6
#define STATS_ERROR_NO_ANTENNA_RECS_FOUND_IN_QUERY                    -7
#define STATS_ERROR_NO_DTK_RECS_FOUND_IN_RETRIEVE                     -8
#define STATS_ERROR_GT_1_ANTENNA_RECS_FOUND_IN_QUERY                  -9
#define STATS_ERROR_NULL_POINTER_IN_ARGUMENT                         -10
#define STATS_ERROR_MALLOC_FAILED                                    -11
#define STATS_ERROR_TRANSLATING_SENSOR_TO_SENSOR_MODE                -12
#define STATS_ERROR_CONVERTING_ASFTIME_TO_ODL_TIME                   -13
#define STATS_ERROR_IN_CONDITION_PMF_VALUE                           -14
#define STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_AOS_LOS                -15
#define STATS_ERROR_IN_PMF_TIMES_IN_REDUCED_AOS_LOS                  -16
#define STATS_ERROR_IN_PMF_TIMES_IN_SCHEDULED_AOS_LOS                -17
#define STATS_ERROR_IN_PMF_DOWNLINK_ACTIVITY_ID                      -18
#define STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_DOWNLINK               -19
#define STATS_ERROR_IN_PMF_TIMES_IN_REDUCED_DOWNLINK                 -20
#define STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_SAR                    -21
#define STATS_ERROR_IN_PMF_TIMES_IN_PLANNED_SAR                      -22
#define STATS_ERROR_UNKNOWN_PMF_TYPE                                 -23
#define STATS_ERROR_COULD_NOT_OPEN_PMF_FILE                          -24
#define STATS_ERROR_IMS_ERROR                                        -25
#define STATS_ERROR_IMS_FATAL                                        -26
#define STATS_ERROR_IMS_UNKNOWN_RETURN_CODE                          -27

#endif  /* APS_STATISTICS_H */

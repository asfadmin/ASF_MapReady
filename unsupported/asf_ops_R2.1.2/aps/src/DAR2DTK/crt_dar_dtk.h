#ifndef CRT_DAR_DTK_H
#define CRT_DAR_DTK_H

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:       crt_dar_dtk.h
Description:    
Creator:        Lawrence Stevens
Notes:          
==============================================================================*/
#pragma ident   "@(#)crt_dar_dtk.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/DAR2DTK/SCCS/s.crt_dar_dtk.h"

#include <db_sybint.h>   /* for DBPROCESS    */

/* use this to create a string from error code:  */
extern char *crt_dar_dtk_errors[] ;
#define CRT_DAR_DTK_ERROR_MESSAGE( code ) \
           crt_dar_dtk_errors[ -(code) ]

int crt_dar_dtk(
    DBPROCESS       *APS_dbproc,
    char            *progname,
    int             permission_id, /* if != 0, DAR & planning permission id. */
    int             n_seconds_retry, /* seconds between Multi-User retries  */
    int             n_retries,       /* no. retries for Multi-User.         */
    int             darid,
    char            *dtkstat,    /* desired status of new dtk: SUB|PLN|SCH  */
    char            planner_quicklook, /* desired planner quicklook: Y | N  */

    char            ASFmaskinout_flag, /* Y means to use maskinout.         */
                                       /* if N: no realtime dtks at ASF.    */

    char            MCMmaskinout_flag, /* Y means to use maskinout.         */
                                       /* if N: no realtime dtks at MCM.    */

    int             max_minutes, /* max time duration allowed for DAR.      */
    FILE            *rptfp  ) ;  /* if != NULL, report file pointer.        */


#define CRT_DAR_DTK_ERROR_BAD_ENDTIME                                 -1
#define CRT_DAR_DTK_ERROR_BAD_STRTTIME                                -2
#define CRT_DAR_DTK_ERROR_DARID_LE_ZERO                               -3
#define CRT_DAR_DTK_ERROR_DAR_NOT_FOUND                               -4
#define CRT_DAR_DTK_ERROR_DAR_PERMISSION_NOT_GRANTED                  -5
#define CRT_DAR_DTK_ERROR_DB_QUERY_ERROR                              -6
#define CRT_DAR_DTK_ERROR_DTKSTAT_NOT_PLN_SUB_OR_SCH                  -7
#define CRT_DAR_DTK_ERROR_GT_ONE_DAR_FOUND                            -8
#define CRT_DAR_DTK_ERROR_IN_PROCESSING_DTK_PROPOSAL                  -9
#define CRT_DAR_DTK_ERROR_PLANNING_PERMISSION_NOT_GRANTED            -10
#define CRT_DAR_DTK_ERROR_SAT_NOT_R1                                 -11
#define CRT_DAR_DTK_ERROR_SENSOR_NOT_USABLE                          -12
#define CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_AMM_PHASE                 -13
#define CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_ANY_PHASE                 -14
#define CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_SAME_REV                  -15
#define CRT_DAR_DTK_ERROR_TIME_DURATION_LE_ZERO                      -16
#define CRT_DAR_DTK_ERROR_TIME_DURATION_TOO_LONG_FOR_AMM             -17
#define CRT_DAR_DTK_ERROR_GETTING_DEFAULT_VALUES                     -18
#define CRT_DAR_DTK_ERROR_FROM_DTKM_CHECK_VALUES                     -19
#define CRT_DAR_DTK_ERROR_MAX_MINUTES_LE_ZERO                        -20
#define CRT_DAR_DTK_ERROR_PLANNER_QUICKLOOK_NOT_Y_OR_N               -21
#define CRT_DAR_DTK_ERROR_MASKINOUT_FLAG_NOT_Y_OR_N                  -22
#define CRT_DAR_DTK_ERROR_DETERMINING_MASK                           -23
#define CRT_DAR_DTK_ERROR_DETERMINING_DELTA_TIME                     -24

#endif  /* CRT_DAR_DTK_H */

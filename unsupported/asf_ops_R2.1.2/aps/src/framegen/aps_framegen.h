#ifndef APS_FRAMEGEN_H
#define APS_FRAMEGEN_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   aps_framegen.h
Description:    Include file for framegen exe.  
Creator:    unknown
Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#pragma ident   "@(#)aps_framegen.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.aps_framegen.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <unistd.h>
 
#include "aps_log_msg.h"  /* for aps_log_msg() stuff and MSG_LEN      */
#include "aps_defs.h"
#include "db_sybint.h"      /* for APS sybase interface routines.  includes
                               dapps_list.h                         */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_dtk.h"         /* for dtk table                        */
#include "db_framegen_calls.h"  /* for framegen_calls table         */
#include "dtkm_utilities.h"  /* for dtkm_print_list() etc...        */
#include "timeconv.h"        /* for tc_validate_asf_datetime() etc...        */

#include "aps_framegen_msg.h"  /* APS_FRAMEGEN messages, in this directory  */

#include <framegen.h>    /* for typedef TimePair  etc.  */

int aps_framegen( 
    char        *progname, 
    FILE        *logfp, 
    DBPROCESS   *APS_dbproc, 
    char        *yesterday_dtkdate ) ;
 
int clean_up_framegen_calls(
    char        *progname,
    FILE        *logfp,                 /* log file pointer.                */
    DBPROCESS   *APS_dbproc,
    char        *today_dtkdate ) ;

int dtk_was_reported(
    char        *progname,
    FILE        *logfp,                 /* log file pointer.                */
    DB_RECORD   **dtk_rec,             /* input data-take.   */
    DB_RECORD   **framegen_calls_rec );/* if NULL, then don't return data */

int call_framegen(
    char        *progname,
    FILE        *logfp,                 /* log file pointer.                */
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_rec,              /* report this dtk                  */
    char        *dtkstat,               /* use this status                  */
    int         time_pairs_count,       /* report this number of time-pairs */
    TimePair    *time_pairs,            /* array of TimePair structures     */
    DB_RECORD   **framegen_calls_rec ); /* write rec values here.           */

int report_dtk(
    char        *progname,
    FILE        *logfp,                 /* log file pointer.                */
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_rec,
    int         *non_fatal_error_count  ) ;

int report_expired_dtks(
    char        *progname,
    FILE        *logfp,                 /* log file pointer.                */
    DBPROCESS   *APS_dbproc,
    char        *today_dtkdate,
    int         *non_fatal_error_count  ) ;

#endif /* APS_FRAMEGEN_H */

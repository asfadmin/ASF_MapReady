#ifndef APSCW_H
#define APSCW_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   aps_coverage_window.h
Description:    
Creator:    Lawrence Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)aps_coverage_window.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_coverage_window/SCCS/s.aps_coverage_window.h"

#include <db_sybint.h>      /* for APS sybase interface defininitions.  */
#include <aps_db_table.h>   /* for APS database tables.                 */
#include <dapps_defs.h>     /* for ASF_TIME_STR_LENGTH                  */
#include <aps_defs.h>       /* for APS_EXIT_ERROR, APS_EXIT_OK          */
#include <aps_log_msg.h>    /* for aps_log_msg() stuff and MSG_LEN      */
#include <apspath.h>        /* for APS_TEMP, APS_LOG, etc.              */
#include <timeconv.h>       /* for tc_systime2asf() etc.                */
#include <string.h>         /* for strncmp() etc.                       */
#include <stdlib.h>         /* for system() etc.                        */
#include <stdio.h>         /* for system() etc.                        */
#include <dtkm_utilities.h> /* for dtkm_sat_has_recorder()              */

void
aps_access() ;

int aps_coverage_window( 
    char        *progname, 
    DBPROCESS   *APS_dbproc,
    char        *sybase_userid,
    char        *sybase_password,
	int			n_retries,
	int			n_seconds_retry,
    char        *asftime,
    int         adays,        /* when coding, be careful about the order.     */
    int         bdays,        /* the integer parameters are in alphabetical   */
    int         dminutes,     /* to help you.                                 */
    int         gdays, 
    int         mdays, 
    int         wdays,
    FILE        *logfp ) ;

#endif  /* APSCW_H  */

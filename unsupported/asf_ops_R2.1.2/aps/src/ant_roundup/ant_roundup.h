#ifndef ANT_ROUNDUP_H
#define ANT_ROUNDUP_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   ant_roundup.h
Description:    
Creator:    Larry Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)ant_roundup.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/ant_roundup/SCCS/s.ant_roundup.h"

#include <stdio.h>          /* for FILE *                           */

#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */

#include "dtkm_utilities.h"  /* for dtkm_process_dtk_proposal_list  */
#include "db_dtk.h"          /* for dtk relation.                   */
#include "db_antenna.h"      /* for the antenna relation            */


#define ANT_ROUNDUP_OK                                    0

/* ERROR CODES:  all must be unique,  -3000 < code < -2000   */
 
#define ANT_ROUNDUP_ERROR_MESSAGE( code ) \
        ant_roundup_error_message[ -(code + 2000) ]

#define ANT_ROUNDUP_ERROR_SETTING_CON_VALUE              -2001
#define ANT_ROUNDUP_ERROR_MOVING_CONS                    -2002
#define ANT_ROUNDUP_ERROR_MOVING_QUES                    -2003
#define ANT_ROUNDUP_ERROR_MOVING_SUBS                    -2004
#define ANT_ROUNDUP_ERROR_MOVING_PLNS                    -2005
#define ANT_ROUNDUP_ERROR_MOVING_SCHS                    -2006
#define ANT_ROUNDUP_ERROR_IN_STATION_ID                  -2007
#define ANT_ROUNDUP_ERROR_IN_STRTTIME                    -2008
#define ANT_ROUNDUP_ERROR_IN_STOPTIME                    -2009
#define ANT_ROUNDUP_ERROR_SETTING_QUE_VALUE              -2010
#define ANT_ROUNDUP_ERROR_SETTING_SUB_VALUE              -2011
#define ANT_ROUNDUP_ERROR_SETTING_PLN_VALUE              -2012
#define ANT_ROUNDUP_ERROR_SETTING_SCH_VALUE              -2013
#define ANT_ROUNDUP_ERROR_INITIALIZING_STOICFILE         -2014
#define ANT_ROUNDUP_ERROR_NULL_REPORT_FILE_POINTER       -2015
#define ANT_ROUNDUP_ERROR_SETTING_ZERO_VALUE             -2016

int ant_roundup(
    DBPROCESS   *APS_dbproc,
    char        *station_id,
    char        *strttime,
    char        *stoptime,
    FILE        *report_fp ) ;

#endif  /* ANT_ROUNDUP_H */

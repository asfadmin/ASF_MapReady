#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	CON_roundup.h
Description:	
Creator:	Larry Stevens
Notes:		
==============================================================================*/
#pragma ident	"@(#)CON_roundup.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/CON_roundup/SCCS/s.CON_roundup.h"

#ifndef CON_ROUNDUP_H
#define CON_ROUNDUP_H

#include <stdio.h>          /* for FILE *                           */

#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */

#include "dtkm_utilities.h"  /* for dtkm_process_dtk_proposal_list  */
#include "db_dtk.h"          /* for dtk relation.                   */
#include "db_antenna.h"      /* for antenna relation.               */


#define CON_ROUNDUP_OK                                    0

/* ERROR CODES:  all must be unique,  -4000 < code < -3000   */
 
#define CON_ROUNDUP_ERROR_MESSAGE( code ) \
        CON_roundup_error_message[ -(code + 3000) ]

#define CON_ROUNDUP_ERROR_MOVING_QUES                    -3001
#define CON_ROUNDUP_ERROR_MOVING_SUBS                    -3002
#define CON_ROUNDUP_ERROR_MOVING_PLNS                    -3003
#define CON_ROUNDUP_ERROR_MOVING_SCHS                    -3004
#define CON_ROUNDUP_ERROR_IN_STATION_ID                  -3005
#define CON_ROUNDUP_ERROR_IN_STRTTIME                    -3006
#define CON_ROUNDUP_ERROR_IN_STOPTIME                    -3007
#define CON_ROUNDUP_ERROR_INITIALIZING_STOICFILE         -3008
#define CON_ROUNDUP_ERROR_NULL_REPORT_FILE_POINTER       -3009

int CON_roundup(
	DBPROCESS	*APS_dbproc,
	char		*station_id,
	char		*strttime,
	char		*stoptime,
	FILE		*report_fp ) ;

#endif	/* CON_ROUNDUP_H */

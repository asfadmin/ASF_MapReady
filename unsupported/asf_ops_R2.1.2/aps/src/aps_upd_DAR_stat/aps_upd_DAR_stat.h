#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:       aps_upd_DAR_stat.h
Description:    
Creator:        Philip Yurchuk
Notes:          
==============================================================================*/
#pragma ident   "@(#)aps_upd_DAR_stat.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_upd_DAR_stat/SCCS/s.aps_upd_DAR_stat.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <unistd.h>
#include <db_sybint.h>   /* for DBPROCESS    */
 
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>

/* use this to create a string from error code:  */
extern char *crt_dar_dtk_errors[] ;
#define CRT_DAR_DTK_ERROR_MESSAGE( code ) \
           crt_dar_dtk_errors[ -(code) ]

#define APS_UPD_DAR_STAT_DAR_NOT_FOUND	       	    	    	     2
#define APS_UPD_DAR_STAT_OK	    	    	    	    	     1
#define APS_UPD_DAR_STAT_ERROR_DB_QUERY_ERROR	    	    	    -1
#define APS_UPD_DAR_STAT_ERROR_PERMISSION_ERROR	    	    	    -2
#define APS_UPD_DAR_STAT_ERROR_PERM_NOT_GRANTED	    	    	    -3
#define APS_UPD_DAR_STAT_ERROR_NO_IMS_MSG	    	    	    -4
#define APS_UPD_DAR_STAT_ERROR_IMS_FAILURE	    	    	    -5

#define APS_IMS_SERVER_ENVVAR	"IMS_SERVER"
#define APS_IMS_DB_ENVVAR		"IMS_DB"
#define APS_IMS_PROG_SUFFIX		"_ims"


IMS_MSG_STRUCT *alloc_imsMsgStruct();

void free_imsMsgStruct( IMS_MSG_STRUCT **imsMsgStruct );

int aps_darQuery( char* dar_status, char* dar_startTime , char* dar_endTime,
	IMS_DAR_LIST *resultDarList, IMS_MSG_STRUCT *msgDesc);
					 
int aps_darStatus( int darid, char *apsStatus, char *apsPlnrCmnt,
	IMS_MSG_STRUCT *msgDesc );

int aps_upd_DAR_stat(
    char	    *progname,
    DBPROCESS       *APS_dbproc,
    char    	    *sat,             
    char            *start_time,     
    char    	    *stop_time,
    char    	    *comment,     /* to be appended to the planner comment */
    int             no_perms ) ;  /* if == 1, no perm checking will be done */


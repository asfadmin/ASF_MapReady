#ifndef APS_IMSDB_H
#define APS_IMSDB_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps_imsDB.h
Description:	Include file for routines that access the IMS db.
Creator:		Quentin Sun
Notes:		
==============================================================================*/
#pragma ident	"@(#)aps_imsDB.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.aps_imsDB.h"
 
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <unistd.h>
 
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>

#define APS_IMS_SERVER_ENVVAR	"IMS_SERVER"
#define APS_IMS_DB_ENVVAR		"IMS_DB"
#define APS_IMS_PROG_SUFFIX		"_ims"


IMS_MSG_STRUCT *alloc_imsMsgStruct();

void free_imsMsgStruct( IMS_MSG_STRUCT **imsMsgStruct );

int aps_darQuery( char* dar_status, char* dar_startTime , char* dar_endTime,
	IMS_DAR_LIST *resultDarList, IMS_MSG_STRUCT *msgDesc);
					 
int aps_darStatus( int darid, char *apsStatus, char *apsPlnrCmnt,
	IMS_MSG_STRUCT *msgDesc );

#endif	/* APS_IMSDB_H */

#ifndef APS_DARSTATISTICS_H
#define APS_DARSTATISTICS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   aps_darStatistics.h
Description:    Include file for darstats exe.  
Creator:    unknown
Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#pragma ident   "@(#)aps_darStatistics.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/darstats/SCCS/s.aps_darStatistics.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <unistd.h>
 
 
#include <ims_dbms.h>
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>

#include "aps_log_msg.h"
#include "aps_defs.h"

int write_ims_darStatistics(
    int dar_id,
    short item_id,
    char* dtk_status,
    char* dar_timeStamp,
    int dar_seconds,
    char* ims_username,
    char* ims_password,
    char* progName,
    IMS_MSG_STRUCT *msgDesc) ;

int aps_darStatistics(
    char        *progname,
    char        *ims_username,
    char        *ims_password,
    char        *today_dtkdate ) ;

void error_exit(char *progname) ;

#endif  /* APS_DARSTATISTICS_H */

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_defs.h

Description:    aps-wide "defines" (constants) for aps programs

==============================================================================*/
#pragma ident	"@(#)aps_defs.h	5.2 98/03/03 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.aps_defs.h"


#ifndef APS_DEFS_H
#define APS_DEFS_H

/*  
**	System/Subsystem Names 
*/
#define APS_SUBSYS_NAME     "APS"               /* name of the subsystem */

/*  
**	Environment Variable Names 
*/
#define APSDB_ENVVAR        "APSDB"             /* aps sybase database */
#define APS_USERID_ENVVAR   "APS_SYBASE_USERID" /* aps sybase userid */

#define	APS_MU_INT_ENVVAR	"APS_MU_INTERVAL"	/* mu auto-update default secs*/
#define	APS_MU_T_O_ENVVAR	"APS_MU_TIMEOUT"	/* permission timeout */

#define	APS_EPHEM_XLATE_TBL_ENVVAR	"APS_EPHEM_XLATE_TBL"	/* xlate tbl name */
#define	APS_WOS_XLATE_TBL_ENVVAR	"APS_WOS_XLATE_TBL"		/* xlate tbl name */

#define IMS_DB_ENVVAR       "IMS_DB"            /* IMS sybase database      */
#define IMS_SERVER_ENVVAR   "IMS_SERVER"        /* IMS sybase server name   */
#define IMS_ACCOUNT_ENVVAR  "IMS_ACCOUNT_ID"    /* IMS account id           */

/* 
**	File name suffix defined for IMS archiving
*/
#define PMF_EXTENSION           "M"
#define DATAFILE_EXTENSION      "D"

/*
**	Command name used to send a file out to the external agency via FAIF
*/
#define FAIF_XMITCLIENT_CMD     "faif_xmitClient"

/*  
**	DataBase Sizes/Limits 
*/
#define APS_MAX_ANTENNA_LEN         2               /*max. # digits in antenna*/

/*  
** DAR/Datatake Values 
*/
#define APS_MIN_DTKID               1               /* min. value for dtkid */
#define APS_MAX_DTKID               99              /* max. value for dtkid */
#define APS_NO_ANTENNA              0               /* antenna value for none */
#define APS_DEF_QLOOK               'N'             /* default db value */
#define APS_DEF_QLOOK_STR           "No"            /* default string for gui */

/*
-- Generic return code used by the APS programs 
--
-- NOTE:  these 2 codes must match that in aps_log_msg.inc:  
*/
#define APS_EXIT_OK             0   /* APS program exit code for success */
#define APS_EXIT_ERROR          99  /* APS program exit code for error */

#endif /* APS_DEFS_H */

/******************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
** 
** File:	ims_v0Cat.h
**
** Function:	This is the header file for the ims_v0Cat.c functions which
**		perform catalog queries and updates for the IMS V0 server process.
**
** Author:	Hoshyar Sayah, Julie Wang 
**
** Date:    April 28, 1994	
**
******************************************************************************/

#ifndef _IMS_V0CAT_H
#define _IMS_V0CAT_H

static char *sccsv0Cat = "@(#)ims_v0Cat.h	5.6  04/16/97";

#define V0_SERVER ""
#define V0_DBNAME ""
#define V0_PROGRAM "v0_server"


/*
** Internal CDB file descriptor.  Contains information on files, most
** of which comes from the catalog.
*/

typedef struct v0CatUserSpec 
{
	char *dbUserName;
	char *dbPassword;
	char *program;
	char *server;
	char *dbName;
} V0_CAT_USERSPEC;   

/*
** Following is the request structure passed  to the catalog function, 
** ims_v0Cat.  It contains only the information pertinent to the 
** current request.
*/

typedef struct v0CatStruct
{
	IMS_MSG_STRUCT *msgDesc;  /* msg facility descriptor */
	IMS_QI_DESC_OBJ *qDesc;   /* query descriptor, ims_qi */
	V0_CAT_USERSPEC userSpec;	/* database login information. */
	void *item[10];           /* Array of pointers for input & output */
} V0_CAT_STRUCT;

/*
** The following enum definitions define ims_v0Cat events.
*/
typedef enum v0CatEvent 
{V0_USEDATABASE, 
V0_OPENCONNECTION, V0_CLOSECONNECTION, 
V0_BEGINTRANSACTION, V0_ROLLBACKTRANSACTION, V0_COMMITTRANSACTION, 
V0_EXECDATASETSEARCH, V0_EXECGRANULESEARCH, V0_EXECCAMPAIGNSEARCH, 
V0_GETKEYWORDTYPE, V0_GETDETAILKEYWORD, 
V0_GETACCTLIST, V0_GETCONTACTINFO, V0_GETPROCINFO, 
V0_GETV0ORDERLOCK, V0_GETACCOUNTLOCK,
V0_GETDSINFO, V0_GETPROCMEDIAINFO, V0_GETACCTINFO, V0_GETDATASETINFO,
V0_VERIFYUSER, V0_VERIFYVAL, V0_VERIFYENTRYCHAR,
V0_GETLINEITEMCOST, V0_GETGNULINFO, V0_GETPROFILEINFO,
V0_GETDATATAKE, V0_GETDOWNLINK,
V0_GETNEWID, V0_GETSTR, V0_GETSTR2, V0_GETSTR3, V0_GETINT, V0_GETTINYINT,
V0_GETSMALLINT,
V0_INSERTROW 
} V0_CAT_EVENT;

int v0_cat (V0_CAT_STRUCT *, V0_CAT_EVENT);

#endif	/* _IMS_V0CAT_H */

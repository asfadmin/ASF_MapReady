/******************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** File:	ims_stepCat.h
**
** Function:	This is the header file for ims_stepCat.c functions which
**		perform catalog queries and updates for the ims_step process. 
**
** Author:	Julie Wang 
**
** Date:    Aug 3, 1995	
**
******************************************************************************/

#ifndef IMS_STEPCAT_H
#define IMS_STEPCAT_H

static char *sccsstepCat = "@(#)ims_stepCat.h	5.4  01/14/97";

#define STEP_SERVER ""
#define STEP_DBNAME ""
#define STEP_PROGRAM "ims_step"

typedef enum stepCatEvent 
{
	STEP_GETITEMINFO,
	STEP_GETNEXTSTEP,
	STEP_GETPHOTOTYPE,
	STEP_GETPPSBASIC,
	STEP_GETGNULTBL,
	STEP_GETRFCPROC,
	STEP_GETRFCGNUL,
	STEP_GETTSRGNUL,
	STEP_GETSTR,
	STEP_GETSTR3,
	STEP_GETSTR4,
	STEP_GETSMALLINT,
	STEP_GETCURRITEMINFO,
	STEP_GETITEMCOST

} STEP_CAT_EVENT;

int step_cat (V0_CAT_STRUCT *, STEP_CAT_EVENT);

#endif	/* !IMS_STEPCAT_H */

/******************************************************************************
**
** File:	ims_dispCat.h
**
** Function:	This is the header file for the ims_dispCat.c functions which
**		perform catalog queries and updates for the ims_disp process. 
**
** Author:	Julie Wang 
**
** Date:    June 13, 1995	
**
******************************************************************************/

#ifndef IMS_DISPCAT_H
#define IMS_DISPCAT_H

static char *sccsdispCat = "@(#)ims_dispCat.h	5.2  10 Apr 1996";

#define DISP_SERVER ""
#define DISP_DBNAME ""
#define DISP_PROGRAM "ims_disp"

typedef enum dispCatEvent 
{ DISP_GETITEMLIST,
	DISP_GETNEWSV,
	DISP_RECHECKSTATUS,
	DISP_GETORDERLOCK
} DISP_CAT_EVENT;

int disp_cat (V0_CAT_STRUCT *, DISP_CAT_EVENT);

#endif	/* !IMS_DISPCAT_H */

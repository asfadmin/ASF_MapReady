/*****************************************************************************
*
**
** File:    ims_fa_tracking.h
**
** Function: Include file for FA Tracking mechanism
**
** Author: Dan Crichton
**
** Date:    2/19/96
**
**
*****************************************************************************/

#ifndef IMS_FA_TRACKING_H

#define IMS_FA_TRACKING_H

static char *sccsFATracking = "@(#)ims_fa_track.h	5.3 05/13/97";



/*
** Structure to the tracking database.
*/
 
typedef struct IMS_FA_TRACKING
{
	 int orbit_id;
	 int message_id;
	 char platform[IMS_COL15_LEN+1];
	 char start_time[IMS_DATETIME_LEN+1];
	 char end_time[IMS_DATETIME_LEN+1];
	 int segment_id;
	 char plan_num[IMS_COL15_LEN+1];
	 int report_id;
	 char name[32];
	 char station_id[IMS_COL15_LEN+1];
	 char received_date[IMS_DATETIME_LEN+1];
	 struct IMS_FA_TRACKING *next;
} IMS_FA_TRACKING;



#endif

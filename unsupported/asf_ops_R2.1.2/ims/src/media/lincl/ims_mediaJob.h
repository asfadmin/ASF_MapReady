/*
** File:  ims_mediaJob.h
*/

#ifndef IMS_MEDIAJOB_H
#define IMS_MEDIAJOB_H

static char *sccsMediaJob = "@(#)ims_mediaJob.h	6.1  03/06/98";

typedef struct ims_media_params 
{
	IMS_MSG_STRUCT *msgDesc;
	struct
	{
		char username[IMS_NAME_LEN+1];
		char password[IMS_NAME_LEN+1];
		char program[IMS_PROGRAM_LEN+1];
		char server[IMS_NAME_LEN+1];
		char database[IMS_NAME_LEN+1];
	} catSpec;

	DEVICE_INFO deviceInfo;
	DBSMALLINT media_type;
	DBSMALLINT media_fmt_type;
	DBINT order_id;
	char mediaId[IMS_COL15_LEN+1];
	int mediaItemCount;
	struct
	{
		DBSMALLINT item_id;
		DBSMALLINT status;
	} mediaItemArray[IMS_COL1024_LEN+1];

} IMS_MEDIA_PARAMS;

#endif  /* !IMS_MEDIAJOB_H */

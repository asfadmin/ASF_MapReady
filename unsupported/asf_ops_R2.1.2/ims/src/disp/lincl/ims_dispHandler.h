/************************************************************************* 
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** File :  ims_dispHandler.h 
**
** Function: This is the header file for the IMS V0 server application.
**
** Creator: Julie Wang
**
** Date:    June 13, 1995 
**
*************************************************************************/

#ifndef _IMS_DISPHANDLER_H
#define _IMS_DISPHANDLER_H 

static char *sccsdispHandler = "@(#)ims_dispHandler.h	5.3  25 Jul 1996";
 
/*
typedef struct disp_value_list
{
	DBINT        int_value;
	DBSMALLINT   smallint_value;
	struct disp_value_list *next_p;
} DISP_VALUE_LIST;
*/

typedef struct disp_order_list
{
	DBINT         order_id;
	DBSMALLINT    item_id;
	DBSMALLINT    initial_status;
	struct disp_order_list  *next_p;
} DISP_ORDER_LIST;

typedef struct disp_sv_list
{
	DBSMALLINT   dataset_idx;
	DBINT        granule_idx;
	DBCHAR       platform[IMS_COL15_LEN];
	DBINT        start_rev;
	DBINT        end_rev;
	DBCHAR       start_time[IMS_COL30_LEN];
	DBCHAR       end_time[IMS_COL30_LEN];
	DBCHAR       sv_precision[IMS_COL15_LEN];
	struct disp_sv_list  *next_p;
} DISP_SV_LIST;

typedef struct disp_full_list
{
	long         order_id;
	short        item_id;
	int          initial_status;
	int          dataset_idx;
	int          granule_idx;
	char         platform[IMS_COL15_LEN];
	int          start_rev;
	int          end_rev;
	char         start_time[IMS_COL30_LEN];
	char         end_time[IMS_COL30_LEN];
	char         sv_precision[IMS_COL15_LEN];
	int          first_item_flag;
	struct disp_full_list  *next_p;
} DISP_FULL_LIST;

/*
** Descriptor structure; having access to all
** globally referenced data items.
*/

typedef struct disp_desc_struct
{
	IMS_MSG_STRUCT  *msgDesc;
	DISP_ORDER_LIST *o_list;
	DISP_SV_LIST    *sv_list;
	DISP_FULL_LIST *disp_list;
	V0_CAT_STRUCT catReq;
} DISP_DESC_STRUCT;


/*
** Function declarations
*/

/* ims_dispHandler */

int ims_dispHandler (IMS_MSG_STRUCT *, char *, char *);


#endif /* !_IMS_DISPHANDLER_H */

/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	fill_order.c

Description:
	This module contains the function used for creating IMS status messages

External Functions: None
	
Static Functions: None
	
External Variables Defined: None
	
File Scope Static Variables: None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)fill_order.c	1.2    12/16/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "PPSdefs.h"
#include "PPShdr.h"
#include "PPSextern.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

/*==============================================================================
Function:	fill_scanorder_status

Description:	
	fill status message to send to IMS

Parameters: 	
	scan request
	status record
	status type
	status id
Returns:	
	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  	fill_scanorder_status(IMS_ScanReq_Record *rec, 
		IMS_Order_Status *status_rec,
		char		*status_type,
		char		*status_id,
		char		*dataset,
		char		*product_filename)
#else
void  	fill_scanorder_status(rec,status_rec,status_type,status_id,dataset,product_filename);
 		IMS_ScanReq_Record 	*rec;
		IMS_Order_Status 	*status_rec;
		char			*status_type;
		char			*status_id;
		char			*dataset;
		char			*product_filename;
#endif
{
	sprintf(status_rec->order_id, "%d", rec->order_id);
	sprintf(status_rec->item_id, "%d", rec->item_id);
	strcpy(status_rec->platform, rec->platform);
	strcpy(status_rec->sensor, rec->sensor);
	strcpy(status_rec->status_type, status_type);
	strcpy(status_rec->status_id, status_id);
	strncpy(status_rec->comment, rec->comment, MAXLINE);
	/* optional fields - set if status_type is FINAL */
	if (dataset)
		strcpy(status_rec->dataset, dataset);
	else
		status_rec->dataset[0] = '\0';
	if (product_filename)
		strcpy(status_rec->product_filename, product_filename);
	else
		status_rec->product_filename[0] = '\0';
	
} /* fill_scanorder_status */




/*==============================================================================
Function:	fill_L1order_status

Description:	
	fill status message to send to IMS

Parameters: 	
	L1pr request
	status record
	status type
	status id
Returns:	
	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  	fill_L1order_status(IMS_L1PReq_Record *rec, 
		IMS_Order_Status *status_rec,
		char		*status_type,
		char		*status_id,
		char		*dataset,
		char		*product_filename)
#else
void  	fill_L1order_status(rec,status_rec,status_type,status_id,dataset,product_filename);
 		IMS_L1PReq_Record 	*rec;
		IMS_Order_Status 	*status_rec;
		char			*status_type;
		char			*status_id;
		char			*dataset;
		char			*product_filename;
#endif
{
	sprintf(status_rec->order_id, "%d", rec->order_id);
	sprintf(status_rec->item_id, "%d", rec->item_id);
	strcpy(status_rec->platform, rec->platform);
	strcpy(status_rec->sensor, rec->sensor);
	strcpy(status_rec->status_type, status_type);
	strcpy(status_rec->status_id, status_id);
	strncpy(status_rec->comment, rec->comment, MAXLINE);
	/* optional field - set if status_type is FINAL */
	if (dataset)
		strcpy(status_rec->dataset, dataset);
	else
		status_rec->dataset[0] = '\0';
	if (product_filename)
		strcpy(status_rec->product_filename, product_filename);
	else
		status_rec->product_filename[0] = '\0';
	
} /* fill_L1order_status */

/* End of File */


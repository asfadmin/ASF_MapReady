/******************************************************************************
**
** File:        ims_dispDef.h
**
** Function:    IMS Order Dispatcher related enumerated types denition. 
**
** Author:      J. Wang 
**
** Date:        6/16/95
**
******************************************************************************/

#ifndef _IMS_DISPDEF_H
#define _IMS_DISPDEF_H

static char *sccsdispDef = "@(#)ims_dispDef.h	5.2  10 Apr 1996";

/*
** Enumerated types for statuses on 'order_item' table
*/
typedef enum ims_order_item_status
{
	IMS_NEW_ITEM         =  1,
	IMS_VALIDATED_ITEM   =  2,
	IMS_IN_PROCESS_ITEM  =  3,
	IMS_ON_LINE_ITEM     =  4,
	IMS_IN_FILM_ITEM     =  5,
	IMS_ON_FILM_ITEM     =  6,
	IMS_IN_PHOTO_ITEM    =  7,
	IMS_IN_MEDIA_ITEM    =  8,
	IMS_GENERATED_ITEM   =  9,
	IMS_COMPLETE_ITEM    = 10,
	IMS_CANCEL_ITEM      = 11,
	IMS_CANCELLED_ITEM   = 12,
	IMS_ERROR_ITEM       = 13,
	IMS_FAILED_ITEM      = 14,
	IMS_HOLD_ITEM        = 15,
	IMS_LOCKED_ITEM      = 20 
}IMS_ORDER_ITEM_STATUS;

/*
** Enumerated types for process_statuses on 'order_item' table
*/
typedef enum ims_order_item_process_status
{
	IMS_P_PENDING                  = 1,
	IMS_P_READY                    = 2,
	IMS_P_AVAILABLE                = 3,
	IMS_P_SUBMITTED                = 4,
	IMS_P_COMPLETED                = 5,
	IMS_P_CANCEL                   = 6
}IMS_ORDER_ITEM_PROCESS_STATUS;

/*
** Enumerated types for status on 'sv_available' table
*/
typedef enum ims_sv_status
{
	IMS_SV_NEW                     = 1,
	IMS_SV_COMPLETE                = 2
} IMS_SV_STATUS;
#endif /* !_IMS_DISPDEF_H */

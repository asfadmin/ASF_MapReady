/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	createIMSmsg.c

Description:	Contains the functions for building the IMS ODL msgs

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)createIMSmsg.c	1.3    04/23/97";

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "odldef.h"
#include "odlinter.h"
#include "ODLcommonhdr.h"
#include "PPSdefs.h"
#include "PPSextern.h"



/*==============================================================================
Function:	create_ODL_common_hdr
Description:	Creates an ODL common header aggregate
Parameters:     ODL_Common_Header *record - C structure containing the values
                                            of the fields in the common header
		AGGREGATE parent - the parent aggregate, which should be the PMF
		                   aggregate
Returns:	
Creator:	Nadia Adhami (nadia.adhami@jpl.nasa.gov)
Creation Date:	8/15/95
Notes:		
==============================================================================*/
void create_ODL_common_hdr(ODL_Common_Header *record, AGGREGATE parent)
{
  AGGREGATE  common_hdr ;       /* common header aggregate */
  PARAMETER  curr_param ;       /* the current parameter   */
  VALUE_DATA curr_value ;       /* the current value       */

  /* create the COMMON_HDR aggregate, and connect it to parent */
  
  common_hdr = NewAggregate(parent, KA_OBJECT, COMMON_HDR_KEYWD, NULL) ;

  /* create the TIME parameter, assign it to common_hdr aggregate, and attach
  -- the file_creation_time value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, TIME_KEYWD) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertDateTime(record->file_creation_time, 
                             sizeof(record->file_creation_time)-1) ;
  NewValue(curr_param, &curr_value) ;
  
  /* create the SOURCE parameter, assign it to common_hdr aggregate, and attach
  -- the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, SOURCE_KEYWD) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(record->file_source, strlen(record->file_source), 2) ;
  NewValue(curr_param, &curr_value) ;
  
  /* create the DESTINATION parameter, assign it to common_hdr aggregate, and 
  -- attach the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, DESTINATION_KEYWD) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(record->file_dest, strlen(record->file_dest), 2) ;
  NewValue(curr_param, &curr_value) ;
 
  /* create the MSG_TYPE parameter, assign it to common_hdr aggregate, and 
  -- attach the file_type value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, MSG_TYPE_KEYWD) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (record->file_type, strlen(record->file_type), 2) ;
  NewValue(curr_param, &curr_value) ;
 
  /* create the NUMBER_OF_RECORDS parameter, 
  -- assign it to common_hdr aggregate, and 
  -- attach the file_type value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, NUM_RECS_KEYWD);
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (record->number_of_records, 
                                 strlen(record->number_of_records), 1) ;
  NewValue(curr_param, &curr_value) ;
 
} /* create_ODL_common_hdr */
 




/*==============================================================================
Function:	create_ODL_order_status_body
Description:	Creates the order status aggregate for sending IMS
Parameters:     IMS_Order_Status *record - C structure containing the values
                                   of the fields in the order status message
		AGGREGATE parent - the parent aggregate, 
Returns:	
Creator:        Nadia Adhami   (Nadia.Adhami@jpl.nasa.ogv)
Creation Date:	8/15/95
Notes:		
==============================================================================*/
void create_ODL_order_status_body(IMS_Order_Status *record, AGGREGATE parent)
{
  AGGREGATE 	body ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
 
  /* create the CATALOG_METADATA aggregate, and connect it to parent */

  body = NewAggregate(parent, KA_OBJECT, BODY_KEYWD, NULL) ;

  if (record->order_id[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, ORDER_ID_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->order_id, 
                                     strlen(record->order_id), 1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->item_id[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, ITEM_ID_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->item_id, 
                                     strlen(record->item_id), 1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->platform[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, PLATFORM_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->platform, strlen(record->platform), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->status_type[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, STATUS_TYPE_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->status_type, 
		strlen(record->status_type), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->status_id[0])
    {
      /*------------------------------------------------*/
      /* if status is CANCEL/FAIL, send FAILED (kludge) */
      /*------------------------------------------------*/
      if (strcmp(record->status_id, CANCELFAIL) == 0)
           (void)strcpy(record->status_id, FAILED);
      curr_param = NewParameter(body, KP_ATTRIBUTE, STATUS_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->status_id, 
                                   strlen(record->status_id), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->comment[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, COMMENT_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->comment, strlen(record->comment), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->sensor[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, SENSOR_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertSymbol (record->sensor, strlen(record->sensor), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->dataset[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, DATASET_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertString(record->dataset,
                          strlen(record->dataset));
      NewValue(curr_param, &curr_value) ;
    }

  if (record->product_filename[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, PRODUCT_ID_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertString (record->product_filename, 
		strlen(record->product_filename)) ;
      NewValue(curr_param, &curr_value) ;
    }

} /* create_ODL_order_status_body */





/*==============================================================================
Function:	void get_current_time()
Description:	Get current time	
Parameters:	None
Returns:	Time string containing current time	
Creator:	Norbert Piega
Creation Date:	Thu Jun 29 15:27:57 PDT 1995
Notes:		
==============================================================================*/
void get_current_time( char *s )
{
  time_t now ;
  struct tm local_tm;
  
  now = time(NULL) ;
  (void) localtime_r(&now,&local_tm);
  (void) strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",&local_tm) ;

  return ;
}



 


/*==============================================================================
Function:	create_ODL_order_status
Description:	create ODL tree containing status info
Parameters: 	status record, ODL tree basenode
Returns:	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  	create_ODL_order_status(
	IMS_Order_Status        *status_rec,
	AGGREGATE		*base_node)
#else
void  	create_ODL_order_status(status_rec, base_node)
	IMS_Order_Status        *status_rec;
	AGGREGATE		*base_node;
#endif
{
  AGGREGATE 		root ;
  AGGREGATE 		parent ;
  ODL_Common_Header 	*commonrecord ;
  ODL_Common_Header 	commonhdrrec ;
 
  commonrecord  = &commonhdrrec ;
 
  get_current_time(commonrecord->file_creation_time) ;
  (void) strcpy(commonrecord->file_source, PPS_KEYWD) ;
  (void) strcpy(commonrecord->file_dest,   IMS_KEYWD) ;
  (void) strcpy(commonrecord->file_type,   IMS_ORDER_STATUS_KEYWD) ;
  (void) strcpy(commonrecord->number_of_records, "1") ; 

  root   = NewAggregate(NULL, KA_OBJECT, "root", NULL) ;
  parent = NewAggregate(root, KA_OBJECT, IMS_ORDER_STATUS_KEYWD, NULL) ;
 
  create_ODL_common_hdr(commonrecord, parent) ;
  create_ODL_order_status_body(status_rec, parent) ;

  *base_node = root;

} /* create_ODL_order_status */


/*==============================================================================
Function:	create_order_status_buf
Description:	create buffer containing order status info
Parameters: 	order status structure, buffer, size of buffer
Returns:	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		Had to create this function since send_status_to_ims()
		can not contain both ODL and IMS header files.
		This function hides the ODL definition from the calling routine.
==============================================================================*/
#ifdef __STDC__
void  	create_order_status_buf(
	IMS_Order_Status        *status_rec,
	char			*buffer,
	int			sizeof_buffer)
#else
void  	create_order_status_buf(status_rec, buffer, sizeof_buffer)
	IMS_Order_Status        *status_rec;
	char			*buffer;
	int			sizeof_buffer;
#endif
{
        AGGREGATE       root;

	buffer[0] = '\0'; 

        /* create ODL format message */
        create_ODL_order_status(status_rec, &root);

        /* write ODL message tree into a buffer */
        WriteLabelBuf (buffer, root, sizeof_buffer);

        /* free the ODL message tree */
        (void) RemoveAggregate(root);
}

/* End of File */

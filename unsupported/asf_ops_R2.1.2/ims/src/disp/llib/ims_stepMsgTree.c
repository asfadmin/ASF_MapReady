static char *sccs = "@(#)ims_stepMsgTree.c	5.6  12/30/96";

/**************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**   ims_stepMsgTree - functions to build step handling related ODL trees 
**
**   Creator    : Julie Wang 
** 
**   Date       : Oct 16 1995 
**
** History:  
**
**           12/12/96   jwang   Added DATA_DIRECTION for L1 and Scan requests
**                              and COMPENSATION_FLAG for L1 request
**
**           07/24/96   jwang   Consolidated SV avail info into main worklist
**                              structure.
**
**           06/06/96   jwang   Hardcode all output format as CEOS for R1B'
**
**           05/24/96   jwang   PR 776, 844, 858
**
**           03/05/96   jwang   changed keyword SITENAME to SITE_NAME
**
**           02/27/96   jwang   R1Bprime preliminary.
**
***************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <string.h>

#include <odldef.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>
#include <ims_disp.h>
#include <ims_timeConv.h>

/**********************************************************************
**
** ims_step_buildRFCTree - build an ODL tree for an RPF/FPR/COR request
**
** return IMS_OK    
**        IMS_FATAL 
**********************************************************************/

int ims_step_buildRFCTree (IMS_MSG_STRUCT *msgDesc, 
                           STEP_INFO      *step, 
                           AGGREGATE      TxTree)
{

	AGGREGATE    pTree;          /* FRAME_ORDER        */
	AGGREGATE    cTree;          /* COMMON_HEADER      */
	AGGREGATE    bTree;          /* BODY               */
	AGGREGATE    sTree;          /* SOURCE_MEDIA       */
	PARAMETER    parameter;      /* temp pointer to parameter nodes */
	VALUE_DATA   value;          /* pointer to values  */
	char         msgbuf[IMS_COL255_LEN+1]; /* temp bufffer for strings */
	PPS_REQ_STRUCT *pps_req;

	/* initialization */
	pTree       = (AGGREGATE)NULL;
	cTree       = (AGGREGATE)NULL;
	bTree       = (AGGREGATE)NULL;
	sTree       = (AGGREGATE)NULL;
	parameter   = (PARAMETER)NULL;
	pps_req     = &(step->pps_req);

	/* 
	** create a new node under the root for FRAME_ORDER object
	*/
	if ( (pTree = NewAggregate (TxTree, KA_OBJECT, "FRAME_ORDER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create FRAME_ORDER object");
		
		return (IMS_ERROR); 
	}

	/*
	** create new nodes under FRAME_ORDER object, 
	** i.e. COMMON_HEADER and BODY
	*/
	if ( (cTree = NewAggregate (pTree, KA_OBJECT, "COMMON_HEADER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create COMMON_HEADER object");
		return (IMS_ERROR);
	}

	if ( (bTree = NewAggregate (pTree, KA_OBJECT, "BODY", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create BODY object");
		return (IMS_ERROR);
	}

	/*
	** Fill in values to fields in COMMON_HEADER group
	*/


	/* TIME */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertDateTime (step->curr_time, strlen(step->curr_time));
	NewValue (parameter, &value);

	/* MSG_TYPE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "MSG_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create MSG_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("FRAME_ORDER", 11);
	NewValue (parameter, &value);

	/* DESTINATION */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "DESTINATION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create DESTINATION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("PPS", 3);
	NewValue (parameter, &value);

	/* SOURCE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "SOURCE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create SOURCE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("IMS", 3);
	NewValue (parameter, &value);

	/* NUMBER_OF_RECORDS */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "NUMBER_OF_RECORDS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create NUMBER_OF_RECORDS.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger ("1", 1);
	NewValue (parameter, &value);

	/*
	** Fill in values to fields in BODY group
	*/

	/* ORDER_TYPE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ORDER_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create ORDER_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (step->order_type, strlen(step->order_type) );
	NewValue (parameter, &value);

	/* ORDER_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ORDER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create ORDER_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->order_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* ITEM_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ITEM_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create ITEM_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->item_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* PRIORITY */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PRIORITY");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create PRIORITY.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (pps_req->char_priority, 
														strlen(pps_req->char_priority) );
	NewValue (parameter, &value);

	/* QUICKLOOK_FLAG */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "QUICKLOOK_FLAG");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create QUICKLOOK_FLAG.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->quicklook_p);
	value = ODLConvertString (pps_req->quicklook_p, 
														strlen(pps_req->quicklook_p) );
	NewValue (parameter, &value);

	/* PLATFORM */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PLATFORM");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create PLATFORM.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->platform);
	value = ODLConvertString (pps_req->platform, strlen(pps_req->platform));
	NewValue (parameter, &value);

	/* SENSOR */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "SENSOR");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create SENSOR.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->sensor);
	value = ODLConvertString (pps_req->sensor, strlen(pps_req->sensor));
	NewValue (parameter, &value);

	/* REVOLUTION */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "REVOLUTION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create REVOLUTION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->rev);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* MODE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "MODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create MODE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->mode);
	value = ODLConvertString (pps_req->mode, strlen(pps_req->mode));
	NewValue (parameter, &value);

	/* SEQUENCE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "SEQUENCE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create SEQUENCE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->sequence);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* PRODUCT_TYPE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PRODUCT_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create PRODUCT_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->product_type);
	value = ODLConvertString (pps_req->product_type, strlen(pps_req->product_type));
	NewValue (parameter, &value);

	/* PIXEL_SPACING */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PIXEL_SPACING");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create PIXEL_SPACING.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%8.2f", pps_req->pixel_spacing);
	value = ODLConvertReal (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* FRAME_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "FRAME_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create FRAME_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->frame_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* FRAME_MODE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "FRAME_MODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create FRAME_MODE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->frame_mode);
	value = ODLConvertString (pps_req->frame_mode, strlen(pps_req->frame_mode));
	NewValue (parameter, &value);

	/* OUTPUT_FORMAT */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "OUTPUT_FORMAT");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create OUTPUT_FORMAT.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	/* for R1Bprime, we hardcode this field to CEOS - jlw */
	value = ODLConvertString("CEOS",4);
	/*
	ims_toUpper (pps_req->output_format);
	value = ODLConvertString(pps_req->output_format,strlen(pps_req->output_format));
	*/
	NewValue (parameter, &value);

	/* ACTIVITY_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ACTIVITY_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create ACTIVITY_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->activity_id);
	value = ODLConvertString (pps_req->activity_id, strlen(pps_req->activity_id));
	NewValue (parameter, &value);

	/* SITE_NAME */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "SITE_NAME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create SITE_NAME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->site_name);
	value = ODLConvertString(pps_req->site_name,strlen(pps_req->site_name));
	NewValue (parameter, &value);

	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PROJECTION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildRFCTree: Failed to create PROJECTION.");
		return (IMS_ERROR);
	}
	
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->projection);
	value = ODLConvertString(pps_req->projection,strlen(pps_req->projection));
	NewValue (parameter, &value);
	
	/* PS_REFERENCE_LAT */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PS_REFERENCE_LAT");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create PS_REFERENCE_LAT.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%10.3f", pps_req->ps_reference_lat);
	value = ODLConvertReal (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* PS_REFERENCE_LON */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PS_REFERENCE_LON");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create PS_REFERENCE_LON.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%10.3f", pps_req->ps_reference_lon);
	value = ODLConvertReal (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* UTM_ZONE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "UTM_ZONE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create UTM_ZONE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->utm_zone);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* LAMBERT_LATITUDE_N */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "LAMBERT_LATITUDE_N");
		if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create LAMBERT_LATITUDE_N.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%10.3f", pps_req->lambert_lat_n);
	value = ODLConvertReal (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* LAMBERT_LATITUDE_S */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "LAMBERT_LATITUDE_S");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create LAMBERT_LATITUDE_S.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%10.3f", pps_req->lambert_lat_s);
	value = ODLConvertReal (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* PROCESSING_GAIN */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PROCESSING_GAIN");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildRFCTree: Failed to create PROCESSING_GAIN.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->processing_gain);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* AVG_TERRAIN_HT */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "AVG_TERRAIN_HT");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildRFCTree: Failed to create AVG_TERRAIN_HT.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%10.3f", pps_req->avg_terrain_ht);
	value = ODLConvertReal (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* DESKEW */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "DESKEW");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create DESKEW.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->deskew_p);
	value = ODLConvertString (pps_req->deskew_p, strlen(pps_req->deskew_p) );
	NewValue (parameter, &value);

	/* COMPENSATION_FLAG */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "COMPENSATION_FLAG");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildRFCTree: Failed to create COMPENSATION_FLAG.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->compensation_p);
	value = ODLConvertString (pps_req->compensation_p, 
														strlen(pps_req->compensation_p) );
	NewValue (parameter, &value);

	/* TERRAIN_CORRECTION */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "TERRAIN_CORRECTION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
   		"ims_step_buildRFCTree: Failed to create TERRAIN_CORRECTION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->terrain_correct_p);
	value = ODLConvertString (pps_req->terrain_correct_p, 
                            strlen(pps_req->terrain_correct_p) );
	NewValue (parameter, &value);

	if (strcmp (pps_req->product_type, "COMPLEX") == 0)
	{
		/* SUBFRAME_ID */
		parameter = NewParameter (bTree, KP_ATTRIBUTE, "SUBFRAME_ID");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
	     		"ims_step_buildRFCTree: Failed to create SUBFRAME_ID.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		msgbuf[0] = '\0';
		(void) sprintf(msgbuf, "%d", pps_req->subframe_id);
		value = ODLConvertInteger (msgbuf, strlen(msgbuf));
		NewValue (parameter, &value);

	}


	/*
	** Fill in values to fields in SOURCE_MEDIA group only if media_id is 
	** not null
	*/

	if (pps_req->media_id[0]  != '\0')
	{
		if ( (sTree = NewAggregate (bTree, KA_OBJECT, "SOURCE_MEDIA", NULL))
			 	== (AGGREGATE)NULL )
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		     	"ims_step_buildRFCTree: Failed to create SOURCE_MEDIA object");
			return (IMS_ERROR);
		}
	
		/* MEDIA_TYPE */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_TYPE");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
	     		"ims_step_buildRFCTree: Failed to create MEDIA_TYPE.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		ims_toUpper (pps_req->media_type);
		value = ODLConvertString (pps_req->media_type,
		                          strlen(pps_req->media_type));
		NewValue (parameter, &value);

		/* MEDIA_ID_TYPE */
		/*
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_ID_TYPE");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		    		"ims_step_buildRFCTree: Failed to create MEDIA_ID_TYPE.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		ims_toUpper (pps_req->media_id_type);
		value = ODLConvertString (pps_req->media_id_type,
		                         strlen(pps_req->media_id_type));
		NewValue (parameter, &value);
		*/
	
		/* MEDIA_ID */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_ID");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		    		"ims_step_buildRFCTree: Failed to create MEDIA_ID.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		ims_toUpper (pps_req->media_id);
		value = ODLConvertString (pps_req->media_id, strlen(pps_req->media_id));
		NewValue (parameter, &value);

		/* MEDIA_LOCATION */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_LOCATION");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		    		"ims_step_buildRFCTree: Failed to create MEDIA_LOCATION.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (pps_req->media_location, 
		                          strlen(pps_req->media_location));
		NewValue (parameter, &value);
	
		/* START_ADDRESS */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "START_ADDRESS");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildRFCTree: Failed to create START_ADDRESS.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		msgbuf[0] = '\0';
		(void) sprintf (msgbuf, "%d", pps_req->start_address);
		value = ODLConvertInteger (msgbuf, strlen(msgbuf));
		NewValue (parameter, &value);
	
		/* END_ADDRESS */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "END_ADDRESS");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildRFCTree: Failed to create END_ADDRESS.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		msgbuf[0] = '\0';
		(void) sprintf (msgbuf, "%d", pps_req->end_address);
		value = ODLConvertInteger (msgbuf,strlen(msgbuf));
		NewValue (parameter, &value);
	
		/* START_TIME */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "START_TIME");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		     	"ims_step_buildRFCTree: Failed to create START_TIME.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertDateTime (pps_req->start_time, strlen(pps_req->start_time));
		NewValue (parameter, &value);
	
		/* END_TIME */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "END_TIME");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		     	"ims_step_buildRFCTree: Failed to create END_TIME.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertDateTime (pps_req->end_time, strlen(pps_req->end_time));
		NewValue (parameter, &value);
	
		/* CENTER_TIME */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "CENTER_TIME");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		     	"ims_step_buildRFCTree: Failed to create CENTER_TIME.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertDateTime (pps_req->center_time, strlen(pps_req->center_time));
		NewValue (parameter, &value);
	
		/* RECORDER_ID */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "RECORDER_ID");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		     	"ims_step_buildRFCTree: Failed to create RECORDER_ID.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		ims_toUpper (pps_req->recorder_id);
		value = ODLConvertString (pps_req->recorder_id, strlen(pps_req->recorder_id));
		NewValue (parameter, &value);
	
		/* STATION_ID */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "STATION_ID");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
		     	"ims_step_buildRFCTree: Failed to create STATION_ID.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		ims_toUpper (pps_req->station_id);
		value = ODLConvertString (pps_req->station_id, strlen(pps_req->station_id));
		NewValue (parameter, &value);
	
		/* DATA_DIRECTION */
		parameter = NewParameter (sTree, KP_ATTRIBUTE, "DATA_DIRECTION");
		if (parameter == (PARAMETER)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
	    		"ims_step_buildRFCTree: Failed to create DATA_DIRECTION.");
			return (IMS_ERROR);
		}
		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (pps_req->data_direction, 
	                          strlen(pps_req->data_direction));
		NewValue (parameter, &value);

	}

	return (IMS_OK);

} /* end of ims_step_buildRFCTree */

/**********************************************************************
**
** ims_step_buildTSRTree - build an ODL for a TSR request
**
** return IMS_OK    
**        IMS_ERROR 
**********************************************************************/

int ims_step_buildTSRTree (IMS_MSG_STRUCT *msgDesc, 
                           STEP_INFO      *step, 
                           AGGREGATE      TxTree)
{

	AGGREGATE    pTree;          /* SCAN_ORDER         */
	AGGREGATE    cTree;          /* COMMON_HEADER      */
	AGGREGATE    bTree;          /* BODY               */
	AGGREGATE    sTree;          /* SOURCE_MEDIA       */
	PARAMETER    parameter;      /* temp pointer to parameter nodes */
	VALUE_DATA   value;          /* pointer to values  */
	char         msgbuf[IMS_COL255_LEN+1]; /* temp bufffer for strings */
	PPS_REQ_STRUCT *pps_req;

	/* initialization */
	pTree       = (AGGREGATE)NULL;
	cTree       = (AGGREGATE)NULL;
	bTree       = (AGGREGATE)NULL;
	sTree       = (AGGREGATE)NULL;
	parameter   = (PARAMETER)NULL;
	pps_req     = &(step->pps_req);

	/* 
	** create a new node under the root for SCAN_ORDER object
	*/
	if ( (pTree = NewAggregate (TxTree, KA_OBJECT, "SCAN_ORDER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create SCAN_ORDER object");
		return (IMS_ERROR);
	}

	/*
	** create new nodes under SCAN_ORDER object, 
	** i.e. COMMON_HEADER, BODY, and SOURCE_MEDIA
	*/
	if ( (cTree = NewAggregate (pTree, KA_OBJECT, "COMMON_HEADER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create COMMON_HEADER object");
		return (IMS_ERROR);
	}

	if ( (bTree = NewAggregate (pTree, KA_OBJECT, "BODY", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create BODY object");
		return (IMS_ERROR);
	}

	if ( (sTree = NewAggregate (bTree, KA_OBJECT, "SOURCE_MEDIA", NULL))
		 	== (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildTSRTree: Failed to create SOURCE_MEDIA object");
		return (IMS_ERROR);
	}

	/*
	** Fill in values to fields in COMMON_HEADER group
	*/


	/* TIME */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertDateTime (step->curr_time, strlen(step->curr_time));
	NewValue (parameter, &value);

	/* MSG_TYPE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "MSG_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create MSG_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("SCAN_ORDER", 10);
	NewValue (parameter, &value);

	/* DESTINATION */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "DESTINATION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create DESTINATION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("PPS", 3);
	NewValue (parameter, &value);

	/* SOURCE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "SOURCE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create SOURCE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("IMS", 3);
	NewValue (parameter, &value);

	/* NUMBER_OF_RECORDS */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "NUMBER_OF_RECORDS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create NUMBER_OF_RECORDS.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger ("1", 1);
	NewValue (parameter, &value);

	/*
	** Fill in values to fields in BODY group
	*/

	/* ORDER_TYPE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ORDER_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create ORDER_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("TSR", 3);
	NewValue (parameter, &value);

	/* ORDER_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ORDER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create ORDER_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->order_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* ITEM_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ITEM_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create ITEM_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->item_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* PRIORITY */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PRIORITY");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create PRIORITY.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (pps_req->char_priority, 
														strlen(pps_req->char_priority) );
	NewValue (parameter, &value);

	/* QUICKLOOK_FLAG */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "QUICKLOOK_FLAG");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create QUICKLOOK_FLAG.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->quicklook_p);
	value = ODLConvertString (pps_req->quicklook_p, 
														strlen(pps_req->quicklook_p) );
	NewValue (parameter, &value);

	/* PLATFORM */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PLATFORM");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create PLATFORM.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->platform);
	value = ODLConvertString (pps_req->platform, strlen(pps_req->platform));
	NewValue (parameter, &value);

	/* SENSOR */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "SENSOR");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create SENSOR.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->sensor);
	value = ODLConvertString (pps_req->sensor, strlen(pps_req->sensor));
	NewValue (parameter, &value);

	/* REVOLUTION */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "REVOLUTION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create REVOLUTION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->rev);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* MODE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "MODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create MODE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->mode);
	value = ODLConvertString (pps_req->mode, strlen(pps_req->mode));
	NewValue (parameter, &value);

	/* SEQUENCE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "SEQUENCE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create SEQUENCE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf(msgbuf, "%d", pps_req->sequence);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* ACTIVITY_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ACTIVITY_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create ACTIVITY_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->activity_id);
	value = ODLConvertString (pps_req->activity_id, strlen(pps_req->activity_id));
	NewValue (parameter, &value);

	/* FRAME_MODE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "FRAME_MODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create FRAME_MODE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->frame_mode);
	value = ODLConvertString (pps_req->frame_mode, strlen(pps_req->frame_mode));
	NewValue (parameter, &value);

	/* SITE_NAME */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "SITE_NAME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildTSRTree: Failed to create SITE_NAME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->site_name);
	value = ODLConvertString(pps_req->site_name,strlen(pps_req->site_name));
	NewValue (parameter, &value);

	/*
	** Fill in values to fields in SOURCE_MEDIA group
	*/

	/* MEDIA_TYPE */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildTSRTree: Failed to create MEDIA_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->media_type);
	value = ODLConvertString (pps_req->media_type, strlen(pps_req->media_type));
	NewValue (parameter, &value);

	
	/* MEDIA_ID_TYPE */
	/*
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_ID_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildTSRTree: Failed to create MEDIA_ID_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->media_id_type);
	value = ODLConvertString (pps_req->media_id_type, 
	                          strlen(pps_req->media_id_type));
	NewValue (parameter, &value);
	*/

	/* MEDIA_ID */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     		"ims_step_buildTSRTree: Failed to create MEDIA_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->media_id);
	value = ODLConvertString (pps_req->media_id, strlen(pps_req->media_id));
	NewValue (parameter, &value);

	/* MEDIA_LOCATION */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "MEDIA_LOCATION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	    		"ims_step_buildTSRTree: Failed to create MEDIA_LOCATION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (pps_req->media_location, 
	                          strlen(pps_req->media_location));
	NewValue (parameter, &value);

	/* DATA_DIRECTION */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "DATA_DIRECTION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	    		"ims_step_buildTSRTree: Failed to create DATA_DIRECTION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (pps_req->data_direction, 
	                          strlen(pps_req->data_direction));
	NewValue (parameter, &value);

	/* START_ADDRESS */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "START_ADDRESS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     	"ims_step_buildTSRTree: Failed to create START_ADDRESS.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", pps_req->start_address);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* END_ADDRESS */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "END_ADDRESS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
     	"ims_step_buildTSRTree: Failed to create END_ADDRESS.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", pps_req->end_address);
	value = ODLConvertInteger (msgbuf,strlen(msgbuf));
	NewValue (parameter, &value);

	/* START_TIME */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "START_TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildTSRTree: Failed to create START_TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertDateTime (pps_req->start_time, strlen(pps_req->start_time));
	NewValue (parameter, &value);

	/* END_TIME */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "END_TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildTSRTree: Failed to create END_TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertDateTime (pps_req->end_time, strlen(pps_req->end_time));
	NewValue (parameter, &value);

	/* RECORDER_ID */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "RECORDER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildTSRTree: Failed to create RECORDER_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->recorder_id);
	value = ODLConvertString (pps_req->recorder_id, strlen(pps_req->recorder_id));
	NewValue (parameter, &value);

	/* STATION_ID */
	parameter = NewParameter (sTree, KP_ATTRIBUTE, "STATION_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
	     	"ims_step_buildTSRTree: Failed to create STATION_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (pps_req->station_id);
	value = ODLConvertString (pps_req->station_id, strlen(pps_req->station_id));
	NewValue (parameter, &value);

	return (IMS_OK);

} /* end of ims_step_buildTSRTree */

/**********************************************************************
**
** ims_step_buildCancelTree - build an ODL for a Cancel request
**
** return IMS_OK    
**        IMS_ERROR 
**********************************************************************/

int ims_step_buildCancelTree (IMS_MSG_STRUCT *msgDesc, 
                           STEP_INFO      *step, 
                           AGGREGATE      TxTree)
{
	AGGREGATE    pTree;          /* CANCEL_ORDER         */
	AGGREGATE    cTree;          /* COMMON_HEADER      */
	AGGREGATE    bTree;          /* BODY               */
	PARAMETER    parameter;      /* temp pointer to parameter nodes */
	VALUE_DATA   value;          /* pointer to values  */
	char         msgbuf[IMS_COL255_LEN+1]; /* temp bufffer for strings */


	/* initialization */
	pTree       = (AGGREGATE)NULL;
	cTree       = (AGGREGATE)NULL;
	bTree       = (AGGREGATE)NULL;
	parameter   = (PARAMETER)NULL;

	/* 
	** create a new node under the root for CANCEL_ORDER object
	*/
	if ( (pTree = NewAggregate (TxTree, KA_OBJECT, "CANCEL_ORDER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create CANCEL_ORDER object");
		
		return (IMS_ERROR); 
	}

	/*
	** create new nodes under CANCEL_ORDER object, 
	** i.e. COMMON_HEADER and BODY
	*/
	if ( (cTree = NewAggregate (pTree, KA_OBJECT, "COMMON_HEADER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create COMMON_HEADER object");
		return (IMS_ERROR);
	}

	if ( (bTree = NewAggregate (pTree, KA_OBJECT, "BODY", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create BODY object");
		return (IMS_ERROR);
	}

	/*
	** Fill in values to fields in COMMON_HEADER group
	*/


	/* TIME */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertDateTime (step->curr_time, strlen(step->curr_time));
	NewValue (parameter, &value);

	/* MSG_TYPE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "MSG_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create MSG_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("CANCEL_ORDER", 12);
	NewValue (parameter, &value);

	/* DESTINATION */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "DESTINATION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create DESTINATION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("PPS", 3);
	NewValue (parameter, &value);

	/* SOURCE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "SOURCE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create SOURCE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("IMS", 3);
	NewValue (parameter, &value);

	/* NUMBER_OF_RECORDS */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "NUMBER_OF_RECORDS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create NUMBER_OF_RECORDS.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger ("1", 1);
	NewValue (parameter, &value);

	/*
	** Fill in values to fields in BODY group
	*/

	/* ORDER_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ORDER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create ORDER_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->order_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* ITEM_ID */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "ITEM_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create ITEM_ID.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->item_id);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* ORDER_TYPE */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "REQUEST_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildCancelTree: Failed to create REQUEST_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;

	switch (step->order_item_type)
	{
		case RPR_TYPE:
		case FPR_TYPE:
		case COR_TYPE:
			value = ODLConvertString ("FRAME", 5);
			break;

		case TSR_TYPE:
			value = ODLConvertString ("SCAN", 4);
			break;

		default:
			msgbuf[0] = '\0';
			(void) sprintf (msgbuf,"ims_step_buildCancelTree: unexpected order type %d",
			         step->order_item_type);
			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
			break;
			
	}
	NewValue (parameter, &value);

	return (IMS_OK);
} /* end of ims_step_buildCancelTree */

/**********************************************************************
**
** ims_step_buildSVTree - build an ODL for a State Vector Avail request
**
** return IMS_OK    
**        IMS_ERROR 
**********************************************************************/

int ims_step_buildSVTree (IMS_MSG_STRUCT *msgDesc, 
                           STEP_INFO      *step, 
                           AGGREGATE      TxTree)
{
	AGGREGATE    pTree;          /* STATE_VECTOR_AVAIL */
	AGGREGATE    cTree;          /* COMMON_HEADER      */
	AGGREGATE    bTree;          /* BODY               */
	PARAMETER    parameter;      /* temp pointer to parameter nodes */
	VALUE_DATA   value;          /* pointer to values  */
	char         msgbuf[IMS_COL255_LEN+1]; /* temp bufffer for strings */

	/* initialization */
	pTree       = (AGGREGATE)NULL;
	cTree       = (AGGREGATE)NULL;
	bTree       = (AGGREGATE)NULL;
	parameter   = (PARAMETER)NULL;

	/* 
	** create a new node under the root for STATE_VECTOR_AVAIL object
	*/
	if ( (pTree = NewAggregate (TxTree, KA_OBJECT, "STATE_VECTOR_AVAIL", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create STATE_VECTOR_AVAIL object");
		
		return (IMS_ERROR); 
	}

	/*
	** create new nodes under STATE_VECTOR_AVAIL object, 
	** i.e. COMMON_HEADER and BODY
	*/
	if ( (cTree = NewAggregate (pTree, KA_OBJECT, "COMMON_HEADER", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create COMMON_HEADER object");
		return (IMS_ERROR);
	}

	if ( (bTree = NewAggregate (pTree, KA_OBJECT, "BODY", NULL))
			 == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create BODY object");
		return (IMS_ERROR);
	}

	/*
	** Fill in values to fields in COMMON_HEADER group
	*/


	/* TIME */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertDateTime (step->curr_time, strlen(step->curr_time));
	NewValue (parameter, &value);

	/* MSG_TYPE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "MSG_TYPE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create MSG_TYPE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("STATE_VECTOR_AVAIL", 18);
	NewValue (parameter, &value);

	/* DESTINATION */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "DESTINATION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create DESTINATION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("PPS", 3);
	NewValue (parameter, &value);

	/* SOURCE */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "SOURCE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create SOURCE.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("IMS", 3);
	NewValue (parameter, &value);

	/* NUMBER_OF_RECORDS */
	parameter = NewParameter (cTree, KP_ATTRIBUTE, "NUMBER_OF_RECORDS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create NUMBER_OF_RECORDS.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger ("1", 1);
	NewValue (parameter, &value);

	/*
	** Fill in values to fields in BODY group
	*/

	/* PLATFORM */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "PLATFORM");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create PLATFORM.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (step->platform);
	value = ODLConvertString (step->platform, strlen(step->platform));
	NewValue (parameter, &value);

	/* START_REV */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "START_REV");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create START_REV.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->start_rev);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* END_REV */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "END_REV");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create END_REV.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	
	msgbuf[0] = '\0';
	(void) sprintf (msgbuf, "%d", step->end_rev);
	value = ODLConvertInteger (msgbuf, strlen(msgbuf));
	NewValue (parameter, &value);

	/* START_TIME */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "START_TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create START_TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (step->start_time);
	value = ODLConvertDateTime (step->start_time, strlen(step->start_time));
	NewValue (parameter, &value);

	/* END_TIME */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "END_TIME");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create END_TIME.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (step->end_time);
	value = ODLConvertDateTime (step->end_time, strlen(step->end_time));
	NewValue (parameter, &value);

	/* STATE_VECTOR_PRECISION */
	parameter = NewParameter (bTree, KP_ATTRIBUTE, "STATE_VECTOR_PRECISION");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		     "ims_step_buildSVTree: Failed to create STATE_VECTOR_PRECISION.");
		return (IMS_ERROR);
	}
	parameter->value_kind = KV_SCALAR;
	ims_toUpper (step->sv_precision);
	value = ODLConvertString (step->sv_precision, strlen(step->sv_precision));
	NewValue (parameter, &value);

	return (IMS_OK);
} /* end of ims_step_buildSVTree */

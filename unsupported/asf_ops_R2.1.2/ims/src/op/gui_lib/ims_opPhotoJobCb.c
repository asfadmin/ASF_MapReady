static char *sccs = "@(#)ims_opPhotoJobCb.c	5.5  05/12/97";
/*******************************************************************************

	File:			ims_opPhotoJob.c

	Function:	Callback functions for Photo Products Job Screen

	Author:		Jennifer Ting

	Date:			5/1995

						1/30/96 - Eliminated view_flag. The original purpose for 
						using view_flag is to force the user to save all the changes
						(do a Complete Job) before opening up another photo job.
						Since the quality valids and item comments are saved to 
						the database upon update, let's take out the view_flag
						for now. view_flag was assigned in photoJob_viewCb and 
						in photoJob_clearCb. In photoJob_viewCb, let us not 
						desensitize clearPB and searchPB when photo job status
						is IN-PHOTO.

						4/26/96 - Modified function photoJob_completeCb
						to update item step information in the order_item table 
						when the item needs to be regenerated.

						4/30/96 - Modified function photoJob_completeCb to 
						remove regenerating items from the fire and laser queues.

						6/10/96 - Modified function photoJob_closeCb to 
						correct PR 942.

*******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <X11/Shell.h>
#include <UxXt.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>

#define _IMS_OP_PHOTOJOBCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opPhotoJob.h>
#undef CONTEXT_MACRO_ACCESS

#define PHOTOJOB_ROWS 18 
#define PHOTO_QUEUE_ROWS 22
static int photoJobWindowTop = 0;
static int photoQueueWindowTop = 0;
static int jobStatus_flag = 0;
/*
static int view_flag = 0;
*/

/* Function local to this file */
int photoJob_createQuery (Widget wgt);
int photoJob_executeQuery (Widget wgt);
static void free_photo_jobList();


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
** 
** Function Name:	photoJob_create_optionMenuCb 
**
** Description:		Callback function to create status option menu
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_create_optionMenuCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int i;
	Dimension  marginLeft, marginRight;
	Dimension  marginWidth;
	Pixel      selectColor;
	Pixel      background;
	XmString   label;
	Widget		 menu_item;
	XmFontList fontList;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{

		/* Get resources from all dummyPB */
		XtVaGetValues(allPB,
						XmNfontList, &fontList,
						XmNbackground, &background,
						XmNmarginLeft, &marginLeft,
						XmNmarginRight, &marginRight,
						XmNmarginWidth, &marginWidth,
						NULL);

		for (i = 0; i < glbData.photojob_status_count; i++)
		{
			label = XmStringCreateLocalized (glbData.photojob_status[i].item_name);
			menu_item = XtVaCreateManagedWidget 
									(glbData.photojob_status[i].item_name,
 									 xmPushButtonWidgetClass, statusOM_pane,
									 XmNlabelString, label,
									 XmNfontList, fontList,
									 XmNbackground, background,
									 XmNmarginLeft, marginLeft,
									 XmNmarginRight, marginRight,
									 XmNmarginWidth, marginWidth,
									 NULL);

		 	XtAddCallback (menu_item, XmNactivateCallback,
										 (XtCallbackProc) photoJob_optionmenu_toggledCb, 
										 (XtPointer) glbData.photojob_status[i].item_id);

			XmStringFree (label);
		}

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: photoJob_optionmenu_toggledCb
**
** Description:		This callback function stores the most recently
**								selected menu item in a global variable matching
**								the option menu.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_optionmenu_toggledCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int which;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{
		which = (int) cd;
		jobStatus_flag = which;
	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_photoType_validsCb 
**
** Description:		Callback function for the Photo Type List button,
**								pops up the valids selection dialog.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_photoType_validsCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{
		selectionDlg_popupCb (glbData.photoJobW, (Widget)photoTypeTF, PHOTO_TYPE);
	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: photoJob_check_date
**
** Description:		Check Date Function
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_check_date(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{

		cbs = (XmTextVerifyCallbackStruct *)cb;
		len = XmTextGetLastPosition(wgt);

		if (cbs->reason == XmCR_MOVING_INSERT_CURSOR)
		{
			if (cbs->newInsert != len)
				cbs->doit = False;
			return;
		}

		/* no backspacing or typing in the middle of the string */
		if (cbs->currInsert < len)
		{
			cbs->doit = False;
			return;
		}

		if (cbs->text->length == 0) /* backspace */
		{
			if ((cbs->startPos == 4) || (cbs->startPos == 7))
				cbs->startPos--;
			return;
		}

		if (cbs->text->length >1) /* don't allow clipboard copies */
		{
			cbs->doit = False;
			return;
		}

		/* don't allow non-digits or let the input exceed 10 chars */
		if (!isdigit(c = cbs->text->ptr[0]) || len >= 10)
			cbs->doit = False;
		else
			if (len == 3 || len == 6)
			{
				cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
				cbs->text->length = 2;
				cbs->text->ptr[0] = c;
				cbs->text->ptr[1] = '-';
			}

	}
	UxPhotoJobContext = UxSaveCtx;
	return;
}


/*===========================================================================*
** 
** Function Name: photoJob_date_looseFocusCb
**
** Description:		Date validation function, this is called whenever 
**								the focus is moved away from the date text field.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void photoJob_date_looseFocusCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int  status;
	char *fieldValue;
	char Msg[IMS_COL1024_LEN+1];

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{

		fieldValue = XmTextGetString (UxWidget);

		if ((status = isDateFieldValid (fieldValue)) < IMS_OK)
		{
			/* Clear the date field */
			XmTextSetString (UxWidget, "");

			/* Display error messages */
			sprintf(Msg, "Invalid Date!");   
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_ERROR, Msg); 

			XtFree(fieldValue);
			return;
		}

	}
	UxPhotoJobContext = UxSaveCtx;
	return;

}



/*===========================================================================*
** 
** Function Name: photoJob_jobId_validsCb 
**
** Description:		displays the photo job id selection dialog for the user
**								to search on valid photo job ids.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_jobId_validsCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );

	{
		selectionDlg_popupCb (glbData.photoJobW, (Widget)jobIdTF1, PHOTOJOB_ID);
	}
	UxPhotoJobContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: photoJob_createQuery 
**
** Description:		Create query to search to photo job. 
**								to search on valid photo job ids.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

int photoJob_createQuery(
	Widget wgt) 
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_QUERY_STRUCT *sql;
	int i, photo_type_id, status;
	char *value;
	char *sqlPtr;
	char *startDate, *endDate; 
	char sDate[9], eDate[9]; 
	char sDateValue[IMS_COL15_LEN+1];
	char eDateValue[IMS_COL15_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );

	{

		clientData = &(glbData.photoClientData);
		sql = &(clientData->queryStruct);

		/*
		** Initialize op_query_struct 
		*/
		sql->select[0] = sql->from[0] = sql->where[0] = sql->sqlBuf[0] = '\0';
		sql->sPtr = sql->select;
		sql->fPtr = sql->from;
		sql->wPtr = sql->where;
		sqlPtr = sql->sqlBuf;

		strcpy (sql->sPtr, 
				"distinct t1.photojob_id, "
				"t1.photo_type, "
				"t1.no_items, "
				"t1.total_prints, "
				"t1.total_cost_$, "
				"convert (char(12), t1.start_time, 100), "
				"convert (char(12), t1.end_time, 100), "
				"t1.status, "
				"t1.op_comment, "
				"t2.work_order, "
				"t2.cost_$ "
				);

		sql->sPtr = sql->sPtr + strlen(sql->sPtr);

		strcpy (sql->fPtr, "photo_job t1, photo_work_order t2 ");
		sql->fPtr = sql->fPtr + strlen(sql->fPtr);

		strcpy (sql->wPtr, "t1.photo_type = t2.photo_type ");
		sql->wPtr = sql->wPtr + strlen(sql->wPtr);


		/*
		** Get text from photoJobId textField 
		*/
		value = XmTextFieldGetString (jobIdTF1);
		ims_truncStr (value);

		if (value && *value)
		{
			if ((status = ims_isInteger(value)) != IMS_OK) 
			{
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* Display error messages */
					strcpy (Msg, "Photo Job ID is invalid.");
					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

					return(IMS_FATAL);
			}
			sprintf (sql->wPtr, " and t1.photojob_id = %s", value);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (value);
		}


		/*
		** Get text from photoType textField 
		*/
		value = XmTextFieldGetString (photoTypeTF);
		ims_truncStr (value);

		if (value && *value)
		{
			i = 0;
			while ((i < glbData.photo_type_count) &&
						 (strcmp (value, glbData.photo_type[i].item_name) != 0))
					i++;

			if (i < glbData.photo_type_count)
				photo_type_id = glbData.photo_type[i].item_id;
			else
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy (Msg, "Photo Processing Type is invalid.");
				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

				return(IMS_FATAL);
			}

			sprintf (sql->wPtr, " and t1.photo_type = %d", photo_type_id);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (value);
		}

		/*
		** Job Status OptionMenu 
		*/
		if (jobStatus_flag)
		{
			sprintf (sql->wPtr, " and t1.status = %d", jobStatus_flag);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}


		/*	
		** Get job start dates 
		** Need to call date validation
		*/
		startDate = XmTextGetString (fromDateText);
		ims_truncStr (startDate);
		endDate = XmTextGetString (toDateText);
		ims_truncStr (endDate);

		if ((*startDate) && (*endDate))
		{
			strcpy (sDateValue, startDate);
			strcpy (eDateValue, endDate);
			ims_truncStr (sDateValue);
			ims_truncStr (eDateValue);

			sDate[0] = sDateValue[0];
			sDate[1] = sDateValue[1];
			sDate[2] = sDateValue[2];
			sDate[3] = sDateValue[3];
			sDate[4] = sDateValue[5];
			sDate[5] = sDateValue[6];
			sDate[6] = sDateValue[8];
			sDate[7] = sDateValue[9];
			sDate[8] = '\0';

			eDate[0] = eDateValue[0];
			eDate[1] = eDateValue[1];
			eDate[2] = eDateValue[2];
			eDate[3] = eDateValue[3];
			eDate[4] = eDateValue[5];
			eDate[5] = eDateValue[6];
			eDate[6] = eDateValue[8];
			eDate[7] = eDateValue[9];
			eDate[8] = '\0';

			if (atoi(sDate) > atoi(eDate))
			{
				/* Display error messages */
				sprintf(Msg, "Photo Job Search:\nStart Date exceeds End Date!");   
				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

				XtFree(startDate);
				XtFree(endDate);
				return (IMS_FATAL);
			}
		}

		if (startDate && *startDate)
		{
			sprintf (sql->wPtr, " and t1.start_time >= '%s'", startDate);  
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (startDate);
		}

		if (endDate && *endDate)
		{
			sprintf (sql->wPtr, " and t1.start_time <= '%s'", endDate);  
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (endDate);
		}


		/*
		** complete the sql statement
		*/

		sprintf (sqlPtr, "select %s\nfrom %s\nwhere %s\norder by t1.photojob_id",
										sql->select, sql->from, sql->where);
		
		sqlPtr = sqlPtr + strlen(sqlPtr);

	}
	UxPhotoJobContext = UxSaveCtx;
	return (IMS_OK);

}


/*===========================================================================*
** 
** Function Name: photoJob_executeQuery 
**
** Description:		Execute the query created by photoJob_createQuery function.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

int photoJob_executeQuery(
	Widget wgt) 
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_PHOTO_JOB_LIST *photoJobPtr;
	int status;
	int jobCount;
	char Msg[IMS_COL1024_LEN+1];

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );

	{

		/*
		** Initialize catalog request structure 
		*/
		clientData = &(glbData.photoClientData);
		catReq = &(clientData->catReq);
		catReq->item[0] = (int *)&jobCount;
		catReq->item[1] = (char *)clientData->queryStruct.sqlBuf;

		if ((status = ims_opCat (catReq, OP_GETPHOTOJOBLIST)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error: photo job retrieval failed.");   
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
			return (IMS_FATAL);
		}

		/* assign returned jobs to glbData.photoClientData->photoJobList */
		clientData->photoJobCount = *(int *)catReq->item[0];
		clientData->photoJobList = (OP_PHOTO_JOB_LIST *)catReq->item[2];

		if ((clientData->photoJobList != (OP_PHOTO_JOB_LIST *)NULL) ||
				(clientData->photoJobCount > 0))
		{
			/* initialize select flag */
			photoJobPtr = clientData->photoJobList;

			while (photoJobPtr != (OP_PHOTO_JOB_LIST *)NULL)
			{
				photoJobPtr->selectFlag = 0;
				photoJobPtr = photoJobPtr->next;

			}
		}
	}

	UxPhotoJobContext = UxSaveCtx;
	return (IMS_OK);
}


/*===========================================================================*
** 
** Function Name: photoJob_searchCb
**
** Description:		callback function for search PB.
**								1. create query
**								2. execute query
**								3. display results
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_searchCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	int status;
	Widget sbar;
	char buffer[IMS_COL10_LEN+1];
	XmScrollBarCallbackStruct *cbs;
	XmString label;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );

	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		clientData = &(glbData.photoClientData);

		/* free up photo_job_list */ 
		(void) free_photo_jobList();

		photoJobWindowTop = 0;
		photoQueueWindowTop = 0;

		/* clean up list & text widgets */
		XmListDeleteAllItems (jobIdSL1);
		XmListDeleteAllItems (photoTypeSL1);
		XmListDeleteAllItems (startDateSL1);
		XmListDeleteAllItems (statusSL1);
		XmListDeleteAllItems (orderIdSL2);
		XmListDeleteAllItems (itemIdSL2);
		XmListDeleteAllItems (productIdSL2);
		XmListDeleteAllItems (qtySL2);
		XmListDeleteAllItems (qualitySL2);
		XmListDeleteAllItems (goodSL2);
		XmListDeleteAllItems (regenSL2);
		XmListDeleteAllItems (cancelSL2);

		XmTextFieldSetString (totalJobsTF, "");
		XmTextFieldSetString (jobIdTF2, "");
		XmTextFieldSetString (workOrderTF, "");
		XmTextFieldSetString (totalPrintsTF, "");
		XmTextFieldSetString (orderDateTF, "");
		XmTextFieldSetString (totalCostTF, "");

		XtSetSensitive (clearPB, True);
		XtSetSensitive (viewPB, False);
		XtSetSensitive (processPB, False);
		XtSetSensitive (completePB, False);
		label = XmStringCreateLocalized ("Edit Comment");
		XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
		XtSetSensitive (commentPB, False); 

		/* Update Scrollbar position */
		XtVaGetValues(photoJobDummySW1, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
			XmNmaximum, 1,
			XmNvalue, photoJobWindowTop,
			XmNsliderSize, 1, 
			NULL);

		/* Update Scrollbar position */
		XtVaGetValues(photoJobDummySW2, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
		XmNmaximum, 1,
		XmNvalue, photoQueueWindowTop,
		XmNsliderSize, 1, 
		NULL);

		if ((status = photoJob_createQuery (glbData.photoJobW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);
			return;
		}

		if ((status = photoJob_executeQuery (glbData.photoJobW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);
			return;
		}

		/* display photo job list */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		photoJob_scroll_jobListsCb (glbData.photoJobW, NULL, cbs);
		free (cbs);

		/* display photo job count */
		sprintf (buffer, "%d", clientData->photoJobCount);
		XmTextFieldSetString (totalJobsTF, buffer);

		if (clientData->photoJobCount > 0)
		/*
		** sensitize view widget
		*/
		XtSetSensitive (viewPB, True);
	
		/* Change cursor back to normal*/
		timeOutCursors (False);

	}
	UxPhotoJobContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name:	photoJob_scroll_jobListsCb 
**
** Description:		Callback function for the CLOSE button,
**								pops down the Photo Job screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_scroll_jobListsCb(
	Widget wgt, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)

{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_JOB_LIST *photoJobPtr;
	int jobCount, count, memory;
	int i, j, k, highLightPos[PHOTOJOB_ROWS];
	Widget sbar;
	XmStringTable jobIdStr, typeStr, startDateStr, statusStr;
	char buffer[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	int slider;
	XmString label;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	{

		/* assign client to photoClientData from glbData structure */
		clientData = &(glbData.photoClientData);
		photoJobPtr = clientData->photoJobList;
		jobCount = clientData->photoJobCount;
		memory = 1;
		j = 0;

		photoJobWindowTop = cbs->value;
		count = jobCount - photoJobWindowTop;

		if (count > PHOTOJOB_ROWS)
			count = PHOTOJOB_ROWS;

		/* If no rows to display, clean up lists and text widgets */
		if ((jobCount == 0) || (photoJobPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (jobIdSL1);
			XmListDeleteAllItems (photoTypeSL1);
			XmListDeleteAllItems (startDateSL1);
			XmListDeleteAllItems (statusSL1);
			XmListDeleteAllItems (orderIdSL2);
			XmListDeleteAllItems (itemIdSL2);
			XmListDeleteAllItems (productIdSL2);
			XmListDeleteAllItems (qtySL2);
			XmListDeleteAllItems (qualitySL2);
			XmListDeleteAllItems (goodSL2);
			XmListDeleteAllItems (regenSL2);
			XmListDeleteAllItems (cancelSL2);

			XmTextFieldSetString (totalJobsTF, "");
			XmTextFieldSetString (jobIdTF2, "");
			XmTextFieldSetString (workOrderTF, "");
			XmTextFieldSetString (totalPrintsTF, "");
			XmTextFieldSetString (orderDateTF, "");
			XmTextFieldSetString (totalCostTF, "");

			XtSetSensitive (viewPB, False);
			XtSetSensitive (processPB, False);
			XtSetSensitive (completePB, False);
			label = XmStringCreateLocalized ("Edit Comment");
			XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
			XtSetSensitive (commentPB, False);

			/* Update Scrollbar position */
			XtVaGetValues(photoJobDummySW1, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, photoJobWindowTop,
				XmNsliderSize, 1, 
				NULL);

			/* Update Scrollbar position */
			XtVaGetValues(photoJobDummySW2, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, photoQueueWindowTop,
				XmNsliderSize, 1, 
				NULL);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		jobIdStr   		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		typeStr    		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		startDateStr  = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		statusStr  		= (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && jobIdStr && typeStr && startDateStr && statusStr;

		if (!memory)
		{
			/* Display error messages in message window */
			sprintf(Msg, "Internal Error: memory allocation failed.");   
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (jobIdStr[i]);
				XmStringFree (typeStr[i]);
				XmStringFree (startDateStr[i]);
				XmStringFree (statusStr[i]);
			}

			XtFree ((char *)jobIdStr);
			XtFree ((char *)typeStr);
			XtFree ((char *)startDateStr);
			XtFree ((char *)statusStr);

			return;
		}

	
		while ((photoJobPtr != NULL)&&(photoJobPtr->position != photoJobWindowTop))
		{
			photoJobPtr = photoJobPtr->next;
		}
		
		for (i = 0; i < count && photoJobPtr != (OP_PHOTO_JOB_LIST *)NULL; i++)
		{
			if (photoJobPtr->photojob_id)
			{
				sprintf (buffer, "%d", photoJobPtr->photojob_id);
				jobIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else 
				jobIdStr[i] = XmStringCreateLocalized("");


			if (photoJobPtr->photo_type)
			{
				k = 0;
				while ((k < glbData.photo_type_count) &&
							 (photoJobPtr->photo_type != glbData.photo_type[k].item_id))
							k++;

				if (k < glbData.photo_type_count)
				{
					typeStr[i] = XmStringCreateLocalized
												(glbData.photo_type[k].item_name);
				}
				else
					/* did not find the matching photo_type id */
					typeStr[i] = XmStringCreateLocalized("");

			}
			else
				typeStr[i] = XmStringCreateLocalized("");


			if (photoJobPtr->start_time[0] != '\0' )
				startDateStr[i] = XmStringCreateLocalized(photoJobPtr->start_time);
			else
				startDateStr[i] = XmStringCreateLocalized("");


			if (photoJobPtr->status)
			{
				k = 0;
				while ((k < glbData.photojob_status_count) && 
							 (photoJobPtr->status != glbData.photojob_status[k].item_id))
							k++;

				if (k < glbData.photojob_status_count)
				{
					statusStr[i] = XmStringCreateLocalized
												(glbData.photojob_status[k].item_name);
				}
				else
					/* did not find the matching order_status id */
					statusStr[i] = XmStringCreateLocalized("");

			}
			else
				statusStr[i] = XmStringCreateLocalized("");


			if (photoJobPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			photoJobPtr = photoJobPtr->next;

		}

		/* Load all the synchronized arrays to list widgets */

		XtVaSetValues(
				jobIdSL1, XmNitems, jobIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				photoTypeSL1, XmNitems, typeStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				startDateSL1, XmNitems, startDateStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				statusSL1, XmNitems, statusStr, XmNitemCount, count, NULL);


		/* Free compound strings */
		while (--i)
		{
			XmStringFree (jobIdStr[i]);
			XmStringFree (typeStr[i]);
			XmStringFree (startDateStr[i]);
			XmStringFree (statusStr[i]);
		}

		XtFree ((char *)jobIdStr);
		XtFree ((char *)typeStr);
		XtFree ((char *)startDateStr);
		XtFree ((char *)statusStr);
	

		/* Update Scrollbar position */
		XtVaGetValues(photoJobDummySW1, XmNverticalScrollBar, &sbar, NULL);

		if (jobCount > PHOTOJOB_ROWS)
			slider = PHOTOJOB_ROWS;
		else
			slider = jobCount;
		
		if (jobCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, jobCount,
				XmNvalue, photoJobWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, photoJobWindowTop,
				XmNsliderSize, 1, 
				NULL);

		XmListDeselectAllItems(jobIdSL1);
		XmListDeselectAllItems(photoTypeSL1);
		XmListDeselectAllItems(startDateSL1);
		XmListDeselectAllItems(statusSL1);


		while (j--)
		{
			XmListSelectPos(jobIdSL1, highLightPos[j], False);
			XmListSelectPos(photoTypeSL1, highLightPos[j], False);
			XmListSelectPos(startDateSL1, highLightPos[j], False);
			XmListSelectPos(statusSL1, highLightPos[j], False);
		}

	}
	UxPhotoJobContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	photoJob_jobLists_selectionCb 
**
** Description:		Callback function for job Lists selection,
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_jobLists_selectionCb(
	Widget wgt, 
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_JOB_LIST *photoJobPtr, *tempPtr;
	int listNo;
	int i, k, count, photoJobCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	{

		clientData = &(glbData.photoClientData);
		photoJobPtr = clientData->photoJobList;
		photoJobCount = clientData->photoJobCount;
		listNo = (int)listNumber;
		itemPosition = cbs->item_position;

		if(listNo != 1)
		{
			XmListDeselectAllItems(jobIdSL1);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(photoTypeSL1);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(startDateSL1);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(statusSL1);
		}


		/*
		** Reset the selectFlag for each item in the photoJobList
		*/
		tempPtr = photoJobPtr;
		while (tempPtr != (OP_PHOTO_JOB_LIST *) NULL) 
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		/* Locate screen items in orderList */
		count = photoJobCount - photoJobWindowTop;

		if (count > PHOTOJOB_ROWS)
			count = PHOTOJOB_ROWS;

		while ((photoJobPtr != NULL)&&(photoJobPtr->position != photoJobWindowTop))
		{
			photoJobPtr = photoJobPtr->next;
		}

  	/* Locate selected order in photoJobList to set the selectFlag */
		tempPtr = photoJobPtr;

		for (k = 0;  (tempPtr != NULL) && (k < itemPosition-1); k++)
			tempPtr = tempPtr->next;

		tempPtr->selectFlag = 1;


		/* Select the item across all photo job list wigets */
		if (listNo != 1)
			XmListSelectPos(jobIdSL1, itemPosition, False);
		if (listNo != 2)
			XmListSelectPos(photoTypeSL1, itemPosition, False);
		if (listNo != 3)
			XmListSelectPos(startDateSL1, itemPosition, False);
		if (listNo != 4)
			XmListSelectPos(statusSL1, itemPosition, False);

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_viewCb 
**
** Description:		Callback function for the View button.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_viewCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_JOB_LIST *photoJobPtr;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	char buffer[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	char query[IMS_COL255_LEN+1];
	int status;
	int i, count, photoJobCount, itemCount;
	XmScrollBarCallbackStruct *cbs;
	XmString label;


	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	{

/*
	if (view_flag)
	{
		** a job is already open, display message, return **
		sprintf(Msg, "Please complete the current job first.");   
		msgBoxDlg_popupCb (glbData.photoJobW, IMS_INFO, Msg); 
		return;
	}
*/

	/* assign client to photoClientData from glbData structure */
	clientData = &(glbData.photoClientData);
	photoJobPtr = clientData->photoJobList;
	photoJobCount = clientData->photoJobCount;

	/* Find the selected job in photo job list */
	while ((photoJobPtr != (OP_PHOTO_JOB_LIST *)NULL) && 
			(photoJobPtr->selectFlag == 0))
	{
		photoJobPtr = photoJobPtr->next;
	}

	if (photoJobPtr == (OP_PHOTO_JOB_LIST *)NULL)
	{
		/* No job is selected, display message, return */
		sprintf(Msg, "No job is selected.");   
		msgBoxDlg_popupCb (glbData.photoJobW, IMS_INFO, Msg); 
		return;
	}

	/* assign clientData->currPhotoJob */
	clientData->currPhotoJob = photoJobPtr;

	if (clientData->currPhotoJob->photoQueueList == (OP_PHOTO_QUEUE_LIST *)NULL)
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** Get photoQList from database for current photo job.		
		** Initialize catalog request structure 
		*/
		catReq = &(clientData->catReq);
		catReq->item[0] = (int *)&itemCount;

		sprintf (query,
						 "select t1.order_id, t1.item_id, t1.photo_type, "
						 "t1.photojob_id, t1.quantity, t1.quality, t1.status, "
						 "t1.op_comment, t2.user_id, t3.p_granule_name "
						 "from photo_queue t1, order_queue t2, order_item t3 "
						 "where t1.photojob_id = %d and "
						 "t1.order_id = t2.order_id and "
						 "t3.order_id = t1.order_id and "
						 "t3.item_id = t1.item_id "
						 "order by t1.order_id, t1.item_id", photoJobPtr->photojob_id);

		catReq->item[1] = (char *)query;
			
		/*
		** Get photo queue list for the current photo job
		*/
		if ((status = ims_opCat (catReq, OP_GETPHOTOQUEUELIST)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Internal Error: photo queue retrieval failed.");   
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
			return;
		}

		/* assign returned items */

/* Do we need to assign the count returned to photoJobPtr->no_items? */
		photoJobPtr->photoQueueList = (OP_PHOTO_QUEUE_LIST *)catReq->item[2];
		clientData->currPhotoJob->photoQueueList =
																	(OP_PHOTO_QUEUE_LIST *)catReq->item[2];

		/* Initialize select Flag and Get the product Id for each item */
		photoQPtr = photoJobPtr->photoQueueList;
		while (photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			/* initialize select flag */
			photoQPtr->selectFlag = 0;

			/*
			** this is not needed for schema v3.30 because p_granule_name
			** from order_item table is used as product_id.
			**
			if ((status = get_productID (glbData.photoJobW, photoQPtr)) < IMS_OK)
			{
				return;
			}
			*/
			photoQPtr = photoQPtr->next;
		}

	}


	/*
	** no items to display, desensitize all item menu items
	*/
	if (photoJobPtr->photoQueueList == (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		/* No items to display, desensitize widgets, this should not happen */
		XtSetSensitive (processPB, False);
		XtSetSensitive (completePB, False);
		label = XmStringCreateLocalized ("Edit Comment");
		XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
		XtSetSensitive (commentPB, False);
	}
	else
	{
		/* there are items to view, initialize view_flag only if 
		** the job status is IN-PHOTO -- modification may take place 
		*/

/*
		if (photoJobPtr->status == PHOTO_JOB_IN_PHOTO)
			view_flag = 1;
*/

		switch (photoJobPtr->status)
		{
			case PHOTO_JOB_NEW:
				/* status == NEW */
				XtSetSensitive (processPB, True);
				XtSetSensitive (completePB, False);
				XtSetSensitive (searchPB, True);
				XtSetSensitive (clearPB, True);
				label = XmStringCreateLocalized ("Edit Comment");
				XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
				XtSetSensitive (commentPB, False);
				XtVaSetValues (totalPrintsTF, XmNeditable, False, NULL); 
				XtVaSetValues (totalCostTF, XmNeditable, False, NULL); 
			break;

			case PHOTO_JOB_IN_PHOTO:
				/* status == IN-PHOTO */
				XtSetSensitive (processPB, True);
				XtSetSensitive (completePB, True);
				XtSetSensitive (searchPB, True);
				XtSetSensitive (clearPB, True);
				label = XmStringCreateLocalized ("Edit Comment");
				XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
				XtSetSensitive (commentPB, True);
				XtVaSetValues (totalPrintsTF, XmNeditable, True, NULL); 
				XtVaSetValues (totalCostTF, XmNeditable, True, NULL); 
			break;

			case PHOTO_JOB_COMPLETE:
				/* status == COMPLETE */
				XtSetSensitive (processPB, False);
				XtSetSensitive (completePB, False);
				XtSetSensitive (searchPB, True);
				XtSetSensitive (clearPB, True);
				label = XmStringCreateLocalized ("View Comment");
				XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
				XtSetSensitive (commentPB, True);
				XtVaSetValues (totalPrintsTF, XmNeditable, False, NULL); 
				XtVaSetValues (totalCostTF, XmNeditable, False, NULL); 
			break;

			default:
				/* desensitize widgets */
				XtSetSensitive (processPB, False);
				XtSetSensitive (completePB, False);
				XtSetSensitive (searchPB, True);
				XtSetSensitive (clearPB, True);
				label = XmStringCreateLocalized ("Edit Comment");
				XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
				XtSetSensitive (commentPB, False);
				XtVaSetValues (totalPrintsTF, XmNeditable, False, NULL); 
				XtVaSetValues (totalCostTF, XmNeditable, False, NULL); 
			break;
		}
	}

	/*
	** call photoJob_scroll_queueListsCb to
	** load data into queue list widgets
	*/
	cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
	cbs->value = 0;
	photoJob_scroll_queueListsCb(glbData.photoJobW, NULL, cbs);
	free (cbs);


	/*
	** Display data in text fields 
	*/
	sprintf (buffer, "%d", photoJobPtr->photojob_id );
	XmTextFieldSetString (jobIdTF2, buffer);

	sprintf (buffer, "%s", photoJobPtr->work_order);
	XmTextFieldSetString (workOrderTF, buffer);

	sprintf (buffer, "%-.2f", photoJobPtr->total_cost);
	XmTextFieldSetString (totalCostTF, buffer);

  sprintf (buffer, "%d", photoJobPtr->total_prints);
	XmTextFieldSetString (totalPrintsTF, buffer);

	sprintf (buffer, "%s", photoJobPtr->start_time);
	XmTextFieldSetString (orderDateTF, buffer);


	/* Change cursor back to normal */
	timeOutCursors (False);

	}
	UxPhotoJobContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	photoJob_scroll_queueListsCb 
**
** Description:		Callback function to scroll queue lists 
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_scroll_queueListsCb(
	Widget wgt, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)

{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	int photoCount, count, memory;
	int i, j, k, highLightPos[PHOTOJOB_ROWS];
	Widget sbar;
	XmStringTable orderIdStr, itemIdStr, productIdStr, qtyStr;
	XmStringTable qualityStr, goodStr, regenStr, cancelStr;
	char buffer[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	int slider;
	XmString label;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	{

		/* assign client to photoClientData from glbData structure */
		clientData = &(glbData.photoClientData);
		photoQPtr = clientData->currPhotoJob->photoQueueList;

/* is this okay ? or should we get the photoQueue count? */
		photoCount = clientData->currPhotoJob->no_items;
		memory = 1;
		j = 0;

		photoQueueWindowTop = cbs->value;
		count = photoCount - photoQueueWindowTop;

		if (count > PHOTO_QUEUE_ROWS)
			count = PHOTO_QUEUE_ROWS;

		/* If no rows to display, clean up lists and text widgets */
		if ((photoCount == 0) || (photoQPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (orderIdSL2);
			XmListDeleteAllItems (itemIdSL2);
			XmListDeleteAllItems (productIdSL2);
			XmListDeleteAllItems (qtySL2);
			XmListDeleteAllItems (qualitySL2);
			XmListDeleteAllItems (goodSL2);
			XmListDeleteAllItems (regenSL2);
			XmListDeleteAllItems (cancelSL2);

			XmTextFieldSetString (jobIdTF2, "");
			XmTextFieldSetString (workOrderTF, "");
			XmTextFieldSetString (totalPrintsTF, "");
			XmTextFieldSetString (orderDateTF, "");
			XmTextFieldSetString (totalCostTF, "");

			XtSetSensitive (processPB, False);
			XtSetSensitive (completePB, False);
			label = XmStringCreateLocalized ("Edit Comment");
			XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
			XtSetSensitive (commentPB, False);

			/* Update Scrollbar position */
			XtVaGetValues(photoJobDummySW2, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, photoQueueWindowTop,
				XmNsliderSize, 1, 
				NULL);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		orderIdStr 	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		itemIdStr 	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		productIdStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		qtyStr 			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		qualityStr 	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		goodStr			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		regenStr 		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		cancelStr		= (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && orderIdStr && itemIdStr && productIdStr && 
						 qtyStr && qualityStr && goodStr && regenStr && cancelStr;

		if (!memory)
		{
			/* Display error messages in message window */
			sprintf(Msg, "Internal Error: memory allocation failed.");   
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (orderIdStr[i]);
				XmStringFree (itemIdStr[i]);
				XmStringFree (productIdStr[i]);
				XmStringFree (qtyStr[i]);
				XmStringFree (qualityStr[i]);
				XmStringFree (goodStr[i]);
				XmStringFree (regenStr[i]);
				XmStringFree (cancelStr[i]);
			}

			XtFree ((char *)orderIdStr);
			XtFree ((char *)itemIdStr);
			XtFree ((char *)productIdStr);
			XtFree ((char *)qtyStr);
			XtFree ((char *)qualityStr);
			XtFree ((char *)goodStr);
			XtFree ((char *)regenStr);
			XtFree ((char *)cancelStr);

			return;
		}

	
		while ((photoQPtr != NULL) && (photoQPtr->position != photoQueueWindowTop))
		{
			photoQPtr = photoQPtr->next;
		}
		
		for (i = 0; i < count && photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL; i++)
		{
			if (photoQPtr->order_id)
			{
				sprintf (buffer, "%d", photoQPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else 
				orderIdStr[i] = XmStringCreateLocalized("");


			if (photoQPtr->item_id)
			{
				sprintf (buffer, "%d", photoQPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else 
				itemIdStr[i] = XmStringCreateLocalized("");


			if (photoQPtr->product_id[0] != '\0' )
				productIdStr[i] = XmStringCreateLocalized(photoQPtr->product_id);
			else
			{	
				/* No p_granule_name found */
				strcpy (buffer, "N/A");
				productIdStr[i] = XmStringCreateLocalized(buffer);
			}

			if (photoQPtr->quantity >= 0)
			{
				sprintf (buffer, "%d", photoQPtr->quantity);
				qtyStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				qtyStr[i] = XmStringCreateLocalized("");


			if (photoQPtr->quality[0] != '\0' )
				qualityStr[i] = XmStringCreateLocalized(photoQPtr->quality);
			else
				qualityStr[i] = XmStringCreateLocalized("");
			
			goodStr[i] = XmStringCreateLocalized("G");
			regenStr[i] = XmStringCreateLocalized("R");
			cancelStr[i] = XmStringCreateLocalized("C");

			if (photoQPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			photoQPtr = photoQPtr->next;

		}

		/* Load all the synchronized arrays to list widgets */

		XtVaSetValues(
				orderIdSL2, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemIdSL2, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				productIdSL2, XmNitems, productIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				qtySL2, XmNitems, qtyStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				qualitySL2, XmNitems, qualityStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				goodSL2, XmNitems, goodStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				regenSL2, XmNitems, regenStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				cancelSL2, XmNitems, cancelStr, XmNitemCount, count, NULL);


		/* Free compound strings */
		while (--i)
		{
			XmStringFree (orderIdStr[i]);
			XmStringFree (itemIdStr[i]);
			XmStringFree (productIdStr[i]);
			XmStringFree (qtyStr[i]);
			XmStringFree (qualityStr[i]);
			XmStringFree (goodStr[i]);
			XmStringFree (regenStr[i]);
			XmStringFree (cancelStr[i]);
		}

		XtFree ((char *)orderIdStr);
		XtFree ((char *)itemIdStr);
		XtFree ((char *)productIdStr);
		XtFree ((char *)qtyStr);
		XtFree ((char *)qualityStr);
		XtFree ((char *)goodStr);
		XtFree ((char *)regenStr);
		XtFree ((char *)cancelStr);
	

		/* Update Scrollbar position */
		XtVaGetValues(photoJobDummySW2, XmNverticalScrollBar, &sbar, NULL);

		if (photoCount > PHOTO_QUEUE_ROWS)
			slider = PHOTO_QUEUE_ROWS;
		else
			slider = photoCount;
		
		if (photoCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, photoCount,
				XmNvalue, photoQueueWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, photoQueueWindowTop,
				XmNsliderSize, 1, 
				NULL);

		XmListDeselectAllItems(orderIdSL2);
		XmListDeselectAllItems(itemIdSL2);
		XmListDeselectAllItems(productIdSL2);
		XmListDeselectAllItems(qtySL2);
		XmListDeselectAllItems(qualitySL2);
		XmListDeselectAllItems(goodSL2);
		XmListDeselectAllItems(regenSL2);
		XmListDeselectAllItems(cancelSL2);


		while (j--)
		{
			XmListSelectPos(orderIdSL2, highLightPos[j], False);
			XmListSelectPos(itemIdSL2, highLightPos[j], False);
			XmListSelectPos(productIdSL2, highLightPos[j], False);
			XmListSelectPos(qtySL2, highLightPos[j], False);
			XmListSelectPos(qualitySL2, highLightPos[j], False);
			XmListSelectPos(goodSL2, highLightPos[j], False);
			XmListSelectPos(regenSL2, highLightPos[j], False);
			XmListSelectPos(cancelSL2, highLightPos[j], False);
		}

	}
	UxPhotoJobContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	photoJob_queueLists_selectionCb 
**
** Description:		Callback function for queue Lists selection,
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_queueLists_selectionCb(
	Widget wgt, 
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr, *tempPtr;
	int listNo;
	int i, k, count, photoCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	{

		clientData = &(glbData.photoClientData);
		photoQPtr = clientData->currPhotoJob->photoQueueList;
		photoCount = clientData->currPhotoJob->no_items;
		listNo = (int)listNumber;
		itemPosition = cbs->item_position;

		if(listNo != 1)
		{
			XmListDeselectAllItems(orderIdSL2);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(itemIdSL2);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(productIdSL2);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(qtySL2);
		}
		if(listNo != 5)
		{
			XmListDeselectAllItems(qualitySL2);
		}


		if(listNo != 6)
		{
			XmListDeselectAllItems(goodSL2);
		}
		if(listNo != 7)
		{
			XmListDeselectAllItems(regenSL2);
		}
		if(listNo != 8)
		{
			XmListDeselectAllItems(cancelSL2);
		}


		/*
		** Reset the selectFlag for each item in the photoQueueList
		*/
		tempPtr = photoQPtr;
		while (tempPtr != (OP_PHOTO_QUEUE_LIST *) NULL) 
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		/* Locate screen items in photoQList */
		count = photoCount - photoQueueWindowTop;

		if (count > PHOTO_QUEUE_ROWS)
			count = PHOTO_QUEUE_ROWS;

		while ((photoQPtr != NULL) && (photoQPtr->position != photoQueueWindowTop))
		{
			photoQPtr = photoQPtr->next;
		}

  	/* Locate selected order in photoQueueList to set the selectFlag */
		tempPtr = photoQPtr;

		for (k = 0;  (tempPtr != NULL) && (k < itemPosition-1); k++)
			tempPtr = tempPtr->next;

		tempPtr->selectFlag = 1;


		/* Select the item across all photo queue list wigets */
		if (listNo != 1)
			XmListSelectPos(orderIdSL2, itemPosition, False);
		if (listNo != 2)
			XmListSelectPos(itemIdSL2, itemPosition, False);
		if (listNo != 3)
			XmListSelectPos(productIdSL2, itemPosition, False);
		if (listNo != 4)
			XmListSelectPos(qtySL2, itemPosition, False);
		if (listNo != 5)
			XmListSelectPos(qualitySL2, itemPosition, False);

		if (listNo != 6)
			XmListSelectPos(goodSL2, itemPosition, False);
		if (listNo != 7)
			XmListSelectPos(regenSL2, itemPosition, False);
		if (listNo != 8)
			XmListSelectPos(cancelSL2, itemPosition, False);

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_quality_validsCb 
**
** Description:		Callback function for list widgets: goodSL2, 
**								regenSL2, and cancelSL2.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_quality_validsCb(
	Widget wgt, 
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	char *value;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr, *tempPtr;
	OP_PHOTO_JOB_LIST *photoJobPtr;
	OP_CAT_STRUCT *catReq;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL15_LEN+1];
	int listNo;
	int i, k, count, photoCount;
	int itemPosition, status;
	XmScrollBarCallbackStruct *scroll_cbs;
	int totalPrints = 0;


	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{

		clientData = &(glbData.photoClientData);
		catReq = &(clientData->catReq);

		/* only when job status is IN-PHOTO, then we continue */
		if (clientData->currPhotoJob->status != PHOTO_JOB_IN_PHOTO)
		{
			XmListDeselectPos(wgt, cbs->item_position);
			return;
		}

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		photoJobPtr = clientData->currPhotoJob;
		photoQPtr = clientData->currPhotoJob->photoQueueList;
		photoCount = clientData->currPhotoJob->no_items;
		listNo = (int)listNumber;
		itemPosition = cbs->item_position;

		/* Locate screen items in photoQList */
		count = photoCount - photoQueueWindowTop;

		if (count > PHOTO_QUEUE_ROWS)
			count = PHOTO_QUEUE_ROWS;

		while ((photoQPtr != NULL) && (photoQPtr->position != photoQueueWindowTop))
		{
			photoQPtr = photoQPtr->next;
		}

  	/* Locate selected item in this segment of photoQueueList */
		for (k = 0;  (photoQPtr != NULL) && (k < itemPosition-1); k++)
			photoQPtr = photoQPtr->next;


		switch (listNo)
		{
			case 6:
				/* quality = GOOD */
				if (strcmp (photoQPtr->quality, "GOOD") != 0) 
				{
					strcpy (photoQPtr->quality, "GOOD");
				}
			break;

			case 7:
				/* quality = REGEN */
				if (strcmp (photoQPtr->quality, "REGEN") != 0) 
				{
					strcpy (photoQPtr->quality, "REGEN");
				}
			break;

			case 8:
				/* quality = CANCEL */
				if (strcmp (photoQPtr->quality, "CANCEL") != 0) 
				{
					strcpy (photoQPtr->quality, "CANCEL");
				}
			break;

			default:
			break;
		}
		
		/* recalculate total prints & cost */
		tempPtr = photoJobPtr->photoQueueList;
		while (tempPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			if (strcmp (tempPtr->quality, "GOOD") == 0)
			{
				totalPrints = totalPrints + tempPtr->quantity;
			}

			tempPtr = tempPtr->next;
		}
		
		photoJobPtr->total_prints = totalPrints;
		photoJobPtr->total_cost =
					photoJobPtr->total_prints * photoJobPtr->print_cost; 
		

		/*
		** Begin the update transaction 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Job Quality: "
									 "OP_BEGINTRANSACTION failed.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return; 
		}

		catReq->item[0] = (OP_PHOTO_QUEUE_LIST *)photoQPtr;
		if ((status = ims_opCat (catReq, OP_UPDATEPHOTOQUALITY)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Job Quality: "
									 "OP_UPDATEPHOTOQUALITY failed.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
	
			return;
		}

		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Job Quality: "
									 "OP_COMMITTRANSACTION failed. \n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return;
		}

	
		/*
		** update photojob cost & prints 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Job Cost: "
									 "OP_BEGINTRANSACTION failed.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return; 
		}

		catReq->item[0] = (OP_PHOTO_JOB_LIST *)photoJobPtr;
		if ((status = ims_opCat (catReq, OP_UPDATEPHOTOJOBCOST)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Job Cost: "
									 "OP_UPDATEPHOTOJOBCOST failed.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
	
			return;
		}

		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Job Cost: "
									 "OP_COMMITTRANSACTION failed. \n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return;
		}
	
		/* re-display photo queue lists */
		scroll_cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		scroll_cbs->value = photoQueueWindowTop;
		photoJob_scroll_queueListsCb (glbData.photoJobW, NULL, scroll_cbs);
		free (scroll_cbs);

		/* re-display total prints and total cost */
		sprintf (buffer, "%-.2f", photoJobPtr->total_cost);
		XmTextFieldSetString (totalCostTF, buffer);

  	sprintf (buffer, "%d", photoJobPtr->total_prints);
		XmTextFieldSetString (totalPrintsTF, buffer);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_commentCb 
**
** Description:		Callback function for the Comment button
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_commentCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, photoCount;
	XmString label;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{

	/* assign clientData */
	clientData = &(glbData.photoClientData);
	photoQPtr = clientData->currPhotoJob->photoQueueList;
	photoCount = clientData->currPhotoJob->no_items;


	/* Find the selected item in the photo queue list */
	i = 0;
	while ((photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL) && (i < photoCount) && 
			(photoQPtr->selectFlag == 0))
	{
		i++;
		photoQPtr = photoQPtr->next;
	}

	if ((photoQPtr == (OP_PHOTO_QUEUE_LIST *)NULL) || (i >= photoCount))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.photoJobW, IMS_INFO, Msg); 
		return;
	}

	/* Found the selected item, popup the comment dialog */
	/* update comment is allowed only if the photo job status is IN-PHOTO */
	
	if (clientData->currPhotoJob->status == PHOTO_JOB_IN_PHOTO)
	{
		commentDlg_popupCb 
				(glbData.photoJobW, (void *)photoQPtr, EDIT_PHOTO_COMMENT);
	}
	else
	{
		commentDlg_popupCb 
				(glbData.photoJobW, (void *)photoQPtr, VIEW_PHOTO_COMMENT);
	}
	
	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_completeCb 
**
** Description:		Callback function for the COMPLETE button
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_completeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob					*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	OP_PHOTO_JOB_LIST *photoJobPtr;
	OP_CAT_STRUCT *catReq;
	char Msg[IMS_COL1024_LEN+1];
	char query[IMS_COL255_LEN+1];
	char account_id[IMS_COL15_LEN+1];
	char msgBoxLabel[IMS_COL128_LEN+1];
	int status;
	int k, i, photoCount;
	short temp;
	short item_status_id;
	float costPerPrint, totalCost;
	char *value;
	int len,totalPrints;
	XmScrollBarCallbackStruct *cbs;
	XmString label;
	float item_cost;
	CONCAT_STR concat_msg = {0};
	DBSMALLINT current_status;
	DBSMALLINT order_item_type, media_class;
	DBSMALLINT process_type, step_sequence;


	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/* assign clientData */
		clientData = &(glbData.photoClientData);
		catReq = &(clientData->catReq);

		photoJobPtr = clientData->currPhotoJob;
		photoQPtr = clientData->currPhotoJob->photoQueueList;
		photoCount = clientData->currPhotoJob->no_items;

		/*
		** 1/17/96 - Let's do the complete photo job process in 
		** one transaction.  So if anything goes wrong, nothing
		** gets updated in order_item, photo_job nor photo_queue
		** tables.
		*/

		/*
		** 1/17/1996 - Verify cost and prints of the photo job first.
		** If the total cost and prints values are not correct, 
		** popup error messages, then return.
		*/

		/* update the cost and prints of the job if operator made changes */
		value = XmTextFieldGetString (totalCostTF);
		if ((status = ims_isReal(value)) != IMS_OK) 
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Total Cost is invalid.");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return;
		}
		totalCost = atof (value);


		/* get total prints from text widget */
		value = XmTextFieldGetString (totalPrintsTF);
		if ((status = ims_isInteger(value)) != IMS_OK) 
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Total Prints value is invalid.");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return;
		}
		totalPrints = atoi (value);
		

		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Complete Photo Job: "
									 "OP_BEGINTRANSACTION failed.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return; 
		}


		if ((photoJobPtr->total_cost != totalCost) ||
				(photoJobPtr->total_prints != totalPrints))
		{
			photoJobPtr->total_cost = totalCost;
			photoJobPtr->total_prints = totalPrints;

			/* operator made modification, call updatePhotoJobCost */

			catReq->item[0] = (OP_PHOTO_JOB_LIST *)photoJobPtr;
			if ((status = ims_opCat (catReq, OP_UPDATEPHOTOJOBCOST)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy (Msg, "Internal Error: Update Photo Job Cost: "
										 "OP_UPDATEPHOTOJOBCOST failed.\n");
				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
	
				return;
			}

		}


		/*
		** Now we are ready to update item status in order_item table. 
		** Lock the order_item table first.
		*/
		if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Complete Photo Job: "
									 "Could not lock table order_item, "
									 "OP_GETITEMLOCK failed.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return;
		}

		/*
		** 1/17/96 - if one item fails, should we continue on or should
		** we rollback everything?
		** catch - if photo job status is COMPLETE, no more update is
		** allowed.
		**
		** 4/26/96 - if one thing fails, we roll back the transaction,
		** rolls back all the updates that had take place, then we
		** pop up the message to inform the user, we do not continue
		** to complete the photo job.
		*/

		/* go through photoQList and do the following: */
		/* update order_item status for each photoQueueList item */
		while (photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			/*
			** Let's verify the photo item status first. If the item
			** has status other than IN-PHOTO, bypass this item.
			** For example: item has been cancelled.
			*/
			catReq->item[0] = (DBINT *)&photoQPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&photoQPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&current_status;
			if ((status = ims_opCat(catReq, OP_GETPHOTOITEMSTATUS)) < 0)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(Msg,
						 "OP_GETPHOTOITEMSTATUS failed for Order: %d, Item: %d\n", 
						 photoQPtr->order_id, photoQPtr->item_id);   

				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}


			/*
			** If current item status is not PHOTO_QUEUE_IN_PHOTO,
			** move on to process the next item.
			*/
			if (current_status != PHOTO_QUEUE_IN_PHOTO)
			{
				k = 0;
				while ((k < glbData.photo_queue_status_count) && 
							 (current_status != glbData.photo_queue_status[k].item_id))
							k++;

				/* Display error messages */
				if (k > glbData.photo_queue_status_count)
				{
					/* could not locate the photo_queue status */
					sprintf(Msg,
							 "The status of Order: %d, Item: %d has been changed.\n" 
							 "No further status update will be processed.\n", 
							 photoQPtr->order_id, photoQPtr->item_id); 
				}
				else
				{
					sprintf(Msg,
							 "The status of Order: %d, Item: %d has been changed to %s\n" 
							 "No further status update will be processed.\n", 
							 photoQPtr->order_id, photoQPtr->item_id, 
							 glbData.photo_queue_status[k].item_name);   
				}

				if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Display error messages */
					strcpy (Msg, "Internal Error: Memory Allocation failed.\n"
											 "Please contact DBA and exit the program.\n");
					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				photoQPtr = photoQPtr->next;
				continue;

			}


			/* if quality == GOOD, order_item.status = GENERATED */
			/* if quality == REGEN, order_item.status = ON-LINE */
			/* if quality == CANCEL, order_item.status = CANCEL */

			if (strcmp (photoQPtr->quality, "GOOD") == 0) 
			{
				item_status_id = ITEM_GENERATED;
			}

			if (strcmp (photoQPtr->quality, "REGEN") == 0)
			{
				item_status_id = ITEM_ON_LINE;   
			}

			if (strcmp (photoQPtr->quality, "CANCEL") == 0)
			{
				item_status_id = ITEM_CANCEL;   
			}

			catReq->item[0] = (DBINT *)&photoQPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&photoQPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&item_status_id;

			if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(Msg,
						 "OP_UPDATEITEMSTATUS failed for Order: %d, Item: %d\n", 
						 photoQPtr->order_id, photoQPtr->item_id);   

				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

				/* free up concat_msg space allocated */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			/*
			** 04/26/96 If the order item status is changed to ON-LINE,
			** we need to update the step information for that item.
			*/

			/*
			** 04/26/96 - get order item information necessary to
			** get the step info.
			*/
			if (item_status_id == ITEM_ON_LINE)
			{
				catReq->item[0] = (DBINT *)&photoQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&photoQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&order_item_type;
				catReq->item[3] = (DBSMALLINT *)&process_type;
				catReq->item[4] = (DBSMALLINT *)&media_class;
				if ((status = ims_opCat(catReq, OP_GETORDERITEMINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg,
						 "OP_GETORDERITEMINFO failed for Order: %d, Item: %d\n", 
						 photoQPtr->order_id, photoQPtr->item_id);   

					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

					/* free up concat_msg space allocated */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}


				/*
				** 04/26/96 - get step sequence by executing get_step_info
				** stored procedure.
				*/
				catReq->item[0] = (DBSMALLINT *)&order_item_type;
				catReq->item[1] = (DBSMALLINT *)&media_class;
				catReq->item[2] = (DBSMALLINT *)&process_type;
				catReq->item[3] = (DBSMALLINT *)&item_status_id;
				catReq->item[4] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_GETITEMSTEPINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg,
						 "OP_GETITEMSTEPINFO failed for Order: %d, Item: %d\n", 
						 photoQPtr->order_id, photoQPtr->item_id);   

					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

					/* free up concat_msg space allocated */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				/*
				** The new step_sequence is the current step_sequence - 1
				*/
				step_sequence--;
	
				/*
				** 04/26/96 - Update item step_sequence, step_name and step_started_p
				** This is to correct problem where ims_do_next_step reported
				** inconsistent step started status.
				*/
				catReq->item[0] = (DBINT *)&photoQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&photoQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_UPDATEITEMSTEPINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg,
						 "OP_UPDATEITEMSTEPINFO failed for Order: %d, Item: %d\n", 
						 photoQPtr->order_id, photoQPtr->item_id);   

					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

					/* free up concat_msg space allocated */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				/*
				** 04/30/96 - call removeFireQueueItem and removeLaserQueueItem
				** to remove the item from fire, laser queues. Since the item
				** may exist in only one of the queues, IMS_AFFECTED in one
				** of the functions may return IMS_WARNING, so we check the 
				** status returned against IMS_WARNING instead of IMS_OK.
				*/

				/*
				** call removeFireQueueItem to remove the item from fire_queue
				*/
				catReq->item[0] = (DBINT *)&photoQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&photoQPtr->item_id;
				if ((status = ims_opCat(catReq, OP_REMOVEFIREQUEUEITEM)) < IMS_WARNING)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nComplete Photo Job,\n"
							 "OP_REMOVEFIREQUEUEITEM failed for Order: %d, Item: %d\n", 
							 photoQPtr->order_id, photoQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.photoJobW, IMS_FATAL, Msg);

					/* free up concat_msg space allocated */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				/*
				** call removeLaserQueueItem to remove the item from laser_queue
				*/
				catReq->item[0] = (DBINT *)&photoQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&photoQPtr->item_id;
				if ((status = ims_opCat(catReq, OP_REMOVELASERQUEUEITEM)) < IMS_WARNING)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nComplete Photo Job,\n"
							 "OP_REMOVELASERQUEUEITEM failed for Order: %d, Item: %d\n", 
							 photoQPtr->order_id, photoQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.photoJobW, IMS_FATAL, Msg);

					/* free up concat_msg space allocated */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

			} /* if item_status_id == ITEM_ON_LINE */

			/******************************************************************/
			/* getNewOrderStatus is not needed anymore, in schema v3.20       */
			/* trigger order_item_update should take care of new order status */ 
			/* if status is generated, then we need to getNewOrderStatus      */
			/* if status is cancel, then we need to getNewOrderStatus         */
			/******************************************************************/
		 
			/******************************************************************
			** NOTE: 12/12/1995 - Accounting adjustment is done by the Server
			**										in R1B Prime.
			** if canceled, call accounting transaction rollback function
			** to adjust the cost for that order
			** ims_acctTran() transaction type 5 - DEBIT-ROLLBACK
			** The following parameters are passed: qDesc, msgDesc, 
			** account_id, order_id, cost of item, transaction type.
			******************************************************************/

			/*****************************************************************
			if (item_status_id == ITEM_CANCEL)
			{
				*** Get the account_id and cost for this item. ***
				sprintf (query, "select t1.account_id, t2.cost from "
												"order_queue t1, order_item t2 "
												"where t1.order_id = %d and t2.item_id = %d and "
												"t1.order_id = t2.order_id",
												photoQPtr->order_id, photoQPtr->item_id);

				catReq->item[0] = (char *)query;
				catReq->item[1] = (char *)account_id;
				catReq->item[2] = (float *)&item_cost;
				if ((status = ims_opCat (catReq, OP_GETITEMACCOUNTCOST)) < IMS_OK)
				{
					*** Change cursor back to normal ***
					timeOutCursors (False);

					*** Display error messages ***
					strcpy (Msg, "Internal Error: OP_GETITEMACCOUNTCOST: "
											 "Could not get item account id and cost.\n");
					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
				}


				if ((status = ims_acctTran(catReq->qDesc, catReq->msgDesc, 
																	account_id, photoQPtr->order_id,
																	item_cost, DEBIT_ROLLBACK)) 
																	< IMS_OK)
				{
					*** Change cursor back to normal ***
					timeOutCursors (False);

					*** Display error messages ***
					sprintf (Msg, "Internal Error: ims_acctTran() failed.\n"
												"Could not adjust account %s for cancelled "
											 	"Order: %d Item: %d\n",
											 	account_id, photoQPtr->order_id, photoQPtr->item_id);

					msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
				}
			}
			*********************************************************************/

			photoQPtr = photoQPtr->next;

		} /* while */
	

		/*
		** Lastly, let's update photo_job and photo_queue status
		** update the photojob status to be COMPLETE 
		** update photo_queue items status to be COMPLETE 
		** update is done in CAT function completePhotoJob
		*/

		catReq->item[0] = (OP_PHOTO_JOB_LIST *)photoJobPtr;
		if ((status = ims_opCat (catReq, OP_COMPLETEPHOTOJOB)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* free up concat_msg space allocated */
			OP_FREECHARPTR(concat_msg.Msg);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Complete Photo Job:\n"
									 "OP_COMPLETEPHOTOJOB failed. Please contact DBA.\n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

			return;
		}


		/*
		** Now we can do Commit Transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* free up concat_msg space allocated */
			OP_FREECHARPTR(concat_msg.Msg);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Complete Photo Job: "
									 "OP_COMMITTRANSACTION failed. \n");
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
			return;
		}


		/* refresh job lists */
		clientData->currPhotoJob->status = PHOTO_JOB_COMPLETE;

		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = photoJobWindowTop;
		photoJob_scroll_jobListsCb (glbData.photoJobW, NULL, cbs);
		free (cbs);

		/* desensitize processPB, commentPB, completePB */

		XtSetSensitive (processPB, False);
		XtSetSensitive (completePB, False);
		label = XmStringCreateLocalized ("Edit Comment");
		XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
		XtSetSensitive (commentPB, False);
	
		XtSetSensitive (searchPB, True);
		XtSetSensitive (clearPB, True);


		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 1024)
			{
				strcpy (msgBoxLabel, "Complete  Photo  Job  Message  Box");
				browseDlg_popupCb (glbData.photoJobW, concat_msg.Msg, msgBoxLabel); 
			}
			else
			{
				msgBoxDlg_popupCb (glbData.photoJobW, IMS_ERROR, concat_msg.Msg); 
			}
		}

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/* reset view_flag, free up label XmString */

/*
		view_flag = 0;
*/

		XmStringFree (label);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_closeCb 
**
** Description:		Callback function for the CLOSE button,
**								pops down the Photo Job screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - took out photoJob context support
**
**==========================================================================*/

void	photoJob_closeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	photoJob_clearCb (glbData.photoJobW, cd, cb);
	XtPopdown (XtParent(glbData.photoJobW));
	glbData.photoJobFlag = 0;
}


/*===========================================================================*
** 
** Function Name:	photoJob_printCb 
**
** Description:		Callback function for the PRINT button,
**								prints the Photo Job screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_printCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.photoJobW);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_processCb 
**
** Description:		Callback function for the Process button
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_processCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoJob          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	XmScrollBarCallbackStruct *cbs;
	int status;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( UxWidget );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		clientData = &(glbData.photoClientData);
		photoQPtr = clientData->currPhotoJob->photoQueueList;

		/*
		** call process_photoOrder to update photojob status 
		** and to print out order form 
		*/
		if ((status = process_photoOrder(1, photoQPtr)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);

			return;
		}
		
		/* change current photojob status to be IN-PHOTO */
		clientData->currPhotoJob->status = PHOTO_JOB_IN_PHOTO;

		/* re-display photo job list */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = photoJobWindowTop;
		photoJob_scroll_jobListsCb (glbData.photoJobW, NULL, cbs);
		free (cbs);

/*
		XtSetSensitive (processPB, False);
*/


		/* Change cursor back to normal*/
		timeOutCursors (False);
	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoJob_clearCb 
**
** Description:		Callback function for the Clear button
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoJob_clearCb(
	Widget wgt,
	XtPointer cd, 
	XtPointer cb)

{
	_UxCphotoJob          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	XmScrollBarCallbackStruct *cbs;
	Widget sbar;
	XmString label;

	UxSaveCtx = UxPhotoJobContext;
	UxPhotoJobContext = UxContext =
			(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	{

		/* free up photo_job_list */ 
		(void) free_photo_jobList();

		photoJobWindowTop = 0;
		photoQueueWindowTop = 0;

		/* clean up list & text widgets */
		XmListDeleteAllItems (jobIdSL1);
		XmListDeleteAllItems (photoTypeSL1);
		XmListDeleteAllItems (startDateSL1);
		XmListDeleteAllItems (statusSL1);
		XmListDeleteAllItems (orderIdSL2);
		XmListDeleteAllItems (itemIdSL2);
		XmListDeleteAllItems (productIdSL2);
		XmListDeleteAllItems (qtySL2);
		XmListDeleteAllItems (qualitySL2);
		XmListDeleteAllItems (goodSL2);
		XmListDeleteAllItems (regenSL2);
		XmListDeleteAllItems (cancelSL2);

		XmTextFieldSetString (totalJobsTF, "");
		XmTextFieldSetString (jobIdTF2, "");
		XmTextFieldSetString (jobIdTF1, "");
		XmTextFieldSetString (workOrderTF, "");
		XmTextFieldSetString (totalPrintsTF, "");
		XmTextFieldSetString (orderDateTF, "");
		XmTextFieldSetString (totalCostTF, "");
		XmTextFieldSetString (photoTypeTF, "");
		XmTextSetString (toDateText, "");
		XmTextSetString (fromDateText, "");

		XtSetSensitive (viewPB, False);
		XtSetSensitive (processPB, False);
		XtSetSensitive (completePB, False);
		XtSetSensitive (searchPB, True);
		XtSetSensitive (clearPB, True);
		label = XmStringCreateLocalized ("Edit Comment");
		XtVaSetValues (commentPB, XmNlabelString, label, NULL); 
		XtSetSensitive (commentPB, False); 

		/* Update Scrollbar position */
		XtVaGetValues(photoJobDummySW1, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
			XmNmaximum, 1,
			XmNvalue, photoJobWindowTop,
			XmNsliderSize, 1, 
			NULL);

		/* Update Scrollbar position */
		XtVaGetValues(photoJobDummySW2, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
		XmNmaximum, 1,
		XmNvalue, photoQueueWindowTop,
		XmNsliderSize, 1, 
		NULL);

		/* Reset Photo Job Status Option Menu */
		XtVaSetValues (statusOptionMenu, XmNmenuHistory, allPB, NULL);

		jobStatus_flag = 0;

/*
		view_flag = 0;
*/

	}
	UxPhotoJobContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	free_photo_jobList
**
** Description:		Function to free the photo_job_list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_photo_jobList()
{
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_JOB_LIST *jPtr, *jNextPtr; 
	OP_PHOTO_QUEUE_LIST *qPtr, *qNextPtr; 

	clientData = &(glbData.photoClientData);

	if (clientData->photoJobList != (OP_PHOTO_JOB_LIST *)NULL)
	{
		jPtr = clientData->photoJobList;

		/* free up the clientData photoJobList */
		while (jPtr != (OP_PHOTO_JOB_LIST *)NULL)
		{
			/* free up the photo queue list associated with the job */
			qPtr = jPtr->photoQueueList;
			while (qPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
			{
				qNextPtr = qPtr->next;
				free(qPtr);
				qPtr = qNextPtr;
			} 
			jPtr->photoQueueList = (OP_PHOTO_QUEUE_LIST *)NULL;
	
			jNextPtr = jPtr->next;
			free(jPtr);
			jPtr = jNextPtr;

		}

		clientData->photoJobCount = 0;
		clientData->photoJobList = (OP_PHOTO_JOB_LIST *)NULL;
		clientData->currPhotoJob = (OP_PHOTO_JOB_LIST *)NULL;

	}

}


/*===========================================================================*
** 
** Function Name: goto_photoOrderScreen
**
** Description:		Pop up the photo Order screen from Photo Job screen  
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	goto_photoOrderScreen(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

		XtPopup(XtParent(glbData.photoOrderW), XtGrabNone);
		glbData.photoOrderFlag = 1;

}


static char *sccs = "@(#)ims_opMediaDeviceCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opMediaDeviceCb.c

	Function:	Callback functions for Media Device Screen

	Author:		Jennifer Ting

	Date:			3/1995

	Revision: 6/10/96 - Modified function mediaDevice_closeCb to correct PR 942.

*******************************************************************************/

#include <stdio.h>
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

#define _IMS_OP_MEDIADEVICECB_C
#include "ims_opCb.h"

/* 
** Local Functions 
*/
static void free_deviceInfoList(DEVICE_INFO *);

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opMediaDevice.h>
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: mediaDevice_create_deviceListCb
**
** Description:		Create callback to create device entries in Media Device 
**								Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void mediaDevice_create_deviceListCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	_UxCmediaDevice         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int i;
	Pixel Label_border;
	Dimension	 PB_height, PB_width, Label_height, Label_width;
	Dimension	 ST_height, ST_width, Label_borderWidth;
	Dimension	 PB_marginLeft, PB_marginRight, PB_marginWidth;
	Position	 PB_xcoord, Avail_xcoord, Alloc_xcoord;
	Position	 JobStart_xcoord, Qual_xcoord, JobDone_xcoord;
	Position	 JobFail_xcoord, Offline_xcoord, ST_xcoord;
	Position 	 PB_ycoord, Label_ycoord, ST_ycoord;
	Dimension  PB_shadowThickness;
	Pixel      PB_background, LBL_background, ST_background;
	XmString   label, Label_string;
	char widget_name[IMS_COL30_LEN+1];
	XmFontList PB_fontList, LBL_fontList, ST_fontList;
	OP_CLIENT_DATA *clientData;
	char buffer[IMS_COL255_LEN+1];
	Widget devicePB, labelW, scrollTextW; 
	Arg args[IMS_COL30_LEN];

	UxSaveCtx = UxMediaDeviceContext;
	UxMediaDeviceContext = UxContext =
			(_UxCmediaDevice *) UxGetContext( UxWidget );
{
	/* Get resources from ftpPB */
	XtVaGetValues(ftpPB,
					XmNfontList, &PB_fontList,
					XmNbackground, &PB_background,
					XmNshadowThickness, &PB_shadowThickness,
					XmNheight, &PB_height,
					XmNwidth, &PB_width,
					XmNmarginLeft, &PB_marginLeft,
					XmNmarginRight, &PB_marginRight,
					XmNmarginWidth, &PB_marginWidth,
					XmNx, &PB_xcoord,
					XmNy, &PB_ycoord,
					NULL);

	/* Get resources from Available label */
	XtVaGetValues(ftpAvailLBL,
					XmNbackground, &LBL_background,
					XmNx, &Avail_xcoord,
					XmNy, &Label_ycoord,
					XmNheight, &Label_height,
					XmNwidth, &Label_width,
					XmNlabelString, &Label_string,
					XmNborderColor, &Label_border,
					XmNborderWidth, &Label_borderWidth,
					NULL);

	/* Get resources from Allocated label */
	XtVaGetValues(ftpAllocLBL,
					XmNx, &Alloc_xcoord,
					NULL);

	/* Get resources from JobStarted label */
	XtVaGetValues(ftpJobStartLBL,
					XmNx, &JobStart_xcoord,
					NULL);

	/* Get resources from QualCheck label */
	XtVaGetValues(ftpQualLBL,
					XmNx, &Qual_xcoord,
					NULL);

	/* Get resources from JobDone label */
	XtVaGetValues(ftpJobDoneLBL,
					XmNx, &JobDone_xcoord,
					NULL);

	/* Get resources from Jobfail label */
	XtVaGetValues(ftpJobFailLBL,
					XmNx, &JobFail_xcoord,
					NULL);

	/* Get resources from Offline label */
	XtVaGetValues(ftpOfflineLBL,
					XmNx, &Offline_xcoord,
					NULL);

	/* Get resources from Msg Scrolled Text */
	XtVaGetValues(ftpST,
					XmNfontList, &ST_fontList,
					XmNbackground, &ST_background,
					XmNheight, &ST_height,
					XmNwidth, &ST_width,
					NULL);

	/* Get resources from Msg Scrolled Window */
	XtVaGetValues(ftpSW,
					XmNx, &ST_xcoord,
					XmNy, &ST_ycoord,
					NULL);
  /* 
	** Unmanage the following dummy wigets.
	** Create all entries for this screen based on 
	** values retrieved from device_policy table.
	*/
	XtUnmanageChild (ftpPB);
	XtUnmanageChild (ftpAvailLBL);
	XtUnmanageChild (ftpAllocLBL);
	XtUnmanageChild (ftpJobStartLBL);
	XtUnmanageChild (ftpQualLBL);
	XtUnmanageChild (ftpJobDoneLBL);
	XtUnmanageChild (ftpJobFailLBL);
	XtUnmanageChild (ftpOfflineLBL);
	XtUnmanageChild (ftpST);
	XtUnmanageChild (ftpSW);

	for (i = 0; i < glbData.device_list_count; i++)
	{
		/* create devicePB */
		sprintf (buffer, "%s Device %d", glbData.device_list[i].item_name,
						 glbData.device_list[i].item_id);

		label = XmStringCreateLocalized (buffer);

		sprintf (widget_name, "devicePB%d", glbData.device_list[i].item_id);

		devicePB = XtVaCreateManagedWidget 
								(widget_name, xmPushButtonWidgetClass, mediaRC,
								 XmNlabelString, label,
								 XmNfontList, PB_fontList,
								 XmNbackground, PB_background,
								 XmNshadowThickness, PB_shadowThickness,
								 XmNheight, PB_height,
								 XmNwidth, PB_width,
								 XmNx, PB_xcoord,
								 XmNy, PB_ycoord,
								 XmNrecomputeSize, False,
								 XmNmarginLeft, PB_marginLeft,
			 					 XmNmarginRight, PB_marginRight,
								 XmNmarginWidth, PB_marginWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].device = devicePB;

		XmStringFree (label);


		/* create Available Label */
		sprintf (widget_name, "availLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, Avail_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].available = labelW;


		/* create Allocated Label */
		sprintf (widget_name, "allocLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, Alloc_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].allocated = labelW;


		/* create Job Started Label */
		sprintf (widget_name, "jobStartLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, JobStart_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].jobStart = labelW;


		/* create Qual Check Label */
		sprintf (widget_name, "qualLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, Qual_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].qualCheck = labelW;

		/* create Job Done Label */
		sprintf (widget_name, "jobDoneLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, JobDone_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].jobDone = labelW;


		/* create Job Fail Label */
		sprintf (widget_name, "jobFailLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, JobFail_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].jobFail = labelW;


		/* create Offline Label */
		sprintf (widget_name, "offlineLBL%d", glbData.device_list[i].item_id);

		labelW = XtVaCreateManagedWidget 
								(widget_name, xmLabelWidgetClass, mediaRC,
								 XmNbackground, LBL_background,
								 XmNx, Offline_xcoord,
								 XmNy, Label_ycoord,
							 	 XmNheight, Label_height,
								 XmNwidth, Label_width,
					       XmNlabelString, Label_string,
								 XmNrecomputeSize, False,
								 XmNborderColor, Label_border,
								 XmNborderWidth, Label_borderWidth,
								 NULL);
    glbData.deviceStats[glbData.device_list[i].item_id].offLine = labelW;


		/* create message scrolled text */
		sprintf (widget_name, "msgText%d", glbData.device_list[i].item_id);

		XtSetArg (args[0], XmNfontList, ST_fontList);
		XtSetArg (args[1], XmNbackground, ST_background);
		XtSetArg (args[2], XmNx, ST_xcoord);
		XtSetArg (args[3], XmNy, ST_ycoord);
		XtSetArg (args[4], XmNheight, ST_height);
		XtSetArg (args[5], XmNwidth, ST_width);
		XtSetArg (args[6], XmNscrollBarDisplayPolicy, XmSTATIC);
		XtSetArg (args[7], XmNscrollingPolicy, XmAPPLICATION_DEFINED);
		XtSetArg (args[8], XmNvisualPolicy, XmVARIABLE);
		XtSetArg (args[9], XmNscrollHorizontal, False);
		XtSetArg (args[10], XmNscrollVertical, True);
		XtSetArg (args[11], XmNeditMode, XmMULTI_LINE_EDIT );
		XtSetArg (args[12], XmNeditable, False);
		XtSetArg (args[13], XmNcursorPositionVisible, False);
		XtSetArg (args[14], XmNwordWrap, True);

		scrollTextW = XmCreateScrolledText (mediaRC, widget_name, args, 15); 
		XtManageChild (scrollTextW);

    glbData.deviceStats[glbData.device_list[i].item_id].msgText = scrollTextW;

		/* calculate coordinates */
		PB_ycoord = PB_ycoord + 140;
		Label_ycoord = Label_ycoord + 140;
		ST_ycoord = ST_ycoord + 140;

	}

}
 UxMediaDeviceContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: mediaDevice_closeCb
**
** Description:		Callback function for the Close button in Media Device 
**								Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - changed to XtPopdown (glbData.mediaW)
**
**==========================================================================*/

void mediaDevice_closeCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	int i;
	OP_CLIENT_DATA *clientData;
	char Msg[IMS_COL1024_LEN+1];

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);

	if (clientData->mediaJobList == (OP_MEDIA_JOB_LIST *)NULL)
	{
		(void) free_deviceInfoList (clientData->deviceInfoList);
		clientData->deviceInfoList = NULL;

		/* clear up all labels and text wigets */
		for (i = 0; i < glbData.device_list_count; i++) 
		{
			(void) order_clearDeviceLabel
						(glbData.deviceStats[i].available, i);

			XmTextSetString
				(glbData.deviceStats[glbData.device_list[i].item_id].msgText, "");
		}

		XtPopdown (XtParent(glbData.mediaW));
		glbData.mediaFlag = 0;
	}
	else
	{
		/* There are media jobs running, cannot pop down screen */
		strcpy (Msg, "Media jobs are in process, cannot close screen."); 
		msgBoxDlg_popupCb (glbData.mediaW, IMS_INFO, Msg); 
		return;
	}

}


/*===========================================================================*
** 
** Function Name:	free_deviceInfoList
**
** Description:		Function to free the device info list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_deviceInfoList(
	DEVICE_INFO *devicePtr) 
{
	DEVICE_INFO *nextPtr;

	while (devicePtr != (DEVICE_INFO *)NULL)
	{
		nextPtr = devicePtr->next;
		free (devicePtr);
		devicePtr = nextPtr;
	}

	return;
}

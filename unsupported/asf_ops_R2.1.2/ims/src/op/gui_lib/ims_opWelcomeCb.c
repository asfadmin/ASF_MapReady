static char *sccs = "@(#)ims_opWelcomeCb.c	5.5  06/27/97";
/*******************************************************************************

	File:			ims_opWelcomeCb.c

	Function:	Callback functions for welcome screen

	Author:		Jennifer Ting

	Date:			3/1995

	Revision: 6/10/1996 - Modified function welcome_create_op_interfaces()
												to correct PR 942.
					  7/8/1996 -  Modified function welcome_orderCb to modify the
												default query for sys_user type to return only
												orders with status values NEW and PENDING.

            7/19/1996 - Modified function welcome_create_op_interfaces
												for PR 986 - display the following:
                        Server/Database/User Id/User Type
												on each major screen.

            9/09/1996 - Modified function welcome_orderCb for PR 85,
												the default search query for users are modified.
												Also, a new command line option "-s" is added
												to indicate execution of default order search
												query is desired, if the flag is absence, a
												blank Order Production Screen will pop up.

            3/26/1997 - added creation of dlSearchW and dl2dtkW screens
												and welcome_downlinkCb().

				    6/12/1997 - Added access permission for downlink management

*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
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
#include <Xm/FileSB.h>

#define _IMS_OP_WELCOMECB_C
#include "ims_opCb.h"
#include <ims_op_accCb.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;
extern int glbDefaultSearchFlag;
extern int glbWelcomeOrderFlag;


/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS
#include <ims_opWelcome.h>
#undef CONTEXT_MACRO_ACCESS

#include <ims_opSearch.h>
#include <ims_opOrder.h>
#include <ims_opMsgBoxDlg.h>
#include <ims_opPhotoOrder.h>
#include <ims_opPhotoJob.h>
#include <ims_opMediaDevice.h>
#include <ims_opFilmGen.h>
#include <ims_opShipping.h>
#include <ims_opShipView.h>
#include <ims_opBilling.h>
#include <ims_opBillView.h>
#include <ims_opDLtoDTKsrch.h>
#include <ims_opDLtoDTK.h>

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
**
** Function Name: welcome_create_op_interfaces
**
** Description:		Creates all the forms for Operator Interface.  This
**								function is registered as a CreateCallback for the
**								welcome screen. It is activated in _Uxbuild_welcome().
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
**
** Return Value: 	None
**
** Revision History: 07/19/96 - modified to display userSpec info on title
**                              bar for each major screen - PR 986.
**
**==========================================================================*/

void welcome_create_op_interfaces(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{
	_UxCorder  *UxOrderContext;
	_UxCphotoOrder *UxPhotoOrderContext;
	_UxCphotoJob *UxPhotoJobContext;
	_UxCfilmGeneration *UxFilmGenerationContext;
	_UxCDLtoDTK  *UxDLtoDTKContext;
	Widget order_sb, item_sb, photo_sb, film_sb, dl_sb, dtk_sb;
	OP_CLIENT_DATA *clientData;
	OP_PHOTO_CLIENT_DATA *photoClientData;
	OP_FILM_CLIENT_DATA *filmClientData;
	OP_DL2DTK_DATA *dlclientData;
	char userName[IMS_COL30_LEN+1];
	char server[IMS_COL30_LEN+1];
	char database[IMS_COL30_LEN+1];
	int k;
	char buffer[IMS_COL128_LEN+1];

	/*
  ** get userName, server, database to print on
	** screen titles.
	*/
	(void) strcpy (userName, glbData.userSpec.userName);

	if (glbData.userSpec.server != (char *)NULL)
	{
		(void) strcpy (server, glbData.userSpec.userName);
	}
	else
	 if (getenv ("IMS_SERVER") != NULL)
	 {
			(void) strcpy (server, getenv ("IMS_SERVER"));
	 }

	if (glbData.userSpec.dbName != (char *)NULL)
	{
		(void) strcpy (database, glbData.userSpec.userName);
	}
	else
	 if (getenv ("IMS_DB") != NULL)
	 {
			(void) strcpy (database, getenv ("IMS_DB"));
	 }


	/* Create Operator Interface screens */
	glbData.searchW 		= create_search(NO_PARENT) ;
	glbData.orderW 			= create_order(NO_PARENT) ;
	glbData.msgDlgW			=	create_msgBoxDlg (NO_PARENT);
	glbData.photoOrderW = create_photoOrder (NO_PARENT);
	glbData.photoJobW 	= create_photoJob (NO_PARENT);
	glbData.mediaW 			= create_mediaDevice (NO_PARENT);
	glbData.filmW  			= create_filmGeneration (NO_PARENT);
	glbData.shippingW 	= create_shipping(NO_PARENT);
	glbData.shipViewW 	= create_shipView(NO_PARENT);
	glbData.billingW 		= create_billing(NO_PARENT);
	glbData.billViewW 	= create_billView(NO_PARENT);
	glbData.dlSearchW 	= create_DLtoDTKsearch(NO_PARENT) ;
	glbData.dl2dtkW 		= create_DLtoDTK(NO_PARENT) ; 

        /*** Accounts and Users ***/
	/* Callbacks for the scrollbars are assigned when they are created */
        accounts_users_create_interfacesCb( );


	/* Initialize orderConnectFlag here */
	clientData = &glbData.orderClientData;
	clientData->orderConnectFlag = 0;

	/* Initialize photoConnectFlag here */
	photoClientData = &glbData.photoClientData;
	photoClientData->photoConnectFlag = 0;

	/* Initialize filmConnectFlag here */
	filmClientData = &glbData.filmClientData;
	filmClientData->filmConnectFlag = 0;

	/* Initialize dlConnectFlag here */
	dlclientData = &glbData.dlClientData;
	dlclientData->dlConnectFlag = 0;


	/* Create toggles for checkboxes in the Search Screens */
	search_create_checkbox_togglesCb(glbData.searchW, cd, cb);
	dl_search_create_checkbox_togglesCb( glbData.dlSearchW, cd, cb ) ;
	photoJob_create_optionMenuCb (glbData.photoJobW, cd, cb);
	filmGen_create_optionMenuCb (glbData.filmW, cd, cb);

	/* Create list of devices in the Media Device Status Screen */
	mediaDevice_create_deviceListCb (glbData.mediaW, cd, cb);

	/* Attach callback to order and item list scrollbars in Order Screen */
	UxOrderContext =
			(_UxCorder *) UxGetContext(glbData.orderW);

		XtVaGetValues(UxOrderContext->UxorderDummySW,
									XmNverticalScrollBar, &order_sb, NULL);
		XtAddCallback(order_sb, XmNvalueChangedCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNdragCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNincrementCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNdecrementCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNpageIncrementCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNpageDecrementCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNtoTopCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);
		XtAddCallback(order_sb, XmNtoBottomCallback,
									(XtCallbackProc)order_scroll_orderListsCb, NULL);


		XtVaGetValues(UxOrderContext->UxitemDummySW,
									XmNverticalScrollBar, &item_sb, NULL);
		XtAddCallback(item_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNdragCallback,
									(XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNincrementCallback,
									(XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNdecrementCallback,
									(XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNpageIncrementCallback,
									(XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNtoTopCallback,
									(XtCallbackProc)order_scroll_itemListsCb, NULL);
		XtAddCallback(item_sb, XmNtoBottomCallback,
									(XtCallbackProc)order_scroll_itemListsCb, NULL);


	/* Attach callback to queue list scrollbars in Photo Order Screen */
	UxPhotoOrderContext =
			(_UxCphotoOrder *) UxGetContext(glbData.photoOrderW);

		XtVaGetValues(UxPhotoOrderContext->UxdummySW1,
									XmNverticalScrollBar, &photo_sb, NULL);
		XtAddCallback(photo_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNdragCallback,
									(XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNincrementCallback,
									(XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNdecrementCallback,
									(XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageIncrementCallback,
									(XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoTopCallback,
									(XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoBottomCallback,
									(XtCallbackProc)photoOrder_scroll_queueListsCb, NULL);


		XtVaGetValues(UxPhotoOrderContext->UxdummySW2,
									XmNverticalScrollBar, &photo_sb, NULL);
		XtAddCallback(photo_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNdragCallback,
									(XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNincrementCallback,
									(XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNdecrementCallback,
									(XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageIncrementCallback,
									(XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoTopCallback,
									(XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoBottomCallback,
									(XtCallbackProc)photoOrder_scroll_jobListsCb, NULL);


	/* Attach callback to list scrollbars in Photo Job Screen */
	UxPhotoJobContext =
			(_UxCphotoJob *) UxGetContext(glbData.photoJobW);

		XtVaGetValues(UxPhotoJobContext->UxphotoJobDummySW1,
									XmNverticalScrollBar, &photo_sb, NULL);
		XtAddCallback(photo_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNdragCallback,
									(XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNincrementCallback,
									(XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNdecrementCallback,
									(XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageIncrementCallback,
									(XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoTopCallback,
									(XtCallbackProc)photoJob_scroll_jobListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoBottomCallback,
									(XtCallbackProc)photoJob_scroll_jobListsCb, NULL);


		XtVaGetValues(UxPhotoJobContext->UxphotoJobDummySW2,
									XmNverticalScrollBar, &photo_sb, NULL);
		XtAddCallback(photo_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNdragCallback,
									(XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNincrementCallback,
									(XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNdecrementCallback,
									(XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageIncrementCallback,
									(XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoTopCallback,
									(XtCallbackProc)photoJob_scroll_queueListsCb, NULL);
		XtAddCallback(photo_sb, XmNtoBottomCallback,
									(XtCallbackProc)photoJob_scroll_queueListsCb, NULL);


	/* Attach callback to list scrollbars in Film Generation Screen */
	UxFilmGenerationContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);

		XtVaGetValues(UxFilmGenerationContext->UxfireDummySW,
									XmNverticalScrollBar, &film_sb, NULL);
		XtAddCallback(film_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNdragCallback,
									(XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNincrementCallback,
									(XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNdecrementCallback,
									(XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNpageIncrementCallback,
									(XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNtoTopCallback,
									(XtCallbackProc)filmGen_scroll_fireListsCb, NULL);
		XtAddCallback(film_sb, XmNtoBottomCallback,
									(XtCallbackProc)filmGen_scroll_fireListsCb, NULL);

		XtVaGetValues(UxFilmGenerationContext->UxttdlDummySW,
									XmNverticalScrollBar, &film_sb, NULL);
		XtAddCallback(film_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNdragCallback,
									(XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNincrementCallback,
									(XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNdecrementCallback,
									(XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNpageIncrementCallback,
									(XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNtoTopCallback,
									(XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);
		XtAddCallback(film_sb, XmNtoBottomCallback,
									(XtCallbackProc)filmGen_scroll_ttdlListsCb, NULL);

		XtVaGetValues(UxFilmGenerationContext->UxlaserDummySW,
									XmNverticalScrollBar, &film_sb, NULL);
		XtAddCallback(film_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNdragCallback,
									(XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNincrementCallback,
									(XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNdecrementCallback,
									(XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNpageIncrementCallback,
									(XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNtoTopCallback,
									(XtCallbackProc)filmGen_scroll_laserListsCb, NULL);
		XtAddCallback(film_sb, XmNtoBottomCallback,
									(XtCallbackProc)filmGen_scroll_laserListsCb, NULL);

	/* Attach callback to dl and dtk list scrollbars in downllink Screen */
	UxDLtoDTKContext =
			(_UxCDLtoDTK *) UxGetContext(glbData.dl2dtkW);

		XtVaGetValues(UxDLtoDTKContext->UxdlDummySW,
									XmNverticalScrollBar, &dl_sb, NULL);
		XtAddCallback(dl_sb, XmNvalueChangedCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNdragCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNincrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNdecrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNpageIncrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNpageDecrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNtoTopCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);
		XtAddCallback(dl_sb, XmNtoBottomCallback,
									(XtCallbackProc)dl2dtk_scroll_dlListsCb, NULL);

		XtVaGetValues(UxDLtoDTKContext->UxdtkDummySW,
									XmNverticalScrollBar, &dtk_sb, NULL);
		XtAddCallback(dtk_sb, XmNvalueChangedCallback,
								  (XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNdragCallback,
									(XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNincrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNdecrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNpageIncrementCallback,
									(XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNpageDecrementCallback,
								  (XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNtoTopCallback,
									(XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);
		XtAddCallback(dtk_sb, XmNtoBottomCallback,
									(XtCallbackProc)dl2dtk_scroll_dtkListsCb, NULL);

		/*
		** This is to add the callbacks to the window manager quit
		** button for each screen, this is to correct PR 942
		*/
		addWinMgrCloseCB (widget, welcome_exitCb, NULL);
		addWinMgrCloseCB (glbData.searchW, search_closeCb, NULL);
		addWinMgrCloseCB (glbData.orderW, order_closeCb, NULL);
		/*
		addWinMgrCloseCB (glbData.msgDlgW, , NULL);
		*/
		addWinMgrCloseCB (glbData.photoOrderW, photoOrder_closeCb, NULL);
		addWinMgrCloseCB (glbData.photoJobW, photoJob_closeCb, NULL);
		addWinMgrCloseCB (glbData.mediaW, mediaDevice_closeCb, NULL);
		addWinMgrCloseCB (glbData.filmW, filmGen_closeCb, NULL);
		addWinMgrCloseCB (glbData.shippingW, shipping_cancelFormCb, NULL);
		addWinMgrCloseCB (glbData.shipViewW, shipView_closeCb, NULL);
		addWinMgrCloseCB (glbData.billingW, billing_cancelFormCb, NULL);
		addWinMgrCloseCB (glbData.billViewW, billView_closeCb, NULL);
		addWinMgrCloseCB (glbData.dlSearchW, dl_search_closeCb, NULL);
		addWinMgrCloseCB (glbData.dl2dtkW, dl2dtk_closeCb, NULL);

		/*
		** 07/19/1996 - PR 986 display the following information on each
		** major screen in IMS_OP: SQL Server/Database/User Id/User Type
		*/

		/* map user_type */

		k = 0;
		while ((k < glbData.op_user_type_count) &&
			 (glbData.userSpec.userType != glbData.op_user_type[k].item_id))
			  k++;
	  /*
		** no checking is done here because if a user does not have
		** user type value, it is assigned as GENERAL in the main program.
		** see ims_op.c validateUser().
		*/

		(void) sprintf (buffer, "Welcome Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(widget), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Order Search Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.searchW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Order Production Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.orderW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Create Film TTDL Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.filmW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Create Photo Jobs Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.photoOrderW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Complete Photo Jobs Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.photoJobW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Media Device Status Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.mediaW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Accounts and Users Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.accounts_usersW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Assign Users Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.assign_usersW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Assign Datasets Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.assign_datasetsW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Downlink Search Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.dlSearchW), XmNtitle, buffer, NULL);

		(void) sprintf (buffer, "Downlink to Data-take Screen  (%s/%s/%s/%s)",
						 server, database, userName, glbData.op_user_type[k].item_name);
 	  XtVaSetValues (XtParent(glbData.dl2dtkW), XmNtitle, buffer, NULL);

}


/*===========================================================================*
**
** Function Name: welcome_orderCb
**
** Description:		This function manages and pops up the order screen,
**								also opens database connection and initializes
**								glbData.orderClientData structure.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
**
** Return Value: 	None
**
** Revision History: 07/08/96 - modified sys_user default query to return
**                              only orders with status NEW and PENDING.
**
**                   09/09/96 - modified op users default search queries:
**                	            1. PROD_USER - return orders with items
**                 		             of IN-FILM-Q and IN-MEDIA-Q status.
**                              2. GDC_USER - return orders with items
**                                 of status NEW except for TDR and TSR
**                                 order_item_type entries.
**                              3. SYS_USER - the default order search for
**                                 system user should be identical to the
**                                 default search for the PROD_USER.
**
**==========================================================================*/

/* ARGSUSED0 */
void	welcome_orderCb(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{
	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_CLIENT_DATA *clientData;
	IMS_MSG_STRUCT *msgDesc;
	int status;
	char Msg[IMS_COL1024_LEN+1];

	if (glbData.orderFlag)
	{
		XtPopup(XtParent(glbData.orderW), XtGrabNone);
		return;
	}
	else
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** Initialize 
		*/
		clientData = &glbData.orderClientData;
		clientData->orderList = (OP_ORDER_LIST *)NULL;
		clientData->orderCount = 0;
		clientData->currOrder = (OP_ORDER_LIST *)NULL;

		/*
		** initialize mediaJobList and deviceInfoList 
		** only if Media Device Status screen is not up.
		** this allows the user to close down the Order
		** Display screen without interferring the Media
		** Job Processing.  
		*/
		if (!glbData.mediaFlag)
		{
			clientData->mediaJobList = (OP_MEDIA_JOB_LIST *)NULL;
			clientData->deviceInfoList = (DEVICE_INFO *)NULL;
		}

		if (!clientData->orderConnectFlag)
		{
			/*
			** First time pop up the Order Display Screen, 
			** do a database open connection here.
			*/ 
		
			msgDesc = glbData.msgDesc;

			catReq = &(clientData->catReq);
			catReq->msgDesc = glbData.msgDesc;
			catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;

			userSpec = &(catReq->userSpec);
			userSpec->dbUserName = glbData.userSpec.userName;
			userSpec->dbPassword = glbData.userSpec.password;
			userSpec->server = glbData.userSpec.server;
			userSpec->dbName = glbData.userSpec.dbName;
			userSpec->program = glbData.program;

			if ((status = ims_opCat (catReq, OP_OPENCONNECTION)) < IMS_OK)
			{

				/* Change cursor back to normal */
				timeOutCursors (False);

				sprintf(Msg, "Database Connection Failed!");   
				msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg); 
				return;
			}
			else
				clientData->orderConnectFlag = 1;

		}

		if (glbDefaultSearchFlag)
		{
			/*
			** set up default search query based on operator type
			*/
			switch (glbData.userSpec.userType)
			{
				case OP_GDC:
					strcpy (clientData->queryStruct.sqlBuf, 
						"select distinct t1.order_id, "
						"t1.user_id, "
						"t1.account_id, "
						"convert (char(20), t1.received_time, 100), "
						"convert (char(20), t1.completed_time, 100), "
						"t1.priority, "
						"t1.item_count, " 
						"t1.status, "
						"t1.op_comment "
						"from order_queue t1, order_item t2 where "
						"t1.order_id = t2.order_id and "
						"t2.status = 1 and "
						"t2.order_item_type not in (5, 6) "
						"order by order_id");
				break;

				case OP_PRODUCTION:
					strcpy (clientData->queryStruct.sqlBuf, 
						"select distinct t1.order_id, "
						"t1.user_id, "
						"t1.account_id, "
						"convert (char(20), t1.received_time, 100), "
						"convert (char(20), t1.completed_time, 100), "
						"t1.priority, "
						"t1.item_count, " 
						"t1.status, "
						"t1.op_comment "
						"from order_queue t1, order_item t2 where "
						"t1.order_id = t2.order_id and "
						"(t2.status  = 5 or t2.status = 8) "
						"order by order_id");
				break;

				case OP_SYSTEM:
					strcpy (clientData->queryStruct.sqlBuf, 
						"select distinct t1.order_id, "
						"t1.user_id, "
						"t1.account_id, "
						"convert (char(20), t1.received_time, 100), "
						"convert (char(20), t1.completed_time, 100), "
						"t1.priority, "
						"t1.item_count, " 
						"t1.status, "
						"t1.op_comment "
						"from order_queue t1, order_item t2 where "
						"t1.order_id = t2.order_id and "
						"(t2.status  = 5 or t2.status = 8) "
						"order by order_id");
					break;

				default:
					strcpy (clientData->queryStruct.sqlBuf, 
						"select distinct t1.order_id, "
						"t1.user_id, "
						"t1.account_id, "
						"convert (char(20), t1.received_time, 100), "
						"convert (char(20), t1.completed_time, 100), "
						"t1.priority, "
						"t1.item_count, " 
						"t1.status, "
						"t1.op_comment "
						"from order_queue t1, order_item t2 where "
						"t1.order_id = t2.order_id and "
						"t2.status = 1 and "
						"t2.order_item_type not in (5, 6) "
						"order by order_id");
					break;
			}

			if ((status = search_executeQuery(glbData.searchW)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

		} /* if (glbDefaultSearch) */

		/*
		** set glbWelcomeOrderFlag to indicate the query is from the 
		** welcome_order screen 
		*/
		glbWelcomeOrderFlag = 1;

		/* Popup order screen with results from the query */
		order_displayResults (glbData.orderW);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}

}


/*===========================================================================*
**
** Function Name: welcome_photoCb
**
** Description:		This function manages and pops up the order screen,
**								also opens database connection and initializes
**								glbData.orderClientData structure.
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

/* ARGSUSED0 */
void	welcome_photoCb(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{
	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_PHOTO_CLIENT_DATA *clientData;
	IMS_MSG_STRUCT *msgDesc;
	int status;
	char Msg[IMS_COL1024_LEN+1];

	/*
	** 1/5/96 - verify glbData.userType, if userType is not
	** OP_GDC or OP_SYSTEM, cannot access the screens.
	*/
	if ((glbData.userSpec.userType != OP_GDC) &&
			(glbData.userSpec.userType != OP_SYSTEM))
	{
		(void) sprintf(Msg, "Access Photo Management Screens: Permission denied.");
		msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
		return;
	}

	/* Temporary till we decide how to pop up the photo screens */

	if (glbData.photoOrderFlag)
	{
		XtPopup(XtParent(glbData.photoOrderW), XtGrabNone);
		return;
	}
	else
		if (glbData.photoJobFlag)
		{
			XtPopup(XtParent(glbData.photoJobW), XtGrabNone);
			return;
		}

	/*
	** If both Order and Job screens are not up
	*/
	if (!glbData.photoOrderFlag && !glbData.photoJobFlag)
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** Initialize
		*/
		clientData = &glbData.photoClientData;
		clientData->photoQueueList = (OP_PHOTO_QUEUE_LIST *)NULL;
		clientData->photoQSelectList = (OP_PHOTO_QUEUE_LIST *)NULL;
		clientData->photoJobList = (OP_PHOTO_JOB_LIST *)NULL;

			/*
			** First time pop up the Photo Screen,
			** do a database open connection here.
			*/

		if (!clientData->photoConnectFlag)
		{
			/*
			** First time pop up the Photo Screen,
			** do a database open connection here.
			*/
			msgDesc = glbData.msgDesc;

			catReq = &(clientData->catReq);
			catReq->msgDesc = glbData.msgDesc;
			catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;

			userSpec = &(catReq->userSpec);
			userSpec->dbUserName = glbData.userSpec.userName;
			userSpec->dbPassword = glbData.userSpec.password;
			userSpec->server = glbData.userSpec.server;
			userSpec->dbName = glbData.userSpec.dbName;
			userSpec->program = glbData.program;

			if ((status = ims_opCat (catReq, OP_OPENCONNECTION)) < IMS_OK)
			{

				/* Change cursor back to normal */
				timeOutCursors (False);

				(void) sprintf(Msg, "Database Connection Failed!");
				msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
				return;
			}
			else
				clientData->photoConnectFlag = 1;
		}

		XtPopup(XtParent(glbData.photoOrderW), XtGrabNone);
		glbData.photoOrderFlag = 1;

		/* Change cursor back to normal */
		timeOutCursors (False);
	}
	return;
}


/*===========================================================================*
**
** Function Name: welcome_filmCb
**
** Description:		This function manages and pops up the film screen,
**								also opens database connection and initializes
**								glbData.orderClientData structure.
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

/* ARGSUSED0 */
void	welcome_filmCb(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{
	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_FILM_CLIENT_DATA *clientData;
	IMS_MSG_STRUCT *msgDesc;
	int status;
	char Msg[IMS_COL1024_LEN+1];


	/*
	** 1/5/96 - verify glbData.userType, if userType is not
	** OP_PRODUCTION or OP_SYSTEM, cannot access the screens.
	*/
	if ((glbData.userSpec.userType != OP_PRODUCTION) &&
			(glbData.userSpec.userType != OP_SYSTEM))
	{
		(void) sprintf(Msg, "Access Film Management Screens: Permission denied.");
		msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
		return;
	}

	if (glbData.filmFlag)
	{
		XtPopup(XtParent(glbData.filmW), XtGrabNone);
		return;
	}
	else
	{
		/*
		** If Film Generation Screen is not up
		*/

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** Initialize
		*/
		clientData = &glbData.filmClientData;
		clientData->fireQueueList = (OP_FIRE_QUEUE_LIST *)NULL;
		clientData->laserQueueList = (OP_LASER_QUEUE_LIST *)NULL;
		clientData->ttdlQueueList = (OP_TTDL_QUEUE_LIST *)NULL;

		if (!clientData->filmConnectFlag)
		{
			/*
			** First time pop up the Film Generation Screen,
			** do a database open connection here.
			*/
			msgDesc = glbData.msgDesc;

			catReq = &(clientData->catReq);
			catReq->msgDesc = glbData.msgDesc;
			catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;

			userSpec = &(catReq->userSpec);
			userSpec->dbUserName = glbData.userSpec.userName;
			userSpec->dbPassword = glbData.userSpec.password;
			userSpec->server = glbData.userSpec.server;
			userSpec->dbName = glbData.userSpec.dbName;
			userSpec->program = glbData.program;

			if ((status = ims_opCat (catReq, OP_OPENCONNECTION)) < IMS_OK)
			{

				/* Change cursor back to normal */
				timeOutCursors (False);

				(void) sprintf(Msg, "Database Connection Failed!");
				msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
				return;
			}
			else
				clientData->filmConnectFlag = 1;
		}

		XtPopup(XtParent(glbData.filmW), XtGrabNone);
		glbData.filmFlag = 1;

		/* Change cursor back to normal */
		timeOutCursors (False);
	}

	return;
}


/*===========================================================================*
**
** Function Name: welcome_accountCb
**
** Description:		This function manages and pops up the accounts_users
**			screen, also opens database connection and initializes
**                      glbData.accounts_users_data.connect_flag and all the
**			accounts and users data.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cb
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/

/* ARGSUSED0 */
void	welcome_accountCb(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{

	char Msg[IMS_COL1024_LEN+1];

	/*
	** 1/5/96 - verify glbData.userType, if userType is not
	** OP_GDC or OP_SYSTEM, cannot access the screens.
	*/
	if ((glbData.userSpec.userType != OP_GDC) &&
			(glbData.userSpec.userType != OP_SYSTEM))
	{
		(void) sprintf(Msg,
				"Access Account Management Screens: Permission denied.");
		msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
		return;
	}

	accounts_users_init_popCb (glbData.accounts_usersW, NULL, NULL ) ;


}


/*===========================================================================*
**
** Function Name: welcome_downlinkCb
**
** Description:		This function manages and pops up the downlink-to-dtk
**								screen, also opens database connection and initializes
**								glbData.dlClientData structure.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cb			- not used
**
** Return Value: 	None
**
** Revision History:
**
**==========================================================================*/
/* ARGSUSED0 */
void
welcome_downlinkCb(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{
	OP_CAT_STRUCT			*catReq;
	OP_CAT_USERSPEC		*userSpec;
	OP_DL2DTK_DATA		*clientData;
	IMS_MSG_STRUCT		*msgDesc;
	char							Msg[IMS_COL1024_LEN+1];

	/* TEST
	** 6/11/96 - verify glbData.userType, if userType is not
	** OP_GDC or OP_SYSTEM, cannot access the screens.
	*/
	if ((glbData.userSpec.userType != OP_GDC) &&
			(glbData.userSpec.userType != OP_SYSTEM))
	{
		(void) sprintf(Msg, "Access Downlink Management Screens: Permission denied.");
		msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
		return;
	}

	if (glbData.dlSearchFlag)
	{
		XtPopup(XtParent(glbData.dl2dtkW), XtGrabNone) ;
		return ;
	}
	else
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** Initialize
		*/
		clientData					= &glbData.dlClientData;
		clientData->dlList	= (OP_DL_LIST *)NULL;
		clientData->dlCount	= 0;
		clientData->currDL	= (OP_DL_LIST *)NULL;

		if (!clientData->dlConnectFlag)
		{
			/*
			** First time pop up the downlink-to-dtk Display Screen,
			** do a database open connection here.
			*/

			msgDesc = glbData.msgDesc;

			catReq					= &(clientData->catReq);
			catReq->msgDesc	= glbData.msgDesc;
			catReq->qDesc		= (IMS_QI_DESC_OBJ *)NULL;

			userSpec							= &(catReq->userSpec);
			userSpec->dbUserName	= glbData.userSpec.userName;
			userSpec->dbPassword	= glbData.userSpec.password;
			userSpec->server			= glbData.userSpec.server;
			userSpec->dbName			= glbData.userSpec.dbName;
			userSpec->program			= glbData.program;

			if (ims_opCat (catReq, OP_OPENCONNECTION) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				(void) sprintf(Msg, "Database Connection Failed!");
				msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
				return;
			}
			else
				clientData->dlConnectFlag = 1;
		}

		/* Popup downlink-to-dtk screen */
		XtPopup(XtParent(glbData.dl2dtkW), XtGrabNone) ;

		/* Change cursor back to normal */
		timeOutCursors (False);
	}

	return ;
}


/*===========================================================================*
**
** Function Name: welcome_exitCb
**
** Description:		exit Operator Interface program
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

/* ARGSUSED0 */
void	welcome_exitCb(
	Widget widget,
	XtPointer cd,
	XtPointer cb)
{
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_PHOTO_CLIENT_DATA *photoClientData;
	OP_FILM_CLIENT_DATA *filmClientData;
	OP_DL2DTK_DATA *dlClientData;
	char Msg[IMS_COL1024_LEN+1];
        OP_ACC_USR_DATA *acc_usr_data_ptr ;

	clientData = &glbData.orderClientData;
	photoClientData = &glbData.photoClientData;
	filmClientData = &glbData.filmClientData;
	dlClientData = &glbData.dlClientData;

	if (clientData->mediaJobList != (OP_MEDIA_JOB_LIST *)NULL)
	{
		/* There are media jobs running, cannot exit program */
		(void) strcpy (Msg,
				"Media jobs are in process, cannot exit IMS Operator Interface.");
		msgBoxDlg_popupCb (glbData.welcomeW, IMS_INFO, Msg);
		return;
	}


	/* Close database connection for the Order screens */
	if (clientData->orderConnectFlag)
	{
		catReq = &(clientData->catReq);

		if (ims_opCat (catReq, OP_CLOSECONNECTION) < IMS_OK)
		{
			(void) sprintf(Msg, "Close Database Connection Failed!");
			msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
			return;
		}
		clientData->orderConnectFlag = 0;

	}

	/* Close database connection for the Photo screens */
	if (photoClientData->photoConnectFlag)
	{
		catReq = &(photoClientData->catReq);

		if (ims_opCat (catReq, OP_CLOSECONNECTION) < IMS_OK)
		{
			(void) sprintf(Msg, "Close Database Connection Failed!");
			msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
			return;
		}
		photoClientData->photoConnectFlag = 0;

	}

	/* Close database connection for the Film Generation screen */
	if (filmClientData->filmConnectFlag)
	{
		catReq = &(filmClientData->catReq);

		if (ims_opCat (catReq, OP_CLOSECONNECTION) < IMS_OK)
		{
			(void) sprintf(Msg, "Close Database Connection Failed!");
			msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
			return;
		}
		filmClientData->filmConnectFlag = 0;

	}

	/*** Accounts and Users ***/
	/* Close database connection for the Accounting screens */
	if (glbData.accounts_users_data.connect_flag)
	{
                acc_usr_data_ptr = &glbData.accounts_users_data;
		catReq = &(acc_usr_data_ptr->catReq);

		if (ims_opCat (catReq, OP_CLOSECONNECTION) < IMS_OK)
		{
			(void) sprintf(Msg, "Close Database Connection Failed!");
			msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
			return;
		}
		glbData.accounts_users_data.connect_flag = 0;

	}

	/* Close database connection for the Downlink screen */
	if (dlClientData->dlConnectFlag)
	{
		catReq = &(dlClientData->catReq);

		if (ims_opCat (catReq, OP_CLOSECONNECTION) < IMS_OK)
		{
			(void) sprintf(Msg, "Close Database Connection Failed!");
			msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, Msg);
			return;
		}
		dlClientData->dlConnectFlag = 0;
	}

	/*
	** Free up glbData.msgDesc to close syslog
	*/
	(void) ims_msgStructFree (glbData.msgDesc);
	exit(0);

}

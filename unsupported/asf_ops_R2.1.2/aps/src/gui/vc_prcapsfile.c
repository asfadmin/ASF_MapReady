
/*******************************************************************************
	vc_prcapsfile.c

       Associated Header file: vc_prcapsfile.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:
 
Description:
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_prcapsfile.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_prcapsfile.c"

#include "apsfiledef.h"
#include "gui_utils.h"
#include "cb_prcapsfile.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_prcapsfile.h"
#undef CONTEXT_MACRO_ACCESS

Widget	APSFileProcessing;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  browseSelectionCB_scrolledList_reports_inbound(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSFileProcessing   *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSFileProcessingContext;
	UxAPSFileProcessingContext = UxContext =
			(_UxCAPSFileProcessing *) UxGetContext( UxWidget );
	{
	int		itemPos ;
	
	itemPos = ((XmListCallbackStruct *) UxCallbackArg)->item_position ;
	XmListDeselectPos( UxWidget, itemPos ) ;
	}
	UxAPSFileProcessingContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSFileProcessing()
{
	Widget		_UxParent;
	Widget		getList_menuBar_p_shell;


	/* Creation of APSFileProcessing */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "APSFileProcessing_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 189,
			XmNy, 77,
			XmNwidth, 700,
			XmNheight, 596,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSFileProcessing",
			XmNiconName, "APSFileProcessing",
			NULL );

	}

	APSFileProcessing = XtVaCreateManagedWidget( "APSFileProcessing",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 700,
			XmNheight, 596,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS File Processing" ),
			NULL );
	UxPutContext( APSFileProcessing, (char *) UxAPSFileProcessingContext );
	UxPutClassCode( APSFileProcessing, _UxIfClassId );


	/* Creation of label161 */
	label161 = XtVaCreateManagedWidget( "label161",
			xmLabelWidgetClass,
			APSFileProcessing,
			XmNx, 223,
			XmNy, 5,
			XmNwidth, 256,
			XmNheight, 45,
			RES_CONVERT( XmNlabelString, "APS  FILE  PROCESSING" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			NULL );
	UxPutContext( label161, (char *) UxAPSFileProcessingContext );


	/* Creation of scrolledWindowList8 */
	scrolledWindowList8 = XtVaCreateManagedWidget( "scrolledWindowList8",
			xmScrolledWindowWidgetClass,
			APSFileProcessing,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 18,
			XmNy, 81,
			XmNwidth, 630,
			XmNheight, 160,
			XmNrightAttachment, XmATTACH_NONE,
			XmNleftAttachment, XmATTACH_NONE,
			XmNtopAttachment, XmATTACH_NONE,
			NULL );
	UxPutContext( scrolledWindowList8, (char *) UxAPSFileProcessingContext );


	/* Creation of scrolledList_reports_inbound */
	scrolledList_reports_inbound = XtVaCreateManagedWidget( "scrolledList_reports_inbound",
			xmListWidgetClass,
			scrolledWindowList8,
			XmNwidth, 613,
			XmNheight, 160,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNlistSizePolicy, XmCONSTANT,
			XmNvisibleItemCount, 10,
			XmNautomaticSelection, TRUE,
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8" ),
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNitemCount, 0,
			XmNlistMarginHeight, 0,
			NULL );
	XtAddCallback( scrolledList_reports_inbound, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_scrolledList_reports_inbound,
		(XtPointer) UxAPSFileProcessingContext );

	UxPutContext( scrolledList_reports_inbound, (char *) UxAPSFileProcessingContext );


	/* Creation of pushButton_APSFileProcQuit */
	pushButton_APSFileProcQuit = XtVaCreateManagedWidget( "pushButton_APSFileProcQuit",
			xmPushButtonWidgetClass,
			APSFileProcessing,
			XmNx, 508,
			XmNy, 272,
			XmNwidth, 105,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNsensitive, TRUE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_APSFileProcQuit, XmNactivateCallback,
		(XtCallbackProc) cb_quit_file_processing,
		(XtPointer) UxAPSFileProcessingContext );

	UxPutContext( pushButton_APSFileProcQuit, (char *) UxAPSFileProcessingContext );


	/* Creation of label160 */
	label160 = XtVaCreateManagedWidget( "label160",
			xmLabelWidgetClass,
			APSFileProcessing,
			XmNx, 26,
			XmNy, 60,
			XmNwidth, 79,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "File Name" ),
			XmNfontList, UxConvertFontList("7x13bold" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label160, (char *) UxAPSFileProcessingContext );


	/* Creation of pushButton_process */
	pushButton_process = XtVaCreateManagedWidget( "pushButton_process",
			xmPushButtonWidgetClass,
			APSFileProcessing,
			XmNx, 214,
			XmNy, 272,
			XmNwidth, 100,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "PROCESS" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_process, XmNactivateCallback,
		(XtCallbackProc) cb_process_all_files,
		(XtPointer) UxAPSFileProcessingContext );

	UxPutContext( pushButton_process, (char *) UxAPSFileProcessingContext );


	/* Creation of scrolledWindowText9 */
	scrolledWindowText9 = XtVaCreateManagedWidget( "scrolledWindowText9",
			xmScrolledWindowWidgetClass,
			APSFileProcessing,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 50,
			XmNy, 376,
			XmNwidth, 590,
			XmNheight, 200,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( scrolledWindowText9, (char *) UxAPSFileProcessingContext );


	/* Creation of scrolledText_procfile */
	scrolledText_procfile = XtVaCreateManagedWidget( "scrolledText_procfile",
			xmTextWidgetClass,
			scrolledWindowText9,
			XmNwidth, 669,
			XmNheight, 141,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( scrolledText_procfile, (char *) UxAPSFileProcessingContext );


	/* Creation of separator11 */
	separator11 = XtVaCreateManagedWidget( "separator11",
			xmSeparatorWidgetClass,
			APSFileProcessing,
			XmNwidth, 704,
			XmNheight, 10,
			XmNx, -2,
			XmNy, 331,
			NULL );
	UxPutContext( separator11, (char *) UxAPSFileProcessingContext );


	/* Creation of label166 */
	label166 = XtVaCreateManagedWidget( "label166",
			xmLabelWidgetClass,
			APSFileProcessing,
			XmNx, 45,
			XmNy, 355,
			XmNwidth, 590,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MESSAGES" ),
			XmNfontList, UxConvertFontList("-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1" ),
			NULL );
	UxPutContext( label166, (char *) UxAPSFileProcessingContext );


	/* Creation of label33 */
	label33 = XtVaCreateManagedWidget( "label33",
			xmLabelWidgetClass,
			APSFileProcessing,
			XmNx, 239,
			XmNy, 60,
			XmNwidth, 120,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Directory Name" ),
			XmNfontList, UxConvertFontList("7x13bold" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( label33, (char *) UxAPSFileProcessingContext );


	/* Creation of pushButton_stop */
	pushButton_stop = XtVaCreateManagedWidget( "pushButton_stop",
			xmPushButtonWidgetClass,
			APSFileProcessing,
			XmNx, 364,
			XmNy, 272,
			XmNwidth, 100,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "STOP" ),
			XmNsensitive, FALSE,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			NULL );
	XtAddCallback( pushButton_stop, XmNactivateCallback,
		(XtCallbackProc) cb_stop_file_processing,
		(XtPointer) UxAPSFileProcessingContext );

	UxPutContext( pushButton_stop, (char *) UxAPSFileProcessingContext );


	/* Creation of getList_menuBar */
	getList_menuBar = XtVaCreateManagedWidget( "getList_menuBar",
			xmRowColumnWidgetClass,
			APSFileProcessing,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 61,
			XmNy, 272,
			XmNwidth, 96,
			XmNheight, 38,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( getList_menuBar, (char *) UxAPSFileProcessingContext );


	/* Creation of getList_menuBar_p */
	getList_menuBar_p_shell = XtVaCreatePopupShell ("getList_menuBar_p_shell",
			xmMenuShellWidgetClass, getList_menuBar,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	getList_menuBar_p = XtVaCreateWidget( "getList_menuBar_p",
			xmRowColumnWidgetClass,
			getList_menuBar_p_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNx, 0,
			XmNy, 284,
			NULL );
	UxPutContext( getList_menuBar_p, (char *) UxAPSFileProcessingContext );


	/* Creation of getList_menuBar_top_b */
	getList_menuBar_top_b = XtVaCreateManagedWidget( "getList_menuBar_top_b",
			xmCascadeButtonWidgetClass,
			getList_menuBar,
			RES_CONVERT( XmNlabelString, "GET LIST" ),
			XmNsubMenuId, getList_menuBar_p,
			XmNmarginHeight, 7,
			XmNmarginWidth, 16,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 5,
			XmNy, 279,
			NULL );
	UxPutContext( getList_menuBar_top_b, (char *) UxAPSFileProcessingContext );


	XtAddCallback( APSFileProcessing, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSFileProcessingContext);


	return ( APSFileProcessing );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSFileProcessing( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSFileProcessing   *UxContext;
	static int		_Uxinit = 0;

	UxAPSFileProcessingContext = UxContext =
		(_UxCAPSFileProcessing *) UxNewContext( sizeof(_UxCAPSFileProcessing), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		XmFontList	fontlist ;
		XmString	pb_string ;
		Widget		temp ;
		char		*typePtr ;
		int			i ;
		rtrn = _Uxbuild_APSFileProcessing();

		/*
		-- Set up the pulldown menu for Get List,
		-- including callbacks
		*/
		
		/* get the resources from the Get List button */
		XtVaGetValues( getList_menuBar_top_b,
			XmNfontList, &fontlist,
			NULL ) ;
		
		/* set the first (& selected) menu button: INBOUND_ALL_TYPES */
		typePtr = INBOUND_ALL_TYPES ;
		pb_string = XmStringCreateLocalized( typePtr ) ;
		temp = XtVaCreateWidget( typePtr,
			xmPushButtonWidgetClass,
			getList_menuBar_p,
			XmNlabelString, pb_string,
			XmNfontList, fontlist,
			NULL) ;
		XtManageChild( temp ) ;
		XtAddCallback( temp, XmNactivateCallback,
			(XtCallbackProc) cb_fill_fileproc_list,
			(XtPointer) typePtr ) ;
		XtVaSetValues( getList_menuBar,
			XmNmenuHistory, temp,
			NULL) ;
		
		for (i = 0 ; (typePtr = reports_inbound[i].type) != NULL ; i++ )
		{
			pb_string = XmStringCreateLocalized( typePtr ) ;
			temp = XtVaCreateWidget( typePtr,
				xmPushButtonWidgetClass,
				getList_menuBar_p,
				XmNlabelString, pb_string,
				XmNfontList, fontlist,
				NULL) ;
			XtManageChild( temp ) ;
			XtAddCallback( temp, XmNactivateCallback,
				(XtCallbackProc) cb_fill_fileproc_list,
				(XtPointer) typePtr ) ;
		}
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


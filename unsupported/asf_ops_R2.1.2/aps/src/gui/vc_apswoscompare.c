
/*******************************************************************************
	vc_apswoscompare.c

       Associated Header file: vc_apswoscompare.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:	vc_permstatus.c

Description:	the gui builder code for reporting the status of the
		multi-user permissions.
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_apswoscompare.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_apswoscompare.c"

#include "cb_apswoscompare.h"
#include "apsfiledef.h"
#include "gui_utils.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_apswoscompare.h"
#undef CONTEXT_MACRO_ACCESS

Widget	APSWOSCompare;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_QUIT(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSWOSCompare       *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSWOSCompareContext;
	UxAPSWOSCompareContext = UxContext =
			(_UxCAPSWOSCompare *) UxGetContext( UxWidget );
	{
	extern Widget apswoscompare_form;
	XtPopdown(XtParent(apswoscompare_form)) ;
	}
	UxAPSWOSCompareContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSWOSCompare()
{
	Widget		_UxParent;


	/* Creation of APSWOSCompare */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "APSWOSCompare_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 360,
			XmNy, 450,
			XmNwidth, 680,
			XmNheight, 515,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSWOSCompare",
			XmNiconName, "APSWOSCompare",
			NULL );

	}

	APSWOSCompare = XtVaCreateManagedWidget( "APSWOSCompare",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 680,
			XmNheight, 515,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "APS WOS Compare" ),
			NULL );
	UxPutContext( APSWOSCompare, (char *) UxAPSWOSCompareContext );
	UxPutClassCode( APSWOSCompare, _UxIfClassId );


	/* Creation of label73 */
	label73 = XtVaCreateManagedWidget( "label73",
			xmLabelWidgetClass,
			APSWOSCompare,
			XmNx, 42,
			XmNy, 62,
			RES_CONVERT( XmNlabelString, "Enter WOS File Name for Comparison with The APS DB:" ),
			NULL );
	UxPutContext( label73, (char *) UxAPSWOSCompareContext );


	/* Creation of textField1 */
	textField1 = XtVaCreateManagedWidget( "textField1",
			xmTextFieldWidgetClass,
			APSWOSCompare,
			XmNwidth, 595,
			XmNx, 42,
			XmNy, 84,
			XmNleftOffset, 165,
			NULL );
	XtAddCallback( textField1, XmNactivateCallback,
		(XtCallbackProc) cb_aps_wos_compare,
		(XtPointer) UxAPSWOSCompareContext );

	UxPutContext( textField1, (char *) UxAPSWOSCompareContext );

	cb_mwos_filename( textField1,
			(XtPointer) UxAPSWOSCompareContext, (XtPointer) NULL );


	/* Creation of label76 */
	label76 = XtVaCreateManagedWidget( "label76",
			xmLabelWidgetClass,
			APSWOSCompare,
			XmNx, 120,
			XmNy, 12,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			XmNheight, 45,
			RES_CONVERT( XmNlabelString, "McMURDO  WOS  COMPARISON  TOOL" ),
			XmNleftOffset, 237,
			XmNleftAttachment, XmATTACH_NONE,
			XmNwidth, 456,
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( label76, (char *) UxAPSWOSCompareContext );


	/* Creation of pushButton_OK */
	pushButton_OK = XtVaCreateManagedWidget( "pushButton_OK",
			xmPushButtonWidgetClass,
			APSWOSCompare,
			XmNx, 43,
			XmNy, 138,
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 40,
			XmNwidth, 105,
			XmNleftOffset, 157,
			NULL );
	XtAddCallback( pushButton_OK, XmNactivateCallback,
		(XtCallbackProc) cb_aps_wos_compare,
		(XtPointer) UxAPSWOSCompareContext );

	UxPutContext( pushButton_OK, (char *) UxAPSWOSCompareContext );


	/* Creation of pushButton_QUIT */
	pushButton_QUIT = XtVaCreateManagedWidget( "pushButton_QUIT",
			xmPushButtonWidgetClass,
			APSWOSCompare,
			XmNx, 533,
			XmNy, 138,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 40,
			XmNwidth, 102,
			XmNrightOffset, 157,
			XmNleftOffset, 418,
			NULL );
	XtAddCallback( pushButton_QUIT, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_QUIT,
		(XtPointer) UxAPSWOSCompareContext );

	UxPutContext( pushButton_QUIT, (char *) UxAPSWOSCompareContext );


	/* Creation of scrolledWindowText10 */
	scrolledWindowText10 = XtVaCreateManagedWidget( "scrolledWindowText10",
			xmScrolledWindowWidgetClass,
			APSWOSCompare,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 40,
			XmNy, 241,
			XmNleftOffset, 40,
			XmNrightOffset, 40,
			XmNwidth, 600,
			NULL );
	UxPutContext( scrolledWindowText10, (char *) UxAPSWOSCompareContext );


	/* Creation of scrolledText_woscompare */
	scrolledText_woscompare = XtVaCreateManagedWidget( "scrolledText_woscompare",
			xmTextWidgetClass,
			scrolledWindowText10,
			XmNwidth, 590,
			XmNheight, 229,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( scrolledText_woscompare, (char *) UxAPSWOSCompareContext );


	/* Creation of separator14 */
	separator14 = XtVaCreateManagedWidget( "separator14",
			xmSeparatorWidgetClass,
			APSWOSCompare,
			XmNwidth, 679,
			XmNheight, 20,
			XmNx, 1,
			XmNy, 192,
			NULL );
	UxPutContext( separator14, (char *) UxAPSWOSCompareContext );


	/* Creation of label74 */
	label74 = XtVaCreateManagedWidget( "label74",
			xmLabelWidgetClass,
			APSWOSCompare,
			XmNx, 46,
			XmNy, 218,
			XmNfontList, UxConvertFontList("-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1" ),
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "MESSAGES" ),
			XmNwidth, 590,
			NULL );
	UxPutContext( label74, (char *) UxAPSWOSCompareContext );


	XtAddCallback( APSWOSCompare, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSWOSCompareContext);


	return ( APSWOSCompare );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSWOSCompare( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSWOSCompare       *UxContext;
	static int		_Uxinit = 0;

	UxAPSWOSCompareContext = UxContext =
		(_UxCAPSWOSCompare *) UxNewContext( sizeof(_UxCAPSWOSCompare), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_APSWOSCompare();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


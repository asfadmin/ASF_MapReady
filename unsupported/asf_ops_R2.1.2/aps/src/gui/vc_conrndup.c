
/*******************************************************************************
	vc_conrndup.c

       Associated Header file: vc_conrndup.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
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
#pragma ident   "@(#)vc_conrndup.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_conrndup.c"

#include "gui_utils.h"
#include "satmenus.h"
#include "cb_datetime.h"
#include "cb_conrndup.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_conrndup.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pb_ConRnd_QUIT(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCConRoundupForm      *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxConRoundupFormContext;
	UxConRoundupFormContext = UxContext =
			(_UxCConRoundupForm *) UxGetContext( UxWidget );
	{
	XtPopdown( gui_GetShellWidget( ConRoundupForm ) ) ;
	}
	UxConRoundupFormContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_ConRoundupForm()
{
	Widget		_UxParent;
	Widget		subMenu_conRnd_stnid_shell;


	/* Creation of ConRoundupForm */
	_UxParent = XtVaCreatePopupShell( "ConRoundupForm_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 220,
			XmNy, 240,
			XmNwidth, 338,
			XmNheight, 278,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "ConRoundupForm",
			XmNiconName, "ConRoundupForm",
			NULL );

	ConRoundupForm = XtVaCreateManagedWidget( "ConRoundupForm",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 338,
			XmNheight, 278,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( ConRoundupForm, (char *) UxConRoundupFormContext );
	UxPutClassCode( ConRoundupForm, _UxIfClassId );


	/* Creation of label36 */
	label36 = XtVaCreateManagedWidget( "label36",
			xmLabelWidgetClass,
			ConRoundupForm,
			XmNx, 67,
			XmNy, 19,
			XmNwidth, 199,
			XmNheight, 35,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CON ROUNDUP" ),
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( label36, (char *) UxConRoundupFormContext );


	/* Creation of label58 */
	label58 = XtVaCreateManagedWidget( "label58",
			xmLabelWidgetClass,
			ConRoundupForm,
			XmNx, 23,
			XmNy, 106,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "START TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label58, (char *) UxConRoundupFormContext );


	/* Creation of label61 */
	label61 = XtVaCreateManagedWidget( "label61",
			xmLabelWidgetClass,
			ConRoundupForm,
			XmNx, 29,
			XmNy, 153,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "STOP TIME:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( label61, (char *) UxConRoundupFormContext );


	/* Creation of TF_CON_RND_STRTTIME */
	TF_CON_RND_STRTTIME = XtVaCreateManagedWidget( "TF_CON_RND_STRTTIME",
			xmTextFieldWidgetClass,
			ConRoundupForm,
			XmNx, 94,
			XmNy, 105,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNsensitive, TRUE,
			XmNwidth, 185,
			NULL );
	XtAddCallback( TF_CON_RND_STRTTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "CON Roundup Start Time" );
	XtAddCallback( TF_CON_RND_STRTTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_CON_RND_STRTTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );
	XtAddCallback( TF_CON_RND_STRTTIME, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );

	UxPutContext( TF_CON_RND_STRTTIME, (char *) UxConRoundupFormContext );


	/* Creation of TF_CON_RND_STOPTIME */
	TF_CON_RND_STOPTIME = XtVaCreateManagedWidget( "TF_CON_RND_STOPTIME",
			xmTextFieldWidgetClass,
			ConRoundupForm,
			XmNx, 94,
			XmNy, 152,
			XmNheight, 32,
			XmNcolumns, 21,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, TRUE,
			XmNresizeWidth, FALSE,
			XmNvalue, "",
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNmaxLength, 21,
			XmNsensitive, TRUE,
			XmNwidth, 185,
			NULL );
	XtAddCallback( TF_CON_RND_STOPTIME, XmNactivateCallback,
		(XtCallbackProc) cb_validate_ASF_datetime,
		(XtPointer) "CON Roundup Stop Time" );
	XtAddCallback( TF_CON_RND_STOPTIME, XmNfocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) TRUE );
	XtAddCallback( TF_CON_RND_STOPTIME, XmNlosingFocusCallback,
		(XtCallbackProc) cb_toggle_cursor,
		(XtPointer) FALSE );
	XtAddCallback( TF_CON_RND_STOPTIME, XmNmodifyVerifyCallback,
		(XtCallbackProc) cb_filter_text,
		(XtPointer) valid_ASF_datetime_chars );

	UxPutContext( TF_CON_RND_STOPTIME, (char *) UxConRoundupFormContext );


	/* Creation of subMenu_conRnd_stnid */
	subMenu_conRnd_stnid_shell = XtVaCreatePopupShell ("subMenu_conRnd_stnid_shell",
			xmMenuShellWidgetClass, ConRoundupForm,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	subMenu_conRnd_stnid = XtVaCreateWidget( "subMenu_conRnd_stnid",
			xmRowColumnWidgetClass,
			subMenu_conRnd_stnid_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNlabelString, "" ),
			XmNheight, 48,
			XmNresizeHeight, FALSE,
			XmNx, 0,
			XmNy, 315,
			XmNsensitive, TRUE,
			XmNmappedWhenManaged, TRUE,
			NULL );
	UxPutContext( subMenu_conRnd_stnid, (char *) UxConRoundupFormContext );


	/* Creation of subMenu_conRnd_stnid_asf */
	subMenu_conRnd_stnid_asf = XtVaCreateManagedWidget( "subMenu_conRnd_stnid_asf",
			xmPushButtonWidgetClass,
			subMenu_conRnd_stnid,
			RES_CONVERT( XmNlabelString, "ASF" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNx, 2,
			XmNy, 328,
			NULL );
	UxPutContext( subMenu_conRnd_stnid_asf, (char *) UxConRoundupFormContext );

	cb_build_station_option_menu( subMenu_conRnd_stnid_asf,
			(XtPointer) UxConRoundupFormContext, (XtPointer) NULL );


	/* Creation of optionMenu_conRnd_stnid */
	optionMenu_conRnd_stnid = XtVaCreateManagedWidget( "optionMenu_conRnd_stnid",
			xmRowColumnWidgetClass,
			ConRoundupForm,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, subMenu_conRnd_stnid,
			XmNx, 19,
			XmNy, 61,
			XmNwidth, 215,
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "STATION ID:" ),
			XmNsensitive, TRUE,
			NULL );
	UxPutContext( optionMenu_conRnd_stnid, (char *) UxConRoundupFormContext );


	/* Creation of pb_ConRnd_OK */
	pb_ConRnd_OK = XtVaCreateManagedWidget( "pb_ConRnd_OK",
			xmPushButtonWidgetClass,
			ConRoundupForm,
			XmNx, 20,
			XmNy, 214,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "OK" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNsensitive, TRUE,
			XmNmultiClick, XmMULTICLICK_DISCARD,
			NULL );
	XtAddCallback( pb_ConRnd_OK, XmNactivateCallback,
		(XtCallbackProc) cb_startConRoundup,
		(XtPointer) 0x0 );

	UxPutContext( pb_ConRnd_OK, (char *) UxConRoundupFormContext );


	/* Creation of pb_ConRnd_QUIT */
	pb_ConRnd_QUIT = XtVaCreateManagedWidget( "pb_ConRnd_QUIT",
			xmPushButtonWidgetClass,
			ConRoundupForm,
			XmNx, 237,
			XmNy, 214,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNmultiClick, XmMULTICLICK_DISCARD,
			NULL );
	XtAddCallback( pb_ConRnd_QUIT, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_ConRnd_QUIT,
		(XtPointer) UxConRoundupFormContext );

	UxPutContext( pb_ConRnd_QUIT, (char *) UxConRoundupFormContext );


	/* Creation of pb_ConRnd_CLEAR */
	pb_ConRnd_CLEAR = XtVaCreateManagedWidget( "pb_ConRnd_CLEAR",
			xmPushButtonWidgetClass,
			ConRoundupForm,
			XmNx, 131,
			XmNy, 214,
			XmNwidth, 90,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "CLEAR" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNmultiClick, XmMULTICLICK_DISCARD,
			NULL );
	XtAddCallback( pb_ConRnd_CLEAR, XmNactivateCallback,
		(XtCallbackProc) cb_clearConRoundupForm,
		(XtPointer) UxConRoundupFormContext );

	UxPutContext( pb_ConRnd_CLEAR, (char *) UxConRoundupFormContext );


	XtAddCallback( ConRoundupForm, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxConRoundupFormContext);


	return ( ConRoundupForm );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_ConRoundupForm( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCConRoundupForm      *UxContext;
	static int		_Uxinit = 0;

	UxConRoundupFormContext = UxContext =
		(_UxCConRoundupForm *) UxNewContext( sizeof(_UxCConRoundupForm), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_ConRoundupForm();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/



/*******************************************************************************
	pps_main.c

       Associated Header file: pps_main.h
*******************************************************************************/

#include <stdio.h>
#include "UxLib.h"
#include "UxCascB.h"
#include "UxRowCol.h"
#include "UxPushB.h"
#include "UxLabel.h"
#include "UxForm.h"
#include "UxTopSh.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "pps_common.h"
#include "pps_util.h"

static char SccsFileId[] = "@(#)pps_main.c	1.2    12/16/96";

char main_labelPixmapString[MAXSMALLBUF];
extern char rootPath[];
extern swidget pps_query, pps_plan, pps_policy;
extern swidget nojoy;
extern char IsAuthorizedUser;

swidget create_pps_query(), create_pps_plan(), create_pps_policy();
swidget create_nojoy();


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "pps_main.h"
#undef CONTEXT_MACRO_ACCESS

swidget	pps_main;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pb_main_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_main            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_mainContext;
	UxPps_mainContext = UxContext =
			(_UxCpps_main *) UxGetContext( UxThisWidget );
	{
	cb_main_query();
	}
	UxPps_mainContext = UxSaveCtx;
}

static void  activateCB_pb_main_plan(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_main            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_mainContext;
	UxPps_mainContext = UxContext =
			(_UxCpps_main *) UxGetContext( UxThisWidget );
	cb_main_plan();
	UxPps_mainContext = UxSaveCtx;
}

static void  activateCB_pb_main_policy(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_main            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_mainContext;
	UxPps_mainContext = UxContext =
			(_UxCpps_main *) UxGetContext( UxThisWidget );
	cb_main_policy();
	UxPps_mainContext = UxSaveCtx;
}

static void  activateCB_pb_main_print_screen(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_main            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_mainContext;
	UxPps_mainContext = UxContext =
			(_UxCpps_main *) UxGetContext( UxThisWidget );
	{
	extern void pps_print_screen(swidget sw);
	
	pps_print_screen(pps_main);
	
	}
	UxPps_mainContext = UxSaveCtx;
}

static void  activateCB_pb_main_exit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_main            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_mainContext;
	UxPps_mainContext = UxContext =
			(_UxCpps_main *) UxGetContext( UxThisWidget );
	{
	UxPopdownInterface(pps_main);
	closelog();   /* close the syslog */
	exit(0);
	}
	UxPps_mainContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_pps_main()
{
	/* Create the swidgets */


	/* Creation of pps_main */
	pps_main = UxCreateTopLevelShell( "pps_main", UxParent );
	UxPutContext( pps_main, UxPps_mainContext );
	UxPutClassCode( pps_main, _UxIfClassId );

	UxPutWidth( pps_main, 699 ),
	UxPutHeight( pps_main, 570 ),
	UxPutX( pps_main, 225 ),
	UxPutY( pps_main, 225 ),
	UxPutIconName( pps_main, "pps" ),
	UxCreateWidget( pps_main );


	/* Creation of f_main */
	f_main = UxCreateForm( "f_main", pps_main );
	UxPutWidth( f_main, 436 ),
	UxPutHeight( f_main, 228 ),
	UxPutResizePolicy( f_main, "resize_none" ),
	UxPutX( f_main, 60 ),
	UxPutY( f_main, 60 ),
	UxPutUnitType( f_main, "pixels" ),
	UxCreateWidget( f_main );


	/* Creation of l_main_bear */
	l_main_bear = UxCreateLabel( "l_main_bear", f_main );
	UxPutX( l_main_bear, 138 ),
	UxPutY( l_main_bear, 205 ),
	UxPutWidth( l_main_bear, 448 ),
	UxPutHeight( l_main_bear, 256 ),
	UxPutLabelType( l_main_bear, "pixmap" ),
	UxCreateWidget( l_main_bear );


	/* Creation of l_main_title */
	l_main_title = UxCreateLabel( "l_main_title", f_main );
	UxPutX( l_main_title, 60 ),
	UxPutY( l_main_title, 52 ),
	UxPutWidth( l_main_title, 604 ),
	UxPutHeight( l_main_title, 44 ),
	UxPutLabelString( l_main_title, "Alaska SAR Facility Production Planning System" ),
	UxPutFontList( l_main_title, "-adobe-helvetica-bold-r-normal--24-240-75-75-p-138-iso8859-1" ),
	UxCreateWidget( l_main_title );


	/* Creation of pb_main_query */
	pb_main_query = UxCreatePushButton( "pb_main_query", f_main );
	UxPutX( pb_main_query, 38 ),
	UxPutY( pb_main_query, 490 ),
	UxPutWidth( pb_main_query, 180 ),
	UxPutHeight( pb_main_query, 56 ),
	UxPutLabelString( pb_main_query, "Query" ),
	UxPutFontList( pb_main_query, "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1" ),
	UxCreateWidget( pb_main_query );

	UxAddCallback( pb_main_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_main_query,
		(XtPointer) UxPps_mainContext );


	/* Creation of pb_main_plan */
	pb_main_plan = UxCreatePushButton( "pb_main_plan", f_main );
	UxPutX( pb_main_plan, 267 ),
	UxPutY( pb_main_plan, 492 ),
	UxPutWidth( pb_main_plan, 180 ),
	UxPutHeight( pb_main_plan, 56 ),
	UxPutLabelString( pb_main_plan, "Plan" ),
	UxPutFontList( pb_main_plan, "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1" ),
	UxCreateWidget( pb_main_plan );

	UxAddCallback( pb_main_plan, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_main_plan,
		(XtPointer) UxPps_mainContext );


	/* Creation of pb_main_policy */
	pb_main_policy = UxCreatePushButton( "pb_main_policy", f_main );
	UxPutX( pb_main_policy, 477 ),
	UxPutY( pb_main_policy, 493 ),
	UxPutWidth( pb_main_policy, 196 ),
	UxPutHeight( pb_main_policy, 56 ),
	UxPutLabelString( pb_main_policy, "Policy" ),
	UxPutFontList( pb_main_policy, "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1" ),
	UxCreateWidget( pb_main_policy );

	UxAddCallback( pb_main_policy, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_main_policy,
		(XtPointer) UxPps_mainContext );


	/* Creation of mb_main */
	mb_main = UxCreateRowColumn( "mb_main", f_main );
	UxPutRowColumnType( mb_main, "menu_bar" ),
	UxPutX( mb_main, 0 ),
	UxPutY( mb_main, 0 ),
	UxPutWidth( mb_main, 504 ),
	UxPutHeight( mb_main, 36 ),
	UxPutMenuAccelerator( mb_main, "<KeyUp>F10" ),
	UxPutRightAttachment( mb_main, "attach_form" ),
	UxPutLeftAttachment( mb_main, "attach_form" ),
	UxCreateWidget( mb_main );


	/* Creation of pb_main_file */
	pb_main_file = UxCreateRowColumn( "pb_main_file", mb_main );
	UxPutRowColumnType( pb_main_file, "menu_pulldown" ),
	UxCreateWidget( pb_main_file );


	/* Creation of pb_main_print_screen */
	pb_main_print_screen = UxCreatePushButton( "pb_main_print_screen", pb_main_file );
	UxPutLabelString( pb_main_print_screen, "Print Screen" ),
	UxCreateWidget( pb_main_print_screen );

	UxAddCallback( pb_main_print_screen, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_main_print_screen,
		(XtPointer) UxPps_mainContext );


	/* Creation of pb_main_exit */
	pb_main_exit = UxCreatePushButton( "pb_main_exit", pb_main_file );
	UxPutLabelString( pb_main_exit, "Exit" ),
	UxPutMnemonic( pb_main_exit, "x" ),
	UxCreateWidget( pb_main_exit );

	UxAddCallback( pb_main_exit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_main_exit,
		(XtPointer) UxPps_mainContext );


	/* Creation of mb_main_file */
	mb_main_file = UxCreateCascadeButton( "mb_main_file", mb_main );
	UxPutLabelString( mb_main_file, "File" ),
	UxPutMnemonic( mb_main_file, "F" ),
	UxPutSubMenuId( mb_main_file, "pb_main_file" ),
	UxCreateWidget( mb_main_file );


	/* Creation of label1 */
	label1 = UxCreateLabel( "label1", f_main );
	UxPutX( label1, 92 ),
	UxPutY( label1, 99 ),
	UxPutWidth( label1, 541 ),
	UxPutHeight( label1, 73 ),
	UxPutLabelString( label1, "Copyright (c) 1996, California Institute of Technology.\nALL RIGHTS RESERVED.\nU.S. Government Sponsorship acknowledged." ),
	UxCreateWidget( label1 );

	UxDelayUpdate( l_main_bear );
	UxPutLabelPixmap( l_main_bear, main_labelPixmapString ),
	UxUpdate( l_main_bear );
	UxAddCallback( pps_main, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPps_mainContext);



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( pps_main );

	return ( pps_main );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_pps_main( swidget _UxUxParent )
{
	swidget                 rtrn;
	_UxCpps_main            *UxContext;
	static int		_Uxinit = 0;

	UxPps_mainContext = UxContext =
		(_UxCpps_main *) UxNewContext( sizeof(_UxCpps_main), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		(void)sprintf(main_labelPixmapString, "%s/%s/whitebear3.xpm",
		rootPath, PPS_PIXMAP_SUBPATH);
		rtrn = _Uxbuild_pps_main();

		pps_query = create_pps_query(NO_PARENT);
		if (IsAuthorizedUser)
		{
		    pps_plan = create_pps_plan(NO_PARENT);
		    pps_policy = create_pps_policy(NO_PARENT);
		    nojoy = create_nojoy(NO_PARENT);
		}
		else
		{
		    XtSetSensitive(UxGetWidget(pb_main_plan), False);
		    XtSetSensitive(UxGetWidget(pb_main_policy), False);
		}
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


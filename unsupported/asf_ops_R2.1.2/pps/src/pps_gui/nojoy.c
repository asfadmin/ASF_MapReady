/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
	nojoy.c

       Associated Header file: nojoy.h
*******************************************************************************/

#include <stdio.h>
#include "UxLib.h"
#include "UxWarnD.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "pps_common.h"
#include "pps_util.h"
char nojoy_labelPixmapString[MAXSMALLBUF];
extern char rootPath[];
static char SccsFileId[] = "@(#)nojoy.c	1.1    11/21/96";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "nojoy.h"
#undef CONTEXT_MACRO_ACCESS

swidget	nojoy;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  cancelCB_nojoy(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCnojoy               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxNojoyContext;
	UxNojoyContext = UxContext =
			(_UxCnojoy *) UxGetContext( UxThisWidget );
	UxPopdownInterface(nojoy);
	UxNojoyContext = UxSaveCtx;
}

static void  helpCB_nojoy(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCnojoy               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxNojoyContext;
	UxNojoyContext = UxContext =
			(_UxCnojoy *) UxGetContext( UxThisWidget );
	UxPopdownInterface(nojoy);
	UxNojoyContext = UxSaveCtx;
}

static void  okCallback_nojoy(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCnojoy               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxNojoyContext;
	UxNojoyContext = UxContext =
			(_UxCnojoy *) UxGetContext( UxThisWidget );
	UxPopdownInterface(nojoy);
	UxNojoyContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_nojoy()
{
	/* Create the swidgets */


	/* Creation of nojoy */
	nojoy = UxCreateWarningDialog( "nojoy", UxParent );
	UxPutContext( nojoy, UxNojoyContext );
	UxPutClassCode( nojoy, _UxIfClassId );
	UxPutDefaultShell( nojoy, "transientShell" );

	UxPutMsgDialogType( nojoy, "dialog_error" ),
	UxPutWidth( nojoy, 324 ),
	UxPutHeight( nojoy, 172 ),
	UxPutX( nojoy, 388 ),
	UxPutY( nojoy, 360 ),
	UxPutUnitType( nojoy, "pixels" ),
	UxPutMessageString( nojoy, "No joy here!\n\nTry something else." ),
	UxPutDialogStyle( nojoy, "dialog_full_application_modal" ),
	UxCreateWidget( nojoy );

	UxAddCallback( nojoy, XmNcancelCallback,
		(XtCallbackProc) cancelCB_nojoy,
		(XtPointer) UxNojoyContext );
	UxAddCallback( nojoy, XmNhelpCallback,
		(XtCallbackProc) helpCB_nojoy,
		(XtPointer) UxNojoyContext );
	UxAddCallback( nojoy, XmNokCallback,
		(XtCallbackProc) okCallback_nojoy,
		(XtPointer) UxNojoyContext );

	UxDelayUpdate( nojoy );
	UxPutSymbolPixmap( nojoy, nojoy_labelPixmapString ),
	UxUpdate( nojoy );
	UxAddCallback( nojoy, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxNojoyContext);



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( nojoy );

	return ( nojoy );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_nojoy( swidget _UxUxParent )
{
	swidget                 rtrn;
	_UxCnojoy               *UxContext;
	static int		_Uxinit = 0;

	UxNojoyContext = UxContext =
		(_UxCnojoy *) UxNewContext( sizeof(_UxCnojoy), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		(void)sprintf(nojoy_labelPixmapString, "%s/%s/tigerfrown.xpm",
		rootPath, PPS_PIXMAP_SUBPATH);
		rtrn = _Uxbuild_nojoy();

		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
	pps_error_dialog.c

       Associated Header file: pps_error_dialog.h
*******************************************************************************/

#include <stdio.h>
#include "UxLib.h"
#include "UxErrorD.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "pps_common.h"
#include "pps_util.h"
char error_labelPixmapString[MAXSMALLBUF];
extern char rootPath[];
static char SccsFileId[] = "@(#)pps_error_dialog.c	1.1    11/21/96";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "pps_error_dialog.h"
#undef CONTEXT_MACRO_ACCESS

swidget	pps_error_dialog;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_pps_error_dialog()
{
	/* Create the swidgets */


	/* Creation of pps_error_dialog */
	pps_error_dialog = UxCreateErrorDialog( "pps_error_dialog", UxParent );
	UxPutContext( pps_error_dialog, UxPps_error_dialogContext );
	UxPutClassCode( pps_error_dialog, _UxIfClassId );
	UxPutDefaultShell( pps_error_dialog, "topLevelShell" );

	UxPutMsgDialogType( pps_error_dialog, "dialog_error" ),
	UxPutWidth( pps_error_dialog, 483 ),
	UxPutHeight( pps_error_dialog, 211 ),
	UxPutX( pps_error_dialog, 427 ),
	UxPutY( pps_error_dialog, 311 ),
	UxPutUnitType( pps_error_dialog, "pixels" ),
	UxPutMarginHeight( pps_error_dialog, 10 ),
	UxPutMessageAlignment( pps_error_dialog, "alignment_center" ),
	UxPutDialogStyle( pps_error_dialog, "dialog_full_application_modal" ),
	UxPutAllowOverlap( pps_error_dialog, "false" ),
	UxCreateWidget( pps_error_dialog );

	UxDelayUpdate( pps_error_dialog );
	UxPutSymbolPixmap( pps_error_dialog, error_labelPixmapString ),
	UxUpdate( pps_error_dialog );
	UxAddCallback( pps_error_dialog, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPps_error_dialogContext);



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( pps_error_dialog );

	return ( pps_error_dialog );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_pps_error_dialog( swidget _UxUxParent )
{
	swidget                 rtrn;
	_UxCpps_error_dialog    *UxContext;
	static int		_Uxinit = 0;

	UxPps_error_dialogContext = UxContext =
		(_UxCpps_error_dialog *) UxNewContext( sizeof(_UxCpps_error_dialog), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		(void)sprintf(error_labelPixmapString,
		"%s/%s/bearwait2.xpm", rootPath, PPS_PIXMAP_SUBPATH);
		rtrn = _Uxbuild_pps_error_dialog();

		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


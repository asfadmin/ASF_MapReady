/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static char SccsFileId[] = "@(#)pps_file_selector.c	1.1    11/21/96";

/*******************************************************************************
	pps_file_selector.c

       Associated Header file: pps_file_selector.h
*******************************************************************************/

#include <stdio.h>
#include "UxLib.h"
#include "UxFsBD.h"



static	int _UxIfClassId;
static	swidget	UxParent;

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "pps_file_selector.h"
#undef CONTEXT_MACRO_ACCESS

swidget	pps_file_selector;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_pps_file_selector()
{
	/* Create the swidgets */


	/* Creation of pps_file_selector */
	pps_file_selector = UxCreateFileSelectionBoxDialog( "pps_file_selector", UxParent );
	UxPutClassCode( pps_file_selector, _UxIfClassId );
	UxPutDefaultShell( pps_file_selector, "topLevelShell" );

	UxPutWidth( pps_file_selector, 574 ),
	UxPutHeight( pps_file_selector, 398 ),
	UxPutDialogType( pps_file_selector, "dialog_file_selection" ),
	UxPutX( pps_file_selector, 390 ),
	UxPutY( pps_file_selector, 230 ),
	UxPutUnitType( pps_file_selector, "pixels" ),
	UxPutResizePolicy( pps_file_selector, "resize_none" ),
	UxPutDialogStyle( pps_file_selector, "dialog_primary_application_modal" ),
	UxCreateWidget( pps_file_selector );



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( pps_file_selector );

	return ( pps_file_selector );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_pps_file_selector( swidget _UxUxParent )
{
	swidget                 rtrn;
	static int		_Uxinit = 0;

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_pps_file_selector();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


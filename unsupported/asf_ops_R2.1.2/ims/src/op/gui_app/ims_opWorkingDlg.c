
/*******************************************************************************
	ims_opWorkingDlg.c

       Associated Header file: ims_opWorkingDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Label.h>
#include <Xm/MessageB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "ims_opCb.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_opWorkingDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_workingDlg()
{
	Widget		_UxParent;


	/* Creation of workingDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "workingDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 451,
			XmNy, 370,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "workingDlg",
			NULL );

	workingDlg = XtVaCreateWidget( "workingDlg",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_WORKING,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmessageString, "\nInitializing Operator Interface, please wait!" ),
			NULL );
	UxPutContext( workingDlg, (char *) UxWorkingDlgContext );
	UxPutClassCode( workingDlg, _UxIfClassId );


	/* Creation of label211 */
	label211 = XtVaCreateManagedWidget( "label211",
			xmLabelWidgetClass,
			workingDlg,
			XmNx, 16,
			XmNy, 56,
			XmNlabelType, XmPIXMAP,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNmarginHeight, 0,
			XmNmarginWidth, 0,
			XmNwidth, 400,
			NULL );
	UxPutContext( label211, (char *) UxWorkingDlgContext );

	XtVaSetValues(label211,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/liftoff.xpm" ),
			NULL );


	XtAddCallback( workingDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxWorkingDlgContext);


	return ( workingDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_workingDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCworkingDlg          *UxContext;
	static int		_Uxinit = 0;

	UxWorkingDlgContext = UxContext =
		(_UxCworkingDlg *) UxNewContext( sizeof(_UxCworkingDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_workingDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


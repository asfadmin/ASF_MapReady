
/*******************************************************************************
	ims_op_accManagerSelectDlg.c

       Associated Header file: ims_op_accManagerSelectDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/SelectioB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_op_accCb.h>


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accManagerSelectDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_managerSelectionDlg()
{
	Widget		_UxParent;


	/* Creation of managerSelectionDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "managerSelectionDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 170,
			XmNy, 400,
			XmNwidth, 250,
			XmNheight, 350,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "managerSelectionDlg",
			NULL );

	managerSelectionDlg = XtVaCreateWidget( "managerSelectionDlg",
			xmSelectionBoxWidgetClass,
			_UxParent,
			XmNwidth, 250,
			XmNheight, 350,
			XmNdialogType, XmDIALOG_SELECTION,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNlistLabelString, "User ID" ),
			XmNtextFontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			NULL );
	XtAddCallback( managerSelectionDlg, XmNokCallback,
		(XtCallbackProc) account_manager_select_dlg_okCb,
		(XtPointer) UxManagerSelectionDlgContext );

	UxPutContext( managerSelectionDlg, (char *) UxManagerSelectionDlgContext );
	UxPutClassCode( managerSelectionDlg, _UxIfClassId );


	XtAddCallback( managerSelectionDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxManagerSelectionDlgContext);


	return ( managerSelectionDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_managerSelectionDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCmanagerSelectionDlg *UxContext;
	static int		_Uxinit = 0;

	UxManagerSelectionDlgContext = UxContext =
		(_UxCmanagerSelectionDlg *) UxNewContext( sizeof(_UxCmanagerSelectionDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_managerSelectionDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


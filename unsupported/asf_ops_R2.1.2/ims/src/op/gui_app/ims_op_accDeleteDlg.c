
/*******************************************************************************
	ims_op_accDeleteDlg.c

       Associated Header file: ims_op_accDeleteDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/MessageB.h>

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
#include "ims_op_accDeleteDlg.h"
#undef CONTEXT_MACRO_ACCESS

Widget	delete_dlg;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_delete_dlg()
{
	Widget		_UxParent;


	/* Creation of delete_dlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "delete_dlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 61,
			XmNy, 576,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "delete_dlg",
			NULL );

	delete_dlg = XtVaCreateWidget( "delete_dlg",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_MESSAGE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmessageString, "" ),
			XmNresizePolicy, XmRESIZE_ANY,
			XmNautoUnmanage, TRUE,
			XmNdialogStyle, XmDIALOG_MODELESS,
			NULL );
	UxPutContext( delete_dlg, (char *) UxDelete_dlgContext );
	UxPutClassCode( delete_dlg, _UxIfClassId );


	XtAddCallback( delete_dlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxDelete_dlgContext);


	return ( delete_dlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_delete_dlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCdelete_dlg          *UxContext;
	static int		_Uxinit = 0;

	UxDelete_dlgContext = UxContext =
		(_UxCdelete_dlg *) UxNewContext( sizeof(_UxCdelete_dlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_delete_dlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/



/*******************************************************************************
	ims_opAskUserDlg.c

       Associated Header file: ims_opAskUserDlg.h
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



static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_opAskUserDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_askUserDlg()
{
	Widget		_UxParent;


	/* Creation of askUserDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "askUserDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 350,
			XmNy, 430,
			XmNheight, 150,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "askUserDlg",
			NULL );

	askUserDlg = XtVaCreateWidget( "askUserDlg",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_WORKING,
			XmNheight, 150,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNdefaultPosition, FALSE,
			NULL );
	UxPutContext( askUserDlg, (char *) UxAskUserDlgContext );
	UxPutClassCode( askUserDlg, _UxIfClassId );

	XtVaSetValues(askUserDlg,
			RES_CONVERT( XmNsymbolPixmap, "/local/imsdads/app-defaults/pixmaps/reels3.xpm" ),
			NULL );


	XtAddCallback( askUserDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAskUserDlgContext);


	return ( askUserDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_askUserDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCaskUserDlg          *UxContext;
	static int		_Uxinit = 0;

	UxAskUserDlgContext = UxContext =
		(_UxCaskUserDlg *) UxNewContext( sizeof(_UxCaskUserDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_askUserDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


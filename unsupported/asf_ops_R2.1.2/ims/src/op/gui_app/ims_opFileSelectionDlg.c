
/*******************************************************************************
	ims_opFileSelectionDlg.c

       Associated Header file: ims_opFileSelectionDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/FileSB.h>



static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_opFileSelectionDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_fileSelectionDlg()
{
	Widget		_UxParent;


	/* Creation of fileSelectionDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "fileSelectionDlg_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 300,
			XmNy, 300,
			XmNheight, 400,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "fileSelectionDlg",
			XmNiconName, "fileSelectionDlg",
			NULL );

	}

	fileSelectionDlg = XtVaCreateManagedWidget( "fileSelectionDlg",
			xmFileSelectionBoxWidgetClass,
			_UxParent,
			XmNheight, 400,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-times-medium-r-normal--14-100-100-100-p-74-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-times-medium-r-normal--14-100-100-100-p-74-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8" ),
			RES_CONVERT( XmNdialogTitle, "Save Results As..." ),
			XmNfileTypeMask, XmFILE_REGULAR,
			RES_CONVERT( XmNdirSpec, "" ),
			RES_CONVERT( XmNdirectory, "" ),
			RES_CONVERT( XmNpattern, "*" ),
			NULL );
	UxPutContext( fileSelectionDlg, (char *) UxFileSelectionDlgContext );
	UxPutClassCode( fileSelectionDlg, _UxIfClassId );

	XtVaSetValues(fileSelectionDlg,
			XmNfileListItemCount, 0,
			NULL );


	XtAddCallback( fileSelectionDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxFileSelectionDlgContext);


	return ( fileSelectionDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_fileSelectionDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCfileSelectionDlg    *UxContext;
	static int		_Uxinit = 0;

	UxFileSelectionDlgContext = UxContext =
		(_UxCfileSelectionDlg *) UxNewContext( sizeof(_UxCfileSelectionDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_fileSelectionDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


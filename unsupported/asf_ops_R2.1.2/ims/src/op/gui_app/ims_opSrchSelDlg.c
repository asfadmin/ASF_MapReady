
/*******************************************************************************
	ims_opSrchSelDlg.c

       Associated Header file: ims_opSrchSelDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/SelectioB.h>

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
#include "ims_opSrchSelDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_srchSelDlg()
{
	Widget		_UxParent;


	/* Creation of srchSelDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "srchSelDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 401,
			XmNy, 196,
			XmNwidth, 350,
			XmNheight, 520,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "srchSelDlg",
			NULL );

	srchSelDlg = XtVaCreateWidget( "srchSelDlg",
			xmSelectionBoxWidgetClass,
			_UxParent,
			XmNwidth, 350,
			XmNheight, 520,
			XmNdialogType, XmDIALOG_SELECTION,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNapplyLabelString, "Filter" ),
			RES_CONVERT( XmNchildPlacement, "place_below_selection" ),
			XmNmustMatch, TRUE,
			NULL );
	XtAddCallback( srchSelDlg, XmNapplyCallback,
		(XtCallbackProc) srchSelDlg_filterCb,
		(XtPointer) UxSrchSelDlgContext );
	XtAddCallback( srchSelDlg, XmNokCallback,
		(XtCallbackProc) srchSelDlg_okCb,
		(XtPointer) UxSrchSelDlgContext );

	UxPutContext( srchSelDlg, (char *) UxSrchSelDlgContext );
	UxPutClassCode( srchSelDlg, _UxIfClassId );


	/* Creation of filterForm */
	filterForm = XtVaCreateManagedWidget( "filterForm",
			xmFormWidgetClass,
			srchSelDlg,
			XmNwidth, 328,
			XmNheight, 85,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 11,
			XmNy, 227,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( filterForm, (char *) UxSrchSelDlgContext );


	/* Creation of filterLB */
	filterLB = XtVaCreateManagedWidget( "filterLB",
			xmLabelWidgetClass,
			filterForm,
			XmNx, 4,
			XmNy, 16,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, "Name Filter: [A-Z*]" ),
			NULL );
	UxPutContext( filterLB, (char *) UxSrchSelDlgContext );


	/* Creation of filterTF */
	filterTF = XtVaCreateManagedWidget( "filterTF",
			xmTextFieldWidgetClass,
			filterForm,
			XmNwidth, 328,
			XmNx, 0,
			XmNy, 36,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( filterTF, (char *) UxSrchSelDlgContext );

	XtVaSetValues(srchSelDlg,
			XmNinitialFocus, filterTF,
			NULL );


	XtAddCallback( srchSelDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSrchSelDlgContext);


	return ( srchSelDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_srchSelDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCsrchSelDlg          *UxContext;
	static int		_Uxinit = 0;

	UxSrchSelDlgContext = UxContext =
		(_UxCsrchSelDlg *) UxNewContext( sizeof(_UxCsrchSelDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_srchSelDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


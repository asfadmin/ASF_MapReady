
/*******************************************************************************
	ims_opSelectionBoxDlg.c

       Associated Header file: ims_opSelectionBoxDlg.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
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
#include "ims_opSelectionBoxDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_selectionBoxDlg()
{
	Widget		_UxParent;


	/* Creation of selectionBoxDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "selectionBoxDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 414,
			XmNy, 216,
			XmNwidth, 324,
			XmNheight, 380,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "selectionBoxDlg",
			NULL );

	selectionBoxDlg = XtVaCreateWidget( "selectionBoxDlg",
			xmSelectionBoxWidgetClass,
			_UxParent,
			XmNwidth, 324,
			XmNheight, 380,
			XmNdialogType, XmDIALOG_SELECTION,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNshadowThickness, 2,
			RES_CONVERT( XmNapplyLabelString, "Apply" ),
			RES_CONVERT( XmNchildPlacement, "place_below_selection" ),
			NULL );
	UxPutContext( selectionBoxDlg, (char *) UxSelectionBoxDlgContext );
	UxPutClassCode( selectionBoxDlg, _UxIfClassId );


	/* Creation of commentFormW */
	commentFormW = XtVaCreateManagedWidget( "commentFormW",
			xmFormWidgetClass,
			selectionBoxDlg,
			XmNwidth, 300,
			XmNheight, 112,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 12,
			XmNy, 196,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( commentFormW, (char *) UxSelectionBoxDlgContext );


	/* Creation of label37 */
	label37 = XtVaCreateManagedWidget( "label37",
			xmLabelWidgetClass,
			commentFormW,
			XmNx, 0,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, "Edit Comments" ),
			NULL );
	UxPutContext( label37, (char *) UxSelectionBoxDlgContext );


	/* Creation of selection_commentSW */
	selection_commentSW = XtVaCreateManagedWidget( "selection_commentSW",
			xmScrolledWindowWidgetClass,
			commentFormW,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 0,
			XmNy, 28,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 300,
			XmNheight, 80,
			NULL );
	UxPutContext( selection_commentSW, (char *) UxSelectionBoxDlgContext );


	/* Creation of selection_commentST */
	selection_commentST = XtVaCreateManagedWidget( "selection_commentST",
			xmTextWidgetClass,
			selection_commentSW,
			XmNwidth, 281,
			XmNheight, 76,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmaxLength, 250,
			XmNwordWrap, TRUE,
			XmNscrollHorizontal, FALSE,
			XmNx, 12,
			XmNy, 0,
			NULL );
	UxPutContext( selection_commentST, (char *) UxSelectionBoxDlgContext );


	XtAddCallback( selectionBoxDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSelectionBoxDlgContext);


	return ( selectionBoxDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_selectionBoxDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCselectionBoxDlg     *UxContext;
	static int		_Uxinit = 0;

	UxSelectionBoxDlgContext = UxContext =
		(_UxCselectionBoxDlg *) UxNewContext( sizeof(_UxCselectionBoxDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_selectionBoxDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


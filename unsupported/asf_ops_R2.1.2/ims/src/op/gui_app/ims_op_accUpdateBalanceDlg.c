
/*******************************************************************************
	ims_op_accUpdateBalanceDlg.c

       Associated Header file: ims_op_accUpdateBalanceDlg.h
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
#include "ims_op_accUpdateBalanceDlg.h"
#undef CONTEXT_MACRO_ACCESS

Widget	update_balance_dlg;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_update_balance_dlg()
{
	Widget		_UxParent;


	/* Creation of update_balance_dlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "update_balance_dlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 331,
			XmNy, 345,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "update_balance_dlg",
			NULL );

	update_balance_dlg = XtVaCreateWidget( "update_balance_dlg",
			xmSelectionBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_PROMPT,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNapplyLabelString, "Apply" ),
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			XmNlabelFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNresizePolicy, XmRESIZE_NONE,
			RES_CONVERT( XmNselectionLabelString, "Enter the amount to be added to\nBegin Balance and Current Balance:" ),
			RES_CONVERT( XmNdialogTitle, "Update Balance" ),
			XmNtextColumns, 20,
			XmNtextFontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNtopShadowColor, "#d8e7ec" ),
			XmNtraversalOn, TRUE,
			XmNnavigationType, XmTAB_GROUP,
			XmNdefaultPosition, TRUE,
			XmNsensitive, TRUE,
			XmNautoUnmanage, FALSE,
			NULL );
	XtAddCallback( update_balance_dlg, XmNokCallback,
		(XtCallbackProc) update_balance_dlg_okCb,
		(XtPointer) 0 );
	XtAddCallback( update_balance_dlg, XmNcancelCallback,
		(XtCallbackProc) update_balance_dlg_okCb,
		(XtPointer) 2 );

	UxPutContext( update_balance_dlg, (char *) UxUpdate_balance_dlgContext );
	UxPutClassCode( update_balance_dlg, _UxIfClassId );

	update_balance_dlg_okCb( update_balance_dlg,
			(XtPointer) 1, (XtPointer) NULL );


	XtAddCallback( update_balance_dlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxUpdate_balance_dlgContext);


	return ( update_balance_dlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_update_balance_dlg( swidget _UxUxParent, Widget _Uxaccount_param, Widget _Uxbegin_param, Widget _Uxcurrent_param, Widget _Uxhold_param )
{
	Widget                  rtrn;
	_UxCupdate_balance_dlg  *UxContext;
	static int		_Uxinit = 0;

	UxUpdate_balance_dlgContext = UxContext =
		(_UxCupdate_balance_dlg *) UxNewContext( sizeof(_UxCupdate_balance_dlg), False );

	UxParent = _UxUxParent;
	account_param = _Uxaccount_param;
	begin_param = _Uxbegin_param;
	current_param = _Uxcurrent_param;
	hold_param = _Uxhold_param;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_update_balance_dlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


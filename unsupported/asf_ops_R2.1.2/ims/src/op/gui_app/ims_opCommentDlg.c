
/*******************************************************************************
	ims_opCommentDlg.c

       Associated Header file: ims_opCommentDlg.h
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
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

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
#include "ims_opCommentDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_commentDlg()
{
	Widget		_UxParent;


	/* Creation of commentDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "commentDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 326,
			XmNy, 200,
			XmNwidth, 493,
			XmNheight, 470,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "commentDlg",
			NULL );

	commentDlg = XtVaCreateWidget( "commentDlg",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 493,
			XmNheight, 470,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			XmNbuttonFontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( commentDlg, (char *) UxCommentDlgContext );
	UxPutClassCode( commentDlg, _UxIfClassId );


	/* Creation of commentUpdatePB */
	commentUpdatePB = XtVaCreateManagedWidget( "commentUpdatePB",
			xmPushButtonWidgetClass,
			commentDlg,
			XmNx, 49,
			XmNy, 376,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNheight, 35,
			XmNwidth, 108,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Update" ),
			XmNshadowThickness, 3,
			XmNleftOffset, 40,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 12,
			NULL );
	XtAddCallback( commentUpdatePB, XmNactivateCallback,
		(XtCallbackProc) commentDlg_updateCb,
		(XtPointer) UxCommentDlgContext );

	UxPutContext( commentUpdatePB, (char *) UxCommentDlgContext );


	/* Creation of commentCancelPB */
	commentCancelPB = XtVaCreateManagedWidget( "commentCancelPB",
			xmPushButtonWidgetClass,
			commentDlg,
			XmNx, 332,
			XmNy, 376,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNheight, 35,
			RES_CONVERT( XmNlabelString, "Cancel" ),
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 40,
			XmNshadowThickness, 3,
			XmNwidth, 108,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 12,
			NULL );
	XtAddCallback( commentCancelPB, XmNactivateCallback,
		(XtCallbackProc) commentDlg_cancelCb,
		(XtPointer) UxCommentDlgContext );

	UxPutContext( commentCancelPB, (char *) UxCommentDlgContext );


	/* Creation of commentSW */
	commentSW = XtVaCreateManagedWidget( "commentSW",
			xmScrolledWindowWidgetClass,
			commentDlg,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 12,
			XmNy, 280,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 470,
			NULL );
	UxPutContext( commentSW, (char *) UxCommentDlgContext );


	/* Creation of commentST */
	commentST = XtVaCreateManagedWidget( "commentST",
			xmTextWidgetClass,
			commentSW,
			XmNwidth, 451,
			XmNheight, 116,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmaxLength, 250,
			XmNwordWrap, TRUE,
			XmNscrollHorizontal, FALSE,
			XmNx, 12,
			XmNy, 0,
			NULL );
	UxPutContext( commentST, (char *) UxCommentDlgContext );


	/* Creation of commentLB */
	commentLB = XtVaCreateManagedWidget( "commentLB",
			xmLabelWidgetClass,
			commentDlg,
			XmNx, 8,
			XmNy, 44,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			XmNleftOffset, 10,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 10,
			XmNleftAttachment, XmATTACH_FORM,
			RES_CONVERT( XmNlabelString, "commentLB" ),
			NULL );
	UxPutContext( commentLB, (char *) UxCommentDlgContext );


	/* Creation of commentOldSW */
	commentOldSW = XtVaCreateManagedWidget( "commentOldSW",
			xmScrolledWindowWidgetClass,
			commentDlg,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 12,
			XmNy, 116,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 470,
			XmNheight, 116,
			NULL );
	UxPutContext( commentOldSW, (char *) UxCommentDlgContext );


	/* Creation of commentOldST */
	commentOldST = XtVaCreateManagedWidget( "commentOldST",
			xmTextWidgetClass,
			commentOldSW,
			XmNwidth, 451,
			XmNheight, 76,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmaxLength, 250,
			XmNwordWrap, TRUE,
			XmNscrollHorizontal, FALSE,
			XmNx, 12,
			XmNy, 0,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( commentOldST, (char *) UxCommentDlgContext );


	/* Creation of commentOldLB */
	commentOldLB = XtVaCreateManagedWidget( "commentOldLB",
			xmLabelWidgetClass,
			commentDlg,
			XmNx, 12,
			XmNy, 92,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Existing Comment:" ),
			NULL );
	UxPutContext( commentOldLB, (char *) UxCommentDlgContext );


	/* Creation of commentNewLB */
	commentNewLB = XtVaCreateManagedWidget( "commentNewLB",
			xmLabelWidgetClass,
			commentDlg,
			XmNx, 12,
			XmNy, 256,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Enter New Comment:" ),
			NULL );
	UxPutContext( commentNewLB, (char *) UxCommentDlgContext );


	/* Creation of commentLB1 */
	commentLB1 = XtVaCreateManagedWidget( "commentLB1",
			xmLabelWidgetClass,
			commentDlg,
			XmNx, 184,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Edit   Comment" ),
			NULL );
	UxPutContext( commentLB1, (char *) UxCommentDlgContext );

	XtVaSetValues(commentDlg,
			XmNcancelButton, commentCancelPB,
			XmNdefaultButton, commentUpdatePB,
			NULL );


	XtAddCallback( commentDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCommentDlgContext);


	return ( commentDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_commentDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCcommentDlg          *UxContext;
	static int		_Uxinit = 0;

	UxCommentDlgContext = UxContext =
		(_UxCcommentDlg *) UxNewContext( sizeof(_UxCcommentDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_commentDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


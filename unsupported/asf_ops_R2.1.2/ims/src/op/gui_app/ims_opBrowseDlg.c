
/*******************************************************************************
	ims_opBrowseDlg.c

       Associated Header file: ims_opBrowseDlg.h
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
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
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
#include "ims_opBrowseDlg.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_browseDlg()
{
	Widget		_UxParent;


	/* Creation of browseDlg */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "browseDlg_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 298,
			XmNy, 283,
			XmNwidth, 600,
			XmNheight, 510,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "browseDlg",
			NULL );

	browseDlg = XtVaCreateWidget( "browseDlg",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 600,
			XmNheight, 510,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( browseDlg, (char *) UxBrowseDlgContext );
	UxPutClassCode( browseDlg, _UxIfClassId );


	/* Creation of browseSW */
	browseSW = XtVaCreateManagedWidget( "browseSW",
			xmScrolledWindowWidgetClass,
			browseDlg,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 8,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNleftOffset, 10,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 10,
			XmNleftAttachment, XmATTACH_FORM,
			XmNheight, 390,
			XmNwidth, 450,
			NULL );
	UxPutContext( browseSW, (char *) UxBrowseDlgContext );


	/* Creation of browseST */
	browseST = XtVaCreateManagedWidget( "browseST",
			xmTextWidgetClass,
			browseSW,
			XmNheight, 352,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollHorizontal, TRUE,
			XmNscrollVertical, TRUE,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcursorPositionVisible, FALSE,
			XmNvalue, "",
			NULL );
	UxPutContext( browseST, (char *) UxBrowseDlgContext );


	/* Creation of browseDonePB */
	browseDonePB = XtVaCreateManagedWidget( "browseDonePB",
			xmPushButtonWidgetClass,
			browseDlg,
			XmNx, 232,
			XmNy, 456,
			XmNwidth, 118,
			XmNheight, 35,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Done" ),
			XmNshadowThickness, 3,
			NULL );
	XtAddCallback( browseDonePB, XmNactivateCallback,
		(XtCallbackProc) browseDlg_okCb,
		(XtPointer) UxBrowseDlgContext );

	UxPutContext( browseDonePB, (char *) UxBrowseDlgContext );


	/* Creation of browseDlgLB */
	browseDlgLB = XtVaCreateManagedWidget( "browseDlgLB",
			xmLabelWidgetClass,
			browseDlg,
			XmNx, 180,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 70,
			XmNrightOffset, 70,
			NULL );
	UxPutContext( browseDlgLB, (char *) UxBrowseDlgContext );


	XtAddCallback( browseDlg, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxBrowseDlgContext);


	return ( browseDlg );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_browseDlg( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCbrowseDlg           *UxContext;
	static int		_Uxinit = 0;

	UxBrowseDlgContext = UxContext =
		(_UxCbrowseDlg *) UxNewContext( sizeof(_UxCbrowseDlg), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_browseDlg();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


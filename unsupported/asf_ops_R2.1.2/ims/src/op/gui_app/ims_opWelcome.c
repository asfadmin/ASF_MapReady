
/*******************************************************************************
	ims_opWelcome.c

       Associated Header file: ims_opWelcome.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/PushB.h>
#include <Xm/Label.h>
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
#include "ims_opWelcome.h"
#undef CONTEXT_MACRO_ACCESS

Widget	dl2dtkPB;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_welcome()
{
	Widget		_UxParent;


	/* Creation of welcome */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "welcome_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 130,
			XmNy, 82,
			XmNwidth, 877,
			XmNheight, 760,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "welcome",
			NULL );

	}

	welcome = XtVaCreateManagedWidget( "welcome",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 877,
			XmNheight, 760,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNborderWidth, 10,
			RES_CONVERT( XmNborderColor, "#9ac0cd" ),
			NULL );
	UxPutContext( welcome, (char *) UxWelcomeContext );
	UxPutClassCode( welcome, _UxIfClassId );

	welcome_create_op_interfaces( welcome,
			(XtPointer) UxWelcomeContext, (XtPointer) NULL );


	/* Creation of welcomeTitleLB1 */
	welcomeTitleLB1 = XtVaCreateManagedWidget( "welcomeTitleLB1",
			xmLabelWidgetClass,
			welcome,
			XmNx, 232,
			XmNy, 44,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--34-240-100-100-p-193-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ALASKA  SAR  FACILITY" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNleftOffset, 224,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 224,
			NULL );
	UxPutContext( welcomeTitleLB1, (char *) UxWelcomeContext );


	/* Creation of welcomeTitleLB2 */
	welcomeTitleLB2 = XtVaCreateManagedWidget( "welcomeTitleLB2",
			xmLabelWidgetClass,
			welcome,
			XmNx, 110,
			XmNy, 96,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--25-180-100-100-p-149-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "GEOPHYSICAL INSTITUTE,  FAIRBANKS,  ALASKA " ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 0,
			XmNleftOffset, 97,
			XmNrightAttachment, XmATTACH_NONE,
			XmNrightOffset, 0,
			NULL );
	UxPutContext( welcomeTitleLB2, (char *) UxWelcomeContext );


	/* Creation of welcomeTitleLB3 */
	welcomeTitleLB3 = XtVaCreateManagedWidget( "welcomeTitleLB3",
			xmLabelWidgetClass,
			welcome,
			XmNx, 272,
			XmNy, 480,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--25-180-100-100-p-149-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "IMS/DADS  OPERATIONS" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNwidth, 337,
			XmNleftOffset, 270,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 270,
			NULL );
	UxPutContext( welcomeTitleLB3, (char *) UxWelcomeContext );


	/* Creation of alaska1LBL */
	alaska1LBL = XtVaCreateManagedWidget( "alaska1LBL",
			xmLabelWidgetClass,
			welcome,
			XmNx, 188,
			XmNy, 132,
			XmNlabelType, XmPIXMAP,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 0,
			XmNwidth, 569,
			XmNheight, 292,
			XmNleftOffset, 154,
			XmNrightAttachment, XmATTACH_NONE,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginWidth, 0,
			NULL );
	UxPutContext( alaska1LBL, (char *) UxWelcomeContext );


	/* Creation of photoPB */
	photoPB = XtVaCreateManagedWidget( "photoPB",
			xmPushButtonWidgetClass,
			welcome,
			XmNx, 532,
			XmNy, 600,
			XmNwidth, 204,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "PHOTO     MGMT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			XmNhighlightThickness, 2,
			XmNshadowThickness, 5,
			NULL );
	XtAddCallback( photoPB, XmNactivateCallback,
		(XtCallbackProc) welcome_photoCb,
		(XtPointer) UxWelcomeContext );

	UxPutContext( photoPB, (char *) UxWelcomeContext );


	/* Creation of accountPB */
	accountPB = XtVaCreateManagedWidget( "accountPB",
			xmPushButtonWidgetClass,
			welcome,
			XmNx, 532,
			XmNy, 536,
			XmNwidth, 204,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "ACCOUNT  MGMT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 5,
			NULL );
	XtAddCallback( accountPB, XmNactivateCallback,
		(XtCallbackProc) welcome_accountCb,
		(XtPointer) UxWelcomeContext );

	UxPutContext( accountPB, (char *) UxWelcomeContext );


	/* Creation of orderPB */
	orderPB = XtVaCreateManagedWidget( "orderPB",
			xmPushButtonWidgetClass,
			welcome,
			XmNx, 160,
			XmNy, 536,
			XmNwidth, 204,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "ORDER    MGMT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 5,
			NULL );
	XtAddCallback( orderPB, XmNactivateCallback,
		(XtCallbackProc) welcome_orderCb,
		(XtPointer) UxWelcomeContext );

	UxPutContext( orderPB, (char *) UxWelcomeContext );


	/* Creation of reportsPB */
	reportsPB = XtVaCreateManagedWidget( "reportsPB",
			xmPushButtonWidgetClass,
			welcome,
			XmNx, 160,
			XmNy, 600,
			XmNwidth, 204,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "FILM     MGMT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 5,
			NULL );
	XtAddCallback( reportsPB, XmNactivateCallback,
		(XtCallbackProc) welcome_filmCb,
		(XtPointer) UxWelcomeContext );

	UxPutContext( reportsPB, (char *) UxWelcomeContext );


	/* Creation of exitPB */
	exitPB = XtVaCreateManagedWidget( "exitPB",
			xmPushButtonWidgetClass,
			welcome,
			XmNx, 532,
			XmNy, 666,
			XmNwidth, 204,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "EXIT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 5,
			NULL );
	XtAddCallback( exitPB, XmNactivateCallback,
		(XtCallbackProc) welcome_exitCb,
		(XtPointer) UxWelcomeContext );

	UxPutContext( exitPB, (char *) UxWelcomeContext );


	/* Creation of alaska2LBL */
	alaska2LBL = XtVaCreateManagedWidget( "alaska2LBL",
			xmLabelWidgetClass,
			welcome,
			XmNx, 440,
			XmNy, 132,
			XmNlabelType, XmPIXMAP,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 0,
			XmNwidth, 569,
			XmNheight, 292,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginWidth, 0,
			NULL );
	UxPutContext( alaska2LBL, (char *) UxWelcomeContext );


	/* Creation of dl2dtkPB */
	dl2dtkPB = XtVaCreateManagedWidget( "dl2dtkPB",
			xmPushButtonWidgetClass,
			welcome,
			XmNx, 160,
			XmNy, 666,
			XmNwidth, 204,
			XmNheight, 40,
			RES_CONVERT( XmNlabelString, "DOWNLINK MGMT" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 5,
			NULL );
	XtAddCallback( dl2dtkPB, XmNactivateCallback,
		(XtCallbackProc) welcome_downlinkCb,
		(XtPointer) UxWelcomeContext );

	UxPutContext( dl2dtkPB, (char *) UxWelcomeContext );

	XtVaSetValues(welcome,
			RES_CONVERT( XmNborderPixmap, "/usr/openwin/include/X11/bitmaps/cross_weave" ),
			NULL );

	XtVaSetValues(alaska1LBL,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/alaska1.xpm" ),
			NULL );

	XtVaSetValues(alaska2LBL,
			RES_CONVERT( XmNlabelPixmap, "/local/imsdads/app-defaults/pixmaps/alaska2.xpm" ),
			NULL );


	XtAddCallback( welcome, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxWelcomeContext);


	return ( welcome );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_welcome( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCwelcome             *UxContext;
	static int		_Uxinit = 0;

	UxWelcomeContext = UxContext =
		(_UxCwelcome *) UxNewContext( sizeof(_UxCwelcome), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_welcome();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


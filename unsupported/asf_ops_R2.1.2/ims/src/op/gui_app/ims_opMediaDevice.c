
/*******************************************************************************
	ims_opMediaDevice.c

       Associated Header file: ims_opMediaDevice.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_opCb.h>


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "ims_opMediaDevice.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_mediaDevice()
{
	Widget		_UxParent;


	/* Creation of mediaDevice */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "mediaDevice_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 134,
			XmNy, 0,
			XmNwidth, 976,
			XmNheight, 840,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "mediaDevice",
			NULL );

	}

	mediaDevice = XtVaCreateManagedWidget( "mediaDevice",
			xmFormWidgetClass,
			_UxParent,
			XmNheight, 840,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 976,
			NULL );
	UxPutContext( mediaDevice, (char *) UxMediaDeviceContext );
	UxPutClassCode( mediaDevice, _UxIfClassId );


	/* Creation of mediaSW */
	mediaSW = XtVaCreateManagedWidget( "mediaSW",
			xmScrolledWindowWidgetClass,
			mediaDevice,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 954,
			XmNheight, 714,
			XmNx, 12,
			XmNy, 80,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNvisualPolicy, XmCONSTANT,
			NULL );
	UxPutContext( mediaSW, (char *) UxMediaDeviceContext );


	/* Creation of mediaRC */
	mediaRC = XtVaCreateManagedWidget( "mediaRC",
			xmRowColumnWidgetClass,
			mediaSW,
			XmNwidth, 923,
			XmNheight, 2000,
			XmNy, 2,
			XmNpacking, XmPACK_NONE,
			XmNradioAlwaysOne, FALSE,
			XmNnumColumns, 8,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNentryVerticalAlignment, XmALIGNMENT_CENTER,
			XmNresizeHeight, FALSE,
			NULL );
	UxPutContext( mediaRC, (char *) UxMediaDeviceContext );


	/* Creation of ftpPB */
	ftpPB = XtVaCreateManagedWidget( "ftpPB",
			xmPushButtonWidgetClass,
			mediaRC,
			XmNx, 4,
			XmNy, 6,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Electronic FTP" ),
			XmNshadowThickness, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNmarginLeft, 3,
			XmNmarginRight, 0,
			XmNmarginHeight, 4,
			XmNwidth, 174,
			XmNrecomputeSize, FALSE,
			XmNmarginWidth, 0,
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( ftpPB, (char *) UxMediaDeviceContext );


	/* Creation of ftpAvailLBL */
	ftpAvailLBL = XtVaCreateManagedWidget( "ftpAvailLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 184,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpAvailLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpAllocLBL */
	ftpAllocLBL = XtVaCreateManagedWidget( "ftpAllocLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 288,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpAllocLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpJobStartLBL */
	ftpJobStartLBL = XtVaCreateManagedWidget( "ftpJobStartLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 392,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpJobStartLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpQualLBL */
	ftpQualLBL = XtVaCreateManagedWidget( "ftpQualLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 496,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpQualLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpJobDoneLBL */
	ftpJobDoneLBL = XtVaCreateManagedWidget( "ftpJobDoneLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 600,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpJobDoneLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpJobFailLBL */
	ftpJobFailLBL = XtVaCreateManagedWidget( "ftpJobFailLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 704,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpJobFailLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpOfflineLBL */
	ftpOfflineLBL = XtVaCreateManagedWidget( "ftpOfflineLBL",
			xmLabelWidgetClass,
			mediaRC,
			XmNx, 808,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "LightSteelBlue3" ),
			XmNheight, 26,
			RES_CONVERT( XmNlabelString, "" ),
			XmNwidth, 103,
			XmNborderWidth, 2,
			XmNrecomputeSize, FALSE,
			NULL );
	UxPutContext( ftpOfflineLBL, (char *) UxMediaDeviceContext );


	/* Creation of ftpSW */
	ftpSW = XtVaCreateManagedWidget( "ftpSW",
			xmScrolledWindowWidgetClass,
			mediaRC,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 6,
			XmNy, 40,
			XmNwidth, 890,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 100,
			NULL );
	UxPutContext( ftpSW, (char *) UxMediaDeviceContext );


	/* Creation of ftpST */
	ftpST = XtVaCreateManagedWidget( "ftpST",
			xmTextWidgetClass,
			ftpSW,
			XmNwidth, 892,
			XmNheight, 100,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNvalue, "",
			XmNscrollHorizontal, FALSE,
			XmNeditable, FALSE,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNcursorPositionVisible, FALSE,
			XmNwordWrap, TRUE,
			XmNvalueWcs, UxConvertValueWcs("" ),
			NULL );
	UxPutContext( ftpST, (char *) UxMediaDeviceContext );


	/* Creation of label189 */
	label189 = XtVaCreateManagedWidget( "label189",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 360,
			XmNy, 8,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media  Device  Status  Screen" ),
			XmNheight, 28,
			XmNwidth, 284,
			NULL );
	UxPutContext( label189, (char *) UxMediaDeviceContext );


	/* Creation of label190 */
	label190 = XtVaCreateManagedWidget( "label190",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 196,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Available" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 104,
			NULL );
	UxPutContext( label190, (char *) UxMediaDeviceContext );


	/* Creation of label191 */
	label191 = XtVaCreateManagedWidget( "label191",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 300,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Allocated" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 107,
			NULL );
	UxPutContext( label191, (char *) UxMediaDeviceContext );


	/* Creation of label192 */
	label192 = XtVaCreateManagedWidget( "label192",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 408,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Job Started" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 103,
			NULL );
	UxPutContext( label192, (char *) UxMediaDeviceContext );


	/* Creation of label193 */
	label193 = XtVaCreateManagedWidget( "label193",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 512,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Qual Check" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 103,
			NULL );
	UxPutContext( label193, (char *) UxMediaDeviceContext );


	/* Creation of label194 */
	label194 = XtVaCreateManagedWidget( "label194",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 616,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Job Done" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 103,
			NULL );
	UxPutContext( label194, (char *) UxMediaDeviceContext );


	/* Creation of label195 */
	label195 = XtVaCreateManagedWidget( "label195",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 720,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Job Failed" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 103,
			NULL );
	UxPutContext( label195, (char *) UxMediaDeviceContext );


	/* Creation of label196 */
	label196 = XtVaCreateManagedWidget( "label196",
			xmLabelWidgetClass,
			mediaDevice,
			XmNx, 824,
			XmNy, 48,
			RES_CONVERT( XmNbackground, "#50a0a0" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Off-Line" ),
			XmNborderWidth, 2,
			XmNheight, 25,
			XmNwidth, 100,
			NULL );
	UxPutContext( label196, (char *) UxMediaDeviceContext );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			mediaDevice,
			XmNx, 408,
			XmNy, 796,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLOSE" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNmarginLeft, 2,
			XmNmarginRight, 2,
			XmNmarginHeight, 4,
			XmNwidth, 156,
			XmNheight, 36,
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) mediaDevice_closeCb,
		(XtPointer) UxMediaDeviceContext );

	UxPutContext( closePB, (char *) UxMediaDeviceContext );


	XtAddCallback( mediaDevice, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxMediaDeviceContext);


	return ( mediaDevice );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_mediaDevice( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCmediaDevice         *UxContext;
	static int		_Uxinit = 0;

	UxMediaDeviceContext = UxContext =
		(_UxCmediaDevice *) UxNewContext( sizeof(_UxCmediaDevice), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_mediaDevice();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/



/*******************************************************************************
	ims_opSearch.c

       Associated Header file: ims_opSearch.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/ScrolledW.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/Frame.h>
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
#include "ims_opSearch.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_search()
{
	Widget		_UxParent;
	Widget		menuBar_p1_shell;
	Widget		menuBar1_p2_shell;
	Widget		menuBar1_p3_shell;
	Widget		optionMenu_p1_shell;
	Widget		priorityOM_pane_shell;


	/* Creation of search */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "search_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 28,
			XmNy, 56,
			XmNwidth, 1080,
			XmNheight, 772,
			XmNallowShellResize, FALSE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "search",
			NULL );

	}

	search = XtVaCreateManagedWidget( "search",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1080,
			XmNheight, 772,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNdialogTitle, "Order Search Screen" ),
			XmNnoResize, TRUE,
			NULL );
	UxPutContext( search, (char *) UxSearchContext );
	UxPutClassCode( search, _UxIfClassId );


	/* Creation of srchOrderSearchLB */
	srchOrderSearchLB = XtVaCreateManagedWidget( "srchOrderSearchLB",
			xmLabelWidgetClass,
			search,
			XmNx, 415,
			XmNy, 37,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order     Search     Screen" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 28,
			NULL );
	UxPutContext( srchOrderSearchLB, (char *) UxSearchContext );


	/* Creation of srchUserIdLB */
	srchUserIdLB = XtVaCreateManagedWidget( "srchUserIdLB",
			xmLabelWidgetClass,
			search,
			XmNx, 729,
			XmNy, 97,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User ID:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchUserIdLB, (char *) UxSearchContext );


	/* Creation of srchUserNameLB */
	srchUserNameLB = XtVaCreateManagedWidget( "srchUserNameLB",
			xmLabelWidgetClass,
			search,
			XmNx, 22,
			XmNy, 514,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Name:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchUserNameLB, (char *) UxSearchContext );


	/* Creation of srchOrderReceivedFrame */
	srchOrderReceivedFrame = XtVaCreateManagedWidget( "srchOrderReceivedFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 515,
			XmNheight, 46,
			XmNx, 172,
			XmNy, 600,
			XmNshadowType, XmSHADOW_IN,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( srchOrderReceivedFrame, (char *) UxSearchContext );


	/* Creation of srchOrderReceivedForm */
	srchOrderReceivedForm = XtVaCreateManagedWidget( "srchOrderReceivedForm",
			xmFormWidgetClass,
			srchOrderReceivedFrame,
			XmNwidth, 700,
			XmNheight, 42,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderReceivedForm, (char *) UxSearchContext );


	/* Creation of srchOrderReceivedStartDateLB */
	srchOrderReceivedStartDateLB = XtVaCreateManagedWidget( "srchOrderReceivedStartDateLB",
			xmLabelWidgetClass,
			srchOrderReceivedForm,
			XmNx, 8,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "Start Date:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderReceivedStartDateLB, (char *) UxSearchContext );


	/* Creation of srchOrderReceivedEndDateLB */
	srchOrderReceivedEndDateLB = XtVaCreateManagedWidget( "srchOrderReceivedEndDateLB",
			xmLabelWidgetClass,
			srchOrderReceivedForm,
			XmNx, 256,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "End Date:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderReceivedEndDateLB, (char *) UxSearchContext );


	/* Creation of ordrRecStartDateText */
	ordrRecStartDateText = XtVaCreateManagedWidget( "ordrRecStartDateText",
			xmTextWidgetClass,
			srchOrderReceivedForm,
			XmNwidth, 140,
			XmNx, 112,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( ordrRecStartDateText, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrRecStartDateText, XmNmotionVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrRecStartDateText, XmNlosingFocusCallback,
		(XtCallbackProc) search_date_loseFocusCb,
		(XtPointer) UxSearchContext );

	UxPutContext( ordrRecStartDateText, (char *) UxSearchContext );


	/* Creation of ordrRecEndDateText */
	ordrRecEndDateText = XtVaCreateManagedWidget( "ordrRecEndDateText",
			xmTextWidgetClass,
			srchOrderReceivedForm,
			XmNwidth, 140,
			XmNx, 360,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( ordrRecEndDateText, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrRecEndDateText, XmNmotionVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrRecEndDateText, XmNlosingFocusCallback,
		(XtCallbackProc) search_date_loseFocusCb,
		(XtPointer) UxSearchContext );

	UxPutContext( ordrRecEndDateText, (char *) UxSearchContext );


	/* Creation of srchOrderCompletedLB */
	srchOrderCompletedLB = XtVaCreateManagedWidget( "srchOrderCompletedLB",
			xmLabelWidgetClass,
			search,
			XmNx, 22,
			XmNy, 674,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Completed:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderCompletedLB, (char *) UxSearchContext );


	/* Creation of srchOrderCompletedFrame */
	srchOrderCompletedFrame = XtVaCreateManagedWidget( "srchOrderCompletedFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 515,
			XmNheight, 46,
			XmNx, 172,
			XmNy, 660,
			XmNshadowType, XmSHADOW_IN,
			NULL );
	UxPutContext( srchOrderCompletedFrame, (char *) UxSearchContext );


	/* Creation of srchOrderCompletedForm */
	srchOrderCompletedForm = XtVaCreateManagedWidget( "srchOrderCompletedForm",
			xmFormWidgetClass,
			srchOrderCompletedFrame,
			XmNwidth, 510,
			XmNheight, 42,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, -12,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderCompletedForm, (char *) UxSearchContext );


	/* Creation of srchOrderCompletedStartDateLB */
	srchOrderCompletedStartDateLB = XtVaCreateManagedWidget( "srchOrderCompletedStartDateLB",
			xmLabelWidgetClass,
			srchOrderCompletedForm,
			XmNx, 8,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "Start Date:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderCompletedStartDateLB, (char *) UxSearchContext );


	/* Creation of srchOrderCompletedEndDateLB */
	srchOrderCompletedEndDateLB = XtVaCreateManagedWidget( "srchOrderCompletedEndDateLB",
			xmLabelWidgetClass,
			srchOrderCompletedForm,
			XmNx, 256,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "End Date:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 100,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderCompletedEndDateLB, (char *) UxSearchContext );


	/* Creation of ordrComEndDateText */
	ordrComEndDateText = XtVaCreateManagedWidget( "ordrComEndDateText",
			xmTextWidgetClass,
			srchOrderCompletedForm,
			XmNwidth, 140,
			XmNx, 360,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( ordrComEndDateText, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrComEndDateText, XmNmotionVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrComEndDateText, XmNlosingFocusCallback,
		(XtCallbackProc) search_date_loseFocusCb,
		(XtPointer) UxSearchContext );

	UxPutContext( ordrComEndDateText, (char *) UxSearchContext );


	/* Creation of ordrComStartDateText */
	ordrComStartDateText = XtVaCreateManagedWidget( "ordrComStartDateText",
			xmTextWidgetClass,
			srchOrderCompletedForm,
			XmNwidth, 140,
			XmNx, 112,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( ordrComStartDateText, XmNmodifyVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrComStartDateText, XmNmotionVerifyCallback,
		(XtCallbackProc) search_check_date,
		(XtPointer) UxSearchContext );
	XtAddCallback( ordrComStartDateText, XmNlosingFocusCallback,
		(XtCallbackProc) search_date_loseFocusCb,
		(XtPointer) UxSearchContext );

	UxPutContext( ordrComStartDateText, (char *) UxSearchContext );


	/* Creation of srchUserNameFrame */
	srchUserNameFrame = XtVaCreateManagedWidget( "srchUserNameFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 515,
			XmNheight, 46,
			XmNx, 172,
			XmNy, 500,
			XmNshadowType, XmSHADOW_IN,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( srchUserNameFrame, (char *) UxSearchContext );


	/* Creation of srchUserNameForm */
	srchUserNameForm = XtVaCreateManagedWidget( "srchUserNameForm",
			xmFormWidgetClass,
			srchUserNameFrame,
			XmNwidth, 518,
			XmNheight, 42,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchUserNameForm, (char *) UxSearchContext );


	/* Creation of srchLastNameLB */
	srchLastNameLB = XtVaCreateManagedWidget( "srchLastNameLB",
			xmLabelWidgetClass,
			srchUserNameForm,
			XmNx, 0,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "Last:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 44,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchLastNameLB, (char *) UxSearchContext );


	/* Creation of srchLastNameTF */
	srchLastNameTF = XtVaCreateManagedWidget( "srchLastNameTF",
			xmTextFieldWidgetClass,
			srchUserNameForm,
			XmNwidth, 160,
			XmNx, 44,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNvalue, "",
			XmNheight, 36,
			NULL );
	UxPutContext( srchLastNameTF, (char *) UxSearchContext );


	/* Creation of srchFirstNameLB */
	srchFirstNameLB = XtVaCreateManagedWidget( "srchFirstNameLB",
			xmLabelWidgetClass,
			srchUserNameForm,
			XmNx, 204,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "First:" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 48,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchFirstNameLB, (char *) UxSearchContext );


	/* Creation of srchFirstNameTF */
	srchFirstNameTF = XtVaCreateManagedWidget( "srchFirstNameTF",
			xmTextFieldWidgetClass,
			srchUserNameForm,
			XmNwidth, 160,
			XmNx, 252,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 36,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNvalue, "",
			NULL );
	UxPutContext( srchFirstNameTF, (char *) UxSearchContext );


	/* Creation of srchFirstNameLB1 */
	srchFirstNameLB1 = XtVaCreateManagedWidget( "srchFirstNameLB1",
			xmLabelWidgetClass,
			srchUserNameForm,
			XmNx, 412,
			XmNy, 12,
			RES_CONVERT( XmNlabelString, "M.I." ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNwidth, 42,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchFirstNameLB1, (char *) UxSearchContext );


	/* Creation of middleNameTF */
	middleNameTF = XtVaCreateManagedWidget( "middleNameTF",
			xmTextFieldWidgetClass,
			srchUserNameForm,
			XmNwidth, 44,
			XmNx, 456,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( middleNameTF, (char *) UxSearchContext );


	/* Creation of srchItemStatusLB */
	srchItemStatusLB = XtVaCreateManagedWidget( "srchItemStatusLB",
			xmLabelWidgetClass,
			search,
			XmNx, 188,
			XmNy, 160,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchItemStatusLB, (char *) UxSearchContext );


	/* Creation of srchMediaTypeLB */
	srchMediaTypeLB = XtVaCreateManagedWidget( "srchMediaTypeLB",
			xmLabelWidgetClass,
			search,
			XmNx, 895,
			XmNy, 160,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Media Type" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchMediaTypeLB, (char *) UxSearchContext );


	/* Creation of srchSeparator2 */
	srchSeparator2 = XtVaCreateManagedWidget( "srchSeparator2",
			xmSeparatorWidgetClass,
			search,
			XmNwidth, 1083,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 432,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( srchSeparator2, (char *) UxSearchContext );


	/* Creation of srchProductionStatusLB */
	srchProductionStatusLB = XtVaCreateManagedWidget( "srchProductionStatusLB",
			xmLabelWidgetClass,
			search,
			XmNx, 501,
			XmNy, 160,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "PPF Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchProductionStatusLB, (char *) UxSearchContext );


	/* Creation of srchSeparator3 */
	srchSeparator3 = XtVaCreateManagedWidget( "srchSeparator3",
			xmSeparatorWidgetClass,
			search,
			XmNwidth, 1083,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 716,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( srchSeparator3, (char *) UxSearchContext );


	/* Creation of srchExecuteSearchPB */
	srchExecuteSearchPB = XtVaCreateManagedWidget( "srchExecuteSearchPB",
			xmPushButtonWidgetClass,
			search,
			XmNx, 44,
			XmNy, 728,
			XmNwidth, 192,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, " EXECUTE   SEARCH " ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( srchExecuteSearchPB, XmNactivateCallback,
		(XtCallbackProc) search_executeCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchExecuteSearchPB, (char *) UxSearchContext );


	/* Creation of menuBar1 */
	menuBar1 = XtVaCreateManagedWidget( "menuBar1",
			xmRowColumnWidgetClass,
			search,
			XmNrowColumnType, XmMENU_BAR,
			XmNx, 916,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNheight, 36,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( menuBar1, (char *) UxSearchContext );


	/* Creation of menuBar_p1 */
	menuBar_p1_shell = XtVaCreatePopupShell ("menuBar_p1_shell",
			xmMenuShellWidgetClass, menuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar_p1 = XtVaCreateWidget( "menuBar_p1",
			xmRowColumnWidgetClass,
			menuBar_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar_p1, (char *) UxSearchContext );


	/* Creation of srchWelcomeMPB */
	srchWelcomeMPB = XtVaCreateManagedWidget( "srchWelcomeMPB",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "Welcome Screen" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( srchWelcomeMPB, XmNactivateCallback,
		(XtCallbackProc) search_goto_welcomeCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchWelcomeMPB, (char *) UxSearchContext );


	/* Creation of menuBar_p1_b2 */
	menuBar_p1_b2 = XtVaCreateManagedWidget( "menuBar_p1_b2",
			xmSeparatorWidgetClass,
			menuBar_p1,
			NULL );
	UxPutContext( menuBar_p1_b2, (char *) UxSearchContext );


	/* Creation of gotoOrderMPB */
	gotoOrderMPB = XtVaCreateManagedWidget( "gotoOrderMPB",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "Order Production Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "O" ),
			NULL );
	XtAddCallback( gotoOrderMPB, XmNactivateCallback,
		(XtCallbackProc) search_goto_orderCb,
		(XtPointer) UxSearchContext );

	UxPutContext( gotoOrderMPB, (char *) UxSearchContext );


	/* Creation of menuBar_p1_b4 */
	menuBar_p1_b4 = XtVaCreateManagedWidget( "menuBar_p1_b4",
			xmSeparatorWidgetClass,
			menuBar_p1,
			NULL );
	UxPutContext( menuBar_p1_b4, (char *) UxSearchContext );


	/* Creation of menuBar_p1_b11 */
	menuBar_p1_b11 = XtVaCreateManagedWidget( "menuBar_p1_b11",
			xmPushButtonWidgetClass,
			menuBar_p1,
			RES_CONVERT( XmNlabelString, "Close  Screen" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( menuBar_p1_b11, XmNactivateCallback,
		(XtCallbackProc) search_closeCb,
		(XtPointer) UxSearchContext );

	UxPutContext( menuBar_p1_b11, (char *) UxSearchContext );


	/* Creation of menuBar_top_b1 */
	menuBar_top_b1 = XtVaCreateManagedWidget( "menuBar_top_b1",
			xmCascadeButtonWidgetClass,
			menuBar1,
			RES_CONVERT( XmNlabelString, "Go To" ),
			XmNsubMenuId, menuBar_p1,
			RES_CONVERT( XmNmnemonic, "G" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 10,
			NULL );
	UxPutContext( menuBar_top_b1, (char *) UxSearchContext );


	/* Creation of menuBar1_p2 */
	menuBar1_p2_shell = XtVaCreatePopupShell ("menuBar1_p2_shell",
			xmMenuShellWidgetClass, menuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p2 = XtVaCreateWidget( "menuBar1_p2",
			xmRowColumnWidgetClass,
			menuBar1_p2_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p2, (char *) UxSearchContext );


	/* Creation of srchExecuteSearchMPB */
	srchExecuteSearchMPB = XtVaCreateManagedWidget( "srchExecuteSearchMPB",
			xmPushButtonWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNlabelString, "Execute Search" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( srchExecuteSearchMPB, XmNactivateCallback,
		(XtCallbackProc) search_executeCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchExecuteSearchMPB, (char *) UxSearchContext );


	/* Creation of menuBar1_p2_b7 */
	menuBar1_p2_b7 = XtVaCreateManagedWidget( "menuBar1_p2_b7",
			xmSeparatorWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNbackground, "#7e88ab" ),
			NULL );
	UxPutContext( menuBar1_p2_b7, (char *) UxSearchContext );


	/* Creation of srchClearSearchMPB */
	srchClearSearchMPB = XtVaCreateManagedWidget( "srchClearSearchMPB",
			xmPushButtonWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNlabelString, "Clear  Search" ),
			RES_CONVERT( XmNmnemonic, "l" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( srchClearSearchMPB, XmNactivateCallback,
		(XtCallbackProc) search_clearCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchClearSearchMPB, (char *) UxSearchContext );


	/* Creation of menuBar1_p2_b8 */
	menuBar1_p2_b8 = XtVaCreateManagedWidget( "menuBar1_p2_b8",
			xmSeparatorWidgetClass,
			menuBar1_p2,
			NULL );
	UxPutContext( menuBar1_p2_b8, (char *) UxSearchContext );


	/* Creation of srchPrintScreenMPB */
	srchPrintScreenMPB = XtVaCreateManagedWidget( "srchPrintScreenMPB",
			xmPushButtonWidgetClass,
			menuBar1_p2,
			RES_CONVERT( XmNlabelString, "Print  Screen" ),
			RES_CONVERT( XmNbackground, "cadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( srchPrintScreenMPB, XmNactivateCallback,
		(XtCallbackProc) search_printScreenCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchPrintScreenMPB, (char *) UxSearchContext );


	/* Creation of menuBar1_top_b1 */
	menuBar1_top_b1 = XtVaCreateManagedWidget( "menuBar1_top_b1",
			xmCascadeButtonGadgetClass,
			menuBar1,
			RES_CONVERT( XmNlabelString, "Screen Functions" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, menuBar1_p2,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNmarginWidth, 20,
			NULL );
	UxPutContext( menuBar1_top_b1, (char *) UxSearchContext );


	/* Creation of menuBar1_p3 */
	menuBar1_p3_shell = XtVaCreatePopupShell ("menuBar1_p3_shell",
			xmMenuShellWidgetClass, menuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	menuBar1_p3 = XtVaCreateWidget( "menuBar1_p3",
			xmRowColumnWidgetClass,
			menuBar1_p3_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( menuBar1_p3, (char *) UxSearchContext );


	/* Creation of menuBar1_p3_b1 */
	menuBar1_p3_b1 = XtVaCreateManagedWidget( "menuBar1_p3_b1",
			xmPushButtonWidgetClass,
			menuBar1_p3,
			RES_CONVERT( XmNlabelString, "No Help Available" ),
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	UxPutContext( menuBar1_p3_b1, (char *) UxSearchContext );


	/* Creation of menuBar1_top_b2 */
	menuBar1_top_b2 = XtVaCreateManagedWidget( "menuBar1_top_b2",
			xmCascadeButtonWidgetClass,
			menuBar1,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, menuBar1_p3,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( menuBar1_top_b2, (char *) UxSearchContext );


	/* Creation of srchOrderIdLB */
	srchOrderIdLB = XtVaCreateManagedWidget( "srchOrderIdLB",
			xmLabelWidgetClass,
			search,
			XmNx, 34,
			XmNy, 95,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order ID:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 24,
			NULL );
	UxPutContext( srchOrderIdLB, (char *) UxSearchContext );


	/* Creation of srchProcessingOptionLB */
	srchProcessingOptionLB = XtVaCreateManagedWidget( "srchProcessingOptionLB",
			xmLabelWidgetClass,
			search,
			XmNx, 675,
			XmNy, 160,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Processing Option" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchProcessingOptionLB, (char *) UxSearchContext );


	/* Creation of srchClearSearchPB */
	srchClearSearchPB = XtVaCreateManagedWidget( "srchClearSearchPB",
			xmPushButtonWidgetClass,
			search,
			XmNx, 308,
			XmNy, 728,
			XmNwidth, 192,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLEAR    SEARCH" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( srchClearSearchPB, XmNactivateCallback,
		(XtCallbackProc) search_clearCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchClearSearchPB, (char *) UxSearchContext );


	/* Creation of srchCloseSearchPB */
	srchCloseSearchPB = XtVaCreateManagedWidget( "srchCloseSearchPB",
			xmPushButtonWidgetClass,
			search,
			XmNx, 837,
			XmNy, 728,
			XmNwidth, 192,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "CLOSE    SCREEN" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( srchCloseSearchPB, XmNactivateCallback,
		(XtCallbackProc) search_closeCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchCloseSearchPB, (char *) UxSearchContext );


	/* Creation of srchOrderStatusLB */
	srchOrderStatusLB = XtVaCreateManagedWidget( "srchOrderStatusLB",
			xmLabelWidgetClass,
			search,
			XmNx, 16,
			XmNy, 158,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Status" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNheight, 24,
			NULL );
	UxPutContext( srchOrderStatusLB, (char *) UxSearchContext );


	/* Creation of srchSeparator1 */
	srchSeparator1 = XtVaCreateManagedWidget( "srchSeparator1",
			xmSeparatorWidgetClass,
			search,
			XmNwidth, 1083,
			XmNheight, 8,
			XmNx, 0,
			XmNy, 140,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( srchSeparator1, (char *) UxSearchContext );


	/* Creation of srchOrderReceivedLB */
	srchOrderReceivedLB = XtVaCreateManagedWidget( "srchOrderReceivedLB",
			xmLabelWidgetClass,
			search,
			XmNx, 22,
			XmNy, 614,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Received:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderReceivedLB, (char *) UxSearchContext );


	/* Creation of srchAccountIdLB */
	srchAccountIdLB = XtVaCreateManagedWidget( "srchAccountIdLB",
			xmLabelWidgetClass,
			search,
			XmNx, 335,
			XmNy, 97,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchAccountIdLB, (char *) UxSearchContext );


	/* Creation of srchSeparator4 */
	srchSeparator4 = XtVaCreateManagedWidget( "srchSeparator4",
			xmSeparatorWidgetClass,
			search,
			XmNwidth, 16,
			XmNheight, 278,
			XmNx, 713,
			XmNy, 440,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNorientation, XmVERTICAL,
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( srchSeparator4, (char *) UxSearchContext );


	/* Creation of srchOrderItemLB */
	srchOrderItemLB = XtVaCreateManagedWidget( "srchOrderItemLB",
			xmLabelWidgetClass,
			search,
			XmNx, 895,
			XmNy, 458,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Item" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderItemLB, (char *) UxSearchContext );


	/* Creation of srchValidatedLB */
	srchValidatedLB = XtVaCreateManagedWidget( "srchValidatedLB",
			xmLabelWidgetClass,
			search,
			XmNx, 744,
			XmNy, 498,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Validated:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchValidatedLB, (char *) UxSearchContext );


	/* Creation of srchDebitedLB */
	srchDebitedLB = XtVaCreateManagedWidget( "srchDebitedLB",
			xmLabelWidgetClass,
			search,
			XmNx, 744,
			XmNy, 613,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Debited:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchDebitedLB, (char *) UxSearchContext );


	/* Creation of srchShippedLB */
	srchShippedLB = XtVaCreateManagedWidget( "srchShippedLB",
			xmLabelWidgetClass,
			search,
			XmNx, 744,
			XmNy, 555,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Shipped:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchShippedLB, (char *) UxSearchContext );


	/* Creation of srchBilledLB */
	srchBilledLB = XtVaCreateManagedWidget( "srchBilledLB",
			xmLabelWidgetClass,
			search,
			XmNx, 744,
			XmNy, 669,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Billed:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchBilledLB, (char *) UxSearchContext );


	/* Creation of srchValidatedFrame */
	srchValidatedFrame = XtVaCreateManagedWidget( "srchValidatedFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 225,
			XmNheight, 44,
			XmNx, 831,
			XmNy, 486,
			XmNshadowType, XmSHADOW_IN,
			NULL );
	UxPutContext( srchValidatedFrame, (char *) UxSearchContext );


	/* Creation of srchValidatedRC */
	srchValidatedRC = XtVaCreateManagedWidget( "srchValidatedRC",
			xmRowColumnWidgetClass,
			srchValidatedFrame,
			XmNwidth, 200,
			XmNheight, 38,
			XmNx, 804,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			XmNspacing, 0,
			XmNwhichButton, 1,
			XmNuserData, (XtPointer) 1,
			NULL );
	UxPutContext( srchValidatedRC, (char *) UxSearchContext );


	/* Creation of srchValidatedYesTB */
	srchValidatedYesTB = XtVaCreateManagedWidget( "srchValidatedYesTB",
			xmToggleButtonWidgetClass,
			srchValidatedRC,
			XmNx, 804,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Yes" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 0,
			XmNmarginHeight, 2,
			XmNmarginTop, 6,
			XmNmarginRight, 8,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchValidatedYesTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 0 );

	UxPutContext( srchValidatedYesTB, (char *) UxSearchContext );


	/* Creation of srchValidatedNoTB */
	srchValidatedNoTB = XtVaCreateManagedWidget( "srchValidatedNoTB",
			xmToggleButtonWidgetClass,
			srchValidatedRC,
			XmNx, 804,
			XmNy, 3,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "No" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 67,
			XmNmarginWidth, 2,
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 1,
			XmNheight, 32,
			XmNmarginTop, 6,
			NULL );
	XtAddCallback( srchValidatedNoTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 1 );

	UxPutContext( srchValidatedNoTB, (char *) UxSearchContext );


	/* Creation of srchValidatedBothTB */
	srchValidatedBothTB = XtVaCreateManagedWidget( "srchValidatedBothTB",
			xmToggleButtonWidgetClass,
			srchValidatedRC,
			XmNx, 136,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Any" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNset, TRUE,
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 2,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchValidatedBothTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 2 );

	UxPutContext( srchValidatedBothTB, (char *) UxSearchContext );


	/* Creation of srchShippedFrame */
	srchShippedFrame = XtVaCreateManagedWidget( "srchShippedFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 225,
			XmNheight, 44,
			XmNx, 831,
			XmNy, 543,
			XmNshadowType, XmSHADOW_IN,
			NULL );
	UxPutContext( srchShippedFrame, (char *) UxSearchContext );


	/* Creation of srchShippedRC */
	srchShippedRC = XtVaCreateManagedWidget( "srchShippedRC",
			xmRowColumnWidgetClass,
			srchShippedFrame,
			XmNwidth, 200,
			XmNheight, 36,
			XmNx, 804,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			XmNspacing, 0,
			XmNwhichButton, 1,
			XmNuserData, (XtPointer) 2,
			NULL );
	UxPutContext( srchShippedRC, (char *) UxSearchContext );


	/* Creation of srchShippedYesTB */
	srchShippedYesTB = XtVaCreateManagedWidget( "srchShippedYesTB",
			xmToggleButtonWidgetClass,
			srchShippedRC,
			XmNx, 804,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Yes" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 0,
			XmNmarginRight, 8,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchShippedYesTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 0 );

	UxPutContext( srchShippedYesTB, (char *) UxSearchContext );


	/* Creation of srchShippedNoTB */
	srchShippedNoTB = XtVaCreateManagedWidget( "srchShippedNoTB",
			xmToggleButtonWidgetClass,
			srchShippedRC,
			XmNx, 804,
			XmNy, 3,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "No" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 1,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchShippedNoTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 1 );

	UxPutContext( srchShippedNoTB, (char *) UxSearchContext );


	/* Creation of srchShippedBothTB */
	srchShippedBothTB = XtVaCreateManagedWidget( "srchShippedBothTB",
			xmToggleButtonWidgetClass,
			srchShippedRC,
			XmNx, 75,
			XmNy, 11,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Any" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNset, TRUE,
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 2,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchShippedBothTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 2 );

	UxPutContext( srchShippedBothTB, (char *) UxSearchContext );


	/* Creation of srchDebitedFrame */
	srchDebitedFrame = XtVaCreateManagedWidget( "srchDebitedFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 225,
			XmNheight, 44,
			XmNx, 831,
			XmNy, 601,
			XmNshadowType, XmSHADOW_IN,
			NULL );
	UxPutContext( srchDebitedFrame, (char *) UxSearchContext );


	/* Creation of srchDebitedRC */
	srchDebitedRC = XtVaCreateManagedWidget( "srchDebitedRC",
			xmRowColumnWidgetClass,
			srchDebitedFrame,
			XmNwidth, 200,
			XmNheight, 36,
			XmNx, 804,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			XmNspacing, 0,
			XmNwhichButton, 1,
			XmNuserData, (XtPointer) 3,
			NULL );
	UxPutContext( srchDebitedRC, (char *) UxSearchContext );


	/* Creation of srchDebitedYesTB */
	srchDebitedYesTB = XtVaCreateManagedWidget( "srchDebitedYesTB",
			xmToggleButtonWidgetClass,
			srchDebitedRC,
			XmNx, 804,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Yes" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 0,
			XmNmarginRight, 8,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchDebitedYesTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 0 );

	UxPutContext( srchDebitedYesTB, (char *) UxSearchContext );


	/* Creation of srchDebitedNoTB */
	srchDebitedNoTB = XtVaCreateManagedWidget( "srchDebitedNoTB",
			xmToggleButtonWidgetClass,
			srchDebitedRC,
			XmNx, 804,
			XmNy, 3,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "No" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 1,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchDebitedNoTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 1 );

	UxPutContext( srchDebitedNoTB, (char *) UxSearchContext );


	/* Creation of srchDebitedBothTB */
	srchDebitedBothTB = XtVaCreateManagedWidget( "srchDebitedBothTB",
			xmToggleButtonWidgetClass,
			srchDebitedRC,
			XmNx, 75,
			XmNy, 11,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Any" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNset, TRUE,
			XmNuserData, (XtPointer) 2,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchDebitedBothTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 2 );

	UxPutContext( srchDebitedBothTB, (char *) UxSearchContext );


	/* Creation of srchBilledFrame */
	srchBilledFrame = XtVaCreateManagedWidget( "srchBilledFrame",
			xmFrameWidgetClass,
			search,
			XmNwidth, 225,
			XmNheight, 44,
			XmNx, 831,
			XmNy, 657,
			XmNshadowType, XmSHADOW_IN,
			NULL );
	UxPutContext( srchBilledFrame, (char *) UxSearchContext );


	/* Creation of srchBilledRC */
	srchBilledRC = XtVaCreateManagedWidget( "srchBilledRC",
			xmRowColumnWidgetClass,
			srchBilledFrame,
			XmNwidth, 200,
			XmNheight, 36,
			XmNx, 804,
			XmNy, 2,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNorientation, XmHORIZONTAL,
			XmNradioBehavior, TRUE,
			XmNspacing, 0,
			XmNwhichButton, 1,
			XmNuserData, (XtPointer) 4,
			NULL );
	UxPutContext( srchBilledRC, (char *) UxSearchContext );


	/* Creation of srchBilledYesTB */
	srchBilledYesTB = XtVaCreateManagedWidget( "srchBilledYesTB",
			xmToggleButtonWidgetClass,
			srchBilledRC,
			XmNx, 804,
			XmNy, 3,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Yes" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 0,
			XmNmarginRight, 8,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchBilledYesTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 0 );

	UxPutContext( srchBilledYesTB, (char *) UxSearchContext );


	/* Creation of srchBilledNoTB */
	srchBilledNoTB = XtVaCreateManagedWidget( "srchBilledNoTB",
			xmToggleButtonWidgetClass,
			srchBilledRC,
			XmNx, 804,
			XmNy, 3,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "No" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNuserData, (XtPointer) 1,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchBilledNoTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 1 );

	UxPutContext( srchBilledNoTB, (char *) UxSearchContext );


	/* Creation of srchBilledBothTB */
	srchBilledBothTB = XtVaCreateManagedWidget( "srchBilledBothTB",
			xmToggleButtonWidgetClass,
			srchBilledRC,
			XmNx, 75,
			XmNy, 11,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Any" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			XmNset, TRUE,
			XmNuserData, (XtPointer) 2,
			XmNheight, 32,
			NULL );
	XtAddCallback( srchBilledBothTB, XmNvalueChangedCallback,
		(XtCallbackProc) search_radiobox_toggledCb,
		(XtPointer) 2 );

	UxPutContext( srchBilledBothTB, (char *) UxSearchContext );


	/* Creation of mediaTypeSW */
	mediaTypeSW = XtVaCreateManagedWidget( "mediaTypeSW",
			xmScrolledWindowWidgetClass,
			search,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 180,
			XmNheight, 230,
			XmNx, 891,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	UxPutContext( mediaTypeSW, (char *) UxSearchContext );


	/* Creation of mediaTypeRC */
	mediaTypeRC = XtVaCreateManagedWidget( "mediaTypeRC",
			xmRowColumnWidgetClass,
			mediaTypeSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 149,
			XmNheight, 222,
			XmNresizeWidth, FALSE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNspacing, 0,
			NULL );
	UxPutContext( mediaTypeRC, (char *) UxSearchContext );


	/* Creation of processStatusSW */
	processStatusSW = XtVaCreateManagedWidget( "processStatusSW",
			xmScrolledWindowWidgetClass,
			search,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 165,
			XmNheight, 230,
			XmNx, 476,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	UxPutContext( processStatusSW, (char *) UxSearchContext );


	/* Creation of processStatusRC */
	processStatusRC = XtVaCreateManagedWidget( "processStatusRC",
			xmRowColumnWidgetClass,
			processStatusSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 134,
			XmNheight, 226,
			RES_CONVERT( XmNbackground, "lightSkyBlue3" ),
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			NULL );
	UxPutContext( processStatusRC, (char *) UxSearchContext );


	/* Creation of srchItemStatusSW */
	srchItemStatusSW = XtVaCreateManagedWidget( "srchItemStatusSW",
			xmScrolledWindowWidgetClass,
			search,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 181,
			XmNheight, 230,
			XmNx, 176,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	UxPutContext( srchItemStatusSW, (char *) UxSearchContext );


	/* Creation of itemStatusRC */
	itemStatusRC = XtVaCreateManagedWidget( "itemStatusRC",
			xmRowColumnWidgetClass,
			srchItemStatusSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 150,
			XmNheight, 226,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			NULL );
	UxPutContext( itemStatusRC, (char *) UxSearchContext );


	/* Creation of srchOrderStatusSW */
	srchOrderStatusSW = XtVaCreateManagedWidget( "srchOrderStatusSW",
			xmScrolledWindowWidgetClass,
			search,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 158,
			XmNheight, 230,
			XmNx, 12,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			NULL );
	UxPutContext( srchOrderStatusSW, (char *) UxSearchContext );


	/* Creation of orderStatusRC */
	orderStatusRC = XtVaCreateManagedWidget( "orderStatusRC",
			xmRowColumnWidgetClass,
			srchOrderStatusSW,
			XmNx, 2,
			XmNy, 2,
			XmNwidth, 127,
			XmNheight, 226,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			NULL );
	UxPutContext( orderStatusRC, (char *) UxSearchContext );


	/* Creation of dummyTB */
	dummyTB = XtVaCreateManagedWidget( "dummyTB",
			xmToggleButtonWidgetClass,
			orderStatusRC,
			XmNx, -14,
			XmNy, 0,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNindicatorSize, 25,
			RES_CONVERT( XmNlabelString, "Dummy" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 130,
			RES_CONVERT( XmNselectColor, "SlateGray4" ),
			NULL );
	UxPutContext( dummyTB, (char *) UxSearchContext );


	/* Creation of processOptionSW */
	processOptionSW = XtVaCreateManagedWidget( "processOptionSW",
			xmScrolledWindowWidgetClass,
			search,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNheight, 230,
			XmNx, 648,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 237,
			NULL );
	UxPutContext( processOptionSW, (char *) UxSearchContext );


	/* Creation of processOptionRC */
	processOptionRC = XtVaCreateManagedWidget( "processOptionRC",
			xmRowColumnWidgetClass,
			processOptionSW,
			XmNx, 2,
			XmNy, 4,
			XmNwidth, 206,
			XmNheight, 226,
			RES_CONVERT( XmNbackground, "LightSkyBLue3" ),
			XmNresizeWidth, FALSE,
			XmNmarginWidth, 5,
			XmNspacing, 0,
			NULL );
	UxPutContext( processOptionRC, (char *) UxSearchContext );


	/* Creation of srchOrderIdTF */
	srchOrderIdTF = XtVaCreateManagedWidget( "srchOrderIdTF",
			xmTextFieldWidgetClass,
			search,
			XmNwidth, 150,
			XmNx, 134,
			XmNy, 87,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 40,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( srchOrderIdTF, (char *) UxSearchContext );


	/* Creation of accountIdTF */
	accountIdTF = XtVaCreateManagedWidget( "accountIdTF",
			xmTextFieldWidgetClass,
			search,
			XmNwidth, 160,
			XmNx, 453,
			XmNy, 87,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 40,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNcursorPositionVisible, TRUE,
			NULL );
	UxPutContext( accountIdTF, (char *) UxSearchContext );


	/* Creation of userIdTF */
	userIdTF = XtVaCreateManagedWidget( "userIdTF",
			xmTextFieldWidgetClass,
			search,
			XmNwidth, 160,
			XmNx, 821,
			XmNy, 87,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNheight, 40,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( userIdTF, (char *) UxSearchContext );


	/* Creation of userIdPB */
	userIdPB = XtVaCreateManagedWidget( "userIdPB",
			xmPushButtonWidgetClass,
			search,
			XmNx, 981,
			XmNy, 87,
			XmNwidth, 60,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNheight, 40,
			XmNshadowThickness, 3,
			NULL );
	XtAddCallback( userIdPB, XmNactivateCallback,
		(XtCallbackProc) search_userId_validsCb,
		(XtPointer) UxSearchContext );

	UxPutContext( userIdPB, (char *) UxSearchContext );


	/* Creation of accountIdPB */
	accountIdPB = XtVaCreateManagedWidget( "accountIdPB",
			xmPushButtonWidgetClass,
			search,
			XmNx, 613,
			XmNy, 87,
			XmNwidth, 60,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "List..." ),
			XmNheight, 40,
			XmNshadowThickness, 3,
			NULL );
	XtAddCallback( accountIdPB, XmNactivateCallback,
		(XtCallbackProc) search_accountId_validsCb,
		(XtPointer) UxSearchContext );

	UxPutContext( accountIdPB, (char *) UxSearchContext );


	/* Creation of optionMenu_p1 */
	optionMenu_p1_shell = XtVaCreatePopupShell ("optionMenu_p1_shell",
			xmMenuShellWidgetClass, search,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	optionMenu_p1 = XtVaCreateWidget( "optionMenu_p1",
			xmRowColumnWidgetClass,
			optionMenu_p1_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 150,
			XmNuserData, (XtPointer) 1,
			NULL );
	UxPutContext( optionMenu_p1, (char *) UxSearchContext );


	/* Creation of processTypeAllPB */
	processTypeAllPB = XtVaCreateManagedWidget( "processTypeAllPB",
			xmPushButtonWidgetClass,
			optionMenu_p1,
			RES_CONVERT( XmNlabelString, "ALL" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( processTypeAllPB, XmNactivateCallback,
		(XtCallbackProc) search_optionmenu_toggledCb,
		(XtPointer) 0 );

	UxPutContext( processTypeAllPB, (char *) UxSearchContext );


	/* Creation of optionMenu_p1_b4 */
	optionMenu_p1_b4 = XtVaCreateManagedWidget( "optionMenu_p1_b4",
			xmSeparatorWidgetClass,
			optionMenu_p1,
			NULL );
	UxPutContext( optionMenu_p1_b4, (char *) UxSearchContext );


	/* Creation of optionMenu_p_b1 */
	optionMenu_p_b1 = XtVaCreateManagedWidget( "optionMenu_p_b1",
			xmPushButtonWidgetClass,
			optionMenu_p1,
			RES_CONVERT( XmNlabelString, "STANDARD" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 200,
			XmNmarginWidth, 20,
			XmNuserData, (XtPointer) 0x0,
			NULL );
	XtAddCallback( optionMenu_p_b1, XmNactivateCallback,
		(XtCallbackProc) search_optionmenu_toggledCb,
		(XtPointer) 1 );

	UxPutContext( optionMenu_p_b1, (char *) UxSearchContext );


	/* Creation of optionMenu_p1_b3 */
	optionMenu_p1_b3 = XtVaCreateManagedWidget( "optionMenu_p1_b3",
			xmSeparatorWidgetClass,
			optionMenu_p1,
			NULL );
	UxPutContext( optionMenu_p1_b3, (char *) UxSearchContext );


	/* Creation of optionMenu_p1_b2 */
	optionMenu_p1_b2 = XtVaCreateManagedWidget( "optionMenu_p1_b2",
			xmPushButtonWidgetClass,
			optionMenu_p1,
			RES_CONVERT( XmNlabelString, "QUICK-LOOK" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			XmNuserData, (XtPointer) 0x0,
			NULL );
	XtAddCallback( optionMenu_p1_b2, XmNactivateCallback,
		(XtCallbackProc) search_optionmenu_toggledCb,
		(XtPointer) 2 );

	UxPutContext( optionMenu_p1_b2, (char *) UxSearchContext );


	/* Creation of processingTypeOM */
	processingTypeOM = XtVaCreateManagedWidget( "processingTypeOM",
			xmRowColumnWidgetClass,
			search,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, optionMenu_p1,
			XmNx, 158,
			XmNy, 450,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 200,
			RES_CONVERT( XmNlabelString, " " ),
			NULL );
	UxPutContext( processingTypeOM, (char *) UxSearchContext );


	/* Creation of srchPrintSearchPB */
	srchPrintSearchPB = XtVaCreateManagedWidget( "srchPrintSearchPB",
			xmPushButtonWidgetClass,
			search,
			XmNx, 573,
			XmNy, 728,
			XmNwidth, 192,
			XmNfontList, UxConvertFontList("-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "PRINT     SCREEN" ),
			XmNheight, 32,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNshadowThickness, 4,
			NULL );
	XtAddCallback( srchPrintSearchPB, XmNactivateCallback,
		(XtCallbackProc) search_printScreenCb,
		(XtPointer) UxSearchContext );

	UxPutContext( srchPrintSearchPB, (char *) UxSearchContext );


	/* Creation of srchOrderItemLB1 */
	srchOrderItemLB1 = XtVaCreateManagedWidget( "srchOrderItemLB1",
			xmLabelWidgetClass,
			search,
			XmNx, 382,
			XmNy, 459,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Order Priority:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderItemLB1, (char *) UxSearchContext );


	/* Creation of srchOrderItemLB2 */
	srchOrderItemLB2 = XtVaCreateManagedWidget( "srchOrderItemLB2",
			xmLabelWidgetClass,
			search,
			XmNx, 22,
			XmNy, 459,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Processing Type:" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchOrderItemLB2, (char *) UxSearchContext );


	/* Creation of priorityOM_pane */
	priorityOM_pane_shell = XtVaCreatePopupShell ("priorityOM_pane_shell",
			xmMenuShellWidgetClass, search,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	priorityOM_pane = XtVaCreateWidget( "priorityOM_pane",
			xmRowColumnWidgetClass,
			priorityOM_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 150,
			XmNuserData, (XtPointer) 2,
			NULL );
	UxPutContext( priorityOM_pane, (char *) UxSearchContext );


	/* Creation of orderPriorityAllPB */
	orderPriorityAllPB = XtVaCreateManagedWidget( "orderPriorityAllPB",
			xmPushButtonWidgetClass,
			priorityOM_pane,
			RES_CONVERT( XmNlabelString, "ALL" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNwidth, 200,
			XmNmarginWidth, 20,
			XmNmarginLeft, 10,
			XmNmarginRight, 10,
			NULL );
	XtAddCallback( orderPriorityAllPB, XmNactivateCallback,
		(XtCallbackProc) search_optionmenu_toggledCb,
		(XtPointer) 0 );

	UxPutContext( orderPriorityAllPB, (char *) UxSearchContext );


	/* Creation of orderPriorityOM */
	orderPriorityOM = XtVaCreateManagedWidget( "orderPriorityOM",
			xmRowColumnWidgetClass,
			search,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, priorityOM_pane,
			XmNx, 502,
			XmNy, 450,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 200,
			XmNmarginHeight, 3,
			XmNmarginWidth, 3,
			RES_CONVERT( XmNlabelString, " " ),
			NULL );
	UxPutContext( orderPriorityOM, (char *) UxSearchContext );


	/* Creation of frame1 */
	frame1 = XtVaCreateManagedWidget( "frame1",
			xmFrameWidgetClass,
			search,
			XmNwidth, 516,
			XmNheight, 30,
			XmNx, 172,
			XmNy, 558,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowType, XmSHADOW_ETCHED_IN,
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( frame1, (char *) UxSearchContext );


	/* Creation of label40 */
	label40 = XtVaCreateManagedWidget( "label40",
			xmLabelWidgetClass,
			frame1,
			XmNx, 144,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, "Date Format:   YYYY-MM-DD" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	UxPutContext( label40, (char *) UxSearchContext );


	/* Creation of srchItemStatusLB1 */
	srchItemStatusLB1 = XtVaCreateManagedWidget( "srchItemStatusLB1",
			xmLabelWidgetClass,
			search,
			XmNx, 366,
			XmNy, 160,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Item Type" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( srchItemStatusLB1, (char *) UxSearchContext );


	/* Creation of srchItemTypeSW */
	srchItemTypeSW = XtVaCreateManagedWidget( "srchItemTypeSW",
			xmScrolledWindowWidgetClass,
			search,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 104,
			XmNheight, 230,
			XmNx, 364,
			XmNy, 188,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	UxPutContext( srchItemTypeSW, (char *) UxSearchContext );


	/* Creation of itemTypeRC */
	itemTypeRC = XtVaCreateManagedWidget( "itemTypeRC",
			xmRowColumnWidgetClass,
			srchItemTypeSW,
			XmNy, 0,
			XmNwidth, 73,
			XmNheight, 226,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNresizeHeight, TRUE,
			XmNresizeWidth, FALSE,
			XmNspacing, 0,
			XmNmarginHeight, 5,
			NULL );
	UxPutContext( itemTypeRC, (char *) UxSearchContext );

	XtVaSetValues(menuBar1,
			XmNmenuHelpWidget, menuBar1_top_b2,
			NULL );


	XtAddCallback( search, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxSearchContext);


	return ( search );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_search( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCsearch              *UxContext;
	static int		_Uxinit = 0;

	UxSearchContext = UxContext =
		(_UxCsearch *) UxNewContext( sizeof(_UxCsearch), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_search();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


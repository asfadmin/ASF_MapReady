
/*******************************************************************************
	ims_op_accUserData.c

       Associated Header file: ims_op_accUserData.h
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
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>

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
#include "ims_op_accUserData.h"
#undef CONTEXT_MACRO_ACCESS

Widget	user_data;
Widget	user_dataLB;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_user_data()
{
	Widget		_UxParent;
	Widget		option_menu_pane_type_shell;
	Widget		option_menu_pane_priority_shell;


	/* Creation of user_data */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "user_data_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 538,
			XmNy, 48,
			XmNwidth, 559,
			XmNheight, 671,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "user_data",
			NULL );

	}

	user_data = XtVaCreateManagedWidget( "user_data",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 559,
			XmNheight, 671,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( user_data, (char *) UxUser_dataContext );
	UxPutClassCode( user_data, _UxIfClassId );


	/* Creation of scrolledWindow2 */
	scrolledWindow2 = XtVaCreateManagedWidget( "scrolledWindow2",
			xmScrolledWindowWidgetClass,
			user_data,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNwidth, 530,
			XmNheight, 566,
			XmNx, 16,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( scrolledWindow2, (char *) UxUser_dataContext );


	/* Creation of form5 */
	form5 = XtVaCreateManagedWidget( "form5",
			xmFormWidgetClass,
			scrolledWindow2,
			XmNwidth, 496,
			XmNheight, 758,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 2,
			XmNy, -2,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form5, (char *) UxUser_dataContext );


	/* Creation of label25 */
	label25 = XtVaCreateManagedWidget( "label25",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 90,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Last Name:" ),
			NULL );
	UxPutContext( label25, (char *) UxUser_dataContext );


	/* Creation of label26 */
	label26 = XtVaCreateManagedWidget( "label26",
			xmLabelWidgetClass,
			form5,
			XmNx, 352,
			XmNy, 50,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "M.I.:" ),
			NULL );
	UxPutContext( label26, (char *) UxUser_dataContext );


	/* Creation of idTF */
	idTF = XtVaCreateManagedWidget( "idTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 6,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 15,
			XmNcolumns, 15,
			NULL );
	XtAddCallback( idTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( idTF, (char *) UxUser_dataContext );

	user_data_user_idCb( idTF,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of m_iTF */
	m_iTF = XtVaCreateManagedWidget( "m_iTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 44,
			XmNx, 402,
			XmNy, 44,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 3,
			XmNmaxLength, 3,
			NULL );
	XtAddCallback( m_iTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer)XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( m_iTF, (char *) UxUser_dataContext );


	/* Creation of label27 */
	label27 = XtVaCreateManagedWidget( "label27",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 132,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Type:" ),
			NULL );
	UxPutContext( label27, (char *) UxUser_dataContext );


	/* Creation of label28 */
	label28 = XtVaCreateManagedWidget( "label28",
			xmLabelWidgetClass,
			form5,
			XmNx, 10,
			XmNy, 254,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Title:" ),
			NULL );
	UxPutContext( label28, (char *) UxUser_dataContext );


	/* Creation of label29 */
	label29 = XtVaCreateManagedWidget( "label29",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 372,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "City:" ),
			NULL );
	UxPutContext( label29, (char *) UxUser_dataContext );


	/* Creation of label30 */
	label30 = XtVaCreateManagedWidget( "label30",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 414,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "State:" ),
			XmNheight, 28,
			NULL );
	UxPutContext( label30, (char *) UxUser_dataContext );


	/* Creation of label31 */
	label31 = XtVaCreateManagedWidget( "label31",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 336,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Address:" ),
			NULL );
	UxPutContext( label31, (char *) UxUser_dataContext );


	/* Creation of label32 */
	label32 = XtVaCreateManagedWidget( "label32",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 492,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Zip:" ),
			NULL );
	UxPutContext( label32, (char *) UxUser_dataContext );


	/* Creation of label33 */
	label33 = XtVaCreateManagedWidget( "label33",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 456,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Country:" ),
			NULL );
	UxPutContext( label33, (char *) UxUser_dataContext );


	/* Creation of label34 */
	label34 = XtVaCreateManagedWidget( "label34",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 534,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Phone:" ),
			XmNwidth, 52,
			NULL );
	UxPutContext( label34, (char *) UxUser_dataContext );


	/* Creation of IDLB */
	IDLB = XtVaCreateManagedWidget( "IDLB",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "ID:" ),
			NULL );
	UxPutContext( IDLB, (char *) UxUser_dataContext );


	/* Creation of first_nameLB */
	first_nameLB = XtVaCreateManagedWidget( "first_nameLB",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 50,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "First Name:" ),
			NULL );
	UxPutContext( first_nameLB, (char *) UxUser_dataContext );


	/* Creation of first_nameTF */
	first_nameTF = XtVaCreateManagedWidget( "first_nameTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 44,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			NULL );
	XtAddCallback( first_nameTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( first_nameTF, (char *) UxUser_dataContext );


	/* Creation of last_nameTF */
	last_nameTF = XtVaCreateManagedWidget( "last_nameTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 84,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNvalue, "",
			NULL );
	XtAddCallback( last_nameTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( last_nameTF, (char *) UxUser_dataContext );


	/* Creation of label37 */
	label37 = XtVaCreateManagedWidget( "label37",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 294,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Organization:" ),
			NULL );
	UxPutContext( label37, (char *) UxUser_dataContext );


	/* Creation of organizationTF */
	organizationTF = XtVaCreateManagedWidget( "organizationTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 286,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 35,
			XmNcolumns, 35,
			XmNvalue, "",
			XmNheight, 35,
			NULL );
	XtAddCallback( organizationTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( organizationTF, (char *) UxUser_dataContext );


	/* Creation of titleTF */
	titleTF = XtVaCreateManagedWidget( "titleTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 244,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 10,
			XmNcolumns, 10,
			XmNheight, 35,
			NULL );
	XtAddCallback( titleTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( titleTF, (char *) UxUser_dataContext );


	/* Creation of addressTF */
	addressTF = XtVaCreateManagedWidget( "addressTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 326,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 128,
			XmNcolumns, 128,
			XmNheight, 35,
			NULL );
	XtAddCallback( addressTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( addressTF, (char *) UxUser_dataContext );


	/* Creation of cityTF */
	cityTF = XtVaCreateManagedWidget( "cityTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 364,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 30,
			XmNcolumns, 30,
			XmNheight, 35,
			NULL );
	XtAddCallback( cityTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( cityTF, (char *) UxUser_dataContext );


	/* Creation of stateTF */
	stateTF = XtVaCreateManagedWidget( "stateTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 406,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			XmNheight, 35,
			NULL );
	XtAddCallback( stateTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( stateTF, (char *) UxUser_dataContext );


	/* Creation of countryTF */
	countryTF = XtVaCreateManagedWidget( "countryTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 446,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			XmNheight, 35,
			NULL );
	XtAddCallback( countryTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( countryTF, (char *) UxUser_dataContext );


	/* Creation of zipTF */
	zipTF = XtVaCreateManagedWidget( "zipTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 486,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 10,
			XmNcolumns, 10,
			XmNheight, 35,
			NULL );
	XtAddCallback( zipTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( zipTF, (char *) UxUser_dataContext );


	/* Creation of phoneTF */
	phoneTF = XtVaCreateManagedWidget( "phoneTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 526,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 25,
			XmNcolumns, 25,
			XmNheight, 35,
			NULL );
	XtAddCallback( phoneTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( phoneTF, (char *) UxUser_dataContext );


	/* Creation of faxTF */
	faxTF = XtVaCreateManagedWidget( "faxTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 568,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 25,
			XmNcolumns, 25,
			XmNheight, 35,
			NULL );
	XtAddCallback( faxTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( faxTF, (char *) UxUser_dataContext );


	/* Creation of emailTF */
	emailTF = XtVaCreateManagedWidget( "emailTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 342,
			XmNx, 114,
			XmNy, 608,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 128,
			XmNcolumns, 128,
			XmNheight, 35,
			NULL );
	XtAddCallback( emailTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( emailTF, (char *) UxUser_dataContext );


	/* Creation of label38 */
	label38 = XtVaCreateManagedWidget( "label38",
			xmLabelWidgetClass,
			form5,
			XmNx, 2,
			XmNy, 576,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Fax:" ),
			XmNwidth, 52,
			NULL );
	UxPutContext( label38, (char *) UxUser_dataContext );


	/* Creation of label40 */
	label40 = XtVaCreateManagedWidget( "label40",
			xmLabelWidgetClass,
			form5,
			XmNx, -1,
			XmNy, 658,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "FTP Dir:" ),
			XmNwidth, 91,
			NULL );
	UxPutContext( label40, (char *) UxUser_dataContext );


	/* Creation of priorityTF */
	priorityTF = XtVaCreateManagedWidget( "priorityTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 166,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( priorityTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( priorityTF, (char *) UxUser_dataContext );


	/* Creation of typeTF */
	typeTF = XtVaCreateManagedWidget( "typeTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 126,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNeditable, FALSE,
			NULL );
	XtAddCallback( typeTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( typeTF, (char *) UxUser_dataContext );


	/* Creation of label42 */
	label42 = XtVaCreateManagedWidget( "label42",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 210,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Password:" ),
			NULL );
	UxPutContext( label42, (char *) UxUser_dataContext );


	/* Creation of passwordTF */
	passwordTF = XtVaCreateManagedWidget( "passwordTF",
			xmTextFieldWidgetClass,
			form5,
			XmNwidth, 220,
			XmNx, 114,
			XmNy, 206,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNcolumns, 20,
			XmNheight, 35,
			NULL );
	XtAddCallback( passwordTF, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( passwordTF, (char *) UxUser_dataContext );


	/* Creation of label43 */
	label43 = XtVaCreateManagedWidget( "label43",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 614,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Email:" ),
			XmNwidth, 52,
			NULL );
	UxPutContext( label43, (char *) UxUser_dataContext );


	/* Creation of ftp_dirSW */
	ftp_dirSW = XtVaCreateManagedWidget( "ftp_dirSW",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 115,
			XmNy, 653,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 362,
			XmNheight, 35,
			XmNshadowThickness, 1,
			NULL );
	UxPutContext( ftp_dirSW, (char *) UxUser_dataContext );


	/* Creation of ftp_dirST */
	ftp_dirST = XtVaCreateManagedWidget( "ftp_dirST",
			xmTextWidgetClass,
			ftp_dirSW,
			XmNwidth, 340,
			XmNheight, 35,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNwordWrap, TRUE,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 29,
			XmNrows, 2,
			XmNscrollHorizontal, FALSE,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNscrollVertical, TRUE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( ftp_dirST, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( ftp_dirST, (char *) UxUser_dataContext );


	/* Creation of label55 */
	label55 = XtVaCreateManagedWidget( "label55",
			xmLabelWidgetClass,
			form5,
			XmNx, 8,
			XmNy, 170,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Priority:" ),
			NULL );
	UxPutContext( label55, (char *) UxUser_dataContext );


	/* Creation of option_menu_pane_type */
	option_menu_pane_type_shell = XtVaCreatePopupShell ("option_menu_pane_type_shell",
			xmMenuShellWidgetClass, form5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	option_menu_pane_type = XtVaCreateWidget( "option_menu_pane_type",
			xmRowColumnWidgetClass,
			option_menu_pane_type_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 90,
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( option_menu_pane_type, (char *) UxUser_dataContext );


	/* Creation of option_menu_typePB */
	option_menu_typePB = XtVaCreateManagedWidget( "option_menu_typePB",
			xmPushButtonWidgetClass,
			option_menu_pane_type,
			RES_CONVERT( XmNlabelString, "NONE" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( option_menu_typePB, XmNactivateCallback,
		(XtCallbackProc) user_data_option_typeCb,
		(XtPointer) 0 );

	UxPutContext( option_menu_typePB, (char *) UxUser_dataContext );

	user_data_option_typeCb( option_menu_typePB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of option_menu_type */
	option_menu_type = XtVaCreateManagedWidget( "option_menu_type",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_type,
			XmNx, 336,
			XmNy, 124,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			XmNwidth, 133,
			NULL );
	UxPutContext( option_menu_type, (char *) UxUser_dataContext );


	/* Creation of option_menu_pane_priority */
	option_menu_pane_priority_shell = XtVaCreatePopupShell ("option_menu_pane_priority_shell",
			xmMenuShellWidgetClass, form5,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	option_menu_pane_priority = XtVaCreateWidget( "option_menu_pane_priority",
			xmRowColumnWidgetClass,
			option_menu_pane_priority_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 90,
			XmNresizeWidth, FALSE,
			NULL );
	UxPutContext( option_menu_pane_priority, (char *) UxUser_dataContext );


	/* Creation of option_menu_priorityPB */
	option_menu_priorityPB = XtVaCreateManagedWidget( "option_menu_priorityPB",
			xmPushButtonWidgetClass,
			option_menu_pane_priority,
			RES_CONVERT( XmNlabelString, "NONE" ),
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			NULL );
	XtAddCallback( option_menu_priorityPB, XmNactivateCallback,
		(XtCallbackProc) user_data_option_priorityCb,
		(XtPointer) 0 );

	UxPutContext( option_menu_priorityPB, (char *) UxUser_dataContext );

	user_data_option_priorityCb( option_menu_priorityPB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of option_menu_priority */
	option_menu_priority = XtVaCreateManagedWidget( "option_menu_priority",
			xmRowColumnWidgetClass,
			form5,
			XmNrowColumnType, XmMENU_OPTION,
			XmNsubMenuId, option_menu_pane_priority,
			XmNx, 336,
			XmNy, 164,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNlabelString, " " ),
			XmNwidth, 133,
			NULL );
	UxPutContext( option_menu_priority, (char *) UxUser_dataContext );


	/* Creation of access_dirSW */
	access_dirSW = XtVaCreateManagedWidget( "access_dirSW",
			xmScrolledWindowWidgetClass,
			form5,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 115,
			XmNy, 697,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNwidth, 362,
			XmNheight, 35,
			XmNshadowThickness, 1,
			NULL );
	UxPutContext( access_dirSW, (char *) UxUser_dataContext );


	/* Creation of access_dirST */
	access_dirST = XtVaCreateManagedWidget( "access_dirST",
			xmTextWidgetClass,
			access_dirSW,
			XmNwidth, 340,
			XmNheight, 35,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNmaxLength, 255,
			XmNwordWrap, TRUE,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNcolumns, 29,
			XmNrows, 2,
			XmNscrollHorizontal, FALSE,
			XmNresizeHeight, FALSE,
			XmNresizeWidth, FALSE,
			XmNscrollVertical, TRUE,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			NULL );
	XtAddCallback( access_dirST, XmNactivateCallback,
		(XtCallbackProc) XmProcessTraversal,
		(XtPointer) XmTRAVERSE_NEXT_TAB_GROUP );

	UxPutContext( access_dirST, (char *) UxUser_dataContext );


	/* Creation of label153 */
	label153 = XtVaCreateManagedWidget( "label153",
			xmLabelWidgetClass,
			form5,
			XmNx, 2,
			XmNy, 703,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Access Dir:" ),
			XmNwidth, 102,
			NULL );
	UxPutContext( label153, (char *) UxUser_dataContext );


	/* Creation of user_dataLB */
	user_dataLB = XtVaCreateManagedWidget( "user_dataLB",
			xmLabelWidgetClass,
			user_data,
			XmNx, 188,
			XmNy, 22,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Data" ),
			XmNwidth, 182,
			NULL );
	UxPutContext( user_dataLB, (char *) UxUser_dataContext );

	user_data_labelCb( user_dataLB,
			(XtPointer) UxUser_dataContext, (XtPointer) NULL );


	/* Creation of createPB */
	createPB = XtVaCreateManagedWidget( "createPB",
			xmPushButtonWidgetClass,
			user_data,
			XmNx, 18,
			XmNy, 628,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Create" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNrecomputeSize, FALSE,
			NULL );
	XtAddCallback( createPB, XmNactivateCallback,
		(XtCallbackProc) user_data_createCb,
		(XtPointer)0 );

	UxPutContext( createPB, (char *) UxUser_dataContext );

	user_data_createCb( createPB,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of closePB */
	closePB = XtVaCreateManagedWidget( "closePB",
			xmPushButtonWidgetClass,
			user_data,
			XmNx, 404,
			XmNy, 628,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Close" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( closePB, XmNactivateCallback,
		(XtCallbackProc) user_data_closeCb,
		(XtPointer) UxUser_dataContext );

	UxPutContext( closePB, (char *) UxUser_dataContext );


	XtAddCallback( user_data, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxUser_dataContext);


	return ( user_data );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_user_data( swidget _UxUxParent, int _Uxcreate )
{
	Widget                  rtrn;
	_UxCuser_data           *UxContext;
	static int		_Uxinit = 0;

	UxUser_dataContext = UxContext =
		(_UxCuser_data *) UxNewContext( sizeof(_UxCuser_data), False );

	UxParent = _UxUxParent;
	create = _Uxcreate;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_user_data();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


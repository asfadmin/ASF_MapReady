
/*******************************************************************************
	ims_op_accAssignUser.c

       Associated Header file: ims_op_accAssignUser.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/TextF.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
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
#include "ims_op_accAssignUser.h"
#undef CONTEXT_MACRO_ACCESS

Widget	assign_users;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_assign_users()
{
	Widget		_UxParent;


	/* Creation of assign_users */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "assign_users_shell",
			transientShellWidgetClass, UxTopLevel,
			XmNx, 9,
			XmNy, 82,
			XmNwidth, 1036,
			XmNheight, 770,
			XmNallowShellResize, TRUE,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "assign_users",
			NULL );

	}

	assign_users = XtVaCreateManagedWidget( "assign_users",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 1036,
			XmNheight, 770,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			RES_CONVERT( XmNdialogTitle, "Assign Users" ),
			XmNsensitive, TRUE,
			XmNautoUnmanage, TRUE,
			NULL );
	UxPutContext( assign_users, (char *) UxAssign_usersContext );
	UxPutClassCode( assign_users, _UxIfClassId );


	/* Creation of label1 */
	label1 = XtVaCreateManagedWidget( "label1",
			xmLabelWidgetClass,
			assign_users,
			XmNx, 382,
			XmNy, 18,
			XmNwidth, 256,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Assign Users to Account" ),
			NULL );
	UxPutContext( label1, (char *) UxAssign_usersContext );


	/* Creation of frame1 */
	frame1 = XtVaCreateManagedWidget( "frame1",
			xmFrameWidgetClass,
			assign_users,
			XmNwidth, 481,
			XmNheight, 512,
			XmNx, 531,
			XmNy, 204,
			XmNshadowThickness, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame1, (char *) UxAssign_usersContext );


	/* Creation of form1 */
	form1 = XtVaCreateManagedWidget( "form1",
			xmFormWidgetClass,
			frame1,
			XmNwidth, 476,
			XmNheight, 509,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 3,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form1, (char *) UxAssign_usersContext );


	/* Creation of label6 */
	label6 = XtVaCreateManagedWidget( "label6",
			xmLabelWidgetClass,
			form1,
			XmNx, 72,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User ID" ),
			NULL );
	UxPutContext( label6, (char *) UxAssign_usersContext );


	/* Creation of label7 */
	label7 = XtVaCreateManagedWidget( "label7",
			xmLabelWidgetClass,
			form1,
			XmNx, 234,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Name" ),
			NULL );
	UxPutContext( label7, (char *) UxAssign_usersContext );


	/* Creation of deletePB */
	deletePB = XtVaCreateManagedWidget( "deletePB",
			xmPushButtonWidgetClass,
			form1,
			XmNx, 178,
			XmNy, 464,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Delete" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( deletePB, XmNactivateCallback,
		(XtCallbackProc) assign_users_delete_usersCb,
		(XtPointer) UxAssign_usersContext );

	UxPutContext( deletePB, (char *) UxAssign_usersContext );


	/* Creation of assigned_usersSW */
	assigned_usersSW = XtVaCreateManagedWidget( "assigned_usersSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 18,
			XmNy, 40,
			XmNwidth, 422,
			XmNheight, 416,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assigned_usersSW, (char *) UxAssign_usersContext );


	/* Creation of assigned_usersSL */
	assigned_usersSL = XtVaCreateManagedWidget( "assigned_usersSL",
			xmListWidgetClass,
			assigned_usersSW,
			XmNwidth, 422,
			XmNheight, 386,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			NULL );
	XtAddCallback( assigned_usersSL, XmNdefaultActionCallback,
		(XtCallbackProc) assign_users_assigned_listCb,
		(XtPointer) UxAssign_usersContext );
	XtAddCallback( assigned_usersSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_users_assigned_listCb,
		(XtPointer) UxAssign_usersContext );

	UxPutContext( assigned_usersSL, (char *) UxAssign_usersContext );


	/* Creation of assigned_users_sbSW */
	assigned_users_sbSW = XtVaCreateManagedWidget( "assigned_users_sbSW",
			xmScrolledWindowWidgetClass,
			form1,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 1,
			XmNx, 448,
			XmNy, 40,
			XmNwidth, 15,
			XmNheight, 392,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assigned_users_sbSW, (char *) UxAssign_usersContext );


	/* Creation of assigned_users_sbSL */
	assigned_users_sbSL = XtVaCreateManagedWidget( "assigned_users_sbSL",
			xmListWidgetClass,
			assigned_users_sbSW,
			XmNwidth, 2,
			XmNheight, 378,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	UxPutContext( assigned_users_sbSL, (char *) UxAssign_usersContext );

	assign_users_assigned_sbCb( assigned_users_sbSL,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of closeButton */
	closeButton = XtVaCreateManagedWidget( "closeButton",
			xmPushButtonWidgetClass,
			assign_users,
			XmNx, 870,
			XmNy, 728,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Cancel" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNleftOffset, 870,
			XmNtopOffset, 728,
			NULL );
	XtAddCallback( closeButton, XmNactivateCallback,
		(XtCallbackProc) assign_users_closeCb,
		(XtPointer) UxAssign_usersContext );

	UxPutContext( closeButton, (char *) UxAssign_usersContext );


	/* Creation of frame4 */
	frame4 = XtVaCreateManagedWidget( "frame4",
			xmFrameWidgetClass,
			assign_users,
			XmNwidth, 988,
			XmNheight, 104,
			XmNx, 24,
			XmNy, 58,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNshadowThickness, 3,
			NULL );
	UxPutContext( frame4, (char *) UxAssign_usersContext );


	/* Creation of form6 */
	form6 = XtVaCreateManagedWidget( "form6",
			xmFormWidgetClass,
			frame4,
			XmNwidth, 200,
			XmNheight, 200,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 0,
			XmNy, 0,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form6, (char *) UxAssign_usersContext );


	/* Creation of label44 */
	label44 = XtVaCreateManagedWidget( "label44",
			xmLabelWidgetClass,
			form6,
			XmNx, 78,
			XmNy, 18,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Account ID:" ),
			NULL );
	UxPutContext( label44, (char *) UxAssign_usersContext );


	/* Creation of account_idTF */
	account_idTF = XtVaCreateManagedWidget( "account_idTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 192,
			XmNy, 12,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 20,
			XmNheight, 35,
			XmNeditable, FALSE,
			XmNcursorPositionVisible, FALSE,
			NULL );
	UxPutContext( account_idTF, (char *) UxAssign_usersContext );


	/* Creation of label56 */
	label56 = XtVaCreateManagedWidget( "label56",
			xmLabelWidgetClass,
			form6,
			XmNx, 532,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Current Balance:" ),
			NULL );
	UxPutContext( label56, (char *) UxAssign_usersContext );


	/* Creation of current_balanceTF */
	current_balanceTF = XtVaCreateManagedWidget( "current_balanceTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 672,
			XmNy, 10,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( current_balanceTF, (char *) UxAssign_usersContext );


	/* Creation of label59 */
	label59 = XtVaCreateManagedWidget( "label59",
			xmLabelWidgetClass,
			form6,
			XmNx, 78,
			XmNy, 56,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Creation:" ),
			NULL );
	UxPutContext( label59, (char *) UxAssign_usersContext );


	/* Creation of creationTF */
	creationTF = XtVaCreateManagedWidget( "creationTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 192,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( creationTF, (char *) UxAssign_usersContext );


	/* Creation of expirationTF */
	expirationTF = XtVaCreateManagedWidget( "expirationTF",
			xmTextFieldWidgetClass,
			form6,
			XmNwidth, 220,
			XmNx, 672,
			XmNy, 52,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			XmNmaxLength, 50,
			XmNcolumns, 50,
			XmNheight, 35,
			XmNcursorPositionVisible, FALSE,
			XmNeditable, FALSE,
			NULL );
	UxPutContext( expirationTF, (char *) UxAssign_usersContext );


	/* Creation of label60 */
	label60 = XtVaCreateManagedWidget( "label60",
			xmLabelWidgetClass,
			form6,
			XmNx, 532,
			XmNy, 58,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Expiration:" ),
			NULL );
	UxPutContext( label60, (char *) UxAssign_usersContext );


	/* Creation of frame5 */
	frame5 = XtVaCreateManagedWidget( "frame5",
			xmFrameWidgetClass,
			assign_users,
			XmNwidth, 483,
			XmNheight, 512,
			XmNx, 26,
			XmNy, 204,
			XmNshadowThickness, 3,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( frame5, (char *) UxAssign_usersContext );


	/* Creation of form4 */
	form4 = XtVaCreateManagedWidget( "form4",
			xmFormWidgetClass,
			frame5,
			XmNwidth, 475,
			XmNheight, 535,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 4,
			XmNy, 4,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			NULL );
	UxPutContext( form4, (char *) UxAssign_usersContext );


	/* Creation of label57 */
	label57 = XtVaCreateManagedWidget( "label57",
			xmLabelWidgetClass,
			form4,
			XmNx, 72,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User ID" ),
			NULL );
	UxPutContext( label57, (char *) UxAssign_usersContext );


	/* Creation of label58 */
	label58 = XtVaCreateManagedWidget( "label58",
			xmLabelWidgetClass,
			form4,
			XmNx, 234,
			XmNy, 14,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "User Name" ),
			NULL );
	UxPutContext( label58, (char *) UxAssign_usersContext );


	/* Creation of assign_usersSW */
	assign_usersSW = XtVaCreateManagedWidget( "assign_usersSW",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAUTOMATIC,
			XmNvisualPolicy, XmCONSTANT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNshadowThickness, 1,
			XmNx, 17,
			XmNy, 39,
			XmNwidth, 422,
			XmNheight, 416,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assign_usersSW, (char *) UxAssign_usersContext );


	/* Creation of assign_usersSL */
	assign_usersSL = XtVaCreateManagedWidget( "assign_usersSL",
			xmListWidgetClass,
			assign_usersSW,
			XmNwidth, 422,
			XmNheight, 386,
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			XmNscrollBarDisplayPolicy, XmAS_NEEDED,
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1" ),
			NULL );
	XtAddCallback( assign_usersSL, XmNdefaultActionCallback,
		(XtCallbackProc) assign_users_users_listCb,
		(XtPointer) UxAssign_usersContext );
	XtAddCallback( assign_usersSL, XmNextendedSelectionCallback,
		(XtCallbackProc) assign_users_users_listCb,
		(XtPointer) UxAssign_usersContext );

	UxPutContext( assign_usersSL, (char *) UxAssign_usersContext );


	/* Creation of addPB */
	addPB = XtVaCreateManagedWidget( "addPB",
			xmPushButtonWidgetClass,
			form4,
			XmNx, 168,
			XmNy, 464,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Add" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			NULL );
	XtAddCallback( addPB, XmNactivateCallback,
		(XtCallbackProc) assign_users_add_usersCb,
		(XtPointer) UxAssign_usersContext );

	UxPutContext( addPB, (char *) UxAssign_usersContext );


	/* Creation of assign_users_sbSW */
	assign_users_sbSW = XtVaCreateManagedWidget( "assign_users_sbSW",
			xmScrolledWindowWidgetClass,
			form4,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 448,
			XmNy, 40,
			XmNwidth, 15,
			XmNheight, 392,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNborderWidth, 0,
			NULL );
	UxPutContext( assign_users_sbSW, (char *) UxAssign_usersContext );


	/* Creation of assign_users_sbSL */
	assign_users_sbSL = XtVaCreateManagedWidget( "assign_users_sbSL",
			xmListWidgetClass,
			assign_users_sbSW,
			XmNwidth, 2,
			XmNheight, 397,
			XmNfontList, UxConvertFontList("-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1" ),
			RES_CONVERT( XmNbackground, "LightSkyBlue3" ),
			XmNlistSizePolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNselectionPolicy, XmEXTENDED_SELECT,
			NULL );
	UxPutContext( assign_users_sbSL, (char *) UxAssign_usersContext );

	assign_users_users_sbCb( assign_users_sbSL,
			(XtPointer) 1, (XtPointer) NULL );


	/* Creation of label61 */
	label61 = XtVaCreateManagedWidget( "label61",
			xmLabelWidgetClass,
			assign_users,
			XmNx, 232,
			XmNy, 174,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Users" ),
			NULL );
	UxPutContext( label61, (char *) UxAssign_usersContext );


	/* Creation of label62 */
	label62 = XtVaCreateManagedWidget( "label62",
			xmLabelWidgetClass,
			assign_users,
			XmNx, 652,
			XmNy, 174,
			RES_CONVERT( XmNbackground, "#9ac0cd" ),
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Users Assigned to Account" ),
			NULL );
	UxPutContext( label62, (char *) UxAssign_usersContext );


	/* Creation of updateButton */
	updateButton = XtVaCreateManagedWidget( "updateButton",
			xmPushButtonWidgetClass,
			assign_users,
			XmNx, 26,
			XmNy, 728,
			XmNwidth, 138,
			XmNheight, 34,
			XmNfontList, UxConvertFontList("-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1" ),
			RES_CONVERT( XmNlabelString, "Update" ),
			XmNshadowThickness, 4,
			RES_CONVERT( XmNbackground, "CadetBlue" ),
			XmNtopOffset, 728,
			NULL );
	XtAddCallback( updateButton, XmNactivateCallback,
		(XtCallbackProc) assign_users_updateCb,
		(XtPointer) UxAssign_usersContext );

	UxPutContext( updateButton, (char *) UxAssign_usersContext );


	XtAddCallback( assign_users, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAssign_usersContext);


	return ( assign_users );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_assign_users( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCassign_users        *UxContext;
	static int		_Uxinit = 0;

	UxAssign_usersContext = UxContext =
		(_UxCassign_users *) UxNewContext( sizeof(_UxCassign_users), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_assign_users();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/



/*******************************************************************************
       CPquestionBox.c
       (Generated from interface file CPquestionBox.i)
       Associated Header file: CPquestionBox.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/MessageB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static char sccsid_CPquestionBox_i[] = "@(#)CPquestionBox.i	2.7 96/04/10 20:02:32"; 

#ifndef DESIGN_TIME
#include "que_xwp.h"
#else
typedef struct {
  int action;
  char *namePtr;
  Widget buttonWid;
  int pos;
  int rev;
  char platform[30];
} dialogCBdata;
#endif
extern dialogCBdata *doCreateQuestionDlg(Widget wid, dialogCBdata *cbData);


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPquestionBox.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPquestionBox;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPquestionBox()
{
	Widget		_UxParent;


	/* Creation of CPquestionBox */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "CPquestionBox_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 125,
			XmNy, 368,
			XmNheight, 162,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CPquestionBox",
			NULL );

	CPquestionBox = XtVaCreateWidget( "CPquestionBox",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_QUESTION,
			XmNheight, 162,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "Confirmation" ),
			RES_CONVERT( XmNmessageString, "default message string" ),
			XmNdefaultPosition, FALSE,
			NULL );
	UxPutContext( CPquestionBox, (char *) UxCPquestionBoxContext );
	UxPutClassCode( CPquestionBox, _UxIfClassId );


	XtAddCallback( CPquestionBox, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPquestionBoxContext);


	return ( CPquestionBox );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPquestionBox( _UxUxParent, _UxQuestionCBdata )
	swidget	_UxUxParent;
	dialogCBdata	*_UxQuestionCBdata;
{
	Widget                  rtrn;
	_UxCCPquestionBox       *UxContext;
	static int		_Uxinit = 0;

	UxCPquestionBoxContext = UxContext =
		(_UxCCPquestionBox *) UxNewContext( sizeof(_UxCCPquestionBox), False );

	UxParent = _UxUxParent;
	QuestionCBdata = _UxQuestionCBdata;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		dialogCBdata *questionCBdata;
		rtrn = _Uxbuild_CPquestionBox();

#ifndef DESIGN_TIME
		QuestionCBdata = doCreateQuestionDlg(rtrn, QuestionCBdata);
#endif
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


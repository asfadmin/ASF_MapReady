
/*******************************************************************************
	vc_phaseselect.c

       Associated Header file: vc_phaseselect.h
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
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
#include <Xm/Form.h>



static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_phaseselect.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton_QUIT1(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSPhaseSelection   *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSPhaseSelectionContext;
	UxAPSPhaseSelectionContext = UxContext =
			(_UxCAPSPhaseSelection *) UxGetContext( UxWidget );
	{
	extern Widget apsphaseselect_form;
	XtPopdown(XtParent(apsphaseselect_form)) ;
	}
	UxAPSPhaseSelectionContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSPhaseSelection()
{
	Widget		_UxParent;


	/* Creation of APSPhaseSelection */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "APSPhaseSelection_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 300,
			XmNy, 358,
			XmNwidth, 595,
			XmNheight, 250,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSPhaseSelection",
			XmNiconName, "APSPhaseSelection",
			NULL );

	}

	APSPhaseSelection = XtVaCreateManagedWidget( "APSPhaseSelection",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 595,
			XmNheight, 250,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			NULL );
	UxPutContext( APSPhaseSelection, (char *) UxAPSPhaseSelectionContext );
	UxPutClassCode( APSPhaseSelection, _UxIfClassId );


	/* Creation of label72 */
	label72 = XtVaCreateManagedWidget( "label72",
			xmLabelWidgetClass,
			APSPhaseSelection,
			XmNx, 18,
			XmNy, 25,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "PHASE NO.     START TIME         STOP TIME         DUE DATE   " ),
			XmNfontList, UxConvertFontList("-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNwidth, 539,
			NULL );
	UxPutContext( label72, (char *) UxAPSPhaseSelectionContext );


	/* Creation of scrolledWindowList9 */
	scrolledWindowList9 = XtVaCreateManagedWidget( "scrolledWindowList9",
			xmScrolledWindowWidgetClass,
			APSPhaseSelection,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 18,
			XmNy, 57,
			NULL );
	UxPutContext( scrolledWindowList9, (char *) UxAPSPhaseSelectionContext );


	/* Creation of scrolledList_REQQ */
	scrolledList_REQQ = XtVaCreateManagedWidget( "scrolledList_REQQ",
			xmListWidgetClass,
			scrolledWindowList9,
			XmNwidth, 539,
			XmNheight, 132,
			XmNitemCount, 1,
			RES_CONVERT( XmNitems, "    50           1997:000:00:00:00        1997:000:00:00:00      1997:000:00:00:00" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNx, 0,
			NULL );
	UxPutContext( scrolledList_REQQ, (char *) UxAPSPhaseSelectionContext );

	update_reqq_phase_list( scrolledList_REQQ,
			(XtPointer) UxAPSPhaseSelectionContext, (XtPointer) NULL );


	/* Creation of pushButton_QUIT1 */
	pushButton_QUIT1 = XtVaCreateManagedWidget( "pushButton_QUIT1",
			xmPushButtonWidgetClass,
			APSPhaseSelection,
			XmNx, 236,
			XmNy, 200,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			XmNheight, 40,
			XmNwidth, 102,
			NULL );
	XtAddCallback( pushButton_QUIT1, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_QUIT1,
		(XtPointer) UxAPSPhaseSelectionContext );

	UxPutContext( pushButton_QUIT1, (char *) UxAPSPhaseSelectionContext );


	XtAddCallback( APSPhaseSelection, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSPhaseSelectionContext);


	return ( APSPhaseSelection );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSPhaseSelection( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSPhaseSelection   *UxContext;
	static int		_Uxinit = 0;

	UxAPSPhaseSelectionContext = UxContext =
		(_UxCAPSPhaseSelection *) UxNewContext( sizeof(_UxCAPSPhaseSelection), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_APSPhaseSelection();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
	pps_policy.c

       Associated Header file: pps_policy.h
*******************************************************************************/

#include <stdio.h>
#include "UxLib.h"
#include "UxLabelG.h"
#include "UxScList.h"
#include "UxScrW.h"
#include "UxTogB.h"
#include "UxBboard.h"
#include "UxFrame.h"
#include "UxLabel.h"
#include "UxCascB.h"
#include "UxPushB.h"
#include "UxRowCol.h"
#include "UxForm.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "pps_common.h"
#include "pps_util.h"

static char SccsFileId[] = "@(#)pps_policy.c	1.1    11/21/96";

char policy_labelPixmapString[MAXSMALLBUF];
extern char rootPath[];
extern swidget nojoy;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "pps_policy.h"
#undef CONTEXT_MACRO_ACCESS

swidget	pps_policy;
swidget	tb_policy_Urgent_L1;
swidget	tb_policy_Urgent_L1_QLK;
swidget	tb_policy_Urgent_Scan;
swidget	tb_policy_Urgent_Scan_QLK;
swidget	tb_policy_High_L1;
swidget	tb_policy_High_L1_QLK;
swidget	tb_policy_High_Scan;
swidget	tb_policy_High_Scan_QLK;
swidget	tb_policy_Routine_L1;
swidget	tb_policy_Routine_L1_QLK;
swidget	tb_policy_Routine_Scan;
swidget	tb_policy_Routine_Scan_QLK;
swidget	tb_policy_Low_L1;
swidget	tb_policy_Low_L1_QLK;
swidget	tb_policy_Low_Scan;
swidget	tb_policy_Low_Scan_QLK;
swidget	sw_policy_query_results_list;
swidget	pb_policy_select_all;
swidget	pb_policy_deselect_all;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pb_policy_print_results(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	cb_policy_print_results();
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  activateCB_pb_policy_print_screen(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	extern void pps_print_screen(swidget sw);
	
	pps_print_screen(pps_policy);
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  activateCB_pb_policy_exit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	UxPopdownInterface(pps_policy);
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Urgent_L1(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "NO ", "URGENT", "YES");
	else
		cb_policy_change("L1", "NO ", "URGENT", "NO ");
	
	
	
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Urgent_L1_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "YES", "URGENT", "YES");
	else
		cb_policy_change("L1", "YES", "URGENT", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Urgent_Scan(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "NO ", "URGENT", "YES");
	else
		cb_policy_change("SCAN", "NO ", "URGENT", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Urgent_Scan_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "YES", "URGENT", "YES");
	else
		cb_policy_change("SCAN", "YES", "URGENT", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_High_L1(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "NO ", "HIGH", "YES");
	else
		cb_policy_change("L1", "NO ", "HIGH", "NO ");
		
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_High_L1_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "YES", "HIGH", "YES");
	else
		cb_policy_change("L1", "YES", "HIGH", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_High_Scan(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "NO ", "HIGH", "YES");
	else
		cb_policy_change("SCAN", "NO ", "HIGH", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_High_Scan_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "YES", "HIGH", "YES");
	else
		cb_policy_change("SCAN", "YES", "HIGH", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Routine_L1(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "NO ", "ROUTINE", "YES");
	else
		cb_policy_change("L1", "NO ", "ROUTINE", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Routine_L1_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "YES", "ROUTINE", "YES");
	else
		cb_policy_change("L1", "YES", "ROUTINE", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Routine_Scan(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "NO ", "ROUTINE", "YES");
	else
		cb_policy_change("SCAN", "NO ", "ROUTINE", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Routine_Scan_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "YES", "ROUTINE", "YES");
	else
		cb_policy_change("SCAN", "YES", "ROUTINE", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Low_L1(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "NO ", "LOW", "YES");
	else
		cb_policy_change("L1", "NO ", "LOW", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Low_L1_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("L1", "YES", "LOW", "YES");
	else
		cb_policy_change("L1", "YES", "LOW", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Low_Scan(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "NO ", "LOW", "YES");
	else
		cb_policy_change("SCAN", "NO ", "LOW", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  valueChangedCB_tb_policy_Low_Scan_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	if (XmToggleButtonGetState(UxWidget))
		cb_policy_change("SCAN", "YES", "LOW", "YES");
	else
		cb_policy_change("SCAN", "YES", "LOW", "NO ");
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  extendedSelectionCB_sw_policy_query_results_list(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	{
	
	}
	UxPps_policyContext = UxSaveCtx;
}

static void  activateCB_pb_policy_select_all(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	cb_policy_select_all();
	UxPps_policyContext = UxSaveCtx;
}

static void  activateCB_pb_policy_IT_on(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	cb_policy_IT_on();
	UxPps_policyContext = UxSaveCtx;
}

static void  activateCB_pb_policy_deselect_all(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	cb_policy_deselect_all();
	UxPps_policyContext = UxSaveCtx;
}

static void  activateCB_pb_policy_IT_off(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_policy          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_policyContext;
	UxPps_policyContext = UxContext =
			(_UxCpps_policy *) UxGetContext( UxThisWidget );
	cb_policy_IT_off();
	UxPps_policyContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_pps_policy()
{
	/* Create the swidgets */


	/* Creation of pps_policy */
	pps_policy = UxCreateForm( "pps_policy", UxParent );
	UxPutContext( pps_policy, UxPps_policyContext );
	UxPutClassCode( pps_policy, _UxIfClassId );
	UxPutDefaultShell( pps_policy, "transientShell" );

	UxPutWidth( pps_policy, 900 ),
	UxPutHeight( pps_policy, 729 ),
	UxPutResizePolicy( pps_policy, "resize_none" ),
	UxPutX( pps_policy, 45 ),
	UxPutY( pps_policy, 52 ),
	UxPutUnitType( pps_policy, "pixels" ),
	UxCreateWidget( pps_policy );


	/* Creation of mb_policy */
	mb_policy = UxCreateRowColumn( "mb_policy", pps_policy );
	UxPutRowColumnType( mb_policy, "menu_bar" ),
	UxPutX( mb_policy, 0 ),
	UxPutY( mb_policy, 0 ),
	UxPutWidth( mb_policy, 504 ),
	UxPutHeight( mb_policy, 36 ),
	UxPutMenuAccelerator( mb_policy, "<KeyUp>F10" ),
	UxPutRightAttachment( mb_policy, "attach_form" ),
	UxPutLeftAttachment( mb_policy, "attach_form" ),
	UxCreateWidget( mb_policy );


	/* Creation of pb_policy_file */
	pb_policy_file = UxCreateRowColumn( "pb_policy_file", mb_policy );
	UxPutRowColumnType( pb_policy_file, "menu_pulldown" ),
	UxCreateWidget( pb_policy_file );


	/* Creation of pb_policy_print_results */
	pb_policy_print_results = UxCreatePushButton( "pb_policy_print_results", pb_policy_file );
	UxPutLabelString( pb_policy_print_results, "Print Results..." ),
	UxPutMnemonic( pb_policy_print_results, "R" ),
	UxCreateWidget( pb_policy_print_results );

	UxAddCallback( pb_policy_print_results, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_print_results,
		(XtPointer) UxPps_policyContext );


	/* Creation of pb_policy_print_screen */
	pb_policy_print_screen = UxCreatePushButton( "pb_policy_print_screen", pb_policy_file );
	UxPutLabelString( pb_policy_print_screen, "Print Screen" ),
	UxPutMnemonic( pb_policy_print_screen, "P" ),
	UxCreateWidget( pb_policy_print_screen );

	UxAddCallback( pb_policy_print_screen, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_print_screen,
		(XtPointer) UxPps_policyContext );


	/* Creation of pb_policy_exit */
	pb_policy_exit = UxCreatePushButton( "pb_policy_exit", pb_policy_file );
	UxPutLabelString( pb_policy_exit, "Exit" ),
	UxPutMnemonic( pb_policy_exit, "x" ),
	UxCreateWidget( pb_policy_exit );

	UxAddCallback( pb_policy_exit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_exit,
		(XtPointer) UxPps_policyContext );


	/* Creation of cb_policy */
	cb_policy = UxCreateCascadeButton( "cb_policy", mb_policy );
	UxPutLabelString( cb_policy, "File" ),
	UxPutMnemonic( cb_policy, "F" ),
	UxPutSubMenuId( cb_policy, "pb_policy_file" ),
	UxCreateWidget( cb_policy );


	/* Creation of l_policy_matrix */
	l_policy_matrix = UxCreateLabel( "l_policy_matrix", pps_policy );
	UxPutX( l_policy_matrix, 9 ),
	UxPutY( l_policy_matrix, 42 ),
	UxPutWidth( l_policy_matrix, 106 ),
	UxPutHeight( l_policy_matrix, 30 ),
	UxPutLabelString( l_policy_matrix, "Policy Matrix" ),
	UxCreateWidget( l_policy_matrix );


	/* Creation of f_policy_matrix */
	f_policy_matrix = UxCreateFrame( "f_policy_matrix", pps_policy );
	UxPutWidth( f_policy_matrix, 339 ),
	UxPutHeight( f_policy_matrix, 230 ),
	UxPutX( f_policy_matrix, 11 ),
	UxPutY( f_policy_matrix, 77 ),
	UxCreateWidget( f_policy_matrix );


	/* Creation of bb_policy_matrix */
	bb_policy_matrix = UxCreateBulletinBoard( "bb_policy_matrix", f_policy_matrix );
	UxPutResizePolicy( bb_policy_matrix, "resize_none" ),
	UxPutWidth( bb_policy_matrix, 335 ),
	UxPutHeight( bb_policy_matrix, 211 ),
	UxPutX( bb_policy_matrix, 2 ),
	UxPutY( bb_policy_matrix, 2 ),
	UxPutShadowThickness( bb_policy_matrix, 1 ),
	UxCreateWidget( bb_policy_matrix );


	/* Creation of rc_policy_matrix */
	rc_policy_matrix = UxCreateRowColumn( "rc_policy_matrix", bb_policy_matrix );
	UxPutWidth( rc_policy_matrix, 230 ),
	UxPutHeight( rc_policy_matrix, 163 ),
	UxPutX( rc_policy_matrix, 90 ),
	UxPutY( rc_policy_matrix, 40 ),
	UxPutNumColumns( rc_policy_matrix, 4 ),
	UxPutRadioAlwaysOne( rc_policy_matrix, "false" ),
	UxPutOrientation( rc_policy_matrix, "vertical" ),
	UxPutPacking( rc_policy_matrix, "pack_column" ),
	UxPutSpacing( rc_policy_matrix, 16 ),
	UxCreateWidget( rc_policy_matrix );


	/* Creation of tb_policy_Urgent_L1 */
	tb_policy_Urgent_L1 = UxCreateToggleButton( "tb_policy_Urgent_L1", rc_policy_matrix );
	UxPutX( tb_policy_Urgent_L1, 3 ),
	UxPutY( tb_policy_Urgent_L1, 3 ),
	UxPutWidth( tb_policy_Urgent_L1, 20 ),
	UxPutHeight( tb_policy_Urgent_L1, 137 ),
	UxPutLabelString( tb_policy_Urgent_L1, "" ),
	UxPutIndicatorSize( tb_policy_Urgent_L1, 16 ),
	UxPutMarginWidth( tb_policy_Urgent_L1, 10 ),
	UxCreateWidget( tb_policy_Urgent_L1 );

	UxAddCallback( tb_policy_Urgent_L1, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Urgent_L1,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Urgent_L1_QLK */
	tb_policy_Urgent_L1_QLK = UxCreateToggleButton( "tb_policy_Urgent_L1_QLK", rc_policy_matrix );
	UxPutX( tb_policy_Urgent_L1_QLK, 3 ),
	UxPutY( tb_policy_Urgent_L1_QLK, 3 ),
	UxPutWidth( tb_policy_Urgent_L1_QLK, 20 ),
	UxPutHeight( tb_policy_Urgent_L1_QLK, 137 ),
	UxPutLabelString( tb_policy_Urgent_L1_QLK, "" ),
	UxPutIndicatorSize( tb_policy_Urgent_L1_QLK, 16 ),
	UxPutMarginWidth( tb_policy_Urgent_L1_QLK, 10 ),
	UxCreateWidget( tb_policy_Urgent_L1_QLK );

	UxAddCallback( tb_policy_Urgent_L1_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Urgent_L1_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Urgent_Scan */
	tb_policy_Urgent_Scan = UxCreateToggleButton( "tb_policy_Urgent_Scan", rc_policy_matrix );
	UxPutX( tb_policy_Urgent_Scan, 3 ),
	UxPutY( tb_policy_Urgent_Scan, 3 ),
	UxPutWidth( tb_policy_Urgent_Scan, 20 ),
	UxPutHeight( tb_policy_Urgent_Scan, 137 ),
	UxPutLabelString( tb_policy_Urgent_Scan, "" ),
	UxPutIndicatorSize( tb_policy_Urgent_Scan, 16 ),
	UxPutMarginWidth( tb_policy_Urgent_Scan, 10 ),
	UxCreateWidget( tb_policy_Urgent_Scan );

	UxAddCallback( tb_policy_Urgent_Scan, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Urgent_Scan,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Urgent_Scan_QLK */
	tb_policy_Urgent_Scan_QLK = UxCreateToggleButton( "tb_policy_Urgent_Scan_QLK", rc_policy_matrix );
	UxPutX( tb_policy_Urgent_Scan_QLK, 3 ),
	UxPutY( tb_policy_Urgent_Scan_QLK, 3 ),
	UxPutWidth( tb_policy_Urgent_Scan_QLK, 20 ),
	UxPutHeight( tb_policy_Urgent_Scan_QLK, 137 ),
	UxPutLabelString( tb_policy_Urgent_Scan_QLK, "" ),
	UxPutIndicatorSize( tb_policy_Urgent_Scan_QLK, 16 ),
	UxPutMarginWidth( tb_policy_Urgent_Scan_QLK, 10 ),
	UxCreateWidget( tb_policy_Urgent_Scan_QLK );

	UxAddCallback( tb_policy_Urgent_Scan_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Urgent_Scan_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_High_L1 */
	tb_policy_High_L1 = UxCreateToggleButton( "tb_policy_High_L1", rc_policy_matrix );
	UxPutX( tb_policy_High_L1, 3 ),
	UxPutY( tb_policy_High_L1, 3 ),
	UxPutWidth( tb_policy_High_L1, 20 ),
	UxPutHeight( tb_policy_High_L1, 137 ),
	UxPutLabelString( tb_policy_High_L1, "" ),
	UxPutIndicatorSize( tb_policy_High_L1, 16 ),
	UxPutMarginWidth( tb_policy_High_L1, 10 ),
	UxCreateWidget( tb_policy_High_L1 );

	UxAddCallback( tb_policy_High_L1, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_High_L1,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_High_L1_QLK */
	tb_policy_High_L1_QLK = UxCreateToggleButton( "tb_policy_High_L1_QLK", rc_policy_matrix );
	UxPutX( tb_policy_High_L1_QLK, 3 ),
	UxPutY( tb_policy_High_L1_QLK, 3 ),
	UxPutWidth( tb_policy_High_L1_QLK, 20 ),
	UxPutHeight( tb_policy_High_L1_QLK, 137 ),
	UxPutLabelString( tb_policy_High_L1_QLK, "" ),
	UxPutIndicatorSize( tb_policy_High_L1_QLK, 16 ),
	UxPutMarginWidth( tb_policy_High_L1_QLK, 10 ),
	UxCreateWidget( tb_policy_High_L1_QLK );

	UxAddCallback( tb_policy_High_L1_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_High_L1_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_High_Scan */
	tb_policy_High_Scan = UxCreateToggleButton( "tb_policy_High_Scan", rc_policy_matrix );
	UxPutX( tb_policy_High_Scan, 3 ),
	UxPutY( tb_policy_High_Scan, 3 ),
	UxPutWidth( tb_policy_High_Scan, 20 ),
	UxPutHeight( tb_policy_High_Scan, 137 ),
	UxPutLabelString( tb_policy_High_Scan, "" ),
	UxPutIndicatorSize( tb_policy_High_Scan, 16 ),
	UxPutMarginWidth( tb_policy_High_Scan, 10 ),
	UxCreateWidget( tb_policy_High_Scan );

	UxAddCallback( tb_policy_High_Scan, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_High_Scan,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_High_Scan_QLK */
	tb_policy_High_Scan_QLK = UxCreateToggleButton( "tb_policy_High_Scan_QLK", rc_policy_matrix );
	UxPutX( tb_policy_High_Scan_QLK, 3 ),
	UxPutY( tb_policy_High_Scan_QLK, 3 ),
	UxPutWidth( tb_policy_High_Scan_QLK, 20 ),
	UxPutHeight( tb_policy_High_Scan_QLK, 137 ),
	UxPutLabelString( tb_policy_High_Scan_QLK, "" ),
	UxPutIndicatorSize( tb_policy_High_Scan_QLK, 16 ),
	UxPutMarginWidth( tb_policy_High_Scan_QLK, 10 ),
	UxCreateWidget( tb_policy_High_Scan_QLK );

	UxAddCallback( tb_policy_High_Scan_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_High_Scan_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Routine_L1 */
	tb_policy_Routine_L1 = UxCreateToggleButton( "tb_policy_Routine_L1", rc_policy_matrix );
	UxPutX( tb_policy_Routine_L1, 3 ),
	UxPutY( tb_policy_Routine_L1, 3 ),
	UxPutWidth( tb_policy_Routine_L1, 20 ),
	UxPutHeight( tb_policy_Routine_L1, 137 ),
	UxPutLabelString( tb_policy_Routine_L1, "" ),
	UxPutIndicatorSize( tb_policy_Routine_L1, 16 ),
	UxPutMarginWidth( tb_policy_Routine_L1, 10 ),
	UxCreateWidget( tb_policy_Routine_L1 );

	UxAddCallback( tb_policy_Routine_L1, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Routine_L1,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Routine_L1_QLK */
	tb_policy_Routine_L1_QLK = UxCreateToggleButton( "tb_policy_Routine_L1_QLK", rc_policy_matrix );
	UxPutX( tb_policy_Routine_L1_QLK, 3 ),
	UxPutY( tb_policy_Routine_L1_QLK, 3 ),
	UxPutWidth( tb_policy_Routine_L1_QLK, 20 ),
	UxPutHeight( tb_policy_Routine_L1_QLK, 137 ),
	UxPutLabelString( tb_policy_Routine_L1_QLK, "" ),
	UxPutIndicatorSize( tb_policy_Routine_L1_QLK, 16 ),
	UxPutMarginWidth( tb_policy_Routine_L1_QLK, 10 ),
	UxCreateWidget( tb_policy_Routine_L1_QLK );

	UxAddCallback( tb_policy_Routine_L1_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Routine_L1_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Routine_Scan */
	tb_policy_Routine_Scan = UxCreateToggleButton( "tb_policy_Routine_Scan", rc_policy_matrix );
	UxPutX( tb_policy_Routine_Scan, 3 ),
	UxPutY( tb_policy_Routine_Scan, 3 ),
	UxPutWidth( tb_policy_Routine_Scan, 20 ),
	UxPutHeight( tb_policy_Routine_Scan, 137 ),
	UxPutLabelString( tb_policy_Routine_Scan, "" ),
	UxPutIndicatorSize( tb_policy_Routine_Scan, 16 ),
	UxPutMarginWidth( tb_policy_Routine_Scan, 10 ),
	UxCreateWidget( tb_policy_Routine_Scan );

	UxAddCallback( tb_policy_Routine_Scan, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Routine_Scan,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Routine_Scan_QLK */
	tb_policy_Routine_Scan_QLK = UxCreateToggleButton( "tb_policy_Routine_Scan_QLK", rc_policy_matrix );
	UxPutX( tb_policy_Routine_Scan_QLK, 3 ),
	UxPutY( tb_policy_Routine_Scan_QLK, 3 ),
	UxPutWidth( tb_policy_Routine_Scan_QLK, 20 ),
	UxPutHeight( tb_policy_Routine_Scan_QLK, 137 ),
	UxPutLabelString( tb_policy_Routine_Scan_QLK, "" ),
	UxPutIndicatorSize( tb_policy_Routine_Scan_QLK, 16 ),
	UxPutMarginWidth( tb_policy_Routine_Scan_QLK, 10 ),
	UxCreateWidget( tb_policy_Routine_Scan_QLK );

	UxAddCallback( tb_policy_Routine_Scan_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Routine_Scan_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Low_L1 */
	tb_policy_Low_L1 = UxCreateToggleButton( "tb_policy_Low_L1", rc_policy_matrix );
	UxPutX( tb_policy_Low_L1, 3 ),
	UxPutY( tb_policy_Low_L1, 3 ),
	UxPutWidth( tb_policy_Low_L1, 20 ),
	UxPutHeight( tb_policy_Low_L1, 137 ),
	UxPutLabelString( tb_policy_Low_L1, "" ),
	UxPutIndicatorSize( tb_policy_Low_L1, 16 ),
	UxPutMarginWidth( tb_policy_Low_L1, 10 ),
	UxCreateWidget( tb_policy_Low_L1 );

	UxAddCallback( tb_policy_Low_L1, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Low_L1,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Low_L1_QLK */
	tb_policy_Low_L1_QLK = UxCreateToggleButton( "tb_policy_Low_L1_QLK", rc_policy_matrix );
	UxPutX( tb_policy_Low_L1_QLK, 3 ),
	UxPutY( tb_policy_Low_L1_QLK, 3 ),
	UxPutWidth( tb_policy_Low_L1_QLK, 20 ),
	UxPutHeight( tb_policy_Low_L1_QLK, 137 ),
	UxPutLabelString( tb_policy_Low_L1_QLK, "" ),
	UxPutIndicatorSize( tb_policy_Low_L1_QLK, 16 ),
	UxPutMarginWidth( tb_policy_Low_L1_QLK, 10 ),
	UxCreateWidget( tb_policy_Low_L1_QLK );

	UxAddCallback( tb_policy_Low_L1_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Low_L1_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Low_Scan */
	tb_policy_Low_Scan = UxCreateToggleButton( "tb_policy_Low_Scan", rc_policy_matrix );
	UxPutX( tb_policy_Low_Scan, 3 ),
	UxPutY( tb_policy_Low_Scan, 3 ),
	UxPutWidth( tb_policy_Low_Scan, 20 ),
	UxPutHeight( tb_policy_Low_Scan, 137 ),
	UxPutLabelString( tb_policy_Low_Scan, "" ),
	UxPutIndicatorSize( tb_policy_Low_Scan, 16 ),
	UxPutMarginWidth( tb_policy_Low_Scan, 10 ),
	UxCreateWidget( tb_policy_Low_Scan );

	UxAddCallback( tb_policy_Low_Scan, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Low_Scan,
		(XtPointer) UxPps_policyContext );


	/* Creation of tb_policy_Low_Scan_QLK */
	tb_policy_Low_Scan_QLK = UxCreateToggleButton( "tb_policy_Low_Scan_QLK", rc_policy_matrix );
	UxPutX( tb_policy_Low_Scan_QLK, 80 ),
	UxPutY( tb_policy_Low_Scan_QLK, 24 ),
	UxPutWidth( tb_policy_Low_Scan_QLK, 92 ),
	UxPutHeight( tb_policy_Low_Scan_QLK, 76 ),
	UxPutLabelString( tb_policy_Low_Scan_QLK, "" ),
	UxPutIndicatorSize( tb_policy_Low_Scan_QLK, 16 ),
	UxPutMarginWidth( tb_policy_Low_Scan_QLK, 10 ),
	UxCreateWidget( tb_policy_Low_Scan_QLK );

	UxAddCallback( tb_policy_Low_Scan_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_policy_Low_Scan_QLK,
		(XtPointer) UxPps_policyContext );


	/* Creation of label9 */
	label9 = UxCreateLabel( "label9", bb_policy_matrix );
	UxPutX( label9, 84 ),
	UxPutY( label9, 12 ),
	UxPutWidth( label9, 56 ),
	UxPutHeight( label9, 24 ),
	UxPutLabelString( label9, "Urgent" ),
	UxCreateWidget( label9 );


	/* Creation of label10 */
	label10 = UxCreateLabel( "label10", bb_policy_matrix );
	UxPutX( label10, 148 ),
	UxPutY( label10, 12 ),
	UxPutWidth( label10, 56 ),
	UxPutHeight( label10, 24 ),
	UxPutLabelString( label10, "High" ),
	UxCreateWidget( label10 );


	/* Creation of label11 */
	label11 = UxCreateLabel( "label11", bb_policy_matrix );
	UxPutX( label11, 208 ),
	UxPutY( label11, 12 ),
	UxPutWidth( label11, 56 ),
	UxPutHeight( label11, 24 ),
	UxPutLabelString( label11, "Routine" ),
	UxCreateWidget( label11 );


	/* Creation of label12 */
	label12 = UxCreateLabel( "label12", bb_policy_matrix );
	UxPutX( label12, 268 ),
	UxPutY( label12, 12 ),
	UxPutWidth( label12, 56 ),
	UxPutHeight( label12, 24 ),
	UxPutLabelString( label12, "Low" ),
	UxCreateWidget( label12 );


	/* Creation of label13 */
	label13 = UxCreateLabel( "label13", bb_policy_matrix );
	UxPutX( label13, -24 ),
	UxPutY( label13, 48 ),
	UxPutWidth( label13, 44 ),
	UxPutHeight( label13, 24 ),
	UxPutLabelString( label13, "L1" ),
	UxCreateWidget( label13 );


	/* Creation of label14 */
	label14 = UxCreateLabel( "label14", bb_policy_matrix );
	UxPutX( label14, -28 ),
	UxPutY( label14, 88 ),
	UxPutWidth( label14, 56 ),
	UxPutHeight( label14, 24 ),
	UxPutLabelString( label14, "L1 QLK" ),
	UxCreateWidget( label14 );


	/* Creation of label15 */
	label15 = UxCreateLabel( "label15", bb_policy_matrix );
	UxPutX( label15, -28 ),
	UxPutY( label15, 132 ),
	UxPutWidth( label15, 56 ),
	UxPutHeight( label15, 24 ),
	UxPutLabelString( label15, "Scan" ),
	UxCreateWidget( label15 );


	/* Creation of label16 */
	label16 = UxCreateLabel( "label16", bb_policy_matrix );
	UxPutX( label16, -36 ),
	UxPutY( label16, 176 ),
	UxPutWidth( label16, 72 ),
	UxPutHeight( label16, 24 ),
	UxPutLabelString( label16, "Scan QLK" ),
	UxCreateWidget( label16 );


	/* Creation of f_policy_query_results */
	f_policy_query_results = UxCreateFrame( "f_policy_query_results", pps_policy );
	UxPutWidth( f_policy_query_results, 886 ),
	UxPutHeight( f_policy_query_results, 356 ),
	UxPutX( f_policy_query_results, 9 ),
	UxPutY( f_policy_query_results, 366 ),
	UxCreateWidget( f_policy_query_results );


	/* Creation of bb_policy_query_results */
	bb_policy_query_results = UxCreateBulletinBoard( "bb_policy_query_results", f_policy_query_results );
	UxPutResizePolicy( bb_policy_query_results, "resize_none" ),
	UxPutWidth( bb_policy_query_results, 877 ),
	UxPutHeight( bb_policy_query_results, 352 ),
	UxPutX( bb_policy_query_results, 2 ),
	UxPutY( bb_policy_query_results, 2 ),
	UxPutMarginHeight( bb_policy_query_results, 0 ),
	UxPutMarginWidth( bb_policy_query_results, 0 ),
	UxCreateWidget( bb_policy_query_results );


	/* Creation of l_policy_priority */
	l_policy_priority = UxCreateLabel( "l_policy_priority", bb_policy_query_results );
	UxPutX( l_policy_priority, 76 ),
	UxPutY( l_policy_priority, 8 ),
	UxPutWidth( l_policy_priority, 68 ),
	UxPutHeight( l_policy_priority, 32 ),
	UxPutLabelString( l_policy_priority, "Priority" ),
	UxCreateWidget( l_policy_priority );


	/* Creation of l_policy_media_id */
	l_policy_media_id = UxCreateLabel( "l_policy_media_id", bb_policy_query_results );
	UxPutX( l_policy_media_id, 156 ),
	UxPutY( l_policy_media_id, 8 ),
	UxPutWidth( l_policy_media_id, 68 ),
	UxPutHeight( l_policy_media_id, 32 ),
	UxPutLabelString( l_policy_media_id, "Media ID" ),
	UxCreateWidget( l_policy_media_id );


	/* Creation of l_policy_mode */
	l_policy_mode = UxCreateLabel( "l_policy_mode", bb_policy_query_results );
	UxPutX( l_policy_mode, 238 ),
	UxPutY( l_policy_mode, 8 ),
	UxPutWidth( l_policy_mode, 56 ),
	UxPutHeight( l_policy_mode, 32 ),
	UxPutLabelString( l_policy_mode, "Mode" ),
	UxCreateWidget( l_policy_mode );


	/* Creation of l_policy_sat_sens_rev */
	l_policy_sat_sens_rev = UxCreateLabel( "l_policy_sat_sens_rev", bb_policy_query_results );
	UxPutX( l_policy_sat_sens_rev, 304 ),
	UxPutY( l_policy_sat_sens_rev, 8 ),
	UxPutWidth( l_policy_sat_sens_rev, 88 ),
	UxPutHeight( l_policy_sat_sens_rev, 32 ),
	UxPutLabelString( l_policy_sat_sens_rev, "Sat Sens Rev" ),
	UxCreateWidget( l_policy_sat_sens_rev );


	/* Creation of l_policy_frame_id */
	l_policy_frame_id = UxCreateLabel( "l_policy_frame_id", bb_policy_query_results );
	UxPutX( l_policy_frame_id, 400 ),
	UxPutY( l_policy_frame_id, 8 ),
	UxPutWidth( l_policy_frame_id, 48 ),
	UxPutHeight( l_policy_frame_id, 32 ),
	UxPutLabelString( l_policy_frame_id, "Frame\nID" ),
	UxCreateWidget( l_policy_frame_id );


	/* Creation of l_policy_job_id */
	l_policy_job_id = UxCreateLabel( "l_policy_job_id", bb_policy_query_results );
	UxPutX( l_policy_job_id, 458 ),
	UxPutY( l_policy_job_id, 8 ),
	UxPutWidth( l_policy_job_id, 68 ),
	UxPutHeight( l_policy_job_id, 32 ),
	UxPutLabelString( l_policy_job_id, "Job ID" ),
	UxCreateWidget( l_policy_job_id );


	/* Creation of l_policy_order_item */
	l_policy_order_item = UxCreateLabel( "l_policy_order_item", bb_policy_query_results );
	UxPutX( l_policy_order_item, 526 ),
	UxPutY( l_policy_order_item, 8 ),
	UxPutWidth( l_policy_order_item, 100 ),
	UxPutHeight( l_policy_order_item, 32 ),
	UxPutLabelString( l_policy_order_item, "Order & Item ID" ),
	UxCreateWidget( l_policy_order_item );


	/* Creation of l_policy_state */
	l_policy_state = UxCreateLabel( "l_policy_state", bb_policy_query_results );
	UxPutX( l_policy_state, 638 ),
	UxPutY( l_policy_state, 8 ),
	UxPutWidth( l_policy_state, 64 ),
	UxPutHeight( l_policy_state, 32 ),
	UxPutLabelString( l_policy_state, "State" ),
	UxCreateWidget( l_policy_state );


	/* Creation of l_policy_age */
	l_policy_age = UxCreateLabel( "l_policy_age", bb_policy_query_results );
	UxPutX( l_policy_age, 706 ),
	UxPutY( l_policy_age, 8 ),
	UxPutWidth( l_policy_age, 48 ),
	UxPutHeight( l_policy_age, 32 ),
	UxPutLabelString( l_policy_age, "Age\n(Days)" ),
	UxCreateWidget( l_policy_age );


	/* Creation of l_policy_prod_time */
	l_policy_prod_time = UxCreateLabel( "l_policy_prod_time", bb_policy_query_results );
	UxPutX( l_policy_prod_time, 764 ),
	UxPutY( l_policy_prod_time, 8 ),
	UxPutWidth( l_policy_prod_time, 40 ),
	UxPutHeight( l_policy_prod_time, 32 ),
	UxPutLabelString( l_policy_prod_time, "Prod\nTime" ),
	UxCreateWidget( l_policy_prod_time );


	/* Creation of l_policy_order_type */
	l_policy_order_type = UxCreateLabel( "l_policy_order_type", bb_policy_query_results );
	UxPutX( l_policy_order_type, 4 ),
	UxPutY( l_policy_order_type, 8 ),
	UxPutWidth( l_policy_order_type, 68 ),
	UxPutHeight( l_policy_order_type, 32 ),
	UxPutLabelString( l_policy_order_type, "Order\nType" ),
	UxCreateWidget( l_policy_order_type );


	/* Creation of sw_policy_query_results */
	sw_policy_query_results = UxCreateScrolledWindow( "sw_policy_query_results", bb_policy_query_results );
	UxPutScrollingPolicy( sw_policy_query_results, "application_defined" ),
	UxPutVisualPolicy( sw_policy_query_results, "variable" ),
	UxPutScrollBarDisplayPolicy( sw_policy_query_results, "static" ),
	UxPutShadowThickness( sw_policy_query_results, 0 ),
	UxPutX( sw_policy_query_results, 2 ),
	UxPutY( sw_policy_query_results, 46 ),
	UxCreateWidget( sw_policy_query_results );


	/* Creation of sw_policy_query_results_list */
	sw_policy_query_results_list = UxCreateScrolledList( "sw_policy_query_results_list", sw_policy_query_results );
	UxPutWidth( sw_policy_query_results_list, 850 ),
	UxPutHeight( sw_policy_query_results_list, 303 ),
	UxPutScrollBarDisplayPolicy( sw_policy_query_results_list, "static" ),
	UxPutListSizePolicy( sw_policy_query_results_list, "constant" ),
	UxPutFontList( sw_policy_query_results_list, "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1" ),
	UxPutSelectionPolicy( sw_policy_query_results_list, "extended_select" ),
	UxPutAutomaticSelection( sw_policy_query_results_list, "false" ),
	UxCreateWidget( sw_policy_query_results_list );

	UxAddCallback( sw_policy_query_results_list, XmNextendedSelectionCallback,
		(XtCallbackProc) extendedSelectionCB_sw_policy_query_results_list,
		(XtPointer) UxPps_policyContext );


	/* Creation of labelGadget1 */
	labelGadget1 = UxCreateLabelGadget( "labelGadget1", bb_policy_query_results );
	UxPutX( labelGadget1, 800 ),
	UxPutY( labelGadget1, 7 ),
	UxPutWidth( labelGadget1, 54 ),
	UxPutHeight( labelGadget1, 35 ),
	UxPutLabelString( labelGadget1, "Insert\n Top" ),
	UxCreateWidget( labelGadget1 );


	/* Creation of pb_policy_select_all */
	pb_policy_select_all = UxCreatePushButton( "pb_policy_select_all", pps_policy );
	UxPutX( pb_policy_select_all, 362 ),
	UxPutY( pb_policy_select_all, 331 ),
	UxPutWidth( pb_policy_select_all, 93 ),
	UxPutHeight( pb_policy_select_all, 27 ),
	UxPutLabelString( pb_policy_select_all, "Select All" ),
	UxPutSensitive( pb_policy_select_all, "true" ),
	UxCreateWidget( pb_policy_select_all );

	UxAddCallback( pb_policy_select_all, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_select_all,
		(XtPointer) UxPps_policyContext );


	/* Creation of pb_policy_IT_on */
	pb_policy_IT_on = UxCreatePushButton( "pb_policy_IT_on", pps_policy );
	UxPutX( pb_policy_IT_on, 603 ),
	UxPutY( pb_policy_IT_on, 317 ),
	UxPutWidth( pb_policy_IT_on, 127 ),
	UxPutHeight( pb_policy_IT_on, 40 ),
	UxPutLabelString( pb_policy_IT_on, "Insert Top On" ),
	UxPutSensitive( pb_policy_IT_on, "true" ),
	UxCreateWidget( pb_policy_IT_on );

	UxAddCallback( pb_policy_IT_on, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_IT_on,
		(XtPointer) UxPps_policyContext );


	/* Creation of l_policy_pending_jobs */
	l_policy_pending_jobs = UxCreateLabel( "l_policy_pending_jobs", pps_policy );
	UxPutX( l_policy_pending_jobs, 11 ),
	UxPutY( l_policy_pending_jobs, 332 ),
	UxPutWidth( l_policy_pending_jobs, 280 ),
	UxPutHeight( l_policy_pending_jobs, 30 ),
	UxPutLabelString( l_policy_pending_jobs, "Pending Jobs which match the Policy Matrix" ),
	UxCreateWidget( l_policy_pending_jobs );


	/* Creation of pb_policy_deselect_all */
	pb_policy_deselect_all = UxCreatePushButton( "pb_policy_deselect_all", pps_policy );
	UxPutX( pb_policy_deselect_all, 466 ),
	UxPutY( pb_policy_deselect_all, 331 ),
	UxPutWidth( pb_policy_deselect_all, 93 ),
	UxPutHeight( pb_policy_deselect_all, 27 ),
	UxPutLabelString( pb_policy_deselect_all, "Deselect All" ),
	UxPutSensitive( pb_policy_deselect_all, "true" ),
	UxCreateWidget( pb_policy_deselect_all );

	UxAddCallback( pb_policy_deselect_all, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_deselect_all,
		(XtPointer) UxPps_policyContext );


	/* Creation of pb_policy_IT_off */
	pb_policy_IT_off = UxCreatePushButton( "pb_policy_IT_off", pps_policy );
	UxPutX( pb_policy_IT_off, 747 ),
	UxPutY( pb_policy_IT_off, 318 ),
	UxPutWidth( pb_policy_IT_off, 127 ),
	UxPutHeight( pb_policy_IT_off, 40 ),
	UxPutLabelString( pb_policy_IT_off, "Insert Top Off" ),
	UxPutSensitive( pb_policy_IT_off, "true" ),
	UxCreateWidget( pb_policy_IT_off );

	UxAddCallback( pb_policy_IT_off, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_policy_IT_off,
		(XtPointer) UxPps_policyContext );


	/* Creation of f_policy_bear */
	f_policy_bear = UxCreateFrame( "f_policy_bear", pps_policy );
	UxPutWidth( f_policy_bear, 455 ),
	UxPutHeight( f_policy_bear, 254 ),
	UxPutX( f_policy_bear, 375 ),
	UxPutY( f_policy_bear, 54 ),
	UxCreateWidget( f_policy_bear );


	/* Creation of l_policy_bear */
	l_policy_bear = UxCreateLabel( "l_policy_bear", f_policy_bear );
	UxPutX( l_policy_bear, 0 ),
	UxPutY( l_policy_bear, 0 ),
	UxPutWidth( l_policy_bear, 453 ),
	UxPutHeight( l_policy_bear, 252 ),
	UxPutLabelType( l_policy_bear, "pixmap" ),
	UxCreateWidget( l_policy_bear );

	UxDelayUpdate( l_policy_bear );
	UxPutLabelPixmap( l_policy_bear, policy_labelPixmapString ),
	UxUpdate( l_policy_bear );
	UxAddCallback( pps_policy, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPps_policyContext);



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( pps_policy );

	return ( pps_policy );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_pps_policy( swidget _UxUxParent )
{
	swidget                 rtrn;
	_UxCpps_policy          *UxContext;
	static int		_Uxinit = 0;

	UxPps_policyContext = UxContext =
		(_UxCpps_policy *) UxNewContext( sizeof(_UxCpps_policy), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		(void)sprintf(policy_labelPixmapString, "%s/%s/happybr0.xpm",
		rootPath, PPS_PIXMAP_SUBPATH);
		rtrn = _Uxbuild_pps_policy();

		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


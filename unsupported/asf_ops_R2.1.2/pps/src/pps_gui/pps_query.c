
/*******************************************************************************
	pps_query.c

       Associated Header file: pps_query.h
*******************************************************************************/

#include <stdio.h>
#include "UxLib.h"
#include "UxScList.h"
#include "UxScrW.h"
#include "UxTextF.h"
#include "UxLabel.h"
#include "UxTogB.h"
#include "UxBboard.h"
#include "UxFrame.h"
#include "UxCascB.h"
#include "UxPushB.h"
#include "UxRowCol.h"
#include "UxForm.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "pps_common.h"
#include "pps_util.h"

static char SccsFileId[] = "@(#)pps_query.c	1.8  10/31/97";

char query_labelPixmapString[MAXSMALLBUF];
extern char rootPath[];
extern swidget nojoy;

extern char IsAuthorizedUser;

extern void cb_resubmit_to_top(Widget, XtPointer, XtPointer);
extern void cb_resubmit_to_bottom(Widget, XtPointer, XtPointer);


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "pps_query.h"
#undef CONTEXT_MACRO_ACCESS

swidget	pps_query;
swidget	tb_query_L1_Orders;
swidget	tb_query_L1_QLK;
swidget	tb_query_Scan_Orders;
swidget	tb_query_Scan_QLK;
swidget	rc_query_sat;
swidget	pb_query_sat_any;
swidget	pb_query_sat_e1;
swidget	pb_query_sat_e2;
swidget	pb_query_sat_j1;
swidget	pb_query_sat_r1;
swidget	om_query_sat;
swidget	rc_query_sens;
swidget	pb_query_sens_any;
swidget	pb_query_sens_s;
swidget	om_query_sens;
swidget	rc_query_activity;
swidget	pb_query_activity_any;
swidget	pb_query_activity_rlt;
swidget	pb_query_activity_dmp;
swidget	om_query_activity;
swidget	rc_query_station;
swidget	pb_query_station_any;
swidget	pb_query_station_fa;
swidget	pb_query_station_mc;
swidget	om_query_station;
swidget	rc_query_priority;
swidget	pb_query_priority_any;
swidget	pb_query_priority_low;
swidget	pb_query_priority_routine;
swidget	pb_query_priority_high;
swidget	pb_query_priority_urgent;
swidget	om_query_priority;
swidget	l_query_rev;
swidget	tf_query_rev;
swidget	l_query_seq;
swidget	tf_query_seq;
swidget	l_query_job_id;
swidget	tf_query_job_id;
swidget	l_query_order_id;
swidget	tf_query_order_id;
swidget	l_query_item_id;
swidget	tf_query_item_id;
swidget	l_query_frame_id;
swidget	tf_query_frame_id;
swidget	l_query_subframe_id;
swidget	tf_query_subframe_id;
swidget	rc_query_product_type;
swidget	pb_query_product_type_any;
swidget	pb_query_product_type_standard;
swidget	pb_query_product_type_complex;
swidget	pb_query_product_type_ccsd;
swidget	om_query_product_type;
swidget	l_query_pixel_spacing;
swidget	tf_query_pixel_spacing;
swidget	rc_query_projection;
swidget	pb_query_projection_any;
swidget	pb_query_projection_ground_range;
swidget	pb_query_projection_slant_range;
swidget	pb_query_projection_lambert;
swidget	pb_query_projection_ps;
swidget	pb_query_projection_utm;
swidget	om_query_projection;
swidget	l_query_proc_gain;
swidget	tf_query_proc_gain;
swidget	rc_query_state;
swidget	pb_query_state_any;
swidget	pb_query_state_pending;
swidget	pb_query_state_ready;
swidget	pb_query_state_available;
swidget	pb_query_state_submitted;
swidget	om_query_state;
swidget	rc_query_media_type;
swidget	pb_query_media_type_any;
swidget	pb_query_media_type_disk;
swidget	pb_query_media_type_dcrsi;
swidget	om_query_media_type;
swidget	l_query_media_id;
swidget	tf_query_media_id;
swidget	rc_query_processor_mode;
swidget	pb_query_processor_mode_any;
swidget	pb_query_processor_mode_continuous;
swidget	pb_query_processor_mode_scansar;
swidget	om_query_processor_mode;
swidget	rc_query_data_direction;
swidget	pb_query_data_direction_any;
swidget	pb_query_data_direction_forward;
swidget	pb_query_data_direction_reverse;
swidget	pb_query_data_direction_unknown;
swidget	om_query_data_direction;
swidget	sw_query_results_list;
swidget	l_query_num_items;
swidget	pb_query_query;
swidget	rc_query_order_first;
swidget	pb_query_order_first_none;
swidget	pb_query_order_first_order_type;
swidget	pb_query_order_first_priority;
swidget	pb_query_order_first_media_id;
swidget	pb_query_order_first_mode;
swidget	pb_query_order_first_sat_sens_rev;
swidget	pb_query_order_first_frame_subframe;
swidget	pb_query_order_first_job_id;
swidget	pb_query_order_first_order_item;
swidget	pb_query_order_first_state;
swidget	om_query_order_first;
swidget	rc_query_order_second;
swidget	pb_query_order_second_none;
swidget	pb_query_order_second_order_type;
swidget	pb_query_order_second_priority;
swidget	pb_query_order_second_media_id;
swidget	pb_query_order_second_mode;
swidget	pb_query_order_second_sat_sens_rev;
swidget	pb_query_order_second_frame_subframe;
swidget	pb_query_order_second_job_id;
swidget	pb_query_order_second_order_item;
swidget	pb_query_order_second_state;
swidget	om_query_order_second;
swidget	rc_query_order_third;
swidget	pb_query_order_third_none;
swidget	pb_query_order_third_order_type;
swidget	pb_query_order_third_priority;
swidget	pb_query_order_third_media_id;
swidget	pb_query_order_third_mode;
swidget	pb_query_order_third_sat_sens_rev;
swidget	pb_query_order_third_frame_subframe;
swidget	pb_query_order_third_job_id;
swidget	pb_query_order_third_order_item;
swidget	pb_query_order_third_state;
swidget	om_query_order_third;
swidget	rc_query_order_fourth;
swidget	pb_query_order_fourth_none;
swidget	pb_query_order_fourth_order_type;
swidget	pb_query_order_fourth_priority;
swidget	pb_query_order_fourth_media_id;
swidget	pb_query_order_fourth_mode;
swidget	pb_query_order_fourth_sat_sens_rev;
swidget	pb_query_order_fourth_frame_subframe;
swidget	pb_query_order_fourth_job_id;
swidget	pb_query_order_fourth_order_item;
swidget	pb_query_order_fourth_state;
swidget	om_query_order_fourth;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pb_query_print_results(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	{
	cb_query_print_results();
	}
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_print_screen(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	{
	extern void pps_print_screen(swidget sw);
	
	pps_print_screen(pps_query);
	}
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_exit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	{
	UxPopdownInterface(pps_query);
	
	}
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_check_params(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	recheck_pending();
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_load_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	cb_query_load_query();
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_save_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	cb_query_save_query();
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_save_query_as(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	cb_query_save_query_as();
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_clear(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	cb_query_clear_search();
	UxPps_queryContext = UxSaveCtx;
}

static void  valueChangedCB_tb_query_L1_Orders(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	do_query_new_order_type();
	UxPps_queryContext = UxSaveCtx;
}

static void  valueChangedCB_tb_query_L1_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	do_query_new_order_type();
	UxPps_queryContext = UxSaveCtx;
}

static void  valueChangedCB_tb_query_Scan_Orders(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	do_query_new_order_type();
	UxPps_queryContext = UxSaveCtx;
}

static void  valueChangedCB_tb_query_Scan_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	do_query_new_order_type();
	UxPps_queryContext = UxSaveCtx;
}

static void  activateCB_pb_query_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_query           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_queryContext;
	UxPps_queryContext = UxContext =
			(_UxCpps_query *) UxGetContext( UxThisWidget );
	{
	do_query_query();
	}
	UxPps_queryContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_pps_query()
{
	/* Create the swidgets */


	/* Creation of pps_query */
	pps_query = UxCreateForm( "pps_query", UxParent );
	UxPutContext( pps_query, UxPps_queryContext );
	UxPutClassCode( pps_query, _UxIfClassId );
	UxPutDefaultShell( pps_query, "transientShell" );

	UxPutWidth( pps_query, 1144 ),
	UxPutHeight( pps_query, 782 ),
	UxPutResizePolicy( pps_query, "resize_none" ),
	UxPutX( pps_query, 0 ),
	UxPutY( pps_query, 0 ),
	UxPutUnitType( pps_query, "pixels" ),
	UxCreateWidget( pps_query );


	/* Creation of mb_query */
	mb_query = UxCreateRowColumn( "mb_query", pps_query );
	UxPutRowColumnType( mb_query, "menu_bar" ),
	UxPutX( mb_query, 0 ),
	UxPutY( mb_query, -1 ),
	UxPutWidth( mb_query, 504 ),
	UxPutHeight( mb_query, 36 ),
	UxPutMenuAccelerator( mb_query, "<KeyUp>F10" ),
	UxPutSpacing( mb_query, 30 ),
	UxPutLeftOffset( mb_query, 0 ),
	UxPutRightAttachment( mb_query, "attach_form" ),
	UxPutTopOffset( mb_query, 0 ),
	UxPutResizable( mb_query, "false" ),
	UxPutLeftAttachment( mb_query, "attach_form" ),
	UxCreateWidget( mb_query );


	/* Creation of pb_query_file */
	pb_query_file = UxCreateRowColumn( "pb_query_file", mb_query );
	UxPutRowColumnType( pb_query_file, "menu_pulldown" ),
	UxCreateWidget( pb_query_file );


	/* Creation of pb_query_print_results */
	pb_query_print_results = UxCreatePushButton( "pb_query_print_results", pb_query_file );
	UxPutLabelString( pb_query_print_results, "Print Results..." ),
	UxPutMnemonic( pb_query_print_results, "R" ),
	UxPutSensitive( pb_query_print_results, "true" ),
	UxPutAccelerator( pb_query_print_results, "Ctrl<Key>R" ),
	UxPutAcceleratorText( pb_query_print_results, "^R" ),
	UxCreateWidget( pb_query_print_results );

	UxAddCallback( pb_query_print_results, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_print_results,
		(XtPointer) UxPps_queryContext );


	/* Creation of pb_query_print_screen */
	pb_query_print_screen = UxCreatePushButton( "pb_query_print_screen", pb_query_file );
	UxPutLabelString( pb_query_print_screen, "Print Screen" ),
	UxPutMnemonic( pb_query_print_screen, "P" ),
	UxPutAccelerator( pb_query_print_screen, "Ctrl<Key>P" ),
	UxPutAcceleratorText( pb_query_print_screen, "^P" ),
	UxCreateWidget( pb_query_print_screen );

	UxAddCallback( pb_query_print_screen, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_print_screen,
		(XtPointer) UxPps_queryContext );


	/* Creation of pb_query_exit */
	pb_query_exit = UxCreatePushButton( "pb_query_exit", pb_query_file );
	UxPutLabelString( pb_query_exit, "Exit" ),
	UxPutMnemonic( pb_query_exit, "x" ),
	UxPutAccelerator( pb_query_exit, "Ctrl<Key>X" ),
	UxPutAcceleratorText( pb_query_exit, "^X" ),
	UxCreateWidget( pb_query_exit );

	UxAddCallback( pb_query_exit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_exit,
		(XtPointer) UxPps_queryContext );


	/* Creation of mb_query_file */
	mb_query_file = UxCreateCascadeButton( "mb_query_file", mb_query );
	UxPutLabelString( mb_query_file, "File" ),
	UxPutMnemonic( mb_query_file, "F" ),
	UxPutSubMenuId( mb_query_file, "pb_query_file" ),
	UxCreateWidget( mb_query_file );


	/* Creation of mb_query_edit */
	mb_query_edit = UxCreateRowColumn( "mb_query_edit", mb_query );
	UxPutRowColumnType( mb_query_edit, "menu_pulldown" ),
	UxCreateWidget( mb_query_edit );


	/* Creation of pb_query_check_params */
	pb_query_check_params = UxCreatePushButton( "pb_query_check_params", mb_query_edit );
	UxPutLabelString( pb_query_check_params, "Check Params" ),
	UxPutMnemonic( pb_query_check_params, "C" ),
	UxPutAccelerator( pb_query_check_params, "Ctrl<Key>C" ),
	UxPutAcceleratorText( pb_query_check_params, "^C" ),
	UxCreateWidget( pb_query_check_params );

	UxAddCallback( pb_query_check_params, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_check_params,
		(XtPointer) UxPps_queryContext );


	/* Creation of mb_query_resubmit */
	mb_query_resubmit = UxCreateRowColumn( "mb_query_resubmit", mb_query_edit );
	UxPutRowColumnType( mb_query_resubmit, "menu_pulldown" ),
	UxCreateWidget( mb_query_resubmit );


	/* Creation of mb_query_resubmit_top */
	mb_query_resubmit_top = UxCreatePushButton( "mb_query_resubmit_top", mb_query_resubmit );
	UxPutLabelString( mb_query_resubmit_top, "To Top of Plan" ),
	UxPutMnemonic( mb_query_resubmit_top, "T" ),
	UxPutAccelerator( mb_query_resubmit_top, "Ctrl<Key>T" ),
	UxPutAcceleratorText( mb_query_resubmit_top, "^T" ),
	UxCreateWidget( mb_query_resubmit_top );

	UxAddCallback( mb_query_resubmit_top, XmNactivateCallback,
		(XtCallbackProc) cb_resubmit_to_top,
		(XtPointer) UxPps_queryContext );


	/* Creation of mb_query_resubmit_bottom */
	mb_query_resubmit_bottom = UxCreatePushButton( "mb_query_resubmit_bottom", mb_query_resubmit );
	UxPutLabelString( mb_query_resubmit_bottom, "To Bottom of Plan" ),
	UxPutMnemonic( mb_query_resubmit_bottom, "B" ),
	UxPutAccelerator( mb_query_resubmit_bottom, "Ctrl<Key>B" ),
	UxPutAcceleratorText( mb_query_resubmit_bottom, "^B" ),
	UxCreateWidget( mb_query_resubmit_bottom );

	UxAddCallback( mb_query_resubmit_bottom, XmNactivateCallback,
		(XtCallbackProc) cb_resubmit_to_bottom,
		(XtPointer) UxPps_queryContext );


	/* Creation of pb_query_resubmit */
	pb_query_resubmit = UxCreateCascadeButton( "pb_query_resubmit", mb_query_edit );
	UxPutLabelString( pb_query_resubmit, "Resubmit" ),
	UxPutMnemonic( pb_query_resubmit, "R" ),
	UxPutSubMenuId( pb_query_resubmit, "mb_query_resubmit" ),
	UxCreateWidget( pb_query_resubmit );


	/* Creation of mb_query_top_b1 */
	mb_query_top_b1 = UxCreateCascadeButton( "mb_query_top_b1", mb_query );
	UxPutLabelString( mb_query_top_b1, "Edit" ),
	UxPutMnemonic( mb_query_top_b1, "E" ),
	UxPutSubMenuId( mb_query_top_b1, "mb_query_edit" ),
	UxCreateWidget( mb_query_top_b1 );


	/* Creation of pb_query_setting */
	pb_query_setting = UxCreateRowColumn( "pb_query_setting", mb_query );
	UxPutRowColumnType( pb_query_setting, "menu_pulldown" ),
	UxCreateWidget( pb_query_setting );


	/* Creation of pb_query_load_query */
	pb_query_load_query = UxCreatePushButton( "pb_query_load_query", pb_query_setting );
	UxPutLabelString( pb_query_load_query, "Load Query..." ),
	UxPutMnemonic( pb_query_load_query, "L" ),
	UxPutAccelerator( pb_query_load_query, "Ctrl<Key>L" ),
	UxPutAcceleratorText( pb_query_load_query, "^L" ),
	UxCreateWidget( pb_query_load_query );

	UxAddCallback( pb_query_load_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_load_query,
		(XtPointer) UxPps_queryContext );


	/* Creation of pb_query_save_query */
	pb_query_save_query = UxCreatePushButton( "pb_query_save_query", pb_query_setting );
	UxPutLabelString( pb_query_save_query, "Save Query" ),
	UxPutMnemonic( pb_query_save_query, "S" ),
	UxPutAccelerator( pb_query_save_query, "Ctrl<Key>S" ),
	UxPutAcceleratorText( pb_query_save_query, "^S" ),
	UxCreateWidget( pb_query_save_query );

	UxAddCallback( pb_query_save_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_save_query,
		(XtPointer) UxPps_queryContext );


	/* Creation of pb_query_save_query_as */
	pb_query_save_query_as = UxCreatePushButton( "pb_query_save_query_as", pb_query_setting );
	UxPutLabelString( pb_query_save_query_as, "Save Query As..." ),
	UxPutMnemonic( pb_query_save_query_as, "A" ),
	UxPutAccelerator( pb_query_save_query_as, "Ctrl<Key>A" ),
	UxPutAcceleratorText( pb_query_save_query_as, "^A" ),
	UxCreateWidget( pb_query_save_query_as );

	UxAddCallback( pb_query_save_query_as, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_save_query_as,
		(XtPointer) UxPps_queryContext );


	/* Creation of pb_query_clear */
	pb_query_clear = UxCreatePushButton( "pb_query_clear", pb_query_setting );
	UxPutLabelString( pb_query_clear, "Clear Query" ),
	UxPutMnemonic( pb_query_clear, "C" ),
	UxPutAccelerator( pb_query_clear, "Ctrl<Key>C" ),
	UxPutAcceleratorText( pb_query_clear, "^C" ),
	UxCreateWidget( pb_query_clear );

	UxAddCallback( pb_query_clear, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_clear,
		(XtPointer) UxPps_queryContext );


	/* Creation of mb_query_top_b2 */
	mb_query_top_b2 = UxCreateCascadeButton( "mb_query_top_b2", mb_query );
	UxPutLabelString( mb_query_top_b2, "QuerySetting" ),
	UxPutMnemonic( mb_query_top_b2, "Q" ),
	UxPutSubMenuId( mb_query_top_b2, "pb_query_setting" ),
	UxCreateWidget( mb_query_top_b2 );


	/* Creation of f_query_query_settings */
	f_query_query_settings = UxCreateFrame( "f_query_query_settings", pps_query );
	UxPutWidth( f_query_query_settings, 860 ),
	UxPutHeight( f_query_query_settings, 290 ),
	UxPutX( f_query_query_settings, 10 ),
	UxPutY( f_query_query_settings, 70 ),
	UxCreateWidget( f_query_query_settings );


	/* Creation of bb_query_query_settings */
	bb_query_query_settings = UxCreateBulletinBoard( "bb_query_query_settings", f_query_query_settings );
	UxPutResizePolicy( bb_query_query_settings, "resize_none" ),
	UxPutWidth( bb_query_query_settings, 880 ),
	UxPutHeight( bb_query_query_settings, 286 ),
	UxPutX( bb_query_query_settings, 0 ),
	UxPutY( bb_query_query_settings, 0 ),
	UxCreateWidget( bb_query_query_settings );


	/* Creation of tb_query_L1_Orders */
	tb_query_L1_Orders = UxCreateToggleButton( "tb_query_L1_Orders", bb_query_query_settings );
	UxPutX( tb_query_L1_Orders, 16 ),
	UxPutY( tb_query_L1_Orders, 10 ),
	UxPutWidth( tb_query_L1_Orders, 89 ),
	UxPutHeight( tb_query_L1_Orders, 24 ),
	UxPutLabelString( tb_query_L1_Orders, "L1 Orders" ),
	UxPutSensitive( tb_query_L1_Orders, "true" ),
	UxCreateWidget( tb_query_L1_Orders );

	UxAddCallback( tb_query_L1_Orders, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_query_L1_Orders,
		(XtPointer) UxPps_queryContext );


	/* Creation of tb_query_L1_QLK */
	tb_query_L1_QLK = UxCreateToggleButton( "tb_query_L1_QLK", bb_query_query_settings );
	UxPutX( tb_query_L1_QLK, 115 ),
	UxPutY( tb_query_L1_QLK, 9 ),
	UxPutWidth( tb_query_L1_QLK, 73 ),
	UxPutHeight( tb_query_L1_QLK, 26 ),
	UxPutLabelString( tb_query_L1_QLK, "L1 QLK" ),
	UxPutSensitive( tb_query_L1_QLK, "true" ),
	UxCreateWidget( tb_query_L1_QLK );

	UxAddCallback( tb_query_L1_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_query_L1_QLK,
		(XtPointer) UxPps_queryContext );


	/* Creation of tb_query_Scan_Orders */
	tb_query_Scan_Orders = UxCreateToggleButton( "tb_query_Scan_Orders", bb_query_query_settings );
	UxPutX( tb_query_Scan_Orders, 201 ),
	UxPutY( tb_query_Scan_Orders, 10 ),
	UxPutWidth( tb_query_Scan_Orders, 105 ),
	UxPutHeight( tb_query_Scan_Orders, 24 ),
	UxPutLabelString( tb_query_Scan_Orders, "Scan Orders" ),
	UxCreateWidget( tb_query_Scan_Orders );

	UxAddCallback( tb_query_Scan_Orders, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_query_Scan_Orders,
		(XtPointer) UxPps_queryContext );


	/* Creation of tb_query_Scan_QLK */
	tb_query_Scan_QLK = UxCreateToggleButton( "tb_query_Scan_QLK", bb_query_query_settings );
	UxPutX( tb_query_Scan_QLK, 319 ),
	UxPutY( tb_query_Scan_QLK, 10 ),
	UxPutWidth( tb_query_Scan_QLK, 88 ),
	UxPutHeight( tb_query_Scan_QLK, 24 ),
	UxPutLabelString( tb_query_Scan_QLK, "Scan QLK" ),
	UxCreateWidget( tb_query_Scan_QLK );

	UxAddCallback( tb_query_Scan_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_query_Scan_QLK,
		(XtPointer) UxPps_queryContext );


	/* Creation of rc_query_sat */
	rc_query_sat = UxCreateRowColumn( "rc_query_sat", bb_query_query_settings );
	UxPutRowColumnType( rc_query_sat, "menu_pulldown" ),
	UxPutSensitive( rc_query_sat, "false" ),
	UxCreateWidget( rc_query_sat );


	/* Creation of pb_query_sat_any */
	pb_query_sat_any = UxCreatePushButton( "pb_query_sat_any", rc_query_sat );
	UxPutLabelString( pb_query_sat_any, "Any" ),
	UxPutSensitive( pb_query_sat_any, "false" ),
	UxCreateWidget( pb_query_sat_any );


	/* Creation of pb_query_sat_e1 */
	pb_query_sat_e1 = UxCreatePushButton( "pb_query_sat_e1", rc_query_sat );
	UxPutLabelString( pb_query_sat_e1, "E1" ),
	UxPutSensitive( pb_query_sat_e1, "false" ),
	UxCreateWidget( pb_query_sat_e1 );


	/* Creation of pb_query_sat_e2 */
	pb_query_sat_e2 = UxCreatePushButton( "pb_query_sat_e2", rc_query_sat );
	UxPutLabelString( pb_query_sat_e2, "E2" ),
	UxPutSensitive( pb_query_sat_e2, "false" ),
	UxCreateWidget( pb_query_sat_e2 );


	/* Creation of pb_query_sat_j1 */
	pb_query_sat_j1 = UxCreatePushButton( "pb_query_sat_j1", rc_query_sat );
	UxPutLabelString( pb_query_sat_j1, "J1" ),
	UxPutSensitive( pb_query_sat_j1, "false" ),
	UxCreateWidget( pb_query_sat_j1 );


	/* Creation of pb_query_sat_r1 */
	pb_query_sat_r1 = UxCreatePushButton( "pb_query_sat_r1", rc_query_sat );
	UxPutLabelString( pb_query_sat_r1, "R1" ),
	UxPutSensitive( pb_query_sat_r1, "false" ),
	UxCreateWidget( pb_query_sat_r1 );


	/* Creation of om_query_sat */
	om_query_sat = UxCreateRowColumn( "om_query_sat", bb_query_query_settings );
	UxPutRowColumnType( om_query_sat, "menu_option" ),
	UxPutX( om_query_sat, 15 ),
	UxPutY( om_query_sat, 44 ),
	UxPutWidth( om_query_sat, 48 ),
	UxPutHeight( om_query_sat, 60 ),
	UxPutLabelString( om_query_sat, "Sat" ),
	UxPutSensitive( om_query_sat, "false" ),
	UxPutSubMenuId( om_query_sat, "rc_query_sat" ),
	UxCreateWidget( om_query_sat );


	/* Creation of rc_query_sens */
	rc_query_sens = UxCreateRowColumn( "rc_query_sens", bb_query_query_settings );
	UxPutRowColumnType( rc_query_sens, "menu_pulldown" ),
	UxPutSensitive( rc_query_sens, "false" ),
	UxCreateWidget( rc_query_sens );


	/* Creation of pb_query_sens_any */
	pb_query_sens_any = UxCreatePushButton( "pb_query_sens_any", rc_query_sens );
	UxPutLabelString( pb_query_sens_any, "Any" ),
	UxPutSensitive( pb_query_sens_any, "false" ),
	UxCreateWidget( pb_query_sens_any );


	/* Creation of pb_query_sens_s */
	pb_query_sens_s = UxCreatePushButton( "pb_query_sens_s", rc_query_sens );
	UxPutLabelString( pb_query_sens_s, "S" ),
	UxPutSensitive( pb_query_sens_s, "false" ),
	UxCreateWidget( pb_query_sens_s );


	/* Creation of rc_query_sens_o */
	rc_query_sens_o = UxCreatePushButton( "rc_query_sens_o", rc_query_sens );
	UxPutLabelString( rc_query_sens_o, "O" ),
	UxCreateWidget( rc_query_sens_o );


	/* Creation of rc_query_sens_v */
	rc_query_sens_v = UxCreatePushButton( "rc_query_sens_v", rc_query_sens );
	UxPutLabelString( rc_query_sens_v, "V" ),
	UxCreateWidget( rc_query_sens_v );


	/* Creation of rc_query_sens_z */
	rc_query_sens_z = UxCreatePushButton( "rc_query_sens_z", rc_query_sens );
	UxPutLabelString( rc_query_sens_z, "Z" ),
	UxCreateWidget( rc_query_sens_z );


	/* Creation of om_query_sens */
	om_query_sens = UxCreateRowColumn( "om_query_sens", bb_query_query_settings );
	UxPutRowColumnType( om_query_sens, "menu_option" ),
	UxPutX( om_query_sens, 120 ),
	UxPutY( om_query_sens, 45 ),
	UxPutWidth( om_query_sens, 60 ),
	UxPutHeight( om_query_sens, 54 ),
	UxPutLabelString( om_query_sens, "Sens" ),
	UxPutSensitive( om_query_sens, "false" ),
	UxPutSubMenuId( om_query_sens, "rc_query_sens" ),
	UxCreateWidget( om_query_sens );


	/* Creation of rc_query_activity */
	rc_query_activity = UxCreateRowColumn( "rc_query_activity", bb_query_query_settings );
	UxPutRowColumnType( rc_query_activity, "menu_pulldown" ),
	UxPutSensitive( rc_query_activity, "false" ),
	UxCreateWidget( rc_query_activity );


	/* Creation of pb_query_activity_any */
	pb_query_activity_any = UxCreatePushButton( "pb_query_activity_any", rc_query_activity );
	UxPutLabelString( pb_query_activity_any, "Any" ),
	UxPutSensitive( pb_query_activity_any, "false" ),
	UxCreateWidget( pb_query_activity_any );


	/* Creation of pb_query_activity_rlt */
	pb_query_activity_rlt = UxCreatePushButton( "pb_query_activity_rlt", rc_query_activity );
	UxPutLabelString( pb_query_activity_rlt, "RLT" ),
	UxPutSensitive( pb_query_activity_rlt, "false" ),
	UxCreateWidget( pb_query_activity_rlt );


	/* Creation of pb_query_activity_dmp */
	pb_query_activity_dmp = UxCreatePushButton( "pb_query_activity_dmp", rc_query_activity );
	UxPutLabelString( pb_query_activity_dmp, "DMP" ),
	UxPutSensitive( pb_query_activity_dmp, "false" ),
	UxCreateWidget( pb_query_activity_dmp );


	/* Creation of om_query_activity */
	om_query_activity = UxCreateRowColumn( "om_query_activity", bb_query_query_settings );
	UxPutRowColumnType( om_query_activity, "menu_option" ),
	UxPutX( om_query_activity, 457 ),
	UxPutY( om_query_activity, 43 ),
	UxPutWidth( om_query_activity, 60 ),
	UxPutHeight( om_query_activity, 54 ),
	UxPutLabelString( om_query_activity, "Activity" ),
	UxPutSensitive( om_query_activity, "false" ),
	UxPutSubMenuId( om_query_activity, "rc_query_activity" ),
	UxCreateWidget( om_query_activity );


	/* Creation of rc_query_station */
	rc_query_station = UxCreateRowColumn( "rc_query_station", bb_query_query_settings );
	UxPutRowColumnType( rc_query_station, "menu_pulldown" ),
	UxPutSensitive( rc_query_station, "false" ),
	UxCreateWidget( rc_query_station );


	/* Creation of pb_query_station_any */
	pb_query_station_any = UxCreatePushButton( "pb_query_station_any", rc_query_station );
	UxPutLabelString( pb_query_station_any, "Any" ),
	UxPutSensitive( pb_query_station_any, "false" ),
	UxCreateWidget( pb_query_station_any );


	/* Creation of pb_query_station_fa */
	pb_query_station_fa = UxCreatePushButton( "pb_query_station_fa", rc_query_station );
	UxPutLabelString( pb_query_station_fa, "FA" ),
	UxPutSensitive( pb_query_station_fa, "false" ),
	UxCreateWidget( pb_query_station_fa );


	/* Creation of pb_query_station_mc */
	pb_query_station_mc = UxCreatePushButton( "pb_query_station_mc", rc_query_station );
	UxPutLabelString( pb_query_station_mc, "MC" ),
	UxPutSensitive( pb_query_station_mc, "false" ),
	UxCreateWidget( pb_query_station_mc );


	/* Creation of pb_query_station_gh */
	pb_query_station_gh = UxCreatePushButton( "pb_query_station_gh", rc_query_station );
	UxPutLabelString( pb_query_station_gh, "GH" ),
	UxCreateWidget( pb_query_station_gh );


	/* Creation of pb_query_station_ph */
	pb_query_station_ph = UxCreatePushButton( "pb_query_station_ph", rc_query_station );
	UxPutLabelString( pb_query_station_ph, "PH" ),
	UxCreateWidget( pb_query_station_ph );


	/* Creation of pb_query_station_as */
	pb_query_station_as = UxCreatePushButton( "pb_query_station_as", rc_query_station );
	UxPutLabelString( pb_query_station_as, "AS" ),
	UxCreateWidget( pb_query_station_as );


	/* Creation of pb_query_station_be */
	pb_query_station_be = UxCreatePushButton( "pb_query_station_be", rc_query_station );
	UxPutLabelString( pb_query_station_be, "BE" ),
	UxCreateWidget( pb_query_station_be );


	/* Creation of pb_query_station_co */
	pb_query_station_co = UxCreatePushButton( "pb_query_station_co", rc_query_station );
	UxPutLabelString( pb_query_station_co, "CO" ),
	UxCreateWidget( pb_query_station_co );


	/* Creation of pb_query_station_cu */
	pb_query_station_cu = UxCreatePushButton( "pb_query_station_cu", rc_query_station );
	UxPutLabelString( pb_query_station_cu, "CU" ),
	UxCreateWidget( pb_query_station_cu );


	/* Creation of pb_query_station_es */
	pb_query_station_es = UxCreatePushButton( "pb_query_station_es", rc_query_station );
	UxPutLabelString( pb_query_station_es, "ES" ),
	UxCreateWidget( pb_query_station_es );


	/* Creation of pb_query_station_fs */
	pb_query_station_fs = UxCreatePushButton( "pb_query_station_fs", rc_query_station );
	UxPutLabelString( pb_query_station_fs, "FS" ),
	UxCreateWidget( pb_query_station_fs );


	/* Creation of pb_query_station_ha */
	pb_query_station_ha = UxCreatePushButton( "pb_query_station_ha", rc_query_station );
	UxPutLabelString( pb_query_station_ha, "HA" ),
	UxCreateWidget( pb_query_station_ha );


	/* Creation of pb_query_station_ho */
	pb_query_station_ho = UxCreatePushButton( "pb_query_station_ho", rc_query_station );
	UxPutLabelString( pb_query_station_ho, "HO" ),
	UxCreateWidget( pb_query_station_ho );


	/* Creation of pb_query_station_hy */
	pb_query_station_hy = UxCreatePushButton( "pb_query_station_hy", rc_query_station );
	UxPutLabelString( pb_query_station_hy, "HY" ),
	UxCreateWidget( pb_query_station_hy );


	/* Creation of pb_query_station_is */
	pb_query_station_is = UxCreatePushButton( "pb_query_station_is", rc_query_station );
	UxPutLabelString( pb_query_station_is, "IS" ),
	UxCreateWidget( pb_query_station_is );


	/* Creation of pb_query_station_in */
	pb_query_station_in = UxCreatePushButton( "pb_query_station_in", rc_query_station );
	UxPutLabelString( pb_query_station_in, "IN" ),
	UxCreateWidget( pb_query_station_in );


	/* Creation of pb_query_station_jo */
	pb_query_station_jo = UxCreatePushButton( "pb_query_station_jo", rc_query_station );
	UxPutLabelString( pb_query_station_jo, "JO" ),
	UxCreateWidget( pb_query_station_jo );


	/* Creation of pb_query_station_ks */
	pb_query_station_ks = UxCreatePushButton( "pb_query_station_ks", rc_query_station );
	UxPutLabelString( pb_query_station_ks, "KS" ),
	UxCreateWidget( pb_query_station_ks );


	/* Creation of pb_query_station_ku */
	pb_query_station_ku = UxCreatePushButton( "pb_query_station_ku", rc_query_station );
	UxPutLabelString( pb_query_station_ku, "KU" ),
	UxCreateWidget( pb_query_station_ku );


	/* Creation of pb_query_station_ma */
	pb_query_station_ma = UxCreatePushButton( "pb_query_station_ma", rc_query_station );
	UxPutLabelString( pb_query_station_ma, "MA" ),
	UxCreateWidget( pb_query_station_ma );


	/* Creation of pb_query_station_ms */
	pb_query_station_ms = UxCreatePushButton( "pb_query_station_ms", rc_query_station );
	UxPutLabelString( pb_query_station_ms, "MS" ),
	UxCreateWidget( pb_query_station_ms );


	/* Creation of pb_query_station_pp */
	pb_query_station_pp = UxCreatePushButton( "pb_query_station_pp", rc_query_station );
	UxPutLabelString( pb_query_station_pp, "PP" ),
	UxCreateWidget( pb_query_station_pp );


	/* Creation of pb_query_station_sa */
	pb_query_station_sa = UxCreatePushButton( "pb_query_station_sa", rc_query_station );
	UxPutLabelString( pb_query_station_sa, "SA" ),
	UxCreateWidget( pb_query_station_sa );


	/* Creation of pb_query_station_se */
	pb_query_station_se = UxCreatePushButton( "pb_query_station_se", rc_query_station );
	UxPutLabelString( pb_query_station_se, "SE" ),
	UxCreateWidget( pb_query_station_se );


	/* Creation of pb_query_station_sy */
	pb_query_station_sy = UxCreatePushButton( "pb_query_station_sy", rc_query_station );
	UxPutLabelString( pb_query_station_sy, "SY" ),
	UxCreateWidget( pb_query_station_sy );


	/* Creation of pb_query_station_tf */
	pb_query_station_tf = UxCreatePushButton( "pb_query_station_tf", rc_query_station );
	UxPutLabelString( pb_query_station_tf, "TF" ),
	UxCreateWidget( pb_query_station_tf );


	/* Creation of pb_query_station_tg */
	pb_query_station_tg = UxCreatePushButton( "pb_query_station_tg", rc_query_station );
	UxPutLabelString( pb_query_station_tg, "TG" ),
	UxCreateWidget( pb_query_station_tg );


	/* Creation of pb_query_station_th */
	pb_query_station_th = UxCreatePushButton( "pb_query_station_th", rc_query_station );
	UxPutLabelString( pb_query_station_th, "TH" ),
	UxCreateWidget( pb_query_station_th );


	/* Creation of pb_query_station_to */
	pb_query_station_to = UxCreatePushButton( "pb_query_station_to", rc_query_station );
	UxPutLabelString( pb_query_station_to, "TO" ),
	UxCreateWidget( pb_query_station_to );


	/* Creation of pb_query_station_ts */
	pb_query_station_ts = UxCreatePushButton( "pb_query_station_ts", rc_query_station );
	UxPutLabelString( pb_query_station_ts, "TS" ),
	UxCreateWidget( pb_query_station_ts );


	/* Creation of pb_query_station_wf */
	pb_query_station_wf = UxCreatePushButton( "pb_query_station_wf", rc_query_station );
	UxPutLabelString( pb_query_station_wf, "WF" ),
	UxCreateWidget( pb_query_station_wf );


	/* Creation of pb_query_station_wh */
	pb_query_station_wh = UxCreatePushButton( "pb_query_station_wh", rc_query_station );
	UxPutLabelString( pb_query_station_wh, "WH" ),
	UxCreateWidget( pb_query_station_wh );


	/* Creation of om_query_station */
	om_query_station = UxCreateRowColumn( "om_query_station", bb_query_query_settings );
	UxPutRowColumnType( om_query_station, "menu_option" ),
	UxPutX( om_query_station, 650 ),
	UxPutY( om_query_station, 40 ),
	UxPutWidth( om_query_station, 60 ),
	UxPutHeight( om_query_station, 54 ),
	UxPutLabelString( om_query_station, "Station" ),
	UxPutSensitive( om_query_station, "false" ),
	UxPutSubMenuId( om_query_station, "rc_query_station" ),
	UxCreateWidget( om_query_station );


	/* Creation of rc_query_priority */
	rc_query_priority = UxCreateRowColumn( "rc_query_priority", bb_query_query_settings );
	UxPutRowColumnType( rc_query_priority, "menu_pulldown" ),
	UxPutSensitive( rc_query_priority, "false" ),
	UxCreateWidget( rc_query_priority );


	/* Creation of pb_query_priority_any */
	pb_query_priority_any = UxCreatePushButton( "pb_query_priority_any", rc_query_priority );
	UxPutLabelString( pb_query_priority_any, "Any" ),
	UxPutSensitive( pb_query_priority_any, "false" ),
	UxCreateWidget( pb_query_priority_any );


	/* Creation of pb_query_priority_low */
	pb_query_priority_low = UxCreatePushButton( "pb_query_priority_low", rc_query_priority );
	UxPutLabelString( pb_query_priority_low, "LOW" ),
	UxPutSensitive( pb_query_priority_low, "false" ),
	UxCreateWidget( pb_query_priority_low );


	/* Creation of pb_query_priority_routine */
	pb_query_priority_routine = UxCreatePushButton( "pb_query_priority_routine", rc_query_priority );
	UxPutLabelString( pb_query_priority_routine, "ROUTINE" ),
	UxPutSensitive( pb_query_priority_routine, "false" ),
	UxCreateWidget( pb_query_priority_routine );


	/* Creation of pb_query_priority_high */
	pb_query_priority_high = UxCreatePushButton( "pb_query_priority_high", rc_query_priority );
	UxPutLabelString( pb_query_priority_high, "HIGH" ),
	UxPutSensitive( pb_query_priority_high, "false" ),
	UxCreateWidget( pb_query_priority_high );


	/* Creation of pb_query_priority_urgent */
	pb_query_priority_urgent = UxCreatePushButton( "pb_query_priority_urgent", rc_query_priority );
	UxPutLabelString( pb_query_priority_urgent, "URGENT" ),
	UxPutSensitive( pb_query_priority_urgent, "false" ),
	UxCreateWidget( pb_query_priority_urgent );


	/* Creation of om_query_priority */
	om_query_priority = UxCreateRowColumn( "om_query_priority", bb_query_query_settings );
	UxPutRowColumnType( om_query_priority, "menu_option" ),
	UxPutX( om_query_priority, 463 ),
	UxPutY( om_query_priority, 91 ),
	UxPutWidth( om_query_priority, 60 ),
	UxPutHeight( om_query_priority, 54 ),
	UxPutLabelString( om_query_priority, "Priority" ),
	UxPutSensitive( om_query_priority, "false" ),
	UxPutSubMenuId( om_query_priority, "rc_query_priority" ),
	UxCreateWidget( om_query_priority );


	/* Creation of l_query_rev */
	l_query_rev = UxCreateLabel( "l_query_rev", bb_query_query_settings );
	UxPutX( l_query_rev, 232 ),
	UxPutY( l_query_rev, 47 ),
	UxPutWidth( l_query_rev, 34 ),
	UxPutHeight( l_query_rev, 32 ),
	UxPutLabelString( l_query_rev, "Rev" ),
	UxPutSensitive( l_query_rev, "false" ),
	UxCreateWidget( l_query_rev );


	/* Creation of tf_query_rev */
	tf_query_rev = UxCreateTextField( "tf_query_rev", bb_query_query_settings );
	UxPutWidth( tf_query_rev, 67 ),
	UxPutX( tf_query_rev, 268 ),
	UxPutY( tf_query_rev, 48 ),
	UxPutHeight( tf_query_rev, 32 ),
	UxPutSensitive( tf_query_rev, "false" ),
	UxCreateWidget( tf_query_rev );


	/* Creation of l_query_seq */
	l_query_seq = UxCreateLabel( "l_query_seq", bb_query_query_settings );
	UxPutX( l_query_seq, 345 ),
	UxPutY( l_query_seq, 46 ),
	UxPutWidth( l_query_seq, 34 ),
	UxPutHeight( l_query_seq, 32 ),
	UxPutLabelString( l_query_seq, "Seq" ),
	UxPutSensitive( l_query_seq, "false" ),
	UxCreateWidget( l_query_seq );


	/* Creation of tf_query_seq */
	tf_query_seq = UxCreateTextField( "tf_query_seq", bb_query_query_settings );
	UxPutWidth( tf_query_seq, 55 ),
	UxPutX( tf_query_seq, 383 ),
	UxPutY( tf_query_seq, 47 ),
	UxPutHeight( tf_query_seq, 32 ),
	UxPutSensitive( tf_query_seq, "false" ),
	UxCreateWidget( tf_query_seq );


	/* Creation of l_query_job_id */
	l_query_job_id = UxCreateLabel( "l_query_job_id", bb_query_query_settings );
	UxPutX( l_query_job_id, 9 ),
	UxPutY( l_query_job_id, 94 ),
	UxPutWidth( l_query_job_id, 52 ),
	UxPutHeight( l_query_job_id, 32 ),
	UxPutLabelString( l_query_job_id, "Job ID" ),
	UxPutSensitive( l_query_job_id, "false" ),
	UxCreateWidget( l_query_job_id );


	/* Creation of tf_query_job_id */
	tf_query_job_id = UxCreateTextField( "tf_query_job_id", bb_query_query_settings );
	UxPutWidth( tf_query_job_id, 99 ),
	UxPutX( tf_query_job_id, 67 ),
	UxPutY( tf_query_job_id, 94 ),
	UxPutHeight( tf_query_job_id, 32 ),
	UxPutSensitive( tf_query_job_id, "false" ),
	UxCreateWidget( tf_query_job_id );


	/* Creation of l_query_order_id */
	l_query_order_id = UxCreateLabel( "l_query_order_id", bb_query_query_settings );
	UxPutX( l_query_order_id, 171 ),
	UxPutY( l_query_order_id, 94 ),
	UxPutWidth( l_query_order_id, 73 ),
	UxPutHeight( l_query_order_id, 32 ),
	UxPutLabelString( l_query_order_id, "Order ID" ),
	UxPutSensitive( l_query_order_id, "false" ),
	UxCreateWidget( l_query_order_id );


	/* Creation of tf_query_order_id */
	tf_query_order_id = UxCreateTextField( "tf_query_order_id", bb_query_query_settings );
	UxPutWidth( tf_query_order_id, 94 ),
	UxPutX( tf_query_order_id, 245 ),
	UxPutY( tf_query_order_id, 94 ),
	UxPutHeight( tf_query_order_id, 32 ),
	UxPutSensitive( tf_query_order_id, "false" ),
	UxCreateWidget( tf_query_order_id );


	/* Creation of l_query_item_id */
	l_query_item_id = UxCreateLabel( "l_query_item_id", bb_query_query_settings );
	UxPutX( l_query_item_id, 344 ),
	UxPutY( l_query_item_id, 94 ),
	UxPutWidth( l_query_item_id, 61 ),
	UxPutHeight( l_query_item_id, 32 ),
	UxPutLabelString( l_query_item_id, "Item ID" ),
	UxPutSensitive( l_query_item_id, "false" ),
	UxCreateWidget( l_query_item_id );


	/* Creation of tf_query_item_id */
	tf_query_item_id = UxCreateTextField( "tf_query_item_id", bb_query_query_settings );
	UxPutWidth( tf_query_item_id, 43 ),
	UxPutX( tf_query_item_id, 409 ),
	UxPutY( tf_query_item_id, 93 ),
	UxPutHeight( tf_query_item_id, 32 ),
	UxPutSensitive( tf_query_item_id, "false" ),
	UxCreateWidget( tf_query_item_id );


	/* Creation of l_query_frame_id */
	l_query_frame_id = UxCreateLabel( "l_query_frame_id", bb_query_query_settings );
	UxPutX( l_query_frame_id, 5 ),
	UxPutY( l_query_frame_id, 142 ),
	UxPutWidth( l_query_frame_id, 76 ),
	UxPutHeight( l_query_frame_id, 32 ),
	UxPutLabelString( l_query_frame_id, "Frame ID" ),
	UxPutSensitive( l_query_frame_id, "false" ),
	UxCreateWidget( l_query_frame_id );


	/* Creation of tf_query_frame_id */
	tf_query_frame_id = UxCreateTextField( "tf_query_frame_id", bb_query_query_settings );
	UxPutWidth( tf_query_frame_id, 67 ),
	UxPutX( tf_query_frame_id, 78 ),
	UxPutY( tf_query_frame_id, 142 ),
	UxPutHeight( tf_query_frame_id, 32 ),
	UxPutSensitive( tf_query_frame_id, "false" ),
	UxCreateWidget( tf_query_frame_id );


	/* Creation of l_query_subframe_id */
	l_query_subframe_id = UxCreateLabel( "l_query_subframe_id", bb_query_query_settings );
	UxPutX( l_query_subframe_id, 152 ),
	UxPutY( l_query_subframe_id, 141 ),
	UxPutWidth( l_query_subframe_id, 109 ),
	UxPutHeight( l_query_subframe_id, 32 ),
	UxPutLabelString( l_query_subframe_id, "Subframe ID" ),
	UxPutSensitive( l_query_subframe_id, "false" ),
	UxCreateWidget( l_query_subframe_id );


	/* Creation of tf_query_subframe_id */
	tf_query_subframe_id = UxCreateTextField( "tf_query_subframe_id", bb_query_query_settings );
	UxPutWidth( tf_query_subframe_id, 43 ),
	UxPutX( tf_query_subframe_id, 262 ),
	UxPutY( tf_query_subframe_id, 140 ),
	UxPutHeight( tf_query_subframe_id, 32 ),
	UxPutSensitive( tf_query_subframe_id, "false" ),
	UxCreateWidget( tf_query_subframe_id );


	/* Creation of rc_query_product_type */
	rc_query_product_type = UxCreateRowColumn( "rc_query_product_type", bb_query_query_settings );
	UxPutRowColumnType( rc_query_product_type, "menu_pulldown" ),
	UxPutX( rc_query_product_type, 0 ),
	UxPutY( rc_query_product_type, 88 ),
	UxPutSensitive( rc_query_product_type, "false" ),
	UxCreateWidget( rc_query_product_type );


	/* Creation of pb_query_product_type_any */
	pb_query_product_type_any = UxCreatePushButton( "pb_query_product_type_any", rc_query_product_type );
	UxPutLabelString( pb_query_product_type_any, "Any" ),
	UxPutX( pb_query_product_type_any, 2 ),
	UxPutY( pb_query_product_type_any, 145 ),
	UxPutSensitive( pb_query_product_type_any, "false" ),
	UxCreateWidget( pb_query_product_type_any );


	/* Creation of pb_query_product_type_standard */
	pb_query_product_type_standard = UxCreatePushButton( "pb_query_product_type_standard", rc_query_product_type );
	UxPutLabelString( pb_query_product_type_standard, "STANDARD" ),
	UxPutX( pb_query_product_type_standard, 2 ),
	UxPutY( pb_query_product_type_standard, 145 ),
	UxPutSensitive( pb_query_product_type_standard, "false" ),
	UxCreateWidget( pb_query_product_type_standard );


	/* Creation of rc_query_product_type_calset */
	rc_query_product_type_calset = UxCreatePushButton( "rc_query_product_type_calset", rc_query_product_type );
	UxPutLabelString( rc_query_product_type_calset, "CAL_SET" ),
	UxCreateWidget( rc_query_product_type_calset );


	/* Creation of rc_query_product_type_ramp */
	rc_query_product_type_ramp = UxCreatePushButton( "rc_query_product_type_ramp", rc_query_product_type );
	UxPutLabelString( rc_query_product_type_ramp, "RAMP" ),
	UxCreateWidget( rc_query_product_type_ramp );


	/* Creation of rc_query_product_type_scansar */
	rc_query_product_type_scansar = UxCreatePushButton( "rc_query_product_type_scansar", rc_query_product_type );
	UxPutLabelString( rc_query_product_type_scansar, "SCANSAR" ),
	UxCreateWidget( rc_query_product_type_scansar );


	/* Creation of pb_query_product_type_complex */
	pb_query_product_type_complex = UxCreatePushButton( "pb_query_product_type_complex", rc_query_product_type );
	UxPutLabelString( pb_query_product_type_complex, "COMPLEX" ),
	UxPutX( pb_query_product_type_complex, 2 ),
	UxPutY( pb_query_product_type_complex, 145 ),
	UxPutSensitive( pb_query_product_type_complex, "false" ),
	UxCreateWidget( pb_query_product_type_complex );


	/* Creation of pb_query_product_type_ccsd */
	pb_query_product_type_ccsd = UxCreatePushButton( "pb_query_product_type_ccsd", rc_query_product_type );
	UxPutLabelString( pb_query_product_type_ccsd, "CCSD" ),
	UxPutX( pb_query_product_type_ccsd, 2 ),
	UxPutY( pb_query_product_type_ccsd, 145 ),
	UxPutSensitive( pb_query_product_type_ccsd, "false" ),
	UxCreateWidget( pb_query_product_type_ccsd );


	/* Creation of om_query_product_type */
	om_query_product_type = UxCreateRowColumn( "om_query_product_type", bb_query_query_settings );
	UxPutRowColumnType( om_query_product_type, "menu_option" ),
	UxPutX( om_query_product_type, 350 ),
	UxPutY( om_query_product_type, 140 ),
	UxPutWidth( om_query_product_type, 60 ),
	UxPutHeight( om_query_product_type, 54 ),
	UxPutLabelString( om_query_product_type, "Product Type" ),
	UxPutSensitive( om_query_product_type, "false" ),
	UxPutSubMenuId( om_query_product_type, "rc_query_product_type" ),
	UxCreateWidget( om_query_product_type );


	/* Creation of l_query_pixel_spacing */
	l_query_pixel_spacing = UxCreateLabel( "l_query_pixel_spacing", bb_query_query_settings );
	UxPutX( l_query_pixel_spacing, 616 ),
	UxPutY( l_query_pixel_spacing, 140 ),
	UxPutWidth( l_query_pixel_spacing, 119 ),
	UxPutHeight( l_query_pixel_spacing, 32 ),
	UxPutLabelString( l_query_pixel_spacing, "Pixel Spacing" ),
	UxPutSensitive( l_query_pixel_spacing, "false" ),
	UxCreateWidget( l_query_pixel_spacing );


	/* Creation of tf_query_pixel_spacing */
	tf_query_pixel_spacing = UxCreateTextField( "tf_query_pixel_spacing", bb_query_query_settings );
	UxPutWidth( tf_query_pixel_spacing, 43 ),
	UxPutX( tf_query_pixel_spacing, 744 ),
	UxPutY( tf_query_pixel_spacing, 141 ),
	UxPutHeight( tf_query_pixel_spacing, 32 ),
	UxPutSensitive( tf_query_pixel_spacing, "false" ),
	UxCreateWidget( tf_query_pixel_spacing );


	/* Creation of rc_query_projection */
	rc_query_projection = UxCreateRowColumn( "rc_query_projection", bb_query_query_settings );
	UxPutRowColumnType( rc_query_projection, "menu_pulldown" ),
	UxPutX( rc_query_projection, 0 ),
	UxPutY( rc_query_projection, 77 ),
	UxPutSensitive( rc_query_projection, "false" ),
	UxCreateWidget( rc_query_projection );


	/* Creation of pb_query_projection_any */
	pb_query_projection_any = UxCreatePushButton( "pb_query_projection_any", rc_query_projection );
	UxPutLabelString( pb_query_projection_any, "Any" ),
	UxPutX( pb_query_projection_any, 2 ),
	UxPutY( pb_query_projection_any, 145 ),
	UxPutSensitive( pb_query_projection_any, "false" ),
	UxCreateWidget( pb_query_projection_any );


	/* Creation of pb_query_projection_ground_range */
	pb_query_projection_ground_range = UxCreatePushButton( "pb_query_projection_ground_range", rc_query_projection );
	UxPutLabelString( pb_query_projection_ground_range, "GROUND_RANGE" ),
	UxPutX( pb_query_projection_ground_range, 2 ),
	UxPutY( pb_query_projection_ground_range, 145 ),
	UxPutSensitive( pb_query_projection_ground_range, "false" ),
	UxCreateWidget( pb_query_projection_ground_range );


	/* Creation of pb_query_projection_slant_range */
	pb_query_projection_slant_range = UxCreatePushButton( "pb_query_projection_slant_range", rc_query_projection );
	UxPutLabelString( pb_query_projection_slant_range, "SLANT_RANGE" ),
	UxPutX( pb_query_projection_slant_range, 2 ),
	UxPutY( pb_query_projection_slant_range, 145 ),
	UxPutSensitive( pb_query_projection_slant_range, "false" ),
	UxCreateWidget( pb_query_projection_slant_range );


	/* Creation of pb_query_projection_lambert */
	pb_query_projection_lambert = UxCreatePushButton( "pb_query_projection_lambert", rc_query_projection );
	UxPutLabelString( pb_query_projection_lambert, "LAMBERT" ),
	UxPutX( pb_query_projection_lambert, 2 ),
	UxPutY( pb_query_projection_lambert, 145 ),
	UxPutSensitive( pb_query_projection_lambert, "false" ),
	UxCreateWidget( pb_query_projection_lambert );


	/* Creation of pb_query_projection_ps */
	pb_query_projection_ps = UxCreatePushButton( "pb_query_projection_ps", rc_query_projection );
	UxPutLabelString( pb_query_projection_ps, "PS" ),
	UxPutX( pb_query_projection_ps, 2 ),
	UxPutY( pb_query_projection_ps, 145 ),
	UxPutSensitive( pb_query_projection_ps, "false" ),
	UxCreateWidget( pb_query_projection_ps );


	/* Creation of pb_query_projection_utm */
	pb_query_projection_utm = UxCreatePushButton( "pb_query_projection_utm", rc_query_projection );
	UxPutLabelString( pb_query_projection_utm, "UTM" ),
	UxPutX( pb_query_projection_utm, 2 ),
	UxPutY( pb_query_projection_utm, 145 ),
	UxPutSensitive( pb_query_projection_utm, "false" ),
	UxCreateWidget( pb_query_projection_utm );


	/* Creation of om_query_projection */
	om_query_projection = UxCreateRowColumn( "om_query_projection", bb_query_query_settings );
	UxPutRowColumnType( om_query_projection, "menu_option" ),
	UxPutX( om_query_projection, 14 ),
	UxPutY( om_query_projection, 187 ),
	UxPutWidth( om_query_projection, 60 ),
	UxPutHeight( om_query_projection, 54 ),
	UxPutLabelString( om_query_projection, "Projection" ),
	UxPutSensitive( om_query_projection, "false" ),
	UxPutSubMenuId( om_query_projection, "rc_query_projection" ),
	UxCreateWidget( om_query_projection );


	/* Creation of l_query_proc_gain */
	l_query_proc_gain = UxCreateLabel( "l_query_proc_gain", bb_query_query_settings );
	UxPutX( l_query_proc_gain, 360 ),
	UxPutY( l_query_proc_gain, 190 ),
	UxPutWidth( l_query_proc_gain, 88 ),
	UxPutHeight( l_query_proc_gain, 32 ),
	UxPutLabelString( l_query_proc_gain, "Proc Gain" ),
	UxPutSensitive( l_query_proc_gain, "false" ),
	UxCreateWidget( l_query_proc_gain );


	/* Creation of tf_query_proc_gain */
	tf_query_proc_gain = UxCreateTextField( "tf_query_proc_gain", bb_query_query_settings );
	UxPutWidth( tf_query_proc_gain, 49 ),
	UxPutX( tf_query_proc_gain, 460 ),
	UxPutY( tf_query_proc_gain, 190 ),
	UxPutHeight( tf_query_proc_gain, 32 ),
	UxPutSensitive( tf_query_proc_gain, "false" ),
	UxCreateWidget( tf_query_proc_gain );


	/* Creation of rc_query_state */
	rc_query_state = UxCreateRowColumn( "rc_query_state", bb_query_query_settings );
	UxPutRowColumnType( rc_query_state, "menu_pulldown" ),
	UxPutSensitive( rc_query_state, "false" ),
	UxCreateWidget( rc_query_state );


	/* Creation of pb_query_state_any */
	pb_query_state_any = UxCreatePushButton( "pb_query_state_any", rc_query_state );
	UxPutLabelString( pb_query_state_any, "Any" ),
	UxPutSensitive( pb_query_state_any, "false" ),
	UxCreateWidget( pb_query_state_any );


	/* Creation of pb_query_state_pending */
	pb_query_state_pending = UxCreatePushButton( "pb_query_state_pending", rc_query_state );
	UxPutLabelString( pb_query_state_pending, "PENDING" ),
	UxPutSensitive( pb_query_state_pending, "false" ),
	UxCreateWidget( pb_query_state_pending );


	/* Creation of pb_query_state_ready */
	pb_query_state_ready = UxCreatePushButton( "pb_query_state_ready", rc_query_state );
	UxPutLabelString( pb_query_state_ready, "READY" ),
	UxPutSensitive( pb_query_state_ready, "false" ),
	UxCreateWidget( pb_query_state_ready );


	/* Creation of pb_query_state_available */
	pb_query_state_available = UxCreatePushButton( "pb_query_state_available", rc_query_state );
	UxPutLabelString( pb_query_state_available, "AVAILABLE" ),
	UxPutSensitive( pb_query_state_available, "false" ),
	UxCreateWidget( pb_query_state_available );


	/* Creation of pb_query_state_submitted */
	pb_query_state_submitted = UxCreatePushButton( "pb_query_state_submitted", rc_query_state );
	UxPutLabelString( pb_query_state_submitted, "SUBMITTED" ),
	UxPutSensitive( pb_query_state_submitted, "false" ),
	UxCreateWidget( pb_query_state_submitted );


	/* Creation of pb_query_state_retry */
	pb_query_state_retry = UxCreatePushButton( "pb_query_state_retry", rc_query_state );
	UxPutLabelString( pb_query_state_retry, "RETRY" ),
	UxCreateWidget( pb_query_state_retry );


	/* Creation of rc_query_state_completed */
	rc_query_state_completed = UxCreatePushButton( "rc_query_state_completed", rc_query_state );
	UxPutLabelString( rc_query_state_completed, "COMPLETED" ),
	UxCreateWidget( rc_query_state_completed );


	/* Creation of rc_query_state_fail */
	rc_query_state_fail = UxCreatePushButton( "rc_query_state_fail", rc_query_state );
	UxPutLabelString( rc_query_state_fail, "CANCEL/FAIL" ),
	UxCreateWidget( rc_query_state_fail );


	/* Creation of om_query_state */
	om_query_state = UxCreateRowColumn( "om_query_state", bb_query_query_settings );
	UxPutRowColumnType( om_query_state, "menu_option" ),
	UxPutX( om_query_state, 655 ),
	UxPutY( om_query_state, 89 ),
	UxPutWidth( om_query_state, 60 ),
	UxPutHeight( om_query_state, 54 ),
	UxPutLabelString( om_query_state, "State" ),
	UxPutSensitive( om_query_state, "false" ),
	UxPutSubMenuId( om_query_state, "rc_query_state" ),
	UxCreateWidget( om_query_state );


	/* Creation of rc_query_media_type */
	rc_query_media_type = UxCreateRowColumn( "rc_query_media_type", bb_query_query_settings );
	UxPutRowColumnType( rc_query_media_type, "menu_pulldown" ),
	UxPutX( rc_query_media_type, 0 ),
	UxPutY( rc_query_media_type, 217 ),
	UxPutSensitive( rc_query_media_type, "false" ),
	UxCreateWidget( rc_query_media_type );


	/* Creation of pb_query_media_type_any */
	pb_query_media_type_any = UxCreatePushButton( "pb_query_media_type_any", rc_query_media_type );
	UxPutLabelString( pb_query_media_type_any, "Any" ),
	UxPutX( pb_query_media_type_any, 2 ),
	UxPutY( pb_query_media_type_any, 241 ),
	UxPutSensitive( pb_query_media_type_any, "false" ),
	UxCreateWidget( pb_query_media_type_any );


	/* Creation of pb_query_media_type_disk */
	pb_query_media_type_disk = UxCreatePushButton( "pb_query_media_type_disk", rc_query_media_type );
	UxPutLabelString( pb_query_media_type_disk, "DISK" ),
	UxPutX( pb_query_media_type_disk, 2 ),
	UxPutY( pb_query_media_type_disk, 241 ),
	UxPutSensitive( pb_query_media_type_disk, "false" ),
	UxCreateWidget( pb_query_media_type_disk );


	/* Creation of pb_query_media_type_dcrsi */
	pb_query_media_type_dcrsi = UxCreatePushButton( "pb_query_media_type_dcrsi", rc_query_media_type );
	UxPutLabelString( pb_query_media_type_dcrsi, "DCRSI" ),
	UxPutX( pb_query_media_type_dcrsi, 2 ),
	UxPutY( pb_query_media_type_dcrsi, 241 ),
	UxPutSensitive( pb_query_media_type_dcrsi, "false" ),
	UxCreateWidget( pb_query_media_type_dcrsi );


	/* Creation of rc_query_media_type_id1 */
	rc_query_media_type_id1 = UxCreatePushButton( "rc_query_media_type_id1", rc_query_media_type );
	UxPutLabelString( rc_query_media_type_id1, "ID-1" ),
	UxCreateWidget( rc_query_media_type_id1 );


	/* Creation of om_query_media_type */
	om_query_media_type = UxCreateRowColumn( "om_query_media_type", bb_query_query_settings );
	UxPutRowColumnType( om_query_media_type, "menu_option" ),
	UxPutX( om_query_media_type, 360 ),
	UxPutY( om_query_media_type, 240 ),
	UxPutWidth( om_query_media_type, 60 ),
	UxPutHeight( om_query_media_type, 54 ),
	UxPutLabelString( om_query_media_type, "Media Type" ),
	UxPutSensitive( om_query_media_type, "false" ),
	UxPutSubMenuId( om_query_media_type, "rc_query_media_type" ),
	UxCreateWidget( om_query_media_type );


	/* Creation of l_query_media_id */
	l_query_media_id = UxCreateLabel( "l_query_media_id", bb_query_query_settings );
	UxPutX( l_query_media_id, 634 ),
	UxPutY( l_query_media_id, 239 ),
	UxPutWidth( l_query_media_id, 87 ),
	UxPutHeight( l_query_media_id, 32 ),
	UxPutLabelString( l_query_media_id, "Media ID" ),
	UxPutSensitive( l_query_media_id, "false" ),
	UxCreateWidget( l_query_media_id );


	/* Creation of tf_query_media_id */
	tf_query_media_id = UxCreateTextField( "tf_query_media_id", bb_query_query_settings );
	UxPutWidth( tf_query_media_id, 83 ),
	UxPutX( tf_query_media_id, 730 ),
	UxPutY( tf_query_media_id, 236 ),
	UxPutHeight( tf_query_media_id, 32 ),
	UxPutSensitive( tf_query_media_id, "false" ),
	UxCreateWidget( tf_query_media_id );


	/* Creation of rc_query_processor_mode */
	rc_query_processor_mode = UxCreateRowColumn( "rc_query_processor_mode", bb_query_query_settings );
	UxPutRowColumnType( rc_query_processor_mode, "menu_pulldown" ),
	UxPutSensitive( rc_query_processor_mode, "false" ),
	UxCreateWidget( rc_query_processor_mode );


	/* Creation of pb_query_processor_mode_any */
	pb_query_processor_mode_any = UxCreatePushButton( "pb_query_processor_mode_any", rc_query_processor_mode );
	UxPutLabelString( pb_query_processor_mode_any, "Any" ),
	UxPutSensitive( pb_query_processor_mode_any, "false" ),
	UxCreateWidget( pb_query_processor_mode_any );


	/* Creation of pb_query_processor_mode_continuous */
	pb_query_processor_mode_continuous = UxCreatePushButton( "pb_query_processor_mode_continuous", rc_query_processor_mode );
	UxPutLabelString( pb_query_processor_mode_continuous, "CONTINUOUS" ),
	UxPutSensitive( pb_query_processor_mode_continuous, "false" ),
	UxCreateWidget( pb_query_processor_mode_continuous );


	/* Creation of pb_query_processor_mode_scansar */
	pb_query_processor_mode_scansar = UxCreatePushButton( "pb_query_processor_mode_scansar", rc_query_processor_mode );
	UxPutLabelString( pb_query_processor_mode_scansar, "SCANSAR" ),
	UxPutSensitive( pb_query_processor_mode_scansar, "false" ),
	UxCreateWidget( pb_query_processor_mode_scansar );


	/* Creation of om_query_processor_mode */
	om_query_processor_mode = UxCreateRowColumn( "om_query_processor_mode", bb_query_query_settings );
	UxPutRowColumnType( om_query_processor_mode, "menu_option" ),
	UxPutX( om_query_processor_mode, 600 ),
	UxPutY( om_query_processor_mode, 190 ),
	UxPutLabelString( om_query_processor_mode, "Processor Mode" ),
	UxPutSensitive( om_query_processor_mode, "false" ),
	UxPutSubMenuId( om_query_processor_mode, "rc_query_processor_mode" ),
	UxCreateWidget( om_query_processor_mode );


	/* Creation of rc_query_data_direction */
	rc_query_data_direction = UxCreateRowColumn( "rc_query_data_direction", bb_query_query_settings );
	UxPutRowColumnType( rc_query_data_direction, "menu_pulldown" ),
	UxPutSensitive( rc_query_data_direction, "false" ),
	UxCreateWidget( rc_query_data_direction );


	/* Creation of pb_query_data_direction_any */
	pb_query_data_direction_any = UxCreatePushButton( "pb_query_data_direction_any", rc_query_data_direction );
	UxPutLabelString( pb_query_data_direction_any, "Any" ),
	UxPutSensitive( pb_query_data_direction_any, "false" ),
	UxCreateWidget( pb_query_data_direction_any );


	/* Creation of pb_query_data_direction_forward */
	pb_query_data_direction_forward = UxCreatePushButton( "pb_query_data_direction_forward", rc_query_data_direction );
	UxPutLabelString( pb_query_data_direction_forward, "FORWARD" ),
	UxPutSensitive( pb_query_data_direction_forward, "false" ),
	UxCreateWidget( pb_query_data_direction_forward );


	/* Creation of pb_query_data_direction_reverse */
	pb_query_data_direction_reverse = UxCreatePushButton( "pb_query_data_direction_reverse", rc_query_data_direction );
	UxPutLabelString( pb_query_data_direction_reverse, "REVERSE" ),
	UxPutSensitive( pb_query_data_direction_reverse, "false" ),
	UxCreateWidget( pb_query_data_direction_reverse );


	/* Creation of pb_query_data_direction_unknown */
	pb_query_data_direction_unknown = UxCreatePushButton( "pb_query_data_direction_unknown", rc_query_data_direction );
	UxPutLabelString( pb_query_data_direction_unknown, "UNKNOWN" ),
	UxPutSensitive( pb_query_data_direction_unknown, "false" ),
	UxCreateWidget( pb_query_data_direction_unknown );


	/* Creation of om_query_data_direction */
	om_query_data_direction = UxCreateRowColumn( "om_query_data_direction", bb_query_query_settings );
	UxPutRowColumnType( om_query_data_direction, "menu_option" ),
	UxPutX( om_query_data_direction, 20 ),
	UxPutY( om_query_data_direction, 240 ),
	UxPutLabelString( om_query_data_direction, "Data Direction" ),
	UxPutSensitive( om_query_data_direction, "false" ),
	UxPutSubMenuId( om_query_data_direction, "rc_query_data_direction" ),
	UxCreateWidget( om_query_data_direction );


	/* Creation of f_query_query_results */
	f_query_query_results = UxCreateFrame( "f_query_query_results", pps_query );
	UxPutWidth( f_query_query_results, 1122 ),
	UxPutHeight( f_query_query_results, 368 ),
	UxPutX( f_query_query_results, 8 ),
	UxPutY( f_query_query_results, 412 ),
	UxCreateWidget( f_query_query_results );


	/* Creation of bb_query_query_results */
	bb_query_query_results = UxCreateBulletinBoard( "bb_query_query_results", f_query_query_results );
	UxPutResizePolicy( bb_query_query_results, "resize_none" ),
	UxPutWidth( bb_query_query_results, 850 ),
	UxPutHeight( bb_query_query_results, 352 ),
	UxPutX( bb_query_query_results, 2 ),
	UxPutY( bb_query_query_results, 2 ),
	UxPutMarginHeight( bb_query_query_results, 0 ),
	UxPutMarginWidth( bb_query_query_results, 0 ),
	UxCreateWidget( bb_query_query_results );


	/* Creation of l_query_priority */
	l_query_priority = UxCreateLabel( "l_query_priority", bb_query_query_results );
	UxPutX( l_query_priority, 76 ),
	UxPutY( l_query_priority, 8 ),
	UxPutWidth( l_query_priority, 68 ),
	UxPutHeight( l_query_priority, 32 ),
	UxPutLabelString( l_query_priority, "Priority" ),
	UxCreateWidget( l_query_priority );


	/* Creation of l_query_media */
	l_query_media = UxCreateLabel( "l_query_media", bb_query_query_results );
	UxPutX( l_query_media, 156 ),
	UxPutY( l_query_media, 8 ),
	UxPutWidth( l_query_media, 68 ),
	UxPutHeight( l_query_media, 32 ),
	UxPutLabelString( l_query_media, "Media ID" ),
	UxCreateWidget( l_query_media );


	/* Creation of l_query_mode */
	l_query_mode = UxCreateLabel( "l_query_mode", bb_query_query_results );
	UxPutX( l_query_mode, 238 ),
	UxPutY( l_query_mode, 8 ),
	UxPutWidth( l_query_mode, 56 ),
	UxPutHeight( l_query_mode, 32 ),
	UxPutLabelString( l_query_mode, "Mode" ),
	UxCreateWidget( l_query_mode );


	/* Creation of l_query_sat_sens_rev */
	l_query_sat_sens_rev = UxCreateLabel( "l_query_sat_sens_rev", bb_query_query_results );
	UxPutX( l_query_sat_sens_rev, 304 ),
	UxPutY( l_query_sat_sens_rev, 8 ),
	UxPutWidth( l_query_sat_sens_rev, 88 ),
	UxPutHeight( l_query_sat_sens_rev, 32 ),
	UxPutLabelString( l_query_sat_sens_rev, "Datatake ID" ),
	UxCreateWidget( l_query_sat_sens_rev );


	/* Creation of l_query_frame */
	l_query_frame = UxCreateLabel( "l_query_frame", bb_query_query_results );
	UxPutX( l_query_frame, 420 ),
	UxPutY( l_query_frame, 10 ),
	UxPutWidth( l_query_frame, 48 ),
	UxPutHeight( l_query_frame, 32 ),
	UxPutLabelString( l_query_frame, "Frame\nID" ),
	UxCreateWidget( l_query_frame );


	/* Creation of l_query_job */
	l_query_job = UxCreateLabel( "l_query_job", bb_query_query_results );
	UxPutX( l_query_job, 470 ),
	UxPutY( l_query_job, 10 ),
	UxPutWidth( l_query_job, 62 ),
	UxPutHeight( l_query_job, 32 ),
	UxPutLabelString( l_query_job, "Job ID" ),
	UxCreateWidget( l_query_job );


	/* Creation of l_query_order */
	l_query_order = UxCreateLabel( "l_query_order", bb_query_query_results );
	UxPutX( l_query_order, 540 ),
	UxPutY( l_query_order, 10 ),
	UxPutWidth( l_query_order, 110 ),
	UxPutHeight( l_query_order, 30 ),
	UxPutLabelString( l_query_order, "Order & Item ID" ),
	UxCreateWidget( l_query_order );


	/* Creation of l_query_state */
	l_query_state = UxCreateLabel( "l_query_state", bb_query_query_results );
	UxPutX( l_query_state, 670 ),
	UxPutY( l_query_state, 10 ),
	UxPutWidth( l_query_state, 64 ),
	UxPutHeight( l_query_state, 32 ),
	UxPutLabelString( l_query_state, "State" ),
	UxCreateWidget( l_query_state );


	/* Creation of l_query_age */
	l_query_age = UxCreateLabel( "l_query_age", bb_query_query_results );
	UxPutX( l_query_age, 750 ),
	UxPutY( l_query_age, 10 ),
	UxPutWidth( l_query_age, 48 ),
	UxPutHeight( l_query_age, 32 ),
	UxPutLabelString( l_query_age, "Age\n(Days)" ),
	UxCreateWidget( l_query_age );


	/* Creation of l_query_order_type */
	l_query_order_type = UxCreateLabel( "l_query_order_type", bb_query_query_results );
	UxPutX( l_query_order_type, 4 ),
	UxPutY( l_query_order_type, 8 ),
	UxPutWidth( l_query_order_type, 68 ),
	UxPutHeight( l_query_order_type, 32 ),
	UxPutLabelString( l_query_order_type, "Order\nType" ),
	UxCreateWidget( l_query_order_type );


	/* Creation of scrolledWindowList2 */
	scrolledWindowList2 = UxCreateScrolledWindow( "scrolledWindowList2", bb_query_query_results );
	UxPutScrollingPolicy( scrolledWindowList2, "application_defined" ),
	UxPutVisualPolicy( scrolledWindowList2, "variable" ),
	UxPutScrollBarDisplayPolicy( scrolledWindowList2, "static" ),
	UxPutShadowThickness( scrolledWindowList2, 0 ),
	UxPutX( scrolledWindowList2, 3 ),
	UxPutY( scrolledWindowList2, 46 ),
	UxCreateWidget( scrolledWindowList2 );


	/* Creation of sw_query_results_list */
	sw_query_results_list = UxCreateScrolledList( "sw_query_results_list", scrolledWindowList2 );
	UxPutWidth( sw_query_results_list, 1090 ),
	UxPutHeight( sw_query_results_list, 300 ),
	UxPutScrollBarDisplayPolicy( sw_query_results_list, "static" ),
	UxPutListSizePolicy( sw_query_results_list, "constant" ),
	UxPutFontList( sw_query_results_list, "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1" ),
	UxPutSelectionPolicy( sw_query_results_list, "single_select" ),
	UxCreateWidget( sw_query_results_list );


	/* Creation of l_query_tce */
	l_query_tce = UxCreateLabel( "l_query_tce", bb_query_query_results );
	UxPutX( l_query_tce, 820 ),
	UxPutY( l_query_tce, 8 ),
	UxPutWidth( l_query_tce, 40 ),
	UxPutHeight( l_query_tce, 32 ),
	UxPutLabelString( l_query_tce, "TCE" ),
	UxCreateWidget( l_query_tce );


	/* Creation of l_query_gha */
	l_query_gha = UxCreateLabel( "l_query_gha", bb_query_query_results );
	UxPutX( l_query_gha, 870 ),
	UxPutY( l_query_gha, 8 ),
	UxPutWidth( l_query_gha, 40 ),
	UxPutHeight( l_query_gha, 32 ),
	UxPutLabelString( l_query_gha, "GHA" ),
	UxCreateWidget( l_query_gha );


	/* Creation of l_query_sv */
	l_query_sv = UxCreateLabel( "l_query_sv", bb_query_query_results );
	UxPutX( l_query_sv, 920 ),
	UxPutY( l_query_sv, 8 ),
	UxPutWidth( l_query_sv, 40 ),
	UxPutHeight( l_query_sv, 32 ),
	UxPutLabelString( l_query_sv, "State\nVector" ),
	UxCreateWidget( l_query_sv );


	/* Creation of l_query_scan_results */
	l_query_scan_results = UxCreateLabel( "l_query_scan_results", bb_query_query_results );
	UxPutX( l_query_scan_results, 960 ),
	UxPutY( l_query_scan_results, 8 ),
	UxPutWidth( l_query_scan_results, 72 ),
	UxPutHeight( l_query_scan_results, 32 ),
	UxPutLabelString( l_query_scan_results, "Scan\nResults" ),
	UxPutRecomputeSize( l_query_scan_results, "true" ),
	UxCreateWidget( l_query_scan_results );


	/* Creation of l_query_calib_params */
	l_query_calib_params = UxCreateLabel( "l_query_calib_params", bb_query_query_results );
	UxPutX( l_query_calib_params, 1030 ),
	UxPutY( l_query_calib_params, 8 ),
	UxPutWidth( l_query_calib_params, 69 ),
	UxPutHeight( l_query_calib_params, 32 ),
	UxPutLabelString( l_query_calib_params, "Calib\nParams" ),
	UxPutRecomputeSize( l_query_calib_params, "true" ),
	UxCreateWidget( l_query_calib_params );


	/* Creation of l_query_query_results */
	l_query_query_results = UxCreateLabel( "l_query_query_results", pps_query );
	UxPutX( l_query_query_results, 10 ),
	UxPutY( l_query_query_results, 370 ),
	UxPutWidth( l_query_query_results, 108 ),
	UxPutHeight( l_query_query_results, 30 ),
	UxPutLabelString( l_query_query_results, "Query Results" ),
	UxCreateWidget( l_query_query_results );


	/* Creation of f_query_items_found */
	f_query_items_found = UxCreateFrame( "f_query_items_found", pps_query );
	UxPutWidth( f_query_items_found, 142 ),
	UxPutHeight( f_query_items_found, 34 ),
	UxPutX( f_query_items_found, 118 ),
	UxPutY( f_query_items_found, 370 ),
	UxCreateWidget( f_query_items_found );


	/* Creation of bb_query_items_found */
	bb_query_items_found = UxCreateBulletinBoard( "bb_query_items_found", f_query_items_found );
	UxPutResizePolicy( bb_query_items_found, "resize_none" ),
	UxPutWidth( bb_query_items_found, 132 ),
	UxPutHeight( bb_query_items_found, 28 ),
	UxPutX( bb_query_items_found, 2 ),
	UxPutY( bb_query_items_found, 2 ),
	UxPutMarginHeight( bb_query_items_found, 0 ),
	UxPutMarginWidth( bb_query_items_found, 0 ),
	UxCreateWidget( bb_query_items_found );


	/* Creation of l_query_items_found */
	l_query_items_found = UxCreateLabel( "l_query_items_found", bb_query_items_found );
	UxPutX( l_query_items_found, 0 ),
	UxPutY( l_query_items_found, 0 ),
	UxPutWidth( l_query_items_found, 80 ),
	UxPutHeight( l_query_items_found, 32 ),
	UxPutLabelString( l_query_items_found, "Items Found" ),
	UxCreateWidget( l_query_items_found );


	/* Creation of l_query_num_items */
	l_query_num_items = UxCreateLabel( "l_query_num_items", bb_query_items_found );
	UxPutX( l_query_num_items, 90 ),
	UxPutY( l_query_num_items, 6 ),
	UxPutWidth( l_query_num_items, 43 ),
	UxPutHeight( l_query_num_items, 20 ),
	UxPutLabelString( l_query_num_items, " 999999" ),
	UxPutAlignment( l_query_num_items, "alignment_end" ),
	UxPutRecomputeSize( l_query_num_items, "false" ),
	UxCreateWidget( l_query_num_items );


	/* Creation of l_query_query_settings */
	l_query_query_settings = UxCreateLabel( "l_query_query_settings", pps_query );
	UxPutX( l_query_query_settings, 8 ),
	UxPutY( l_query_query_settings, 40 ),
	UxPutWidth( l_query_query_settings, 116 ),
	UxPutHeight( l_query_query_settings, 30 ),
	UxPutLabelString( l_query_query_settings, "Query Settings" ),
	UxCreateWidget( l_query_query_settings );


	/* Creation of pb_query_query */
	pb_query_query = UxCreatePushButton( "pb_query_query", pps_query );
	UxPutX( pb_query_query, 568 ),
	UxPutY( pb_query_query, 360 ),
	UxPutWidth( pb_query_query, 80 ),
	UxPutHeight( pb_query_query, 40 ),
	UxPutLabelString( pb_query_query, "Query" ),
	UxPutFontList( pb_query_query, "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1" ),
	UxPutSensitive( pb_query_query, "false" ),
	UxPutShowAsDefault( pb_query_query, 1 ),
	UxCreateWidget( pb_query_query );

	UxAddCallback( pb_query_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_query_query,
		(XtPointer) UxPps_queryContext );


	/* Creation of frame2 */
	frame2 = UxCreateFrame( "frame2", pps_query );
	UxPutWidth( frame2, 228 ),
	UxPutHeight( frame2, 290 ),
	UxPutX( frame2, 894 ),
	UxPutY( frame2, 70 ),
	UxCreateWidget( frame2 );


	/* Creation of bulletinBoard1 */
	bulletinBoard1 = UxCreateBulletinBoard( "bulletinBoard1", frame2 );
	UxPutResizePolicy( bulletinBoard1, "resize_none" ),
	UxPutWidth( bulletinBoard1, 250 ),
	UxPutHeight( bulletinBoard1, 286 ),
	UxPutX( bulletinBoard1, -47 ),
	UxPutY( bulletinBoard1, 0 ),
	UxCreateWidget( bulletinBoard1 );


	/* Creation of rc_query_order_first */
	rc_query_order_first = UxCreateRowColumn( "rc_query_order_first", bulletinBoard1 );
	UxPutRowColumnType( rc_query_order_first, "menu_pulldown" ),
	UxPutSensitive( rc_query_order_first, "false" ),
	UxPutX( rc_query_order_first, 93 ),
	UxPutY( rc_query_order_first, 0 ),
	UxCreateWidget( rc_query_order_first );


	/* Creation of pb_query_order_first_none */
	pb_query_order_first_none = UxCreatePushButton( "pb_query_order_first_none", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_none, "None" ),
	UxPutSensitive( pb_query_order_first_none, "false" ),
	UxPutX( pb_query_order_first_none, 97 ),
	UxPutY( pb_query_order_first_none, 2 ),
	UxCreateWidget( pb_query_order_first_none );


	/* Creation of pb_query_order_first_order_type */
	pb_query_order_first_order_type = UxCreatePushButton( "pb_query_order_first_order_type", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_order_type, "Order Type" ),
	UxPutX( pb_query_order_first_order_type, 97 ),
	UxPutY( pb_query_order_first_order_type, 24 ),
	UxCreateWidget( pb_query_order_first_order_type );


	/* Creation of pb_query_order_first_priority */
	pb_query_order_first_priority = UxCreatePushButton( "pb_query_order_first_priority", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_priority, "Priority" ),
	UxPutX( pb_query_order_first_priority, 97 ),
	UxPutY( pb_query_order_first_priority, 46 ),
	UxCreateWidget( pb_query_order_first_priority );


	/* Creation of pb_query_order_first_media_id */
	pb_query_order_first_media_id = UxCreatePushButton( "pb_query_order_first_media_id", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_media_id, "Media ID" ),
	UxPutX( pb_query_order_first_media_id, 97 ),
	UxPutY( pb_query_order_first_media_id, 68 ),
	UxCreateWidget( pb_query_order_first_media_id );


	/* Creation of pb_query_order_first_mode */
	pb_query_order_first_mode = UxCreatePushButton( "pb_query_order_first_mode", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_mode, "Mode" ),
	UxPutX( pb_query_order_first_mode, 97 ),
	UxPutY( pb_query_order_first_mode, 90 ),
	UxCreateWidget( pb_query_order_first_mode );


	/* Creation of pb_query_order_first_sat_sens_rev */
	pb_query_order_first_sat_sens_rev = UxCreatePushButton( "pb_query_order_first_sat_sens_rev", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_query_order_first_sat_sens_rev, 97 ),
	UxPutY( pb_query_order_first_sat_sens_rev, 112 ),
	UxCreateWidget( pb_query_order_first_sat_sens_rev );


	/* Creation of pb_query_order_first_frame_subframe */
	pb_query_order_first_frame_subframe = UxCreatePushButton( "pb_query_order_first_frame_subframe", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_frame_subframe, "Frame ID" ),
	UxPutX( pb_query_order_first_frame_subframe, 97 ),
	UxPutY( pb_query_order_first_frame_subframe, 134 ),
	UxCreateWidget( pb_query_order_first_frame_subframe );


	/* Creation of pb_query_order_first_job_id */
	pb_query_order_first_job_id = UxCreatePushButton( "pb_query_order_first_job_id", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_job_id, "Job ID" ),
	UxPutX( pb_query_order_first_job_id, 97 ),
	UxPutY( pb_query_order_first_job_id, 156 ),
	UxCreateWidget( pb_query_order_first_job_id );


	/* Creation of pb_query_order_first_order_item */
	pb_query_order_first_order_item = UxCreatePushButton( "pb_query_order_first_order_item", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_order_item, "Order & Item ID" ),
	UxPutX( pb_query_order_first_order_item, 97 ),
	UxPutY( pb_query_order_first_order_item, 178 ),
	UxCreateWidget( pb_query_order_first_order_item );


	/* Creation of pb_query_order_first_state */
	pb_query_order_first_state = UxCreatePushButton( "pb_query_order_first_state", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_state, "State" ),
	UxPutX( pb_query_order_first_state, 97 ),
	UxPutY( pb_query_order_first_state, 200 ),
	UxCreateWidget( pb_query_order_first_state );


	/* Creation of pb_query_order_first_age */
	pb_query_order_first_age = UxCreatePushButton( "pb_query_order_first_age", rc_query_order_first );
	UxPutLabelString( pb_query_order_first_age, "Age (Days)" ),
	UxPutX( pb_query_order_first_age, 97 ),
	UxPutY( pb_query_order_first_age, 222 ),
	UxCreateWidget( pb_query_order_first_age );


	/* Creation of om_query_order_first */
	om_query_order_first = UxCreateRowColumn( "om_query_order_first", bulletinBoard1 );
	UxPutRowColumnType( om_query_order_first, "menu_option" ),
	UxPutX( om_query_order_first, 12 ),
	UxPutY( om_query_order_first, 37 ),
	UxPutWidth( om_query_order_first, 60 ),
	UxPutHeight( om_query_order_first, 54 ),
	UxPutLabelString( om_query_order_first, "First    " ),
	UxPutSensitive( om_query_order_first, "false" ),
	UxPutSubMenuId( om_query_order_first, "rc_query_order_first" ),
	UxCreateWidget( om_query_order_first );


	/* Creation of rc_query_order_second */
	rc_query_order_second = UxCreateRowColumn( "rc_query_order_second", bulletinBoard1 );
	UxPutRowColumnType( rc_query_order_second, "menu_pulldown" ),
	UxPutSensitive( rc_query_order_second, "false" ),
	UxPutX( rc_query_order_second, 93 ),
	UxPutY( rc_query_order_second, 0 ),
	UxCreateWidget( rc_query_order_second );


	/* Creation of pb_query_order_second_none */
	pb_query_order_second_none = UxCreatePushButton( "pb_query_order_second_none", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_none, "None" ),
	UxPutSensitive( pb_query_order_second_none, "false" ),
	UxPutX( pb_query_order_second_none, 97 ),
	UxPutY( pb_query_order_second_none, 2 ),
	UxCreateWidget( pb_query_order_second_none );


	/* Creation of pb_query_order_second_order_type */
	pb_query_order_second_order_type = UxCreatePushButton( "pb_query_order_second_order_type", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_order_type, "Order Type" ),
	UxPutX( pb_query_order_second_order_type, 97 ),
	UxPutY( pb_query_order_second_order_type, 24 ),
	UxCreateWidget( pb_query_order_second_order_type );


	/* Creation of pb_query_order_second_priority */
	pb_query_order_second_priority = UxCreatePushButton( "pb_query_order_second_priority", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_priority, "Priority" ),
	UxPutX( pb_query_order_second_priority, 97 ),
	UxPutY( pb_query_order_second_priority, 46 ),
	UxCreateWidget( pb_query_order_second_priority );


	/* Creation of pb_query_order_second_media_id */
	pb_query_order_second_media_id = UxCreatePushButton( "pb_query_order_second_media_id", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_media_id, "Media ID" ),
	UxPutX( pb_query_order_second_media_id, 97 ),
	UxPutY( pb_query_order_second_media_id, 68 ),
	UxCreateWidget( pb_query_order_second_media_id );


	/* Creation of pb_query_order_second_mode */
	pb_query_order_second_mode = UxCreatePushButton( "pb_query_order_second_mode", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_mode, "Mode" ),
	UxPutX( pb_query_order_second_mode, 97 ),
	UxPutY( pb_query_order_second_mode, 90 ),
	UxCreateWidget( pb_query_order_second_mode );


	/* Creation of pb_query_order_second_sat_sens_rev */
	pb_query_order_second_sat_sens_rev = UxCreatePushButton( "pb_query_order_second_sat_sens_rev", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_query_order_second_sat_sens_rev, 97 ),
	UxPutY( pb_query_order_second_sat_sens_rev, 112 ),
	UxCreateWidget( pb_query_order_second_sat_sens_rev );


	/* Creation of pb_query_order_second_frame_subframe */
	pb_query_order_second_frame_subframe = UxCreatePushButton( "pb_query_order_second_frame_subframe", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_frame_subframe, "Frame ID" ),
	UxPutX( pb_query_order_second_frame_subframe, 97 ),
	UxPutY( pb_query_order_second_frame_subframe, 134 ),
	UxCreateWidget( pb_query_order_second_frame_subframe );


	/* Creation of pb_query_order_second_job_id */
	pb_query_order_second_job_id = UxCreatePushButton( "pb_query_order_second_job_id", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_job_id, "Job ID" ),
	UxPutX( pb_query_order_second_job_id, 97 ),
	UxPutY( pb_query_order_second_job_id, 156 ),
	UxCreateWidget( pb_query_order_second_job_id );


	/* Creation of pb_query_order_second_order_item */
	pb_query_order_second_order_item = UxCreatePushButton( "pb_query_order_second_order_item", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_order_item, "Order & Item ID" ),
	UxPutX( pb_query_order_second_order_item, 97 ),
	UxPutY( pb_query_order_second_order_item, 178 ),
	UxCreateWidget( pb_query_order_second_order_item );


	/* Creation of pb_query_order_second_state */
	pb_query_order_second_state = UxCreatePushButton( "pb_query_order_second_state", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_state, "State" ),
	UxPutX( pb_query_order_second_state, 97 ),
	UxPutY( pb_query_order_second_state, 200 ),
	UxCreateWidget( pb_query_order_second_state );


	/* Creation of pb_query_order_second_age */
	pb_query_order_second_age = UxCreatePushButton( "pb_query_order_second_age", rc_query_order_second );
	UxPutLabelString( pb_query_order_second_age, "Age (Days)" ),
	UxPutX( pb_query_order_second_age, 97 ),
	UxPutY( pb_query_order_second_age, 222 ),
	UxCreateWidget( pb_query_order_second_age );


	/* Creation of om_query_order_second */
	om_query_order_second = UxCreateRowColumn( "om_query_order_second", bulletinBoard1 );
	UxPutRowColumnType( om_query_order_second, "menu_option" ),
	UxPutX( om_query_order_second, 11 ),
	UxPutY( om_query_order_second, 90 ),
	UxPutWidth( om_query_order_second, 60 ),
	UxPutHeight( om_query_order_second, 54 ),
	UxPutLabelString( om_query_order_second, "Second" ),
	UxPutSensitive( om_query_order_second, "false" ),
	UxPutSubMenuId( om_query_order_second, "rc_query_order_second" ),
	UxCreateWidget( om_query_order_second );


	/* Creation of rc_query_order_third */
	rc_query_order_third = UxCreateRowColumn( "rc_query_order_third", bulletinBoard1 );
	UxPutRowColumnType( rc_query_order_third, "menu_pulldown" ),
	UxPutSensitive( rc_query_order_third, "false" ),
	UxPutX( rc_query_order_third, 93 ),
	UxPutY( rc_query_order_third, 0 ),
	UxCreateWidget( rc_query_order_third );


	/* Creation of pb_query_order_third_none */
	pb_query_order_third_none = UxCreatePushButton( "pb_query_order_third_none", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_none, "None" ),
	UxPutSensitive( pb_query_order_third_none, "false" ),
	UxPutX( pb_query_order_third_none, 97 ),
	UxPutY( pb_query_order_third_none, 2 ),
	UxCreateWidget( pb_query_order_third_none );


	/* Creation of pb_query_order_third_order_type */
	pb_query_order_third_order_type = UxCreatePushButton( "pb_query_order_third_order_type", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_order_type, "Order Type" ),
	UxPutX( pb_query_order_third_order_type, 97 ),
	UxPutY( pb_query_order_third_order_type, 24 ),
	UxCreateWidget( pb_query_order_third_order_type );


	/* Creation of pb_query_order_third_priority */
	pb_query_order_third_priority = UxCreatePushButton( "pb_query_order_third_priority", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_priority, "Priority" ),
	UxPutX( pb_query_order_third_priority, 97 ),
	UxPutY( pb_query_order_third_priority, 46 ),
	UxCreateWidget( pb_query_order_third_priority );


	/* Creation of pb_query_order_third_media_id */
	pb_query_order_third_media_id = UxCreatePushButton( "pb_query_order_third_media_id", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_media_id, "Media ID" ),
	UxPutX( pb_query_order_third_media_id, 97 ),
	UxPutY( pb_query_order_third_media_id, 68 ),
	UxCreateWidget( pb_query_order_third_media_id );


	/* Creation of pb_query_order_third_mode */
	pb_query_order_third_mode = UxCreatePushButton( "pb_query_order_third_mode", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_mode, "Mode" ),
	UxPutX( pb_query_order_third_mode, 97 ),
	UxPutY( pb_query_order_third_mode, 90 ),
	UxCreateWidget( pb_query_order_third_mode );


	/* Creation of pb_query_order_third_sat_sens_rev */
	pb_query_order_third_sat_sens_rev = UxCreatePushButton( "pb_query_order_third_sat_sens_rev", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_query_order_third_sat_sens_rev, 97 ),
	UxPutY( pb_query_order_third_sat_sens_rev, 112 ),
	UxCreateWidget( pb_query_order_third_sat_sens_rev );


	/* Creation of pb_query_order_third_frame_subframe */
	pb_query_order_third_frame_subframe = UxCreatePushButton( "pb_query_order_third_frame_subframe", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_frame_subframe, "Frame ID" ),
	UxPutX( pb_query_order_third_frame_subframe, 97 ),
	UxPutY( pb_query_order_third_frame_subframe, 134 ),
	UxCreateWidget( pb_query_order_third_frame_subframe );


	/* Creation of pb_query_order_third_job_id */
	pb_query_order_third_job_id = UxCreatePushButton( "pb_query_order_third_job_id", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_job_id, "Job ID" ),
	UxPutX( pb_query_order_third_job_id, 97 ),
	UxPutY( pb_query_order_third_job_id, 156 ),
	UxCreateWidget( pb_query_order_third_job_id );


	/* Creation of pb_query_order_third_order_item */
	pb_query_order_third_order_item = UxCreatePushButton( "pb_query_order_third_order_item", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_order_item, "Order & Item ID" ),
	UxPutX( pb_query_order_third_order_item, 97 ),
	UxPutY( pb_query_order_third_order_item, 178 ),
	UxCreateWidget( pb_query_order_third_order_item );


	/* Creation of pb_query_order_third_state */
	pb_query_order_third_state = UxCreatePushButton( "pb_query_order_third_state", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_state, "State" ),
	UxPutX( pb_query_order_third_state, 97 ),
	UxPutY( pb_query_order_third_state, 200 ),
	UxCreateWidget( pb_query_order_third_state );


	/* Creation of pb_query_order_third_age */
	pb_query_order_third_age = UxCreatePushButton( "pb_query_order_third_age", rc_query_order_third );
	UxPutLabelString( pb_query_order_third_age, "Age (Days)" ),
	UxPutX( pb_query_order_third_age, 97 ),
	UxPutY( pb_query_order_third_age, 222 ),
	UxCreateWidget( pb_query_order_third_age );


	/* Creation of om_query_order_third */
	om_query_order_third = UxCreateRowColumn( "om_query_order_third", bulletinBoard1 );
	UxPutRowColumnType( om_query_order_third, "menu_option" ),
	UxPutX( om_query_order_third, 11 ),
	UxPutY( om_query_order_third, 143 ),
	UxPutWidth( om_query_order_third, 60 ),
	UxPutHeight( om_query_order_third, 54 ),
	UxPutLabelString( om_query_order_third, "Third    " ),
	UxPutSensitive( om_query_order_third, "false" ),
	UxPutSubMenuId( om_query_order_third, "rc_query_order_third" ),
	UxCreateWidget( om_query_order_third );


	/* Creation of rc_query_order_fourth */
	rc_query_order_fourth = UxCreateRowColumn( "rc_query_order_fourth", bulletinBoard1 );
	UxPutRowColumnType( rc_query_order_fourth, "menu_pulldown" ),
	UxPutSensitive( rc_query_order_fourth, "false" ),
	UxPutX( rc_query_order_fourth, 93 ),
	UxPutY( rc_query_order_fourth, 0 ),
	UxCreateWidget( rc_query_order_fourth );


	/* Creation of pb_query_order_fourth_none */
	pb_query_order_fourth_none = UxCreatePushButton( "pb_query_order_fourth_none", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_none, "None" ),
	UxPutSensitive( pb_query_order_fourth_none, "false" ),
	UxPutX( pb_query_order_fourth_none, 97 ),
	UxPutY( pb_query_order_fourth_none, 2 ),
	UxCreateWidget( pb_query_order_fourth_none );


	/* Creation of pb_query_order_fourth_order_type */
	pb_query_order_fourth_order_type = UxCreatePushButton( "pb_query_order_fourth_order_type", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_order_type, "Order Type" ),
	UxPutX( pb_query_order_fourth_order_type, 97 ),
	UxPutY( pb_query_order_fourth_order_type, 24 ),
	UxCreateWidget( pb_query_order_fourth_order_type );


	/* Creation of pb_query_order_fourth_priority */
	pb_query_order_fourth_priority = UxCreatePushButton( "pb_query_order_fourth_priority", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_priority, "Priority" ),
	UxPutX( pb_query_order_fourth_priority, 97 ),
	UxPutY( pb_query_order_fourth_priority, 46 ),
	UxCreateWidget( pb_query_order_fourth_priority );


	/* Creation of pb_query_order_fourth_media_id */
	pb_query_order_fourth_media_id = UxCreatePushButton( "pb_query_order_fourth_media_id", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_media_id, "Media ID" ),
	UxPutX( pb_query_order_fourth_media_id, 97 ),
	UxPutY( pb_query_order_fourth_media_id, 68 ),
	UxCreateWidget( pb_query_order_fourth_media_id );


	/* Creation of pb_query_order_fourth_mode */
	pb_query_order_fourth_mode = UxCreatePushButton( "pb_query_order_fourth_mode", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_mode, "Mode" ),
	UxPutX( pb_query_order_fourth_mode, 97 ),
	UxPutY( pb_query_order_fourth_mode, 90 ),
	UxCreateWidget( pb_query_order_fourth_mode );


	/* Creation of pb_query_order_fourth_sat_sens_rev */
	pb_query_order_fourth_sat_sens_rev = UxCreatePushButton( "pb_query_order_fourth_sat_sens_rev", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_query_order_fourth_sat_sens_rev, 97 ),
	UxPutY( pb_query_order_fourth_sat_sens_rev, 112 ),
	UxCreateWidget( pb_query_order_fourth_sat_sens_rev );


	/* Creation of pb_query_order_fourth_frame_subframe */
	pb_query_order_fourth_frame_subframe = UxCreatePushButton( "pb_query_order_fourth_frame_subframe", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_frame_subframe, "Frame ID" ),
	UxPutX( pb_query_order_fourth_frame_subframe, 97 ),
	UxPutY( pb_query_order_fourth_frame_subframe, 134 ),
	UxCreateWidget( pb_query_order_fourth_frame_subframe );


	/* Creation of pb_query_order_fourth_job_id */
	pb_query_order_fourth_job_id = UxCreatePushButton( "pb_query_order_fourth_job_id", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_job_id, "Job ID" ),
	UxPutX( pb_query_order_fourth_job_id, 97 ),
	UxPutY( pb_query_order_fourth_job_id, 156 ),
	UxCreateWidget( pb_query_order_fourth_job_id );


	/* Creation of pb_query_order_fourth_order_item */
	pb_query_order_fourth_order_item = UxCreatePushButton( "pb_query_order_fourth_order_item", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_order_item, "Order & Item ID" ),
	UxPutX( pb_query_order_fourth_order_item, 97 ),
	UxPutY( pb_query_order_fourth_order_item, 178 ),
	UxCreateWidget( pb_query_order_fourth_order_item );


	/* Creation of pb_query_order_fourth_state */
	pb_query_order_fourth_state = UxCreatePushButton( "pb_query_order_fourth_state", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_state, "State" ),
	UxPutX( pb_query_order_fourth_state, 97 ),
	UxPutY( pb_query_order_fourth_state, 200 ),
	UxCreateWidget( pb_query_order_fourth_state );


	/* Creation of pb_query_order_fourth_age */
	pb_query_order_fourth_age = UxCreatePushButton( "pb_query_order_fourth_age", rc_query_order_fourth );
	UxPutLabelString( pb_query_order_fourth_age, "Age (Days)" ),
	UxPutX( pb_query_order_fourth_age, 97 ),
	UxPutY( pb_query_order_fourth_age, 222 ),
	UxCreateWidget( pb_query_order_fourth_age );


	/* Creation of om_query_order_fourth */
	om_query_order_fourth = UxCreateRowColumn( "om_query_order_fourth", bulletinBoard1 );
	UxPutRowColumnType( om_query_order_fourth, "menu_option" ),
	UxPutX( om_query_order_fourth, 11 ),
	UxPutY( om_query_order_fourth, 198 ),
	UxPutWidth( om_query_order_fourth, 60 ),
	UxPutHeight( om_query_order_fourth, 54 ),
	UxPutLabelString( om_query_order_fourth, "Fourth  " ),
	UxPutSensitive( om_query_order_fourth, "false" ),
	UxPutSubMenuId( om_query_order_fourth, "rc_query_order_fourth" ),
	UxCreateWidget( om_query_order_fourth );


	/* Creation of l_query_order_by */
	l_query_order_by = UxCreateLabel( "l_query_order_by", pps_query );
	UxPutX( l_query_order_by, 891 ),
	UxPutY( l_query_order_by, 42 ),
	UxPutWidth( l_query_order_by, 71 ),
	UxPutHeight( l_query_order_by, 27 ),
	UxPutLabelString( l_query_order_by, "Order By" ),
	UxCreateWidget( l_query_order_by );

	UxDelayUpdate( pps_query );
	UxPutDefaultButton( pps_query, "pb_query_query" ),
	UxUpdate( pps_query );
	UxAddCallback( pps_query, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPps_queryContext);



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( pps_query );

	return ( pps_query );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_pps_query( swidget _UxUxParent )
{
	swidget                 rtrn;
	_UxCpps_query           *UxContext;
	static int		_Uxinit = 0;

	UxPps_queryContext = UxContext =
		(_UxCpps_query *) UxNewContext( sizeof(_UxCpps_query), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		(void)sprintf(query_labelPixmapString, "%s/%s/bearhd0.xpm",
		rootPath, PPS_PIXMAP_SUBPATH);
		rtrn = _Uxbuild_pps_query();

		if ( ! IsAuthorizedUser)
			XtSetSensitive(UxGetWidget(mb_query_top_b1), False);
		
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/



/*******************************************************************************
	pps_plan.c

       Associated Header file: pps_plan.h
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

extern swidget nojoy;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "pps_plan.h"
#undef CONTEXT_MACRO_ACCESS

swidget	pps_plan;
swidget	tb_plan_L1_Orders;
swidget	tb_plan_L1_QLK;
swidget	tb_plan_Scan_Orders;
swidget	tb_plan_Scan_QLK;
swidget	rc_plan_sat;
swidget	pb_plan_sat_any;
swidget	pb_plan_sat_e1;
swidget	pb_plan_sat_e2;
swidget	pb_plan_sat_j1;
swidget	pb_plan_sat_r1;
swidget	om_plan_sat;
swidget	rc_plan_sens;
swidget	pb_plan_sens_any;
swidget	pb_plan_sens_s;
swidget	om_plan_sens;
swidget	rc_plan_activity;
swidget	pb_plan_activity_any;
swidget	pb_plan_activity_rlt;
swidget	pb_plan_activity_dmp;
swidget	om_plan_activity;
swidget	rc_plan_station;
swidget	pb_plan_station_any;
swidget	pb_plan_station_fa;
swidget	pb_plan_station_mc;
swidget	om_plan_station;
swidget	rc_plan_priority;
swidget	pb_plan_priority_any;
swidget	pb_plan_priority_low;
swidget	pb_plan_priority_routine;
swidget	pb_plan_priority_high;
swidget	pb_plan_priority_urgent;
swidget	om_plan_priority;
swidget	l_plan_rev;
swidget	tf_plan_rev;
swidget	l_plan_seq;
swidget	tf_plan_seq;
swidget	l_plan_job_id;
swidget	tf_plan_job_id;
swidget	l_plan_order_id;
swidget	tf_plan_order_id;
swidget	l_plan_item_id;
swidget	tf_plan_item_id;
swidget	l_plan_frame_id;
swidget	tf_plan_frame_id;
swidget	l_plan_subframe_id;
swidget	tf_plan_subframe_id;
swidget	rc_plan_product_type;
swidget	pb_plan_product_type_any;
swidget	pb_plan_product_type_standard;
swidget	pb_plan_product_type_complex;
swidget	pb_plan_product_type_ccsd;
swidget	om_plan_product_type;
swidget	rc_plan_media_type;
swidget	pb_plan_media_type_any;
swidget	pb_plan_media_type_disk;
swidget	pb_plan_media_type_dcrsi;
swidget	om_plan_media_type;
swidget	l_plan_media_id;
swidget	tf_plan_media_id;
swidget	pb_plan_query;
swidget	rc_plan_processor_mode;
swidget	pb_plan_processor_mode_any;
swidget	pb_plan_processor_mode_continuous;
swidget	pb_plan_processor_mode_scansar;
swidget	om_plan_processor_mode;
swidget	rc_plan_data_direction;
swidget	pb_plan_data_direction_any;
swidget	pb_plan_data_direction_forward;
swidget	pb_plan_data_direction_reverse;
swidget	pb_plan_data_direction_unknown;
swidget	om_plan_data_direction;
swidget	sw_plan_ready_list;
swidget	sw_plan_available_list;
swidget	rc_plan_order_first;
swidget	pb_plan_order_first_none;
swidget	pb_plan_order_first_order_type;
swidget	pb_plan_order_first_priority;
swidget	pb_plan_order_first_sat_sens_rev;
swidget	pb_plan_order_first_mode;
swidget	pb_plan_order_first_pixel_spacing;
swidget	pb_plan_order_first_media_id;
swidget	pb_plan_order_first_job_id;
swidget	pb_plan_order_first_insert_top;
swidget	om_plan_order_first;
swidget	rc_plan_order_second;
swidget	pb_plan_order_second_none;
swidget	pb_plan_order_second_order_type;
swidget	pb_plan_order_second_priority;
swidget	pb_plan_order_second_sat_sens_rev;
swidget	pb_plan_order_second_mode;
swidget	pb_plan_order_second_pixel_spacing;
swidget	pb_plan_order_second_media_id;
swidget	pb_plan_order_second_job_id;
swidget	pb_plan_order_second_insert_top;
swidget	om_plan_order_second;
swidget	rc_plan_order_third;
swidget	pb_plan_order_third_none;
swidget	pb_plan_order_third_order_type;
swidget	pb_plan_order_third_priority;
swidget	pb_plan_order_third_sat_sens_rev;
swidget	pb_plan_order_third_mode;
swidget	pb_plan_order_third_pixel_spacing;
swidget	pb_plan_order_third_media_id;
swidget	pb_plan_order_third_job_id;
swidget	pb_plan_order_third_insert_top;
swidget	om_plan_order_third;
swidget	rc_plan_order_fourth;
swidget	pb_plan_order_fourth_none;
swidget	pb_plan_order_fourth_order_type;
swidget	pb_plan_order_fourth_priority;
swidget	pb_plan_order_fourth_sat_sens_rev;
swidget	pb_plan_order_fourth_mode;
swidget	pb_plan_order_fourth_pixel_spacing;
swidget	pb_plan_order_fourth_media_id;
swidget	pb_plan_order_fourth_job_id;
swidget	pb_plan_order_fourth_insert_top;
swidget	om_plan_order_fourth;
swidget	l_plan_ready_num_displayed;
swidget	l_plan_ready_num_selected;
swidget	l_plan_available_num_displayed;
swidget	l_plan_available_num_selected;
swidget	pb_plan_ready_select_all;
swidget	pb_plan_ready_deselect_all;
swidget	pb_plan_available_select_all;
swidget	pb_plan_available_deselect_all;
swidget	pb_plan_available_refresh;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pb_plan_print_results(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	{
	cb_plan_print_results();
	}
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_print_screen(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	{
	extern void pps_print_screen(swidget sw);
	
	pps_print_screen(pps_plan);
	
	}
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_exit(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	UxPopdownInterface(pps_plan);
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_load_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_load_query();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_save_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_save_query();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_save_query_as(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_save_query_as();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_clear_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_clear_query();
	UxPps_planContext = UxSaveCtx;
}

static void  valueChangedCB_tb_plan_L1_Orders(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	do_plan_new_order_type();
	UxPps_planContext = UxSaveCtx;
}

static void  valueChangedCB_tb_plan_L1_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	do_plan_new_order_type();
	UxPps_planContext = UxSaveCtx;
}

static void  valueChangedCB_tb_plan_Scan_Orders(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	do_plan_new_order_type();
	UxPps_planContext = UxSaveCtx;
}

static void  valueChangedCB_tb_plan_Scan_QLK(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	do_plan_new_order_type();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_query(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	{
	do_plan_ready_query();
	}
	UxPps_planContext = UxSaveCtx;
}

static void  extendedSelectionCB_sw_plan_ready_list(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	{
	extern cb_plan_ready_extsel(XmListCallbackStruct *cbs);
	extern cb_plan_available_extsel(XmListCallbackStruct *cbs);
	extern swidget sw_plan_ready_list;
	
	if (UxThisWidget == sw_plan_ready_list)
		cb_plan_ready_extsel((XmListCallbackStruct *)UxCallbackArg);
	else
		cb_plan_available_extsel((XmListCallbackStruct *)UxCallbackArg);
	
	}
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_ready_top(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_ready_top();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_ready_IT_on(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_ready_IT_on();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_ready_IT_off(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_ready_IT_off();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_ready_bottom(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_ready_bottom();
	UxPps_planContext = UxSaveCtx;
}

static void  extendedSelectionCB_sw_plan_available_list(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	{
	extern cb_plan_ready_extsel(XmListCallbackStruct *cbs);
	extern cb_plan_available_extsel(XmListCallbackStruct *cbs);
	extern swidget sw_plan_ready_list;
	
	if (UxThisWidget == sw_plan_ready_list)
		cb_plan_ready_extsel((XmListCallbackStruct *)UxCallbackArg);
	else
		cb_plan_available_extsel((XmListCallbackStruct *)UxCallbackArg);
	
	}
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_top(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_top();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_IT_on(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_IT_on();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_IT_off(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_IT_off();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_remove(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_remove();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_bottom(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_bottom();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_ready_select_all(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_ready_select_all();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_ready_deselect_all(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_ready_deselect_all();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_select_all(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_select_all();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_deselect_all(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	cb_plan_available_deselect_all();
	UxPps_planContext = UxSaveCtx;
}

static void  activateCB_pb_plan_available_refresh(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCpps_plan            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	swidget			UxThisWidget;

	UxThisWidget = UxWidgetToSwidget( UxWidget );
	UxSaveCtx = UxPps_planContext;
	UxPps_planContext = UxContext =
			(_UxCpps_plan *) UxGetContext( UxThisWidget );
	do_plan_available_query();
	UxPps_planContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the swidgets and X widgets,
       and sets their properties to the values specified in the
       Property Editor.
*******************************************************************************/

static swidget	_Uxbuild_pps_plan()
{
	/* Create the swidgets */


	/* Creation of pps_plan */
	pps_plan = UxCreateForm( "pps_plan", UxParent );
	UxPutContext( pps_plan, UxPps_planContext );
	UxPutClassCode( pps_plan, _UxIfClassId );
	UxPutDefaultShell( pps_plan, "transientShell" );

	UxPutWidth( pps_plan, 1145 ),
	UxPutHeight( pps_plan, 866 ),
	UxPutResizePolicy( pps_plan, "resize_none" ),
	UxPutX( pps_plan, 0 ),
	UxPutY( pps_plan, 0 ),
	UxPutUnitType( pps_plan, "pixels" ),
	UxCreateWidget( pps_plan );


	/* Creation of mb_plan */
	mb_plan = UxCreateRowColumn( "mb_plan", pps_plan );
	UxPutRowColumnType( mb_plan, "menu_bar" ),
	UxPutX( mb_plan, 0 ),
	UxPutY( mb_plan, 0 ),
	UxPutWidth( mb_plan, 504 ),
	UxPutHeight( mb_plan, 36 ),
	UxPutMenuAccelerator( mb_plan, "<KeyUp>F10" ),
	UxPutSpacing( mb_plan, 20 ),
	UxPutRightAttachment( mb_plan, "attach_form" ),
	UxPutLeftAttachment( mb_plan, "attach_form" ),
	UxCreateWidget( mb_plan );


	/* Creation of pb_plan_file */
	pb_plan_file = UxCreateRowColumn( "pb_plan_file", mb_plan );
	UxPutRowColumnType( pb_plan_file, "menu_pulldown" ),
	UxCreateWidget( pb_plan_file );


	/* Creation of pb_plan_print_results */
	pb_plan_print_results = UxCreatePushButton( "pb_plan_print_results", pb_plan_file );
	UxPutLabelString( pb_plan_print_results, "Print Results..." ),
	UxPutMnemonic( pb_plan_print_results, "R" ),
	UxCreateWidget( pb_plan_print_results );

	UxAddCallback( pb_plan_print_results, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_print_results,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_print_screen */
	pb_plan_print_screen = UxCreatePushButton( "pb_plan_print_screen", pb_plan_file );
	UxPutLabelString( pb_plan_print_screen, "Print Screen" ),
	UxPutMnemonic( pb_plan_print_screen, "P" ),
	UxCreateWidget( pb_plan_print_screen );

	UxAddCallback( pb_plan_print_screen, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_print_screen,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_exit */
	pb_plan_exit = UxCreatePushButton( "pb_plan_exit", pb_plan_file );
	UxPutLabelString( pb_plan_exit, "Exit" ),
	UxPutMnemonic( pb_plan_exit, "x" ),
	UxCreateWidget( pb_plan_exit );

	UxAddCallback( pb_plan_exit, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_exit,
		(XtPointer) UxPps_planContext );


	/* Creation of mb_plan_file */
	mb_plan_file = UxCreateCascadeButton( "mb_plan_file", mb_plan );
	UxPutLabelString( mb_plan_file, "File" ),
	UxPutMnemonic( mb_plan_file, "F" ),
	UxPutSubMenuId( mb_plan_file, "pb_plan_file" ),
	UxCreateWidget( mb_plan_file );


	/* Creation of pb_plan_query_setting */
	pb_plan_query_setting = UxCreateRowColumn( "pb_plan_query_setting", mb_plan );
	UxPutRowColumnType( pb_plan_query_setting, "menu_pulldown" ),
	UxPutX( pb_plan_query_setting, 150 ),
	UxCreateWidget( pb_plan_query_setting );


	/* Creation of pb_plan_load_query */
	pb_plan_load_query = UxCreatePushButton( "pb_plan_load_query", pb_plan_query_setting );
	UxPutLabelString( pb_plan_load_query, "Load Query..." ),
	UxPutMnemonic( pb_plan_load_query, "L" ),
	UxCreateWidget( pb_plan_load_query );

	UxAddCallback( pb_plan_load_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_load_query,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_save_query */
	pb_plan_save_query = UxCreatePushButton( "pb_plan_save_query", pb_plan_query_setting );
	UxPutLabelString( pb_plan_save_query, "Save Query" ),
	UxPutMnemonic( pb_plan_save_query, "S" ),
	UxCreateWidget( pb_plan_save_query );

	UxAddCallback( pb_plan_save_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_save_query,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_save_query_as */
	pb_plan_save_query_as = UxCreatePushButton( "pb_plan_save_query_as", pb_plan_query_setting );
	UxPutLabelString( pb_plan_save_query_as, "Save Query As..." ),
	UxPutMnemonic( pb_plan_save_query_as, "A" ),
	UxCreateWidget( pb_plan_save_query_as );

	UxAddCallback( pb_plan_save_query_as, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_save_query_as,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_clear_query */
	pb_plan_clear_query = UxCreatePushButton( "pb_plan_clear_query", pb_plan_query_setting );
	UxPutLabelString( pb_plan_clear_query, "Clear Query" ),
	UxPutMnemonic( pb_plan_clear_query, "C" ),
	UxCreateWidget( pb_plan_clear_query );

	UxAddCallback( pb_plan_clear_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_clear_query,
		(XtPointer) UxPps_planContext );


	/* Creation of mb_plan_top_b1 */
	mb_plan_top_b1 = UxCreateCascadeButton( "mb_plan_top_b1", mb_plan );
	UxPutLabelString( mb_plan_top_b1, "QuerySetting" ),
	UxPutMnemonic( mb_plan_top_b1, "Q" ),
	UxPutX( mb_plan_top_b1, 100 ),
	UxPutSubMenuId( mb_plan_top_b1, "pb_plan_query_setting" ),
	UxCreateWidget( mb_plan_top_b1 );


	/* Creation of f_plan_query_settings */
	f_plan_query_settings = UxCreateFrame( "f_plan_query_settings", pps_plan );
	UxPutWidth( f_plan_query_settings, 860 ),
	UxPutHeight( f_plan_query_settings, 233 ),
	UxPutX( f_plan_query_settings, 12 ),
	UxPutY( f_plan_query_settings, 64 ),
	UxCreateWidget( f_plan_query_settings );


	/* Creation of bb_plan_query_settings */
	bb_plan_query_settings = UxCreateBulletinBoard( "bb_plan_query_settings", f_plan_query_settings );
	UxPutResizePolicy( bb_plan_query_settings, "resize_none" ),
	UxPutWidth( bb_plan_query_settings, 847 ),
	UxPutHeight( bb_plan_query_settings, 228 ),
	UxPutX( bb_plan_query_settings, 5 ),
	UxPutY( bb_plan_query_settings, 1 ),
	UxCreateWidget( bb_plan_query_settings );


	/* Creation of tb_plan_L1_Orders */
	tb_plan_L1_Orders = UxCreateToggleButton( "tb_plan_L1_Orders", bb_plan_query_settings );
	UxPutX( tb_plan_L1_Orders, 16 ),
	UxPutY( tb_plan_L1_Orders, 10 ),
	UxPutWidth( tb_plan_L1_Orders, 94 ),
	UxPutHeight( tb_plan_L1_Orders, 26 ),
	UxPutLabelString( tb_plan_L1_Orders, "L1 Orders" ),
	UxPutSensitive( tb_plan_L1_Orders, "true" ),
	UxCreateWidget( tb_plan_L1_Orders );

	UxAddCallback( tb_plan_L1_Orders, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_plan_L1_Orders,
		(XtPointer) UxPps_planContext );


	/* Creation of tb_plan_L1_QLK */
	tb_plan_L1_QLK = UxCreateToggleButton( "tb_plan_L1_QLK", bb_plan_query_settings );
	UxPutX( tb_plan_L1_QLK, 160 ),
	UxPutY( tb_plan_L1_QLK, 10 ),
	UxPutWidth( tb_plan_L1_QLK, 73 ),
	UxPutHeight( tb_plan_L1_QLK, 26 ),
	UxPutLabelString( tb_plan_L1_QLK, "L1 QLK" ),
	UxPutSensitive( tb_plan_L1_QLK, "true" ),
	UxCreateWidget( tb_plan_L1_QLK );

	UxAddCallback( tb_plan_L1_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_plan_L1_QLK,
		(XtPointer) UxPps_planContext );


	/* Creation of tb_plan_Scan_Orders */
	tb_plan_Scan_Orders = UxCreateToggleButton( "tb_plan_Scan_Orders", bb_plan_query_settings );
	UxPutX( tb_plan_Scan_Orders, 280 ),
	UxPutY( tb_plan_Scan_Orders, 10 ),
	UxPutWidth( tb_plan_Scan_Orders, 109 ),
	UxPutHeight( tb_plan_Scan_Orders, 26 ),
	UxPutLabelString( tb_plan_Scan_Orders, "Scan Orders" ),
	UxCreateWidget( tb_plan_Scan_Orders );

	UxAddCallback( tb_plan_Scan_Orders, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_plan_Scan_Orders,
		(XtPointer) UxPps_planContext );


	/* Creation of tb_plan_Scan_QLK */
	tb_plan_Scan_QLK = UxCreateToggleButton( "tb_plan_Scan_QLK", bb_plan_query_settings );
	UxPutX( tb_plan_Scan_QLK, 440 ),
	UxPutY( tb_plan_Scan_QLK, 10 ),
	UxPutWidth( tb_plan_Scan_QLK, 88 ),
	UxPutHeight( tb_plan_Scan_QLK, 26 ),
	UxPutLabelString( tb_plan_Scan_QLK, "Scan QLK" ),
	UxCreateWidget( tb_plan_Scan_QLK );

	UxAddCallback( tb_plan_Scan_QLK, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_tb_plan_Scan_QLK,
		(XtPointer) UxPps_planContext );


	/* Creation of rc_plan_sat */
	rc_plan_sat = UxCreateRowColumn( "rc_plan_sat", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_sat, "menu_pulldown" ),
	UxPutSensitive( rc_plan_sat, "false" ),
	UxCreateWidget( rc_plan_sat );


	/* Creation of pb_plan_sat_any */
	pb_plan_sat_any = UxCreatePushButton( "pb_plan_sat_any", rc_plan_sat );
	UxPutLabelString( pb_plan_sat_any, "Any" ),
	UxPutSensitive( pb_plan_sat_any, "false" ),
	UxCreateWidget( pb_plan_sat_any );


	/* Creation of pb_plan_sat_e1 */
	pb_plan_sat_e1 = UxCreatePushButton( "pb_plan_sat_e1", rc_plan_sat );
	UxPutLabelString( pb_plan_sat_e1, "E1" ),
	UxPutSensitive( pb_plan_sat_e1, "false" ),
	UxCreateWidget( pb_plan_sat_e1 );


	/* Creation of pb_plan_sat_e2 */
	pb_plan_sat_e2 = UxCreatePushButton( "pb_plan_sat_e2", rc_plan_sat );
	UxPutLabelString( pb_plan_sat_e2, "E2" ),
	UxPutSensitive( pb_plan_sat_e2, "false" ),
	UxCreateWidget( pb_plan_sat_e2 );


	/* Creation of pb_plan_sat_j1 */
	pb_plan_sat_j1 = UxCreatePushButton( "pb_plan_sat_j1", rc_plan_sat );
	UxPutLabelString( pb_plan_sat_j1, "J1" ),
	UxPutSensitive( pb_plan_sat_j1, "false" ),
	UxCreateWidget( pb_plan_sat_j1 );


	/* Creation of pb_plan_sat_r1 */
	pb_plan_sat_r1 = UxCreatePushButton( "pb_plan_sat_r1", rc_plan_sat );
	UxPutLabelString( pb_plan_sat_r1, "R1" ),
	UxPutSensitive( pb_plan_sat_r1, "false" ),
	UxCreateWidget( pb_plan_sat_r1 );


	/* Creation of om_plan_sat */
	om_plan_sat = UxCreateRowColumn( "om_plan_sat", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_sat, "menu_option" ),
	UxPutX( om_plan_sat, 15 ),
	UxPutY( om_plan_sat, 44 ),
	UxPutWidth( om_plan_sat, 48 ),
	UxPutHeight( om_plan_sat, 60 ),
	UxPutLabelString( om_plan_sat, "Sat" ),
	UxPutSensitive( om_plan_sat, "false" ),
	UxPutSubMenuId( om_plan_sat, "rc_plan_sat" ),
	UxCreateWidget( om_plan_sat );


	/* Creation of rc_plan_sens */
	rc_plan_sens = UxCreateRowColumn( "rc_plan_sens", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_sens, "menu_pulldown" ),
	UxPutSensitive( rc_plan_sens, "false" ),
	UxCreateWidget( rc_plan_sens );


	/* Creation of pb_plan_sens_any */
	pb_plan_sens_any = UxCreatePushButton( "pb_plan_sens_any", rc_plan_sens );
	UxPutLabelString( pb_plan_sens_any, "Any" ),
	UxPutSensitive( pb_plan_sens_any, "false" ),
	UxCreateWidget( pb_plan_sens_any );


	/* Creation of pb_plan_sens_s */
	pb_plan_sens_s = UxCreatePushButton( "pb_plan_sens_s", rc_plan_sens );
	UxPutLabelString( pb_plan_sens_s, "S" ),
	UxPutSensitive( pb_plan_sens_s, "false" ),
	UxCreateWidget( pb_plan_sens_s );


	/* Creation of rc_plan_sens_o */
	rc_plan_sens_o = UxCreatePushButton( "rc_plan_sens_o", rc_plan_sens );
	UxPutLabelString( rc_plan_sens_o, "O" ),
	UxCreateWidget( rc_plan_sens_o );


	/* Creation of rc_plan_sens_v */
	rc_plan_sens_v = UxCreatePushButton( "rc_plan_sens_v", rc_plan_sens );
	UxPutLabelString( rc_plan_sens_v, "V" ),
	UxCreateWidget( rc_plan_sens_v );


	/* Creation of rc_plan_sens_z */
	rc_plan_sens_z = UxCreatePushButton( "rc_plan_sens_z", rc_plan_sens );
	UxPutLabelString( rc_plan_sens_z, "Z" ),
	UxCreateWidget( rc_plan_sens_z );


	/* Creation of om_plan_sens */
	om_plan_sens = UxCreateRowColumn( "om_plan_sens", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_sens, "menu_option" ),
	UxPutX( om_plan_sens, 120 ),
	UxPutY( om_plan_sens, 45 ),
	UxPutWidth( om_plan_sens, 60 ),
	UxPutHeight( om_plan_sens, 54 ),
	UxPutLabelString( om_plan_sens, "Sens" ),
	UxPutSensitive( om_plan_sens, "false" ),
	UxPutSubMenuId( om_plan_sens, "rc_plan_sens" ),
	UxCreateWidget( om_plan_sens );


	/* Creation of rc_plan_activity */
	rc_plan_activity = UxCreateRowColumn( "rc_plan_activity", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_activity, "menu_pulldown" ),
	UxPutSensitive( rc_plan_activity, "false" ),
	UxCreateWidget( rc_plan_activity );


	/* Creation of pb_plan_activity_any */
	pb_plan_activity_any = UxCreatePushButton( "pb_plan_activity_any", rc_plan_activity );
	UxPutLabelString( pb_plan_activity_any, "Any" ),
	UxPutSensitive( pb_plan_activity_any, "false" ),
	UxCreateWidget( pb_plan_activity_any );


	/* Creation of pb_plan_activity_rlt */
	pb_plan_activity_rlt = UxCreatePushButton( "pb_plan_activity_rlt", rc_plan_activity );
	UxPutLabelString( pb_plan_activity_rlt, "RLT" ),
	UxPutSensitive( pb_plan_activity_rlt, "false" ),
	UxCreateWidget( pb_plan_activity_rlt );


	/* Creation of pb_plan_activity_dmp */
	pb_plan_activity_dmp = UxCreatePushButton( "pb_plan_activity_dmp", rc_plan_activity );
	UxPutLabelString( pb_plan_activity_dmp, "DMP" ),
	UxPutSensitive( pb_plan_activity_dmp, "false" ),
	UxCreateWidget( pb_plan_activity_dmp );


	/* Creation of om_plan_activity */
	om_plan_activity = UxCreateRowColumn( "om_plan_activity", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_activity, "menu_option" ),
	UxPutX( om_plan_activity, 490 ),
	UxPutY( om_plan_activity, 46 ),
	UxPutWidth( om_plan_activity, 60 ),
	UxPutHeight( om_plan_activity, 54 ),
	UxPutLabelString( om_plan_activity, "Activity" ),
	UxPutSensitive( om_plan_activity, "false" ),
	UxPutSubMenuId( om_plan_activity, "rc_plan_activity" ),
	UxCreateWidget( om_plan_activity );


	/* Creation of rc_plan_station */
	rc_plan_station = UxCreateRowColumn( "rc_plan_station", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_station, "menu_pulldown" ),
	UxPutSensitive( rc_plan_station, "false" ),
	UxCreateWidget( rc_plan_station );


	/* Creation of pb_plan_station_any */
	pb_plan_station_any = UxCreatePushButton( "pb_plan_station_any", rc_plan_station );
	UxPutLabelString( pb_plan_station_any, "Any" ),
	UxPutSensitive( pb_plan_station_any, "false" ),
	UxCreateWidget( pb_plan_station_any );


	/* Creation of pb_plan_station_fa */
	pb_plan_station_fa = UxCreatePushButton( "pb_plan_station_fa", rc_plan_station );
	UxPutLabelString( pb_plan_station_fa, "FA" ),
	UxPutSensitive( pb_plan_station_fa, "false" ),
	UxCreateWidget( pb_plan_station_fa );


	/* Creation of pb_plan_station_mc */
	pb_plan_station_mc = UxCreatePushButton( "pb_plan_station_mc", rc_plan_station );
	UxPutLabelString( pb_plan_station_mc, "MC" ),
	UxPutSensitive( pb_plan_station_mc, "false" ),
	UxCreateWidget( pb_plan_station_mc );


	/* Creation of pb_plan_station_gh */
	pb_plan_station_gh = UxCreatePushButton( "pb_plan_station_gh", rc_plan_station );
	UxPutLabelString( pb_plan_station_gh, "GH" ),
	UxCreateWidget( pb_plan_station_gh );


	/* Creation of pb_plan_station_ph */
	pb_plan_station_ph = UxCreatePushButton( "pb_plan_station_ph", rc_plan_station );
	UxPutLabelString( pb_plan_station_ph, "PH" ),
	UxCreateWidget( pb_plan_station_ph );


	/* Creation of pb_plan_station_as */
	pb_plan_station_as = UxCreatePushButton( "pb_plan_station_as", rc_plan_station );
	UxPutLabelString( pb_plan_station_as, "AS" ),
	UxCreateWidget( pb_plan_station_as );


	/* Creation of pb_plan_station_be */
	pb_plan_station_be = UxCreatePushButton( "pb_plan_station_be", rc_plan_station );
	UxPutLabelString( pb_plan_station_be, "BE" ),
	UxCreateWidget( pb_plan_station_be );


	/* Creation of pb_plan_station_co */
	pb_plan_station_co = UxCreatePushButton( "pb_plan_station_co", rc_plan_station );
	UxPutLabelString( pb_plan_station_co, "CO" ),
	UxCreateWidget( pb_plan_station_co );


	/* Creation of pb_plan_station_cu */
	pb_plan_station_cu = UxCreatePushButton( "pb_plan_station_cu", rc_plan_station );
	UxPutLabelString( pb_plan_station_cu, "CU" ),
	UxCreateWidget( pb_plan_station_cu );


	/* Creation of pb_plan_station_es */
	pb_plan_station_es = UxCreatePushButton( "pb_plan_station_es", rc_plan_station );
	UxPutLabelString( pb_plan_station_es, "ES" ),
	UxCreateWidget( pb_plan_station_es );


	/* Creation of pb_plan_station_fs */
	pb_plan_station_fs = UxCreatePushButton( "pb_plan_station_fs", rc_plan_station );
	UxPutLabelString( pb_plan_station_fs, "FS" ),
	UxCreateWidget( pb_plan_station_fs );


	/* Creation of pb_plan_station_ha */
	pb_plan_station_ha = UxCreatePushButton( "pb_plan_station_ha", rc_plan_station );
	UxPutLabelString( pb_plan_station_ha, "HA" ),
	UxCreateWidget( pb_plan_station_ha );


	/* Creation of pb_plan_station_ho */
	pb_plan_station_ho = UxCreatePushButton( "pb_plan_station_ho", rc_plan_station );
	UxPutLabelString( pb_plan_station_ho, "HO" ),
	UxCreateWidget( pb_plan_station_ho );


	/* Creation of pb_plan_station_hy */
	pb_plan_station_hy = UxCreatePushButton( "pb_plan_station_hy", rc_plan_station );
	UxPutLabelString( pb_plan_station_hy, "HY" ),
	UxCreateWidget( pb_plan_station_hy );


	/* Creation of pb_plan_station_is */
	pb_plan_station_is = UxCreatePushButton( "pb_plan_station_is", rc_plan_station );
	UxPutLabelString( pb_plan_station_is, "IS" ),
	UxCreateWidget( pb_plan_station_is );


	/* Creation of pb_plan_station_in */
	pb_plan_station_in = UxCreatePushButton( "pb_plan_station_in", rc_plan_station );
	UxPutLabelString( pb_plan_station_in, "IN" ),
	UxCreateWidget( pb_plan_station_in );


	/* Creation of pb_plan_station_jo */
	pb_plan_station_jo = UxCreatePushButton( "pb_plan_station_jo", rc_plan_station );
	UxPutLabelString( pb_plan_station_jo, "JO" ),
	UxCreateWidget( pb_plan_station_jo );


	/* Creation of pb_plan_station_ks */
	pb_plan_station_ks = UxCreatePushButton( "pb_plan_station_ks", rc_plan_station );
	UxPutLabelString( pb_plan_station_ks, "KS" ),
	UxCreateWidget( pb_plan_station_ks );


	/* Creation of pb_plan_station_ku */
	pb_plan_station_ku = UxCreatePushButton( "pb_plan_station_ku", rc_plan_station );
	UxPutLabelString( pb_plan_station_ku, "KU" ),
	UxCreateWidget( pb_plan_station_ku );


	/* Creation of pb_plan_station_ma */
	pb_plan_station_ma = UxCreatePushButton( "pb_plan_station_ma", rc_plan_station );
	UxPutLabelString( pb_plan_station_ma, "MA" ),
	UxCreateWidget( pb_plan_station_ma );


	/* Creation of pb_plan_station_ms */
	pb_plan_station_ms = UxCreatePushButton( "pb_plan_station_ms", rc_plan_station );
	UxPutLabelString( pb_plan_station_ms, "MS" ),
	UxCreateWidget( pb_plan_station_ms );


	/* Creation of pb_plan_station_pp */
	pb_plan_station_pp = UxCreatePushButton( "pb_plan_station_pp", rc_plan_station );
	UxPutLabelString( pb_plan_station_pp, "PP" ),
	UxCreateWidget( pb_plan_station_pp );


	/* Creation of pb_plan_station_sa */
	pb_plan_station_sa = UxCreatePushButton( "pb_plan_station_sa", rc_plan_station );
	UxPutLabelString( pb_plan_station_sa, "SA" ),
	UxCreateWidget( pb_plan_station_sa );


	/* Creation of pb_plan_station_se */
	pb_plan_station_se = UxCreatePushButton( "pb_plan_station_se", rc_plan_station );
	UxPutLabelString( pb_plan_station_se, "SE" ),
	UxCreateWidget( pb_plan_station_se );


	/* Creation of pb_plan_station_sy */
	pb_plan_station_sy = UxCreatePushButton( "pb_plan_station_sy", rc_plan_station );
	UxPutLabelString( pb_plan_station_sy, "SY" ),
	UxCreateWidget( pb_plan_station_sy );


	/* Creation of pb_plan_station_tf */
	pb_plan_station_tf = UxCreatePushButton( "pb_plan_station_tf", rc_plan_station );
	UxPutLabelString( pb_plan_station_tf, "TF" ),
	UxCreateWidget( pb_plan_station_tf );


	/* Creation of pb_plan_station_tg */
	pb_plan_station_tg = UxCreatePushButton( "pb_plan_station_tg", rc_plan_station );
	UxPutLabelString( pb_plan_station_tg, "TG" ),
	UxCreateWidget( pb_plan_station_tg );


	/* Creation of pb_plan_station_th */
	pb_plan_station_th = UxCreatePushButton( "pb_plan_station_th", rc_plan_station );
	UxPutLabelString( pb_plan_station_th, "TH" ),
	UxCreateWidget( pb_plan_station_th );


	/* Creation of pb_plan_station_to */
	pb_plan_station_to = UxCreatePushButton( "pb_plan_station_to", rc_plan_station );
	UxPutLabelString( pb_plan_station_to, "TO" ),
	UxCreateWidget( pb_plan_station_to );


	/* Creation of pb_plan_station_ts */
	pb_plan_station_ts = UxCreatePushButton( "pb_plan_station_ts", rc_plan_station );
	UxPutLabelString( pb_plan_station_ts, "TS" ),
	UxCreateWidget( pb_plan_station_ts );


	/* Creation of pb_plan_station_wf */
	pb_plan_station_wf = UxCreatePushButton( "pb_plan_station_wf", rc_plan_station );
	UxPutLabelString( pb_plan_station_wf, "WF" ),
	UxCreateWidget( pb_plan_station_wf );


	/* Creation of pb_plan_station_wh */
	pb_plan_station_wh = UxCreatePushButton( "pb_plan_station_wh", rc_plan_station );
	UxPutLabelString( pb_plan_station_wh, "WH" ),
	UxCreateWidget( pb_plan_station_wh );


	/* Creation of om_plan_station */
	om_plan_station = UxCreateRowColumn( "om_plan_station", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_station, "menu_option" ),
	UxPutX( om_plan_station, 670 ),
	UxPutY( om_plan_station, 44 ),
	UxPutWidth( om_plan_station, 60 ),
	UxPutHeight( om_plan_station, 54 ),
	UxPutLabelString( om_plan_station, "Station" ),
	UxPutSensitive( om_plan_station, "false" ),
	UxPutSubMenuId( om_plan_station, "rc_plan_station" ),
	UxCreateWidget( om_plan_station );


	/* Creation of rc_plan_priority */
	rc_plan_priority = UxCreateRowColumn( "rc_plan_priority", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_priority, "menu_pulldown" ),
	UxPutSensitive( rc_plan_priority, "false" ),
	UxCreateWidget( rc_plan_priority );


	/* Creation of pb_plan_priority_any */
	pb_plan_priority_any = UxCreatePushButton( "pb_plan_priority_any", rc_plan_priority );
	UxPutLabelString( pb_plan_priority_any, "Any" ),
	UxPutSensitive( pb_plan_priority_any, "false" ),
	UxCreateWidget( pb_plan_priority_any );


	/* Creation of pb_plan_priority_low */
	pb_plan_priority_low = UxCreatePushButton( "pb_plan_priority_low", rc_plan_priority );
	UxPutLabelString( pb_plan_priority_low, "LOW" ),
	UxPutSensitive( pb_plan_priority_low, "false" ),
	UxCreateWidget( pb_plan_priority_low );


	/* Creation of pb_plan_priority_routine */
	pb_plan_priority_routine = UxCreatePushButton( "pb_plan_priority_routine", rc_plan_priority );
	UxPutLabelString( pb_plan_priority_routine, "ROUTINE" ),
	UxPutSensitive( pb_plan_priority_routine, "false" ),
	UxCreateWidget( pb_plan_priority_routine );


	/* Creation of pb_plan_priority_high */
	pb_plan_priority_high = UxCreatePushButton( "pb_plan_priority_high", rc_plan_priority );
	UxPutLabelString( pb_plan_priority_high, "HIGH" ),
	UxPutSensitive( pb_plan_priority_high, "false" ),
	UxCreateWidget( pb_plan_priority_high );


	/* Creation of pb_plan_priority_urgent */
	pb_plan_priority_urgent = UxCreatePushButton( "pb_plan_priority_urgent", rc_plan_priority );
	UxPutLabelString( pb_plan_priority_urgent, "URGENT" ),
	UxPutSensitive( pb_plan_priority_urgent, "false" ),
	UxCreateWidget( pb_plan_priority_urgent );


	/* Creation of om_plan_priority */
	om_plan_priority = UxCreateRowColumn( "om_plan_priority", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_priority, "menu_option" ),
	UxPutX( om_plan_priority, 520 ),
	UxPutY( om_plan_priority, 90 ),
	UxPutWidth( om_plan_priority, 60 ),
	UxPutHeight( om_plan_priority, 54 ),
	UxPutLabelString( om_plan_priority, "Priority" ),
	UxPutSensitive( om_plan_priority, "false" ),
	UxPutSubMenuId( om_plan_priority, "rc_plan_priority" ),
	UxCreateWidget( om_plan_priority );


	/* Creation of l_plan_rev */
	l_plan_rev = UxCreateLabel( "l_plan_rev", bb_plan_query_settings );
	UxPutX( l_plan_rev, 232 ),
	UxPutY( l_plan_rev, 47 ),
	UxPutWidth( l_plan_rev, 34 ),
	UxPutHeight( l_plan_rev, 32 ),
	UxPutLabelString( l_plan_rev, "Rev" ),
	UxPutSensitive( l_plan_rev, "false" ),
	UxCreateWidget( l_plan_rev );


	/* Creation of tf_plan_rev */
	tf_plan_rev = UxCreateTextField( "tf_plan_rev", bb_plan_query_settings );
	UxPutWidth( tf_plan_rev, 67 ),
	UxPutX( tf_plan_rev, 268 ),
	UxPutY( tf_plan_rev, 48 ),
	UxPutHeight( tf_plan_rev, 32 ),
	UxPutSensitive( tf_plan_rev, "false" ),
	UxCreateWidget( tf_plan_rev );


	/* Creation of l_plan_seq */
	l_plan_seq = UxCreateLabel( "l_plan_seq", bb_plan_query_settings );
	UxPutX( l_plan_seq, 345 ),
	UxPutY( l_plan_seq, 46 ),
	UxPutWidth( l_plan_seq, 34 ),
	UxPutHeight( l_plan_seq, 32 ),
	UxPutLabelString( l_plan_seq, "Seq" ),
	UxPutSensitive( l_plan_seq, "false" ),
	UxCreateWidget( l_plan_seq );


	/* Creation of tf_plan_seq */
	tf_plan_seq = UxCreateTextField( "tf_plan_seq", bb_plan_query_settings );
	UxPutWidth( tf_plan_seq, 55 ),
	UxPutX( tf_plan_seq, 383 ),
	UxPutY( tf_plan_seq, 47 ),
	UxPutHeight( tf_plan_seq, 32 ),
	UxPutSensitive( tf_plan_seq, "false" ),
	UxCreateWidget( tf_plan_seq );


	/* Creation of l_plan_job_id */
	l_plan_job_id = UxCreateLabel( "l_plan_job_id", bb_plan_query_settings );
	UxPutX( l_plan_job_id, 15 ),
	UxPutY( l_plan_job_id, 94 ),
	UxPutWidth( l_plan_job_id, 56 ),
	UxPutHeight( l_plan_job_id, 32 ),
	UxPutLabelString( l_plan_job_id, "Job ID" ),
	UxPutSensitive( l_plan_job_id, "false" ),
	UxCreateWidget( l_plan_job_id );


	/* Creation of tf_plan_job_id */
	tf_plan_job_id = UxCreateTextField( "tf_plan_job_id", bb_plan_query_settings );
	UxPutWidth( tf_plan_job_id, 78 ),
	UxPutX( tf_plan_job_id, 74 ),
	UxPutY( tf_plan_job_id, 92 ),
	UxPutHeight( tf_plan_job_id, 32 ),
	UxPutSensitive( tf_plan_job_id, "false" ),
	UxCreateWidget( tf_plan_job_id );


	/* Creation of l_plan_order_id */
	l_plan_order_id = UxCreateLabel( "l_plan_order_id", bb_plan_query_settings );
	UxPutX( l_plan_order_id, 164 ),
	UxPutY( l_plan_order_id, 93 ),
	UxPutWidth( l_plan_order_id, 72 ),
	UxPutHeight( l_plan_order_id, 32 ),
	UxPutLabelString( l_plan_order_id, "Order ID" ),
	UxPutSensitive( l_plan_order_id, "false" ),
	UxCreateWidget( l_plan_order_id );


	/* Creation of tf_plan_order_id */
	tf_plan_order_id = UxCreateTextField( "tf_plan_order_id", bb_plan_query_settings );
	UxPutWidth( tf_plan_order_id, 83 ),
	UxPutX( tf_plan_order_id, 237 ),
	UxPutY( tf_plan_order_id, 93 ),
	UxPutHeight( tf_plan_order_id, 32 ),
	UxPutSensitive( tf_plan_order_id, "false" ),
	UxCreateWidget( tf_plan_order_id );


	/* Creation of l_plan_item_id */
	l_plan_item_id = UxCreateLabel( "l_plan_item_id", bb_plan_query_settings );
	UxPutX( l_plan_item_id, 329 ),
	UxPutY( l_plan_item_id, 93 ),
	UxPutWidth( l_plan_item_id, 75 ),
	UxPutHeight( l_plan_item_id, 32 ),
	UxPutLabelString( l_plan_item_id, "Item ID" ),
	UxPutSensitive( l_plan_item_id, "false" ),
	UxCreateWidget( l_plan_item_id );


	/* Creation of tf_plan_item_id */
	tf_plan_item_id = UxCreateTextField( "tf_plan_item_id", bb_plan_query_settings );
	UxPutWidth( tf_plan_item_id, 43 ),
	UxPutX( tf_plan_item_id, 402 ),
	UxPutY( tf_plan_item_id, 93 ),
	UxPutHeight( tf_plan_item_id, 32 ),
	UxPutSensitive( tf_plan_item_id, "false" ),
	UxCreateWidget( tf_plan_item_id );


	/* Creation of l_plan_frame_id */
	l_plan_frame_id = UxCreateLabel( "l_plan_frame_id", bb_plan_query_settings );
	UxPutX( l_plan_frame_id, 10 ),
	UxPutY( l_plan_frame_id, 141 ),
	UxPutWidth( l_plan_frame_id, 73 ),
	UxPutHeight( l_plan_frame_id, 32 ),
	UxPutLabelString( l_plan_frame_id, "Frame ID" ),
	UxPutSensitive( l_plan_frame_id, "false" ),
	UxCreateWidget( l_plan_frame_id );


	/* Creation of tf_plan_frame_id */
	tf_plan_frame_id = UxCreateTextField( "tf_plan_frame_id", bb_plan_query_settings );
	UxPutWidth( tf_plan_frame_id, 67 ),
	UxPutX( tf_plan_frame_id, 83 ),
	UxPutY( tf_plan_frame_id, 141 ),
	UxPutHeight( tf_plan_frame_id, 32 ),
	UxPutSensitive( tf_plan_frame_id, "false" ),
	UxCreateWidget( tf_plan_frame_id );


	/* Creation of l_plan_subframe_id */
	l_plan_subframe_id = UxCreateLabel( "l_plan_subframe_id", bb_plan_query_settings );
	UxPutX( l_plan_subframe_id, 159 ),
	UxPutY( l_plan_subframe_id, 141 ),
	UxPutWidth( l_plan_subframe_id, 100 ),
	UxPutHeight( l_plan_subframe_id, 32 ),
	UxPutLabelString( l_plan_subframe_id, "Subframe ID" ),
	UxPutSensitive( l_plan_subframe_id, "false" ),
	UxCreateWidget( l_plan_subframe_id );


	/* Creation of tf_plan_subframe_id */
	tf_plan_subframe_id = UxCreateTextField( "tf_plan_subframe_id", bb_plan_query_settings );
	UxPutWidth( tf_plan_subframe_id, 43 ),
	UxPutX( tf_plan_subframe_id, 261 ),
	UxPutY( tf_plan_subframe_id, 140 ),
	UxPutHeight( tf_plan_subframe_id, 32 ),
	UxPutSensitive( tf_plan_subframe_id, "false" ),
	UxCreateWidget( tf_plan_subframe_id );


	/* Creation of rc_plan_product_type */
	rc_plan_product_type = UxCreateRowColumn( "rc_plan_product_type", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_product_type, "menu_pulldown" ),
	UxPutX( rc_plan_product_type, 0 ),
	UxPutY( rc_plan_product_type, 88 ),
	UxPutSensitive( rc_plan_product_type, "false" ),
	UxCreateWidget( rc_plan_product_type );


	/* Creation of pb_plan_product_type_any */
	pb_plan_product_type_any = UxCreatePushButton( "pb_plan_product_type_any", rc_plan_product_type );
	UxPutLabelString( pb_plan_product_type_any, "Any" ),
	UxPutX( pb_plan_product_type_any, 2 ),
	UxPutY( pb_plan_product_type_any, 145 ),
	UxPutSensitive( pb_plan_product_type_any, "false" ),
	UxCreateWidget( pb_plan_product_type_any );


	/* Creation of pb_plan_product_type_standard */
	pb_plan_product_type_standard = UxCreatePushButton( "pb_plan_product_type_standard", rc_plan_product_type );
	UxPutLabelString( pb_plan_product_type_standard, "STANDARD" ),
	UxPutX( pb_plan_product_type_standard, 2 ),
	UxPutY( pb_plan_product_type_standard, 145 ),
	UxPutSensitive( pb_plan_product_type_standard, "false" ),
	UxCreateWidget( pb_plan_product_type_standard );


	/* Creation of rc_plan_product_type_calset */
	rc_plan_product_type_calset = UxCreatePushButton( "rc_plan_product_type_calset", rc_plan_product_type );
	UxPutLabelString( rc_plan_product_type_calset, "CAL_SET" ),
	UxCreateWidget( rc_plan_product_type_calset );


	/* Creation of rc_plan_product_type_ramp */
	rc_plan_product_type_ramp = UxCreatePushButton( "rc_plan_product_type_ramp", rc_plan_product_type );
	UxPutLabelString( rc_plan_product_type_ramp, "RAMP" ),
	UxCreateWidget( rc_plan_product_type_ramp );


	/* Creation of rc_plan_product_type_scansar */
	rc_plan_product_type_scansar = UxCreatePushButton( "rc_plan_product_type_scansar", rc_plan_product_type );
	UxPutLabelString( rc_plan_product_type_scansar, "SCANSAR" ),
	UxCreateWidget( rc_plan_product_type_scansar );


	/* Creation of pb_plan_product_type_complex */
	pb_plan_product_type_complex = UxCreatePushButton( "pb_plan_product_type_complex", rc_plan_product_type );
	UxPutLabelString( pb_plan_product_type_complex, "COMPLEX" ),
	UxPutX( pb_plan_product_type_complex, 2 ),
	UxPutY( pb_plan_product_type_complex, 145 ),
	UxPutSensitive( pb_plan_product_type_complex, "false" ),
	UxCreateWidget( pb_plan_product_type_complex );


	/* Creation of pb_plan_product_type_ccsd */
	pb_plan_product_type_ccsd = UxCreatePushButton( "pb_plan_product_type_ccsd", rc_plan_product_type );
	UxPutLabelString( pb_plan_product_type_ccsd, "CCSD" ),
	UxPutX( pb_plan_product_type_ccsd, 2 ),
	UxPutY( pb_plan_product_type_ccsd, 145 ),
	UxPutSensitive( pb_plan_product_type_ccsd, "false" ),
	UxCreateWidget( pb_plan_product_type_ccsd );


	/* Creation of om_plan_product_type */
	om_plan_product_type = UxCreateRowColumn( "om_plan_product_type", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_product_type, "menu_option" ),
	UxPutX( om_plan_product_type, 330 ),
	UxPutY( om_plan_product_type, 141 ),
	UxPutWidth( om_plan_product_type, 60 ),
	UxPutHeight( om_plan_product_type, 54 ),
	UxPutLabelString( om_plan_product_type, "Product Type" ),
	UxPutSensitive( om_plan_product_type, "false" ),
	UxPutSubMenuId( om_plan_product_type, "rc_plan_product_type" ),
	UxCreateWidget( om_plan_product_type );


	/* Creation of rc_plan_media_type */
	rc_plan_media_type = UxCreateRowColumn( "rc_plan_media_type", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_media_type, "menu_pulldown" ),
	UxPutX( rc_plan_media_type, 0 ),
	UxPutY( rc_plan_media_type, 217 ),
	UxPutSensitive( rc_plan_media_type, "false" ),
	UxCreateWidget( rc_plan_media_type );


	/* Creation of pb_plan_media_type_any */
	pb_plan_media_type_any = UxCreatePushButton( "pb_plan_media_type_any", rc_plan_media_type );
	UxPutLabelString( pb_plan_media_type_any, "Any" ),
	UxPutX( pb_plan_media_type_any, 2 ),
	UxPutY( pb_plan_media_type_any, 241 ),
	UxPutSensitive( pb_plan_media_type_any, "false" ),
	UxCreateWidget( pb_plan_media_type_any );


	/* Creation of pb_plan_media_type_disk */
	pb_plan_media_type_disk = UxCreatePushButton( "pb_plan_media_type_disk", rc_plan_media_type );
	UxPutLabelString( pb_plan_media_type_disk, "DISK" ),
	UxPutX( pb_plan_media_type_disk, 2 ),
	UxPutY( pb_plan_media_type_disk, 241 ),
	UxPutSensitive( pb_plan_media_type_disk, "false" ),
	UxCreateWidget( pb_plan_media_type_disk );


	/* Creation of pb_plan_media_type_dcrsi */
	pb_plan_media_type_dcrsi = UxCreatePushButton( "pb_plan_media_type_dcrsi", rc_plan_media_type );
	UxPutLabelString( pb_plan_media_type_dcrsi, "DCRSI" ),
	UxPutX( pb_plan_media_type_dcrsi, 2 ),
	UxPutY( pb_plan_media_type_dcrsi, 241 ),
	UxPutSensitive( pb_plan_media_type_dcrsi, "false" ),
	UxCreateWidget( pb_plan_media_type_dcrsi );


	/* Creation of rc_plan_media_type_id1 */
	rc_plan_media_type_id1 = UxCreatePushButton( "rc_plan_media_type_id1", rc_plan_media_type );
	UxPutLabelString( rc_plan_media_type_id1, "ID-1" ),
	UxCreateWidget( rc_plan_media_type_id1 );


	/* Creation of om_plan_media_type */
	om_plan_media_type = UxCreateRowColumn( "om_plan_media_type", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_media_type, "menu_option" ),
	UxPutX( om_plan_media_type, 9 ),
	UxPutY( om_plan_media_type, 182 ),
	UxPutWidth( om_plan_media_type, 60 ),
	UxPutHeight( om_plan_media_type, 54 ),
	UxPutLabelString( om_plan_media_type, "Media Type" ),
	UxPutSensitive( om_plan_media_type, "false" ),
	UxPutSubMenuId( om_plan_media_type, "rc_plan_media_type" ),
	UxCreateWidget( om_plan_media_type );


	/* Creation of l_plan_media_id */
	l_plan_media_id = UxCreateLabel( "l_plan_media_id", bb_plan_query_settings );
	UxPutX( l_plan_media_id, 218 ),
	UxPutY( l_plan_media_id, 185 ),
	UxPutWidth( l_plan_media_id, 82 ),
	UxPutHeight( l_plan_media_id, 32 ),
	UxPutLabelString( l_plan_media_id, "Media ID" ),
	UxPutSensitive( l_plan_media_id, "false" ),
	UxCreateWidget( l_plan_media_id );


	/* Creation of tf_plan_media_id */
	tf_plan_media_id = UxCreateTextField( "tf_plan_media_id", bb_plan_query_settings );
	UxPutWidth( tf_plan_media_id, 83 ),
	UxPutX( tf_plan_media_id, 311 ),
	UxPutY( tf_plan_media_id, 185 ),
	UxPutHeight( tf_plan_media_id, 32 ),
	UxPutSensitive( tf_plan_media_id, "false" ),
	UxCreateWidget( tf_plan_media_id );


	/* Creation of pb_plan_query */
	pb_plan_query = UxCreatePushButton( "pb_plan_query", bb_plan_query_settings );
	UxPutX( pb_plan_query, 768 ),
	UxPutY( pb_plan_query, 183 ),
	UxPutWidth( pb_plan_query, 80 ),
	UxPutHeight( pb_plan_query, 40 ),
	UxPutLabelString( pb_plan_query, "Query" ),
	UxPutFontList( pb_plan_query, "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1" ),
	UxPutSensitive( pb_plan_query, "false" ),
	UxCreateWidget( pb_plan_query );

	UxAddCallback( pb_plan_query, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_query,
		(XtPointer) UxPps_planContext );


	/* Creation of rc_plan_processor_mode */
	rc_plan_processor_mode = UxCreateRowColumn( "rc_plan_processor_mode", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_processor_mode, "menu_pulldown" ),
	UxPutSensitive( rc_plan_processor_mode, "false" ),
	UxCreateWidget( rc_plan_processor_mode );


	/* Creation of pb_plan_processor_mode_any */
	pb_plan_processor_mode_any = UxCreatePushButton( "pb_plan_processor_mode_any", rc_plan_processor_mode );
	UxPutLabelString( pb_plan_processor_mode_any, "Any" ),
	UxPutSensitive( pb_plan_processor_mode_any, "false" ),
	UxCreateWidget( pb_plan_processor_mode_any );


	/* Creation of pb_plan_processor_mode_continuous */
	pb_plan_processor_mode_continuous = UxCreatePushButton( "pb_plan_processor_mode_continuous", rc_plan_processor_mode );
	UxPutLabelString( pb_plan_processor_mode_continuous, "CONTINUOUS" ),
	UxPutSensitive( pb_plan_processor_mode_continuous, "false" ),
	UxCreateWidget( pb_plan_processor_mode_continuous );


	/* Creation of pb_plan_processor_mode_scansar */
	pb_plan_processor_mode_scansar = UxCreatePushButton( "pb_plan_processor_mode_scansar", rc_plan_processor_mode );
	UxPutLabelString( pb_plan_processor_mode_scansar, "SCANSAR" ),
	UxPutSensitive( pb_plan_processor_mode_scansar, "false" ),
	UxCreateWidget( pb_plan_processor_mode_scansar );


	/* Creation of om_plan_processor_mode */
	om_plan_processor_mode = UxCreateRowColumn( "om_plan_processor_mode", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_processor_mode, "menu_option" ),
	UxPutX( om_plan_processor_mode, 423 ),
	UxPutY( om_plan_processor_mode, 182 ),
	UxPutWidth( om_plan_processor_mode, 202 ),
	UxPutHeight( om_plan_processor_mode, 35 ),
	UxPutLabelString( om_plan_processor_mode, "Processor Mode" ),
	UxPutSensitive( om_plan_processor_mode, "false" ),
	UxPutSubMenuId( om_plan_processor_mode, "rc_plan_processor_mode" ),
	UxCreateWidget( om_plan_processor_mode );


	/* Creation of rc_plan_data_direction */
	rc_plan_data_direction = UxCreateRowColumn( "rc_plan_data_direction", bb_plan_query_settings );
	UxPutRowColumnType( rc_plan_data_direction, "menu_pulldown" ),
	UxPutSensitive( rc_plan_data_direction, "false" ),
	UxCreateWidget( rc_plan_data_direction );


	/* Creation of pb_plan_data_direction_any */
	pb_plan_data_direction_any = UxCreatePushButton( "pb_plan_data_direction_any", rc_plan_data_direction );
	UxPutLabelString( pb_plan_data_direction_any, "Any" ),
	UxPutSensitive( pb_plan_data_direction_any, "false" ),
	UxCreateWidget( pb_plan_data_direction_any );


	/* Creation of pb_plan_data_direction_forward */
	pb_plan_data_direction_forward = UxCreatePushButton( "pb_plan_data_direction_forward", rc_plan_data_direction );
	UxPutLabelString( pb_plan_data_direction_forward, "FORWARD" ),
	UxPutSensitive( pb_plan_data_direction_forward, "false" ),
	UxCreateWidget( pb_plan_data_direction_forward );


	/* Creation of pb_plan_data_direction_reverse */
	pb_plan_data_direction_reverse = UxCreatePushButton( "pb_plan_data_direction_reverse", rc_plan_data_direction );
	UxPutLabelString( pb_plan_data_direction_reverse, "REVERSE" ),
	UxPutSensitive( pb_plan_data_direction_reverse, "false" ),
	UxCreateWidget( pb_plan_data_direction_reverse );


	/* Creation of pb_plan_data_direction_unknown */
	pb_plan_data_direction_unknown = UxCreatePushButton( "pb_plan_data_direction_unknown", rc_plan_data_direction );
	UxPutLabelString( pb_plan_data_direction_unknown, "UNKNOWN" ),
	UxPutSensitive( pb_plan_data_direction_unknown, "false" ),
	UxCreateWidget( pb_plan_data_direction_unknown );


	/* Creation of om_plan_data_direction */
	om_plan_data_direction = UxCreateRowColumn( "om_plan_data_direction", bb_plan_query_settings );
	UxPutRowColumnType( om_plan_data_direction, "menu_option" ),
	UxPutX( om_plan_data_direction, 600 ),
	UxPutY( om_plan_data_direction, 140 ),
	UxPutWidth( om_plan_data_direction, 60 ),
	UxPutHeight( om_plan_data_direction, 54 ),
	UxPutLabelString( om_plan_data_direction, "Data Direction" ),
	UxPutSensitive( om_plan_data_direction, "false" ),
	UxPutSubMenuId( om_plan_data_direction, "rc_plan_data_direction" ),
	UxCreateWidget( om_plan_data_direction );


	/* Creation of l_plan_query_settings */
	l_plan_query_settings = UxCreateLabel( "l_plan_query_settings", pps_plan );
	UxPutX( l_plan_query_settings, 12 ),
	UxPutY( l_plan_query_settings, 33 ),
	UxPutWidth( l_plan_query_settings, 116 ),
	UxPutHeight( l_plan_query_settings, 30 ),
	UxPutLabelString( l_plan_query_settings, "Query Settings" ),
	UxCreateWidget( l_plan_query_settings );


	/* Creation of l_plan_planned */
	l_plan_planned = UxCreateLabel( "l_plan_planned", pps_plan );
	UxPutX( l_plan_planned, 12 ),
	UxPutY( l_plan_planned, 586 ),
	UxPutWidth( l_plan_planned, 72 ),
	UxPutHeight( l_plan_planned, 30 ),
	UxPutLabelString( l_plan_planned, "Planned" ),
	UxCreateWidget( l_plan_planned );


	/* Creation of l_plan_ready */
	l_plan_ready = UxCreateLabel( "l_plan_ready", pps_plan );
	UxPutX( l_plan_ready, 14 ),
	UxPutY( l_plan_ready, 307 ),
	UxPutWidth( l_plan_ready, 185 ),
	UxPutHeight( l_plan_ready, 30 ),
	UxPutLabelString( l_plan_ready, "Ready to be Planned" ),
	UxCreateWidget( l_plan_ready );


	/* Creation of frame6 */
	frame6 = UxCreateFrame( "frame6", pps_plan );
	UxPutWidth( frame6, 838 ),
	UxPutHeight( frame6, 241 ),
	UxPutX( frame6, 12 ),
	UxPutY( frame6, 339 ),
	UxCreateWidget( frame6 );


	/* Creation of bulletinBoard15 */
	bulletinBoard15 = UxCreateBulletinBoard( "bulletinBoard15", frame6 );
	UxPutResizePolicy( bulletinBoard15, "resize_none" ),
	UxPutWidth( bulletinBoard15, 838 ),
	UxPutHeight( bulletinBoard15, 239 ),
	UxPutX( bulletinBoard15, 2 ),
	UxPutY( bulletinBoard15, 0 ),
	UxPutMarginHeight( bulletinBoard15, 0 ),
	UxPutMarginWidth( bulletinBoard15, 0 ),
	UxCreateWidget( bulletinBoard15 );


	/* Creation of scrolledWindowList12 */
	scrolledWindowList12 = UxCreateScrolledWindow( "scrolledWindowList12", bulletinBoard15 );
	UxPutScrollingPolicy( scrolledWindowList12, "application_defined" ),
	UxPutVisualPolicy( scrolledWindowList12, "variable" ),
	UxPutScrollBarDisplayPolicy( scrolledWindowList12, "static" ),
	UxPutShadowThickness( scrolledWindowList12, 0 ),
	UxPutX( scrolledWindowList12, 4 ),
	UxPutY( scrolledWindowList12, 44 ),
	UxPutHeight( scrolledWindowList12, 188 ),
	UxCreateWidget( scrolledWindowList12 );


	/* Creation of sw_plan_ready_list */
	sw_plan_ready_list = UxCreateScrolledList( "sw_plan_ready_list", scrolledWindowList12 );
	UxPutWidth( sw_plan_ready_list, 810 ),
	UxPutHeight( sw_plan_ready_list, 170 ),
	UxPutScrollBarDisplayPolicy( sw_plan_ready_list, "static" ),
	UxPutItemCount( sw_plan_ready_list, 0 ),
	UxPutFontList( sw_plan_ready_list, "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1" ),
	UxPutListSizePolicy( sw_plan_ready_list, "constant" ),
	UxPutAutomaticSelection( sw_plan_ready_list, "false" ),
	UxPutSelectionPolicy( sw_plan_ready_list, "extended_select" ),
	UxCreateWidget( sw_plan_ready_list );

	UxAddCallback( sw_plan_ready_list, XmNextendedSelectionCallback,
		(XtCallbackProc) extendedSelectionCB_sw_plan_ready_list,
		(XtPointer) UxPps_planContext );


	/* Creation of l_plan_ready_order_type */
	l_plan_ready_order_type = UxCreateLabel( "l_plan_ready_order_type", bulletinBoard15 );
	UxPutX( l_plan_ready_order_type, 3 ),
	UxPutY( l_plan_ready_order_type, 5 ),
	UxPutWidth( l_plan_ready_order_type, 64 ),
	UxPutHeight( l_plan_ready_order_type, 32 ),
	UxPutLabelString( l_plan_ready_order_type, "Order\nType" ),
	UxCreateWidget( l_plan_ready_order_type );


	/* Creation of l_plan_ready_priority */
	l_plan_ready_priority = UxCreateLabel( "l_plan_ready_priority", bulletinBoard15 );
	UxPutX( l_plan_ready_priority, 75 ),
	UxPutY( l_plan_ready_priority, 5 ),
	UxPutWidth( l_plan_ready_priority, 68 ),
	UxPutHeight( l_plan_ready_priority, 32 ),
	UxPutLabelString( l_plan_ready_priority, "Priority" ),
	UxCreateWidget( l_plan_ready_priority );


	/* Creation of l_plan_ready_age */
	l_plan_ready_age = UxCreateLabel( "l_plan_ready_age", bulletinBoard15 );
	UxPutX( l_plan_ready_age, 143 ),
	UxPutY( l_plan_ready_age, 5 ),
	UxPutWidth( l_plan_ready_age, 48 ),
	UxPutHeight( l_plan_ready_age, 32 ),
	UxPutLabelString( l_plan_ready_age, "Age\n(Days)" ),
	UxCreateWidget( l_plan_ready_age );


	/* Creation of l_plan_ready_sat_sens_rev */
	l_plan_ready_sat_sens_rev = UxCreateLabel( "l_plan_ready_sat_sens_rev", bulletinBoard15 );
	UxPutX( l_plan_ready_sat_sens_rev, 203 ),
	UxPutY( l_plan_ready_sat_sens_rev, 5 ),
	UxPutWidth( l_plan_ready_sat_sens_rev, 88 ),
	UxPutHeight( l_plan_ready_sat_sens_rev, 32 ),
	UxPutLabelString( l_plan_ready_sat_sens_rev, "Datatake ID" ),
	UxCreateWidget( l_plan_ready_sat_sens_rev );


	/* Creation of l_plan_ready_job */
	l_plan_ready_job = UxCreateLabel( "l_plan_ready_job", bulletinBoard15 );
	UxPutX( l_plan_ready_job, 570 ),
	UxPutY( l_plan_ready_job, 4 ),
	UxPutWidth( l_plan_ready_job, 68 ),
	UxPutHeight( l_plan_ready_job, 32 ),
	UxPutLabelString( l_plan_ready_job, "Job ID" ),
	UxCreateWidget( l_plan_ready_job );


	/* Creation of l_plan_ready_insert_top */
	l_plan_ready_insert_top = UxCreateLabel( "l_plan_ready_insert_top", bulletinBoard15 );
	UxPutX( l_plan_ready_insert_top, 760 ),
	UxPutY( l_plan_ready_insert_top, 4 ),
	UxPutWidth( l_plan_ready_insert_top, 49 ),
	UxPutHeight( l_plan_ready_insert_top, 32 ),
	UxPutLabelString( l_plan_ready_insert_top, "Insert\nTop" ),
	UxCreateWidget( l_plan_ready_insert_top );


	/* Creation of l_plan_ready_mode */
	l_plan_ready_mode = UxCreateLabel( "l_plan_ready_mode", bulletinBoard15 );
	UxPutX( l_plan_ready_mode, 330 ),
	UxPutY( l_plan_ready_mode, 4 ),
	UxPutWidth( l_plan_ready_mode, 56 ),
	UxPutHeight( l_plan_ready_mode, 32 ),
	UxPutLabelString( l_plan_ready_mode, "Mode" ),
	UxCreateWidget( l_plan_ready_mode );


	/* Creation of l_plan_ready_pixel_spacing */
	l_plan_ready_pixel_spacing = UxCreateLabel( "l_plan_ready_pixel_spacing", bulletinBoard15 );
	UxPutX( l_plan_ready_pixel_spacing, 400 ),
	UxPutY( l_plan_ready_pixel_spacing, 4 ),
	UxPutWidth( l_plan_ready_pixel_spacing, 52 ),
	UxPutHeight( l_plan_ready_pixel_spacing, 32 ),
	UxPutLabelString( l_plan_ready_pixel_spacing, "Pixel\nSpacing" ),
	UxCreateWidget( l_plan_ready_pixel_spacing );


	/* Creation of l_plan_ready_media_id */
	l_plan_ready_media_id = UxCreateLabel( "l_plan_ready_media_id", bulletinBoard15 );
	UxPutX( l_plan_ready_media_id, 480 ),
	UxPutY( l_plan_ready_media_id, 4 ),
	UxPutWidth( l_plan_ready_media_id, 76 ),
	UxPutHeight( l_plan_ready_media_id, 31 ),
	UxPutLabelString( l_plan_ready_media_id, "Media ID" ),
	UxCreateWidget( l_plan_ready_media_id );


	/* Creation of l_plan_available_job2 */
	l_plan_available_job2 = UxCreateLabel( "l_plan_available_job2", bulletinBoard15 );
	UxPutX( l_plan_available_job2, 640 ),
	UxPutY( l_plan_available_job2, 0 ),
	UxPutWidth( l_plan_available_job2, 110 ),
	UxPutHeight( l_plan_available_job2, 40 ),
	UxPutLabelString( l_plan_available_job2, "Order & Item ID" ),
	UxCreateWidget( l_plan_available_job2 );


	/* Creation of f_plan_ready */
	f_plan_ready = UxCreateFrame( "f_plan_ready", pps_plan );
	UxPutWidth( f_plan_ready, 254 ),
	UxPutHeight( f_plan_ready, 126 ),
	UxPutX( f_plan_ready, 874 ),
	UxPutY( f_plan_ready, 339 ),
	UxCreateWidget( f_plan_ready );


	/* Creation of bb_plan_ready */
	bb_plan_ready = UxCreateBulletinBoard( "bb_plan_ready", f_plan_ready );
	UxPutResizePolicy( bb_plan_ready, "resize_none" ),
	UxPutWidth( bb_plan_ready, 339 ),
	UxPutHeight( bb_plan_ready, 171 ),
	UxPutX( bb_plan_ready, 2 ),
	UxPutY( bb_plan_ready, 2 ),
	UxPutMarginHeight( bb_plan_ready, 0 ),
	UxPutMarginWidth( bb_plan_ready, 0 ),
	UxCreateWidget( bb_plan_ready );


	/* Creation of pb_plan_ready_top */
	pb_plan_ready_top = UxCreatePushButton( "pb_plan_ready_top", bb_plan_ready );
	UxPutX( pb_plan_ready_top, 10 ),
	UxPutY( pb_plan_ready_top, 10 ),
	UxPutWidth( pb_plan_ready_top, 112 ),
	UxPutHeight( pb_plan_ready_top, 36 ),
	UxPutLabelString( pb_plan_ready_top, "TOP of Plan" ),
	UxPutSensitive( pb_plan_ready_top, "true" ),
	UxCreateWidget( pb_plan_ready_top );

	UxAddCallback( pb_plan_ready_top, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_ready_top,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_ready_IT_on */
	pb_plan_ready_IT_on = UxCreatePushButton( "pb_plan_ready_IT_on", bb_plan_ready );
	UxPutX( pb_plan_ready_IT_on, 130 ),
	UxPutY( pb_plan_ready_IT_on, 10 ),
	UxPutWidth( pb_plan_ready_IT_on, 112 ),
	UxPutHeight( pb_plan_ready_IT_on, 36 ),
	UxPutLabelString( pb_plan_ready_IT_on, "Insert Top On" ),
	UxPutSensitive( pb_plan_ready_IT_on, "true" ),
	UxCreateWidget( pb_plan_ready_IT_on );

	UxAddCallback( pb_plan_ready_IT_on, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_ready_IT_on,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_ready_IT_off */
	pb_plan_ready_IT_off = UxCreatePushButton( "pb_plan_ready_IT_off", bb_plan_ready );
	UxPutX( pb_plan_ready_IT_off, 130 ),
	UxPutY( pb_plan_ready_IT_off, 60 ),
	UxPutWidth( pb_plan_ready_IT_off, 112 ),
	UxPutHeight( pb_plan_ready_IT_off, 36 ),
	UxPutLabelString( pb_plan_ready_IT_off, "Insert Top Off" ),
	UxPutSensitive( pb_plan_ready_IT_off, "true" ),
	UxCreateWidget( pb_plan_ready_IT_off );

	UxAddCallback( pb_plan_ready_IT_off, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_ready_IT_off,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_ready_bottom */
	pb_plan_ready_bottom = UxCreatePushButton( "pb_plan_ready_bottom", bb_plan_ready );
	UxPutX( pb_plan_ready_bottom, 10 ),
	UxPutY( pb_plan_ready_bottom, 60 ),
	UxPutWidth( pb_plan_ready_bottom, 112 ),
	UxPutHeight( pb_plan_ready_bottom, 36 ),
	UxPutLabelString( pb_plan_ready_bottom, "BOTTOM of Plan" ),
	UxPutSensitive( pb_plan_ready_bottom, "true" ),
	UxCreateWidget( pb_plan_ready_bottom );

	UxAddCallback( pb_plan_ready_bottom, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_ready_bottom,
		(XtPointer) UxPps_planContext );


	/* Creation of frame7 */
	frame7 = UxCreateFrame( "frame7", pps_plan );
	UxPutWidth( frame7, 838 ),
	UxPutHeight( frame7, 243 ),
	UxPutX( frame7, 12 ),
	UxPutY( frame7, 617 ),
	UxCreateWidget( frame7 );


	/* Creation of bulletinBoard2 */
	bulletinBoard2 = UxCreateBulletinBoard( "bulletinBoard2", frame7 );
	UxPutResizePolicy( bulletinBoard2, "resize_none" ),
	UxPutWidth( bulletinBoard2, 838 ),
	UxPutHeight( bulletinBoard2, 241 ),
	UxPutX( bulletinBoard2, 2 ),
	UxPutY( bulletinBoard2, 0 ),
	UxPutMarginHeight( bulletinBoard2, 0 ),
	UxPutMarginWidth( bulletinBoard2, 0 ),
	UxCreateWidget( bulletinBoard2 );


	/* Creation of scrolledWindowList1 */
	scrolledWindowList1 = UxCreateScrolledWindow( "scrolledWindowList1", bulletinBoard2 );
	UxPutScrollingPolicy( scrolledWindowList1, "application_defined" ),
	UxPutVisualPolicy( scrolledWindowList1, "variable" ),
	UxPutScrollBarDisplayPolicy( scrolledWindowList1, "static" ),
	UxPutShadowThickness( scrolledWindowList1, 0 ),
	UxPutX( scrolledWindowList1, 3 ),
	UxPutY( scrolledWindowList1, 42 ),
	UxPutHeight( scrolledWindowList1, 188 ),
	UxCreateWidget( scrolledWindowList1 );


	/* Creation of sw_plan_available_list */
	sw_plan_available_list = UxCreateScrolledList( "sw_plan_available_list", scrolledWindowList1 );
	UxPutWidth( sw_plan_available_list, 810 ),
	UxPutHeight( sw_plan_available_list, 170 ),
	UxPutScrollBarDisplayPolicy( sw_plan_available_list, "static" ),
	UxPutItemCount( sw_plan_available_list, 0 ),
	UxPutFontList( sw_plan_available_list, "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1" ),
	UxPutListSizePolicy( sw_plan_available_list, "constant" ),
	UxPutAutomaticSelection( sw_plan_available_list, "false" ),
	UxPutSelectionPolicy( sw_plan_available_list, "extended_select" ),
	UxCreateWidget( sw_plan_available_list );

	UxAddCallback( sw_plan_available_list, XmNextendedSelectionCallback,
		(XtCallbackProc) extendedSelectionCB_sw_plan_available_list,
		(XtPointer) UxPps_planContext );


	/* Creation of l_plan_available_order_type */
	l_plan_available_order_type = UxCreateLabel( "l_plan_available_order_type", bulletinBoard2 );
	UxPutX( l_plan_available_order_type, 4 ),
	UxPutY( l_plan_available_order_type, 6 ),
	UxPutWidth( l_plan_available_order_type, 64 ),
	UxPutHeight( l_plan_available_order_type, 32 ),
	UxPutLabelString( l_plan_available_order_type, "Order\nType" ),
	UxCreateWidget( l_plan_available_order_type );


	/* Creation of l_plan_available_priority */
	l_plan_available_priority = UxCreateLabel( "l_plan_available_priority", bulletinBoard2 );
	UxPutX( l_plan_available_priority, 76 ),
	UxPutY( l_plan_available_priority, 6 ),
	UxPutWidth( l_plan_available_priority, 68 ),
	UxPutHeight( l_plan_available_priority, 32 ),
	UxPutLabelString( l_plan_available_priority, "Priority" ),
	UxCreateWidget( l_plan_available_priority );


	/* Creation of l_plan_available_age */
	l_plan_available_age = UxCreateLabel( "l_plan_available_age", bulletinBoard2 );
	UxPutX( l_plan_available_age, 144 ),
	UxPutY( l_plan_available_age, 6 ),
	UxPutWidth( l_plan_available_age, 48 ),
	UxPutHeight( l_plan_available_age, 32 ),
	UxPutLabelString( l_plan_available_age, "Age\n(Days)" ),
	UxCreateWidget( l_plan_available_age );


	/* Creation of l_plan_available_sat_sens_rev */
	l_plan_available_sat_sens_rev = UxCreateLabel( "l_plan_available_sat_sens_rev", bulletinBoard2 );
	UxPutX( l_plan_available_sat_sens_rev, 204 ),
	UxPutY( l_plan_available_sat_sens_rev, 6 ),
	UxPutWidth( l_plan_available_sat_sens_rev, 88 ),
	UxPutHeight( l_plan_available_sat_sens_rev, 32 ),
	UxPutLabelString( l_plan_available_sat_sens_rev, "Datatake ID" ),
	UxCreateWidget( l_plan_available_sat_sens_rev );


	/* Creation of l_plan_available_mode */
	l_plan_available_mode = UxCreateLabel( "l_plan_available_mode", bulletinBoard2 );
	UxPutX( l_plan_available_mode, 330 ),
	UxPutY( l_plan_available_mode, 6 ),
	UxPutWidth( l_plan_available_mode, 49 ),
	UxPutHeight( l_plan_available_mode, 32 ),
	UxPutLabelString( l_plan_available_mode, "Mode" ),
	UxCreateWidget( l_plan_available_mode );


	/* Creation of l_plan_available_pixel_spacing */
	l_plan_available_pixel_spacing = UxCreateLabel( "l_plan_available_pixel_spacing", bulletinBoard2 );
	UxPutX( l_plan_available_pixel_spacing, 400 ),
	UxPutY( l_plan_available_pixel_spacing, 6 ),
	UxPutWidth( l_plan_available_pixel_spacing, 60 ),
	UxPutHeight( l_plan_available_pixel_spacing, 34 ),
	UxPutLabelString( l_plan_available_pixel_spacing, "Pixel\nSpacing" ),
	UxCreateWidget( l_plan_available_pixel_spacing );


	/* Creation of l_plan_available_media_id */
	l_plan_available_media_id = UxCreateLabel( "l_plan_available_media_id", bulletinBoard2 );
	UxPutX( l_plan_available_media_id, 480 ),
	UxPutY( l_plan_available_media_id, 6 ),
	UxPutWidth( l_plan_available_media_id, 60 ),
	UxPutHeight( l_plan_available_media_id, 32 ),
	UxPutLabelString( l_plan_available_media_id, "Media ID" ),
	UxCreateWidget( l_plan_available_media_id );


	/* Creation of l_plan_available_job */
	l_plan_available_job = UxCreateLabel( "l_plan_available_job", bulletinBoard2 );
	UxPutX( l_plan_available_job, 570 ),
	UxPutY( l_plan_available_job, 6 ),
	UxPutWidth( l_plan_available_job, 60 ),
	UxPutHeight( l_plan_available_job, 32 ),
	UxPutLabelString( l_plan_available_job, "Job ID" ),
	UxCreateWidget( l_plan_available_job );


	/* Creation of l_plan_available_insert_top */
	l_plan_available_insert_top = UxCreateLabel( "l_plan_available_insert_top", bulletinBoard2 );
	UxPutX( l_plan_available_insert_top, 760 ),
	UxPutY( l_plan_available_insert_top, 6 ),
	UxPutWidth( l_plan_available_insert_top, 63 ),
	UxPutHeight( l_plan_available_insert_top, 32 ),
	UxPutLabelString( l_plan_available_insert_top, "Insert\nTop" ),
	UxCreateWidget( l_plan_available_insert_top );


	/* Creation of l_plan_available_job1 */
	l_plan_available_job1 = UxCreateLabel( "l_plan_available_job1", bulletinBoard2 );
	UxPutX( l_plan_available_job1, 640 ),
	UxPutY( l_plan_available_job1, 6 ),
	UxPutWidth( l_plan_available_job1, 110 ),
	UxPutHeight( l_plan_available_job1, 34 ),
	UxPutLabelString( l_plan_available_job1, "Order & Item ID" ),
	UxCreateWidget( l_plan_available_job1 );


	/* Creation of frame3 */
	frame3 = UxCreateFrame( "frame3", pps_plan );
	UxPutWidth( frame3, 216 ),
	UxPutHeight( frame3, 236 ),
	UxPutX( frame3, 893 ),
	UxPutY( frame3, 62 ),
	UxCreateWidget( frame3 );


	/* Creation of bulletinBoard4 */
	bulletinBoard4 = UxCreateBulletinBoard( "bulletinBoard4", frame3 );
	UxPutResizePolicy( bulletinBoard4, "resize_none" ),
	UxPutWidth( bulletinBoard4, 250 ),
	UxPutHeight( bulletinBoard4, 228 ),
	UxPutX( bulletinBoard4, 0 ),
	UxPutY( bulletinBoard4, 0 ),
	UxCreateWidget( bulletinBoard4 );


	/* Creation of rc_plan_order_first */
	rc_plan_order_first = UxCreateRowColumn( "rc_plan_order_first", bulletinBoard4 );
	UxPutRowColumnType( rc_plan_order_first, "menu_pulldown" ),
	UxPutSensitive( rc_plan_order_first, "false" ),
	UxPutX( rc_plan_order_first, 93 ),
	UxPutY( rc_plan_order_first, 0 ),
	UxCreateWidget( rc_plan_order_first );


	/* Creation of pb_plan_order_first_none */
	pb_plan_order_first_none = UxCreatePushButton( "pb_plan_order_first_none", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_none, "None" ),
	UxPutSensitive( pb_plan_order_first_none, "false" ),
	UxPutX( pb_plan_order_first_none, 97 ),
	UxPutY( pb_plan_order_first_none, 2 ),
	UxCreateWidget( pb_plan_order_first_none );


	/* Creation of pb_plan_order_first_order_type */
	pb_plan_order_first_order_type = UxCreatePushButton( "pb_plan_order_first_order_type", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_order_type, "Order Type" ),
	UxPutX( pb_plan_order_first_order_type, 97 ),
	UxPutY( pb_plan_order_first_order_type, 24 ),
	UxCreateWidget( pb_plan_order_first_order_type );


	/* Creation of pb_plan_order_first_priority */
	pb_plan_order_first_priority = UxCreatePushButton( "pb_plan_order_first_priority", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_priority, "Priority" ),
	UxPutX( pb_plan_order_first_priority, 97 ),
	UxPutY( pb_plan_order_first_priority, 46 ),
	UxCreateWidget( pb_plan_order_first_priority );


	/* Creation of pb_plan_order_first_age */
	pb_plan_order_first_age = UxCreatePushButton( "pb_plan_order_first_age", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_age, "Age (Days)" ),
	UxPutX( pb_plan_order_first_age, 97 ),
	UxPutY( pb_plan_order_first_age, 222 ),
	UxCreateWidget( pb_plan_order_first_age );


	/* Creation of pb_plan_order_first_sat_sens_rev */
	pb_plan_order_first_sat_sens_rev = UxCreatePushButton( "pb_plan_order_first_sat_sens_rev", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_plan_order_first_sat_sens_rev, 97 ),
	UxPutY( pb_plan_order_first_sat_sens_rev, 112 ),
	UxCreateWidget( pb_plan_order_first_sat_sens_rev );


	/* Creation of pb_plan_order_first_mode */
	pb_plan_order_first_mode = UxCreatePushButton( "pb_plan_order_first_mode", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_mode, "Mode" ),
	UxPutX( pb_plan_order_first_mode, 97 ),
	UxPutY( pb_plan_order_first_mode, 90 ),
	UxCreateWidget( pb_plan_order_first_mode );


	/* Creation of pb_plan_order_first_pixel_spacing */
	pb_plan_order_first_pixel_spacing = UxCreatePushButton( "pb_plan_order_first_pixel_spacing", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_pixel_spacing, "Pixel Spacing" ),
	UxPutX( pb_plan_order_first_pixel_spacing, 97 ),
	UxPutY( pb_plan_order_first_pixel_spacing, 134 ),
	UxCreateWidget( pb_plan_order_first_pixel_spacing );


	/* Creation of pb_plan_order_first_media_id */
	pb_plan_order_first_media_id = UxCreatePushButton( "pb_plan_order_first_media_id", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_media_id, "Media ID" ),
	UxPutX( pb_plan_order_first_media_id, 97 ),
	UxPutY( pb_plan_order_first_media_id, 68 ),
	UxCreateWidget( pb_plan_order_first_media_id );


	/* Creation of pb_plan_order_first_job_id */
	pb_plan_order_first_job_id = UxCreatePushButton( "pb_plan_order_first_job_id", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_job_id, "Job ID" ),
	UxPutX( pb_plan_order_first_job_id, 97 ),
	UxPutY( pb_plan_order_first_job_id, 156 ),
	UxCreateWidget( pb_plan_order_first_job_id );


	/* Creation of pb_plan_order_first_order_item */
	pb_plan_order_first_order_item = UxCreatePushButton( "pb_plan_order_first_order_item", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_order_item, "Order & Item ID" ),
	UxCreateWidget( pb_plan_order_first_order_item );


	/* Creation of pb_plan_order_first_insert_top */
	pb_plan_order_first_insert_top = UxCreatePushButton( "pb_plan_order_first_insert_top", rc_plan_order_first );
	UxPutLabelString( pb_plan_order_first_insert_top, "Insert Top" ),
	UxPutX( pb_plan_order_first_insert_top, 97 ),
	UxPutY( pb_plan_order_first_insert_top, 200 ),
	UxCreateWidget( pb_plan_order_first_insert_top );


	/* Creation of om_plan_order_first */
	om_plan_order_first = UxCreateRowColumn( "om_plan_order_first", bulletinBoard4 );
	UxPutRowColumnType( om_plan_order_first, "menu_option" ),
	UxPutX( om_plan_order_first, 12 ),
	UxPutY( om_plan_order_first, 14 ),
	UxPutWidth( om_plan_order_first, 60 ),
	UxPutHeight( om_plan_order_first, 54 ),
	UxPutLabelString( om_plan_order_first, "First    " ),
	UxPutSensitive( om_plan_order_first, "false" ),
	UxPutSubMenuId( om_plan_order_first, "rc_plan_order_first" ),
	UxCreateWidget( om_plan_order_first );


	/* Creation of rc_plan_order_second */
	rc_plan_order_second = UxCreateRowColumn( "rc_plan_order_second", bulletinBoard4 );
	UxPutRowColumnType( rc_plan_order_second, "menu_pulldown" ),
	UxPutSensitive( rc_plan_order_second, "false" ),
	UxPutX( rc_plan_order_second, 93 ),
	UxPutY( rc_plan_order_second, 0 ),
	UxCreateWidget( rc_plan_order_second );


	/* Creation of pb_plan_order_second_none */
	pb_plan_order_second_none = UxCreatePushButton( "pb_plan_order_second_none", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_none, "None" ),
	UxPutSensitive( pb_plan_order_second_none, "false" ),
	UxPutX( pb_plan_order_second_none, 97 ),
	UxPutY( pb_plan_order_second_none, 2 ),
	UxCreateWidget( pb_plan_order_second_none );


	/* Creation of pb_plan_order_second_order_type */
	pb_plan_order_second_order_type = UxCreatePushButton( "pb_plan_order_second_order_type", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_order_type, "Order Type" ),
	UxPutX( pb_plan_order_second_order_type, 97 ),
	UxPutY( pb_plan_order_second_order_type, 24 ),
	UxCreateWidget( pb_plan_order_second_order_type );


	/* Creation of pb_plan_order_second_priority */
	pb_plan_order_second_priority = UxCreatePushButton( "pb_plan_order_second_priority", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_priority, "Priority" ),
	UxPutX( pb_plan_order_second_priority, 97 ),
	UxPutY( pb_plan_order_second_priority, 46 ),
	UxCreateWidget( pb_plan_order_second_priority );


	/* Creation of pb_plan_order_second_age */
	pb_plan_order_second_age = UxCreatePushButton( "pb_plan_order_second_age", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_age, "Age (Days)" ),
	UxPutX( pb_plan_order_second_age, 97 ),
	UxPutY( pb_plan_order_second_age, 222 ),
	UxCreateWidget( pb_plan_order_second_age );


	/* Creation of pb_plan_order_second_sat_sens_rev */
	pb_plan_order_second_sat_sens_rev = UxCreatePushButton( "pb_plan_order_second_sat_sens_rev", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_plan_order_second_sat_sens_rev, 97 ),
	UxPutY( pb_plan_order_second_sat_sens_rev, 112 ),
	UxCreateWidget( pb_plan_order_second_sat_sens_rev );


	/* Creation of pb_plan_order_second_mode */
	pb_plan_order_second_mode = UxCreatePushButton( "pb_plan_order_second_mode", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_mode, "Mode" ),
	UxPutX( pb_plan_order_second_mode, 97 ),
	UxPutY( pb_plan_order_second_mode, 90 ),
	UxCreateWidget( pb_plan_order_second_mode );


	/* Creation of pb_plan_order_second_pixel_spacing */
	pb_plan_order_second_pixel_spacing = UxCreatePushButton( "pb_plan_order_second_pixel_spacing", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_pixel_spacing, "Pixel Spacing" ),
	UxPutX( pb_plan_order_second_pixel_spacing, 97 ),
	UxPutY( pb_plan_order_second_pixel_spacing, 134 ),
	UxCreateWidget( pb_plan_order_second_pixel_spacing );


	/* Creation of pb_plan_order_second_media_id */
	pb_plan_order_second_media_id = UxCreatePushButton( "pb_plan_order_second_media_id", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_media_id, "Media ID" ),
	UxPutX( pb_plan_order_second_media_id, 97 ),
	UxPutY( pb_plan_order_second_media_id, 68 ),
	UxCreateWidget( pb_plan_order_second_media_id );


	/* Creation of pb_plan_order_second_job_id */
	pb_plan_order_second_job_id = UxCreatePushButton( "pb_plan_order_second_job_id", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_job_id, "Job ID" ),
	UxPutX( pb_plan_order_second_job_id, 97 ),
	UxPutY( pb_plan_order_second_job_id, 156 ),
	UxCreateWidget( pb_plan_order_second_job_id );


	/* Creation of pb_plan_order_second_order_item */
	pb_plan_order_second_order_item = UxCreatePushButton( "pb_plan_order_second_order_item", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_order_item, "Order & Item ID" ),
	UxCreateWidget( pb_plan_order_second_order_item );


	/* Creation of pb_plan_order_second_insert_top */
	pb_plan_order_second_insert_top = UxCreatePushButton( "pb_plan_order_second_insert_top", rc_plan_order_second );
	UxPutLabelString( pb_plan_order_second_insert_top, "Insert Top" ),
	UxPutX( pb_plan_order_second_insert_top, 97 ),
	UxPutY( pb_plan_order_second_insert_top, 200 ),
	UxCreateWidget( pb_plan_order_second_insert_top );


	/* Creation of om_plan_order_second */
	om_plan_order_second = UxCreateRowColumn( "om_plan_order_second", bulletinBoard4 );
	UxPutRowColumnType( om_plan_order_second, "menu_option" ),
	UxPutX( om_plan_order_second, 11 ),
	UxPutY( om_plan_order_second, 67 ),
	UxPutWidth( om_plan_order_second, 60 ),
	UxPutHeight( om_plan_order_second, 54 ),
	UxPutLabelString( om_plan_order_second, "Second" ),
	UxPutSensitive( om_plan_order_second, "false" ),
	UxPutSubMenuId( om_plan_order_second, "rc_plan_order_second" ),
	UxCreateWidget( om_plan_order_second );


	/* Creation of rc_plan_order_third */
	rc_plan_order_third = UxCreateRowColumn( "rc_plan_order_third", bulletinBoard4 );
	UxPutRowColumnType( rc_plan_order_third, "menu_pulldown" ),
	UxPutSensitive( rc_plan_order_third, "false" ),
	UxPutX( rc_plan_order_third, 93 ),
	UxPutY( rc_plan_order_third, 0 ),
	UxCreateWidget( rc_plan_order_third );


	/* Creation of pb_plan_order_third_none */
	pb_plan_order_third_none = UxCreatePushButton( "pb_plan_order_third_none", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_none, "None" ),
	UxPutSensitive( pb_plan_order_third_none, "false" ),
	UxPutX( pb_plan_order_third_none, 97 ),
	UxPutY( pb_plan_order_third_none, 2 ),
	UxCreateWidget( pb_plan_order_third_none );


	/* Creation of pb_plan_order_third_order_type */
	pb_plan_order_third_order_type = UxCreatePushButton( "pb_plan_order_third_order_type", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_order_type, "Order Type" ),
	UxPutX( pb_plan_order_third_order_type, 97 ),
	UxPutY( pb_plan_order_third_order_type, 24 ),
	UxCreateWidget( pb_plan_order_third_order_type );


	/* Creation of pb_plan_order_third_priority */
	pb_plan_order_third_priority = UxCreatePushButton( "pb_plan_order_third_priority", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_priority, "Priority" ),
	UxPutX( pb_plan_order_third_priority, 97 ),
	UxPutY( pb_plan_order_third_priority, 46 ),
	UxCreateWidget( pb_plan_order_third_priority );


	/* Creation of pb_plan_order_third_age */
	pb_plan_order_third_age = UxCreatePushButton( "pb_plan_order_third_age", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_age, "Age (Days)" ),
	UxPutX( pb_plan_order_third_age, 97 ),
	UxPutY( pb_plan_order_third_age, 222 ),
	UxCreateWidget( pb_plan_order_third_age );


	/* Creation of pb_plan_order_third_sat_sens_rev */
	pb_plan_order_third_sat_sens_rev = UxCreatePushButton( "pb_plan_order_third_sat_sens_rev", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_plan_order_third_sat_sens_rev, 97 ),
	UxPutY( pb_plan_order_third_sat_sens_rev, 112 ),
	UxCreateWidget( pb_plan_order_third_sat_sens_rev );


	/* Creation of pb_plan_order_third_mode */
	pb_plan_order_third_mode = UxCreatePushButton( "pb_plan_order_third_mode", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_mode, "Mode" ),
	UxPutX( pb_plan_order_third_mode, 97 ),
	UxPutY( pb_plan_order_third_mode, 90 ),
	UxCreateWidget( pb_plan_order_third_mode );


	/* Creation of pb_plan_order_third_pixel_spacing */
	pb_plan_order_third_pixel_spacing = UxCreatePushButton( "pb_plan_order_third_pixel_spacing", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_pixel_spacing, "Pixel Spacing" ),
	UxPutX( pb_plan_order_third_pixel_spacing, 97 ),
	UxPutY( pb_plan_order_third_pixel_spacing, 178 ),
	UxCreateWidget( pb_plan_order_third_pixel_spacing );


	/* Creation of pb_plan_order_third_media_id */
	pb_plan_order_third_media_id = UxCreatePushButton( "pb_plan_order_third_media_id", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_media_id, "Media ID" ),
	UxPutX( pb_plan_order_third_media_id, 97 ),
	UxPutY( pb_plan_order_third_media_id, 68 ),
	UxCreateWidget( pb_plan_order_third_media_id );


	/* Creation of pb_plan_order_third_job_id */
	pb_plan_order_third_job_id = UxCreatePushButton( "pb_plan_order_third_job_id", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_job_id, "Job ID" ),
	UxPutX( pb_plan_order_third_job_id, 97 ),
	UxPutY( pb_plan_order_third_job_id, 156 ),
	UxCreateWidget( pb_plan_order_third_job_id );


	/* Creation of pb_plan_order_third_order_item */
	pb_plan_order_third_order_item = UxCreatePushButton( "pb_plan_order_third_order_item", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_order_item, "Order & Item ID" ),
	UxCreateWidget( pb_plan_order_third_order_item );


	/* Creation of pb_plan_order_third_insert_top */
	pb_plan_order_third_insert_top = UxCreatePushButton( "pb_plan_order_third_insert_top", rc_plan_order_third );
	UxPutLabelString( pb_plan_order_third_insert_top, "Insert Top" ),
	UxPutX( pb_plan_order_third_insert_top, 97 ),
	UxPutY( pb_plan_order_third_insert_top, 200 ),
	UxCreateWidget( pb_plan_order_third_insert_top );


	/* Creation of om_plan_order_third */
	om_plan_order_third = UxCreateRowColumn( "om_plan_order_third", bulletinBoard4 );
	UxPutRowColumnType( om_plan_order_third, "menu_option" ),
	UxPutX( om_plan_order_third, 11 ),
	UxPutY( om_plan_order_third, 120 ),
	UxPutWidth( om_plan_order_third, 60 ),
	UxPutHeight( om_plan_order_third, 54 ),
	UxPutLabelString( om_plan_order_third, "Third    " ),
	UxPutSensitive( om_plan_order_third, "false" ),
	UxPutSubMenuId( om_plan_order_third, "rc_plan_order_third" ),
	UxCreateWidget( om_plan_order_third );


	/* Creation of rc_plan_order_fourth */
	rc_plan_order_fourth = UxCreateRowColumn( "rc_plan_order_fourth", bulletinBoard4 );
	UxPutRowColumnType( rc_plan_order_fourth, "menu_pulldown" ),
	UxPutSensitive( rc_plan_order_fourth, "false" ),
	UxPutX( rc_plan_order_fourth, 93 ),
	UxPutY( rc_plan_order_fourth, 0 ),
	UxCreateWidget( rc_plan_order_fourth );


	/* Creation of pb_plan_order_fourth_none */
	pb_plan_order_fourth_none = UxCreatePushButton( "pb_plan_order_fourth_none", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_none, "None" ),
	UxPutSensitive( pb_plan_order_fourth_none, "false" ),
	UxPutX( pb_plan_order_fourth_none, 97 ),
	UxPutY( pb_plan_order_fourth_none, 2 ),
	UxCreateWidget( pb_plan_order_fourth_none );


	/* Creation of pb_plan_order_fourth_order_type */
	pb_plan_order_fourth_order_type = UxCreatePushButton( "pb_plan_order_fourth_order_type", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_order_type, "Order Type" ),
	UxPutX( pb_plan_order_fourth_order_type, 97 ),
	UxPutY( pb_plan_order_fourth_order_type, 24 ),
	UxCreateWidget( pb_plan_order_fourth_order_type );


	/* Creation of pb_plan_order_fourth_priority */
	pb_plan_order_fourth_priority = UxCreatePushButton( "pb_plan_order_fourth_priority", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_priority, "Priority" ),
	UxPutX( pb_plan_order_fourth_priority, 97 ),
	UxPutY( pb_plan_order_fourth_priority, 46 ),
	UxCreateWidget( pb_plan_order_fourth_priority );


	/* Creation of pb_plan_order_fourth_age */
	pb_plan_order_fourth_age = UxCreatePushButton( "pb_plan_order_fourth_age", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_age, "Age (Days)" ),
	UxPutX( pb_plan_order_fourth_age, 97 ),
	UxPutY( pb_plan_order_fourth_age, 222 ),
	UxCreateWidget( pb_plan_order_fourth_age );


	/* Creation of pb_plan_order_fourth_sat_sens_rev */
	pb_plan_order_fourth_sat_sens_rev = UxCreatePushButton( "pb_plan_order_fourth_sat_sens_rev", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_sat_sens_rev, "Datatake ID" ),
	UxPutX( pb_plan_order_fourth_sat_sens_rev, 97 ),
	UxPutY( pb_plan_order_fourth_sat_sens_rev, 112 ),
	UxCreateWidget( pb_plan_order_fourth_sat_sens_rev );


	/* Creation of pb_plan_order_fourth_mode */
	pb_plan_order_fourth_mode = UxCreatePushButton( "pb_plan_order_fourth_mode", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_mode, "Mode" ),
	UxPutX( pb_plan_order_fourth_mode, 97 ),
	UxPutY( pb_plan_order_fourth_mode, 90 ),
	UxCreateWidget( pb_plan_order_fourth_mode );


	/* Creation of pb_plan_order_fourth_pixel_spacing */
	pb_plan_order_fourth_pixel_spacing = UxCreatePushButton( "pb_plan_order_fourth_pixel_spacing", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_pixel_spacing, "Pixel Spacing" ),
	UxPutX( pb_plan_order_fourth_pixel_spacing, 97 ),
	UxPutY( pb_plan_order_fourth_pixel_spacing, 134 ),
	UxCreateWidget( pb_plan_order_fourth_pixel_spacing );


	/* Creation of pb_plan_order_fourth_media_id */
	pb_plan_order_fourth_media_id = UxCreatePushButton( "pb_plan_order_fourth_media_id", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_media_id, "Media ID" ),
	UxPutX( pb_plan_order_fourth_media_id, 97 ),
	UxPutY( pb_plan_order_fourth_media_id, 68 ),
	UxCreateWidget( pb_plan_order_fourth_media_id );


	/* Creation of pb_plan_order_fourth_job_id */
	pb_plan_order_fourth_job_id = UxCreatePushButton( "pb_plan_order_fourth_job_id", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_job_id, "Job ID" ),
	UxPutX( pb_plan_order_fourth_job_id, 97 ),
	UxPutY( pb_plan_order_fourth_job_id, 156 ),
	UxCreateWidget( pb_plan_order_fourth_job_id );


	/* Creation of pb_plan_order_fourth_order_item */
	pb_plan_order_fourth_order_item = UxCreatePushButton( "pb_plan_order_fourth_order_item", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_order_item, "Order & Item ID" ),
	UxCreateWidget( pb_plan_order_fourth_order_item );


	/* Creation of pb_plan_order_fourth_insert_top */
	pb_plan_order_fourth_insert_top = UxCreatePushButton( "pb_plan_order_fourth_insert_top", rc_plan_order_fourth );
	UxPutLabelString( pb_plan_order_fourth_insert_top, "Insert Top" ),
	UxPutX( pb_plan_order_fourth_insert_top, 97 ),
	UxPutY( pb_plan_order_fourth_insert_top, 178 ),
	UxCreateWidget( pb_plan_order_fourth_insert_top );


	/* Creation of om_plan_order_fourth */
	om_plan_order_fourth = UxCreateRowColumn( "om_plan_order_fourth", bulletinBoard4 );
	UxPutRowColumnType( om_plan_order_fourth, "menu_option" ),
	UxPutX( om_plan_order_fourth, 11 ),
	UxPutY( om_plan_order_fourth, 175 ),
	UxPutWidth( om_plan_order_fourth, 60 ),
	UxPutHeight( om_plan_order_fourth, 54 ),
	UxPutLabelString( om_plan_order_fourth, "Fourth  " ),
	UxPutSensitive( om_plan_order_fourth, "false" ),
	UxPutSubMenuId( om_plan_order_fourth, "rc_plan_order_fourth" ),
	UxCreateWidget( om_plan_order_fourth );


	/* Creation of l_plan_order_by */
	l_plan_order_by = UxCreateLabel( "l_plan_order_by", pps_plan );
	UxPutX( l_plan_order_by, 894 ),
	UxPutY( l_plan_order_by, 35 ),
	UxPutWidth( l_plan_order_by, 71 ),
	UxPutHeight( l_plan_order_by, 24 ),
	UxPutLabelString( l_plan_order_by, "Order By" ),
	UxCreateWidget( l_plan_order_by );


	/* Creation of f_plan_items_ready */
	f_plan_items_ready = UxCreateFrame( "f_plan_items_ready", pps_plan );
	UxPutWidth( f_plan_items_ready, 260 ),
	UxPutHeight( f_plan_items_ready, 56 ),
	UxPutX( f_plan_items_ready, 870 ),
	UxPutY( f_plan_items_ready, 484 ),
	UxCreateWidget( f_plan_items_ready );


	/* Creation of bb_plan_items_ready */
	bb_plan_items_ready = UxCreateBulletinBoard( "bb_plan_items_ready", f_plan_items_ready );
	UxPutResizePolicy( bb_plan_items_ready, "resize_none" ),
	UxPutWidth( bb_plan_items_ready, 276 ),
	UxPutHeight( bb_plan_items_ready, 64 ),
	UxPutX( bb_plan_items_ready, 2 ),
	UxPutY( bb_plan_items_ready, 2 ),
	UxPutMarginHeight( bb_plan_items_ready, 0 ),
	UxPutMarginWidth( bb_plan_items_ready, 0 ),
	UxCreateWidget( bb_plan_items_ready );


	/* Creation of l_query_items_found1 */
	l_query_items_found1 = UxCreateLabel( "l_query_items_found1", bb_plan_items_ready );
	UxPutX( l_query_items_found1, 50 ),
	UxPutY( l_query_items_found1, 0 ),
	UxPutWidth( l_query_items_found1, 108 ),
	UxPutHeight( l_query_items_found1, 22 ),
	UxPutLabelString( l_query_items_found1, "Items Displayed" ),
	UxCreateWidget( l_query_items_found1 );


	/* Creation of l_plan_ready_num_displayed */
	l_plan_ready_num_displayed = UxCreateLabel( "l_plan_ready_num_displayed", bb_plan_items_ready );
	UxPutX( l_plan_ready_num_displayed, 170 ),
	UxPutY( l_plan_ready_num_displayed, 0 ),
	UxPutWidth( l_plan_ready_num_displayed, 60 ),
	UxPutHeight( l_plan_ready_num_displayed, 20 ),
	UxPutLabelString( l_plan_ready_num_displayed, "9999" ),
	UxPutAlignment( l_plan_ready_num_displayed, "alignment_beginning" ),
	UxPutRecomputeSize( l_plan_ready_num_displayed, "false" ),
	UxCreateWidget( l_plan_ready_num_displayed );


	/* Creation of l_query_items_found4 */
	l_query_items_found4 = UxCreateLabel( "l_query_items_found4", bb_plan_items_ready );
	UxPutX( l_query_items_found4, 50 ),
	UxPutY( l_query_items_found4, 30 ),
	UxPutWidth( l_query_items_found4, 108 ),
	UxPutHeight( l_query_items_found4, 22 ),
	UxPutLabelString( l_query_items_found4, "Items Selected" ),
	UxCreateWidget( l_query_items_found4 );


	/* Creation of l_plan_ready_num_selected */
	l_plan_ready_num_selected = UxCreateLabel( "l_plan_ready_num_selected", bb_plan_items_ready );
	UxPutX( l_plan_ready_num_selected, 170 ),
	UxPutY( l_plan_ready_num_selected, 30 ),
	UxPutWidth( l_plan_ready_num_selected, 46 ),
	UxPutHeight( l_plan_ready_num_selected, 19 ),
	UxPutLabelString( l_plan_ready_num_selected, "9999" ),
	UxPutAlignment( l_plan_ready_num_selected, "alignment_beginning" ),
	UxPutRecomputeSize( l_plan_ready_num_selected, "false" ),
	UxCreateWidget( l_plan_ready_num_selected );


	/* Creation of f_plan_available */
	f_plan_available = UxCreateFrame( "f_plan_available", pps_plan );
	UxPutWidth( f_plan_available, 255 ),
	UxPutHeight( f_plan_available, 170 ),
	UxPutX( f_plan_available, 871 ),
	UxPutY( f_plan_available, 616 ),
	UxCreateWidget( f_plan_available );


	/* Creation of bb_plan_available */
	bb_plan_available = UxCreateBulletinBoard( "bb_plan_available", f_plan_available );
	UxPutResizePolicy( bb_plan_available, "resize_none" ),
	UxPutWidth( bb_plan_available, 248 ),
	UxPutHeight( bb_plan_available, 168 ),
	UxPutX( bb_plan_available, 2 ),
	UxPutY( bb_plan_available, 42 ),
	UxPutMarginHeight( bb_plan_available, 0 ),
	UxPutMarginWidth( bb_plan_available, 0 ),
	UxCreateWidget( bb_plan_available );


	/* Creation of pb_plan_available_top */
	pb_plan_available_top = UxCreatePushButton( "pb_plan_available_top", bb_plan_available );
	UxPutX( pb_plan_available_top, 8 ),
	UxPutY( pb_plan_available_top, 12 ),
	UxPutWidth( pb_plan_available_top, 112 ),
	UxPutHeight( pb_plan_available_top, 36 ),
	UxPutLabelString( pb_plan_available_top, "TOP of Plan" ),
	UxPutSensitive( pb_plan_available_top, "true" ),
	UxCreateWidget( pb_plan_available_top );

	UxAddCallback( pb_plan_available_top, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_top,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_available_IT_on */
	pb_plan_available_IT_on = UxCreatePushButton( "pb_plan_available_IT_on", bb_plan_available );
	UxPutX( pb_plan_available_IT_on, 130 ),
	UxPutY( pb_plan_available_IT_on, 10 ),
	UxPutWidth( pb_plan_available_IT_on, 112 ),
	UxPutHeight( pb_plan_available_IT_on, 36 ),
	UxPutLabelString( pb_plan_available_IT_on, "Insert Top On" ),
	UxPutSensitive( pb_plan_available_IT_on, "true" ),
	UxCreateWidget( pb_plan_available_IT_on );

	UxAddCallback( pb_plan_available_IT_on, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_IT_on,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_available_IT_off */
	pb_plan_available_IT_off = UxCreatePushButton( "pb_plan_available_IT_off", bb_plan_available );
	UxPutX( pb_plan_available_IT_off, 130 ),
	UxPutY( pb_plan_available_IT_off, 60 ),
	UxPutWidth( pb_plan_available_IT_off, 112 ),
	UxPutHeight( pb_plan_available_IT_off, 36 ),
	UxPutLabelString( pb_plan_available_IT_off, "Insert Top Off" ),
	UxPutSensitive( pb_plan_available_IT_off, "true" ),
	UxCreateWidget( pb_plan_available_IT_off );

	UxAddCallback( pb_plan_available_IT_off, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_IT_off,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_available_remove */
	pb_plan_available_remove = UxCreatePushButton( "pb_plan_available_remove", bb_plan_available );
	UxPutX( pb_plan_available_remove, 8 ),
	UxPutY( pb_plan_available_remove, 118 ),
	UxPutWidth( pb_plan_available_remove, 119 ),
	UxPutHeight( pb_plan_available_remove, 36 ),
	UxPutLabelString( pb_plan_available_remove, "Remove from Plan" ),
	UxPutSensitive( pb_plan_available_remove, "true" ),
	UxCreateWidget( pb_plan_available_remove );

	UxAddCallback( pb_plan_available_remove, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_remove,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_available_bottom */
	pb_plan_available_bottom = UxCreatePushButton( "pb_plan_available_bottom", bb_plan_available );
	UxPutX( pb_plan_available_bottom, 8 ),
	UxPutY( pb_plan_available_bottom, 64 ),
	UxPutWidth( pb_plan_available_bottom, 112 ),
	UxPutHeight( pb_plan_available_bottom, 36 ),
	UxPutLabelString( pb_plan_available_bottom, "BOTTOM of Plan" ),
	UxPutSensitive( pb_plan_available_bottom, "true" ),
	UxCreateWidget( pb_plan_available_bottom );

	UxAddCallback( pb_plan_available_bottom, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_bottom,
		(XtPointer) UxPps_planContext );


	/* Creation of f_plan_items_found */
	f_plan_items_found = UxCreateFrame( "f_plan_items_found", pps_plan );
	UxPutWidth( f_plan_items_found, 245 ),
	UxPutHeight( f_plan_items_found, 60 ),
	UxPutX( f_plan_items_found, 871 ),
	UxPutY( f_plan_items_found, 800 ),
	UxCreateWidget( f_plan_items_found );


	/* Creation of bb_plan_items_available */
	bb_plan_items_available = UxCreateBulletinBoard( "bb_plan_items_available", f_plan_items_found );
	UxPutResizePolicy( bb_plan_items_available, "resize_none" ),
	UxPutWidth( bb_plan_items_available, 248 ),
	UxPutHeight( bb_plan_items_available, 64 ),
	UxPutX( bb_plan_items_available, 30 ),
	UxPutY( bb_plan_items_available, 0 ),
	UxPutMarginHeight( bb_plan_items_available, 0 ),
	UxPutMarginWidth( bb_plan_items_available, 0 ),
	UxCreateWidget( bb_plan_items_available );


	/* Creation of l_query_items_found2 */
	l_query_items_found2 = UxCreateLabel( "l_query_items_found2", bb_plan_items_available );
	UxPutX( l_query_items_found2, 50 ),
	UxPutY( l_query_items_found2, 0 ),
	UxPutWidth( l_query_items_found2, 95 ),
	UxPutHeight( l_query_items_found2, 22 ),
	UxPutLabelString( l_query_items_found2, "Items Planned" ),
	UxCreateWidget( l_query_items_found2 );


	/* Creation of l_plan_available_num_displayed */
	l_plan_available_num_displayed = UxCreateLabel( "l_plan_available_num_displayed", bb_plan_items_available );
	UxPutX( l_plan_available_num_displayed, 170 ),
	UxPutY( l_plan_available_num_displayed, 0 ),
	UxPutWidth( l_plan_available_num_displayed, 46 ),
	UxPutHeight( l_plan_available_num_displayed, 19 ),
	UxPutLabelString( l_plan_available_num_displayed, "9999" ),
	UxPutAlignment( l_plan_available_num_displayed, "alignment_beginning" ),
	UxPutRecomputeSize( l_plan_available_num_displayed, "false" ),
	UxCreateWidget( l_plan_available_num_displayed );


	/* Creation of l_query_items_found7 */
	l_query_items_found7 = UxCreateLabel( "l_query_items_found7", bb_plan_items_available );
	UxPutX( l_query_items_found7, 50 ),
	UxPutY( l_query_items_found7, 30 ),
	UxPutWidth( l_query_items_found7, 108 ),
	UxPutHeight( l_query_items_found7, 22 ),
	UxPutLabelString( l_query_items_found7, "Items Selected" ),
	UxCreateWidget( l_query_items_found7 );


	/* Creation of l_plan_available_num_selected */
	l_plan_available_num_selected = UxCreateLabel( "l_plan_available_num_selected", bb_plan_items_available );
	UxPutX( l_plan_available_num_selected, 170 ),
	UxPutY( l_plan_available_num_selected, 30 ),
	UxPutWidth( l_plan_available_num_selected, 46 ),
	UxPutHeight( l_plan_available_num_selected, 19 ),
	UxPutLabelString( l_plan_available_num_selected, "9999" ),
	UxPutAlignment( l_plan_available_num_selected, "alignment_beginning" ),
	UxPutRecomputeSize( l_plan_available_num_selected, "false" ),
	UxCreateWidget( l_plan_available_num_selected );


	/* Creation of pb_plan_ready_select_all */
	pb_plan_ready_select_all = UxCreatePushButton( "pb_plan_ready_select_all", pps_plan );
	UxPutX( pb_plan_ready_select_all, 610 ),
	UxPutY( pb_plan_ready_select_all, 310 ),
	UxPutWidth( pb_plan_ready_select_all, 119 ),
	UxPutHeight( pb_plan_ready_select_all, 27 ),
	UxPutLabelString( pb_plan_ready_select_all, "Select All" ),
	UxPutSensitive( pb_plan_ready_select_all, "false" ),
	UxCreateWidget( pb_plan_ready_select_all );

	UxAddCallback( pb_plan_ready_select_all, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_ready_select_all,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_ready_deselect_all */
	pb_plan_ready_deselect_all = UxCreatePushButton( "pb_plan_ready_deselect_all", pps_plan );
	UxPutX( pb_plan_ready_deselect_all, 730 ),
	UxPutY( pb_plan_ready_deselect_all, 310 ),
	UxPutWidth( pb_plan_ready_deselect_all, 119 ),
	UxPutHeight( pb_plan_ready_deselect_all, 27 ),
	UxPutLabelString( pb_plan_ready_deselect_all, "Deselect All" ),
	UxPutSensitive( pb_plan_ready_deselect_all, "false" ),
	UxCreateWidget( pb_plan_ready_deselect_all );

	UxAddCallback( pb_plan_ready_deselect_all, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_ready_deselect_all,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_available_select_all */
	pb_plan_available_select_all = UxCreatePushButton( "pb_plan_available_select_all", pps_plan );
	UxPutX( pb_plan_available_select_all, 620 ),
	UxPutY( pb_plan_available_select_all, 590 ),
	UxPutWidth( pb_plan_available_select_all, 102 ),
	UxPutHeight( pb_plan_available_select_all, 27 ),
	UxPutLabelString( pb_plan_available_select_all, "Select All" ),
	UxPutSensitive( pb_plan_available_select_all, "true" ),
	UxCreateWidget( pb_plan_available_select_all );

	UxAddCallback( pb_plan_available_select_all, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_select_all,
		(XtPointer) UxPps_planContext );


	/* Creation of pb_plan_available_deselect_all */
	pb_plan_available_deselect_all = UxCreatePushButton( "pb_plan_available_deselect_all", pps_plan );
	UxPutX( pb_plan_available_deselect_all, 728 ),
	UxPutY( pb_plan_available_deselect_all, 590 ),
	UxPutWidth( pb_plan_available_deselect_all, 119 ),
	UxPutHeight( pb_plan_available_deselect_all, 27 ),
	UxPutLabelString( pb_plan_available_deselect_all, "Deselect All" ),
	UxPutSensitive( pb_plan_available_deselect_all, "true" ),
	UxCreateWidget( pb_plan_available_deselect_all );

	UxAddCallback( pb_plan_available_deselect_all, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_deselect_all,
		(XtPointer) UxPps_planContext );


	/* Creation of l_plan_modify_ready */
	l_plan_modify_ready = UxCreateLabel( "l_plan_modify_ready", pps_plan );
	UxPutX( l_plan_modify_ready, 930 ),
	UxPutY( l_plan_modify_ready, 310 ),
	UxPutWidth( l_plan_modify_ready, 131 ),
	UxPutHeight( l_plan_modify_ready, 24 ),
	UxPutLabelString( l_plan_modify_ready, "Update Ready Jobs" ),
	UxCreateWidget( l_plan_modify_ready );


	/* Creation of l_plan_modify_available */
	l_plan_modify_available = UxCreateLabel( "l_plan_modify_available", pps_plan );
	UxPutX( l_plan_modify_available, 940 ),
	UxPutY( l_plan_modify_available, 590 ),
	UxPutWidth( l_plan_modify_available, 140 ),
	UxPutHeight( l_plan_modify_available, 20 ),
	UxPutLabelString( l_plan_modify_available, "Update Planned Jobs" ),
	UxCreateWidget( l_plan_modify_available );


	/* Creation of pb_plan_available_refresh */
	pb_plan_available_refresh = UxCreatePushButton( "pb_plan_available_refresh", pps_plan );
	UxPutX( pb_plan_available_refresh, 512 ),
	UxPutY( pb_plan_available_refresh, 590 ),
	UxPutWidth( pb_plan_available_refresh, 102 ),
	UxPutHeight( pb_plan_available_refresh, 27 ),
	UxPutLabelString( pb_plan_available_refresh, "Refresh" ),
	UxPutSensitive( pb_plan_available_refresh, "true" ),
	UxCreateWidget( pb_plan_available_refresh );

	UxAddCallback( pb_plan_available_refresh, XmNactivateCallback,
		(XtCallbackProc) activateCB_pb_plan_available_refresh,
		(XtPointer) UxPps_planContext );

	UxAddCallback( pps_plan, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxPps_planContext);



	/* UxRealizeInterface creates the X windows for the widgets above. */

	UxRealizeInterface( pps_plan );

	return ( pps_plan );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

swidget	create_pps_plan( swidget _UxUxParent )
{
	swidget                 rtrn;
	_UxCpps_plan            *UxContext;
	static int		_Uxinit = 0;

	UxPps_planContext = UxContext =
		(_UxCpps_plan *) UxNewContext( sizeof(_UxCpps_plan), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_pps_plan();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/


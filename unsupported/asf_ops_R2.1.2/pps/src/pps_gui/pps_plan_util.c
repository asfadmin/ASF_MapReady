/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static char SccsFileId[] = "@(#)pps_plan_util.c	1.11    10/31/97";

/* support routines for various callbacks for PPS GUI */

/*---------------------------------------------------*/
/* 1/26/96: Sally Chou                               */
/*      added priority_num to sort priority          */
/*      by types instead of alphabets.               */
/*---------------------------------------------------*/

#include <unistd.h>
#include <ctype.h>

#include "UxLib.h"
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <X11/cursorfont.h>

#include "defs.h"
#include "PPShdr.h"
#include "PPSerr.h"
#include "pps.h"
#include "pps_check_params.h"
#include "pps_plan.h"
#include "pps_file_selector.h"
#include "pps_util.h"
#include "pps_db.h"
#include "resload.h"

extern CS_CONNECTION *query_connection;
extern CS_CONNECTION *exec_connection;

/* defines for ppsgui_orders plan items */
#define	MAX_JOB_TYPE_CHARS		4
#define	MAX_QUICKLOOK_FLAG_CHARS	4
#define	MAX_PRIORITY_CHARS		10
#define	MAX_MEDIA_ID_CHARS		12
#define	MAX_MODE_CHARS			3
#define	MAX_PLATFORM_CHARS		2
#define	MAX_SENSOR_CHARS		3
#define	MAX_JOB_STATE_CHARS		15
#define	MAX_INSERT_TOP_FLAG_CHARS	4

/* summary counters */
int ready_num_displayed;
int ready_num_selected;
int available_num_displayed;
int available_num_selected;

/* job context */
struct job_context_dcl {
	int	job_id;
	char	insert_top_flag[MAX_INSERT_TOP_FLAG_CHARS+1];
	char	quicklook_flag[MAX_QUICKLOOK_FLAG_CHARS+1];
};

#define MAX_READY_JOBS			9999
#define MAX_AVAILABLE_JOBS		9999

struct job_context_dcl ready_jobs[MAX_READY_JOBS];
struct job_context_dcl available_jobs[MAX_AVAILABLE_JOBS];

/* plan file selector context */
char plan_filename[MAXSMALLBUF];

/* buffer to hold the last successful query string */
char queryBuf[MAXBIGBUF];
extern GlobalData globalData;
 
/* action proc for XmList select all */
XtActionProc selectAllActionProc = 0;

extern char	ProgName[];

/* a prototype or two */
void plan_load_query(char *fname);
void do_plan_available_query(void);
static void send_new_status_to_ims(int job_id, char	*job_type, char	*new_state);

/* structure for rows returned from ppsgui_orders plan */
struct plan_row_dcl {
	char		job_type[MAX_JOB_TYPE_CHARS+1];
	char		quicklook_flag[MAX_QUICKLOOK_FLAG_CHARS+1];
	char		priority[MAX_PRIORITY_CHARS+1];
	int		priority_num;
	int		age;
	char		platform[MAX_PLATFORM_CHARS+1];
	char		sensor[MAX_SENSOR_CHARS+1];
	int		rev;
	int		sequence;
	char		mode[MAX_MODE_CHARS+1];
	float		pixel_spacing;
	char		media_id[MAX_MEDIA_ID_CHARS+1];
	int		job_id;
	char		insert_top_flag[MAX_INSERT_TOP_FLAG_CHARS+1];
        int             order_id;
        int             item_id;
} plan_row;

/* plan query context structure */
struct pps_db_exec_dcl plan_query;

/* plan exec context structure */
struct pps_db_exec_dcl plan_exec;

/* structure for Order By option menu strings and clauses */
struct order_by_dcl {
	char *label;
	char *clause;
};
struct order_by_dcl plan_order[] = {
	"Order Type",		"ppsgui_orders.job_type, ppsgui_orders.quicklook_flag",
	"Priority",		"ppsgui_orders.priority_num",
	"Age (Days)",		"ppsgui_orders.state_date",
	"Datatake ID",		"ppsgui_orders.platform, ppsgui_orders.sensor, ppsgui_orders.rev, ppsgui_orders.sequence",
	"Mode",			"ppsgui_orders.mode",
	"Pixel Spacing",	"ppsgui_orders.pixel_spacing",
	"Media ID",		"ppsgui_orders.media_id",
	"Job ID",		"ppsgui_orders.job_id",
        "Order & Item ID",      "ppsgui_orders.order_id, ppsgui_orders.item_id",
	"Insert Top",		"ppsgui_orders.insert_top_flag",
	NULL,			NULL
};

/* struct for defining which resources to save and load */
struct plan_settings_dcl {

	swidget *sw;	/* swidget id */

	/* flags to select the resources to save - note that you don't
	   necessarily want to always save, say, the LabelString when
	   your widget is a label! */

	int tb;		/* Togglebutton?	XmToggleButtonGetState */
	int om;		/* OptionMenu?		UxGetMenuHistory */
	int tf;		/* TextField?		UxGetText */
};
struct plan_settings_dcl plan_settings[] = {
	&tb_plan_L1_Orders,			1, 0, 0,
	&tb_plan_L1_QLK,			1, 0, 0,
	&tb_plan_Scan_Orders,			1, 0, 0,
	&tb_plan_Scan_QLK,			1, 0, 0,
	&om_plan_sat,				0, 1, 0,
	&om_plan_sens,				0, 1, 0,
	&om_plan_activity,			0, 1, 0,
	&om_plan_station,			0, 1, 0,
	&om_plan_priority,			0, 1, 0,
	&tf_plan_rev,				0, 0, 1,
	&tf_plan_seq,				0, 0, 1,
	&tf_plan_job_id,			0, 0, 1,
	&tf_plan_order_id,			0, 0, 1,
	&tf_plan_item_id,			0, 0, 1,
	&tf_plan_frame_id,			0, 0, 1,
	&tf_plan_subframe_id,			0, 0, 1,
	&om_plan_product_type,			0, 1, 0,
	&om_plan_data_direction,                0, 1, 0,
	&om_plan_processor_mode,		0, 1, 0,
	&om_plan_media_type,			0, 1, 0,
	&tf_plan_media_id,			0, 0, 1,
	&om_plan_order_first,			0, 1, 0,
	&om_plan_order_second,			0, 1, 0,
	&om_plan_order_third,			0, 1, 0,
	&om_plan_order_fourth,			0, 1, 0,
	NULL,					0, 0, 0
};

extern Widget create_print_results_dialog(Widget parent,
				XmString dialogTitle, char* filename);

void
get_select_all_action(void)
{
	int				i=0;
	XtActionList	actionTable=0;
	Cardinal		numActions=0;

	if (selectAllActionProc)
		return;
	
	XtGetActionList(xmListWidgetClass, &actionTable, &numActions);
	for (i=0; i < numActions; i++)
	{
		if (strcmp(actionTable[i].string, "ListKbdSelectAll") == 0)
			selectAllActionProc = actionTable[i].proc;
	}

	if (actionTable)
		XtFree((char*)actionTable);

	if (selectAllActionProc == 0)
	{
		fprintf(stderr, "Internal Error: no select all action\n");
		return;
	}

}/* get_select_all_action */

/* update screen totals */
update_plan_totals()
{
	char buf[MAXSMALLBUF];

	sprintf(buf, "%5d", ready_num_displayed);
	UxPutLabelString(l_plan_ready_num_displayed, buf);
	sprintf(buf, "%5d", ready_num_selected);
	UxPutLabelString(l_plan_ready_num_selected, buf);

	sprintf(buf, "%5d", available_num_displayed);
	UxPutLabelString(l_plan_available_num_displayed, buf);
	sprintf(buf, "%5d", available_num_selected);
	UxPutLabelString(l_plan_available_num_selected, buf);
}

/* callback code for main screen plan button */
void cb_main_plan()
{
	char buf[10];

	/* clear the plan ready and available areas */
	XmListDeleteAllItems(UxGetWidget(sw_plan_ready_list));
	XmListDeleteAllItems(UxGetWidget(sw_plan_available_list));

	/* deselect all order types */
	XmToggleButtonSetState(UxGetWidget(tb_plan_L1_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_plan_L1_QLK), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_plan_Scan_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_plan_Scan_QLK), False, True);

	/* no items currently */
	ready_num_displayed = 0;
	ready_num_selected = 0;
	available_num_displayed = 0;
	available_num_selected = 0;
	update_plan_totals();

	/* initialize the rest of the plan settings */
	UxPutMenuHistory(om_plan_sat, pb_plan_sat_any);
	UxPutMenuHistory(om_plan_sens, pb_plan_sens_any);
	UxPutText(tf_plan_rev, "");
	UxPutText(tf_plan_seq, "");
	UxPutMenuHistory(om_plan_activity, pb_plan_activity_any);
	UxPutMenuHistory(om_plan_station, pb_plan_station_any);
	UxPutText(tf_plan_job_id, "");
	UxPutText(tf_plan_order_id, "");
	UxPutText(tf_plan_item_id, "");
	UxPutMenuHistory(om_plan_priority, pb_plan_priority_any);
	UxPutMenuHistory(om_plan_data_direction, pb_plan_data_direction_any);
	UxPutText(tf_plan_frame_id, "");
	UxPutText(tf_plan_subframe_id, "");
	UxPutMenuHistory(om_plan_product_type, pb_plan_product_type_any);
	UxPutMenuHistory(om_plan_processor_mode, pb_plan_processor_mode_any);
	UxPutMenuHistory(om_plan_media_type, pb_plan_media_type_any);
	UxPutText(tf_plan_media_id, "");
	UxPutMenuHistory(om_plan_order_first, pb_plan_order_first_none);
	UxPutMenuHistory(om_plan_order_second, pb_plan_order_second_none);
	UxPutMenuHistory(om_plan_order_third, pb_plan_order_third_none);
	UxPutMenuHistory(om_plan_order_fourth, pb_plan_order_fourth_none);

	/* pop up the interface */
	UxPopupInterface(pps_plan, no_grab);

	XMapRaised(XtDisplay(XtParent(UxGetWidget(pps_plan))),
				XtWindow(XtParent(UxGetWidget(pps_plan))));

	/* get the current plan */
	do_plan_available_query();

	/* load the default plan, if any */
	plan_load_query("default.ppf");
}

/* common code for plan screen order type togglebutton callbacks */
void do_plan_new_order_type()
{
	int L1, Scan;

	L1 = XmToggleButtonGetState(UxGetWidget(tb_plan_L1_Orders)) ||
		XmToggleButtonGetState(UxGetWidget(tb_plan_L1_QLK));
	Scan = XmToggleButtonGetState(UxGetWidget(tb_plan_Scan_Orders)) ||
		XmToggleButtonGetState(UxGetWidget(tb_plan_Scan_QLK));

	if (Scan) {
		XtSetSensitive(UxGetWidget(om_plan_sat), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_e1), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_e2), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_j1), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_r1), True);
		XtSetSensitive(UxGetWidget(rc_plan_sat), True);
		XtSetSensitive(UxGetWidget(om_plan_sens), True);
		XtSetSensitive(UxGetWidget(pb_plan_sens_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_sens_s), True);
		XtSetSensitive(UxGetWidget(rc_plan_sens), True);
		XtSetSensitive(UxGetWidget(om_plan_activity), True);
		XtSetSensitive(UxGetWidget(pb_plan_activity_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_activity_rlt), True);
		XtSetSensitive(UxGetWidget(pb_plan_activity_dmp), True);
		XtSetSensitive(UxGetWidget(rc_plan_activity), True);
		XtSetSensitive(UxGetWidget(om_plan_station), True);
		XtSetSensitive(UxGetWidget(pb_plan_station_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_station_fa), True);
		XtSetSensitive(UxGetWidget(pb_plan_station_mc), True);
		XtSetSensitive(UxGetWidget(rc_plan_station), True);
		XtSetSensitive(UxGetWidget(om_plan_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_low), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_routine), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_high), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_urgent), True);
		XtSetSensitive(UxGetWidget(rc_plan_priority), True);
		XtSetSensitive(UxGetWidget(om_plan_data_direction), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_forward), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_reverse), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_unknown), True);
		XtSetSensitive(UxGetWidget(rc_plan_data_direction), True);
		XtSetSensitive(UxGetWidget(l_plan_rev), True);
		XtSetSensitive(UxGetWidget(tf_plan_rev), True);
		XtSetSensitive(UxGetWidget(l_plan_seq), True);
		XtSetSensitive(UxGetWidget(tf_plan_seq), True);
		XtSetSensitive(UxGetWidget(l_plan_job_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_job_id), True);
		XtSetSensitive(UxGetWidget(l_plan_order_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_order_id), True);
		XtSetSensitive(UxGetWidget(l_plan_item_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_item_id), True);
		XtSetSensitive(UxGetWidget(l_plan_frame_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_frame_id), False);
		XtSetSensitive(UxGetWidget(l_plan_subframe_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_subframe_id), False);
		XtSetSensitive(UxGetWidget(om_plan_product_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_standard), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_complex), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_ccsd), False);
		XtSetSensitive(UxGetWidget(rc_plan_product_type), False);
		XtSetSensitive(UxGetWidget(om_plan_processor_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_continuous), True);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_scansar), True);
		XtSetSensitive(UxGetWidget(rc_plan_processor_mode), True);
		XtSetSensitive(UxGetWidget(om_plan_media_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_disk), True);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_dcrsi), True);
		XtSetSensitive(UxGetWidget(rc_plan_media_type), True);
		XtSetSensitive(UxGetWidget(l_plan_media_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_query), True);
		XtSetSensitive(UxGetWidget(pb_plan_ready_select_all), True);
		XtSetSensitive(UxGetWidget(pb_plan_ready_deselect_all), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_first), True);
		XtSetSensitive(UxGetWidget(om_plan_order_first), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_job_id), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_second), True);
		XtSetSensitive(UxGetWidget(om_plan_order_second), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_job_id), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_third), True);
		XtSetSensitive(UxGetWidget(om_plan_order_third), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_job_id), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_fourth), True);
		XtSetSensitive(UxGetWidget(om_plan_order_fourth), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_job_id), True);
	} else if (L1) {
		XtSetSensitive(UxGetWidget(om_plan_sat), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_e1), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_e2), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_j1), True);
		XtSetSensitive(UxGetWidget(pb_plan_sat_r1), True);
		XtSetSensitive(UxGetWidget(rc_plan_sat), True);
		XtSetSensitive(UxGetWidget(om_plan_sens), True);
		XtSetSensitive(UxGetWidget(pb_plan_sens_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_sens_s), True);
		XtSetSensitive(UxGetWidget(rc_plan_sens), True);
		XtSetSensitive(UxGetWidget(om_plan_activity), True);
		XtSetSensitive(UxGetWidget(pb_plan_activity_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_activity_rlt), True);
		XtSetSensitive(UxGetWidget(pb_plan_activity_dmp), True);
		XtSetSensitive(UxGetWidget(rc_plan_activity), True);
		XtSetSensitive(UxGetWidget(om_plan_station), True);
		XtSetSensitive(UxGetWidget(pb_plan_station_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_station_fa), True);
		XtSetSensitive(UxGetWidget(pb_plan_station_mc), True);
		XtSetSensitive(UxGetWidget(rc_plan_station), True);
		XtSetSensitive(UxGetWidget(om_plan_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_low), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_routine), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_high), True);
		XtSetSensitive(UxGetWidget(pb_plan_priority_urgent), True);
		XtSetSensitive(UxGetWidget(rc_plan_priority), True);
		XtSetSensitive(UxGetWidget(om_plan_data_direction), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_forward), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_reverse), True);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_unknown), True);
		XtSetSensitive(UxGetWidget(rc_plan_data_direction), True);
		XtSetSensitive(UxGetWidget(l_plan_rev), True);
		XtSetSensitive(UxGetWidget(tf_plan_rev), True);
		XtSetSensitive(UxGetWidget(l_plan_seq), True);
		XtSetSensitive(UxGetWidget(tf_plan_seq), True);
		XtSetSensitive(UxGetWidget(l_plan_job_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_job_id), True);
		XtSetSensitive(UxGetWidget(l_plan_order_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_order_id), True);
		XtSetSensitive(UxGetWidget(l_plan_item_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_item_id), True);
		XtSetSensitive(UxGetWidget(l_plan_frame_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_frame_id), True);
		XtSetSensitive(UxGetWidget(l_plan_subframe_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_subframe_id), True);
		XtSetSensitive(UxGetWidget(om_plan_product_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_standard), True);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_complex), True);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_ccsd), True);
		XtSetSensitive(UxGetWidget(rc_plan_product_type), True);
		XtSetSensitive(UxGetWidget(om_plan_processor_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_continuous), True);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_scansar), True);
		XtSetSensitive(UxGetWidget(rc_plan_processor_mode), True);
		XtSetSensitive(UxGetWidget(om_plan_media_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_any), True);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_disk), True);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_dcrsi), True);
		XtSetSensitive(UxGetWidget(rc_plan_media_type), True);
		XtSetSensitive(UxGetWidget(l_plan_media_id), True);
		XtSetSensitive(UxGetWidget(tf_plan_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_query), True);
		XtSetSensitive(UxGetWidget(pb_plan_ready_select_all), True);
		XtSetSensitive(UxGetWidget(pb_plan_ready_deselect_all), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_first), True);
		XtSetSensitive(UxGetWidget(om_plan_order_first), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_job_id), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_second), True);
		XtSetSensitive(UxGetWidget(om_plan_order_second), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_job_id), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_third), True);
		XtSetSensitive(UxGetWidget(om_plan_order_third), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_job_id), True);
		XtSetSensitive(UxGetWidget(rc_plan_order_fourth), True);
		XtSetSensitive(UxGetWidget(om_plan_order_fourth), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_none), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_order_type), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_priority), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_media_id), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_mode), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_job_id), True);
	} else {
		XtSetSensitive(UxGetWidget(om_plan_sat), False);
		XtSetSensitive(UxGetWidget(pb_plan_sat_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_sat_e1), False);
		XtSetSensitive(UxGetWidget(pb_plan_sat_e2), False);
		XtSetSensitive(UxGetWidget(pb_plan_sat_j1), False);
		XtSetSensitive(UxGetWidget(pb_plan_sat_r1), False);
		XtSetSensitive(UxGetWidget(rc_plan_sat), False);
		XtSetSensitive(UxGetWidget(om_plan_sens), False);
		XtSetSensitive(UxGetWidget(pb_plan_sens_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_sens_s), False);
		XtSetSensitive(UxGetWidget(rc_plan_sens), False);
		XtSetSensitive(UxGetWidget(om_plan_activity), False);
		XtSetSensitive(UxGetWidget(pb_plan_activity_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_activity_rlt), False);
		XtSetSensitive(UxGetWidget(pb_plan_activity_dmp), False);
		XtSetSensitive(UxGetWidget(rc_plan_activity), False);
		XtSetSensitive(UxGetWidget(om_plan_station), False);
		XtSetSensitive(UxGetWidget(pb_plan_station_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_station_fa), False);
		XtSetSensitive(UxGetWidget(pb_plan_station_mc), False);
		XtSetSensitive(UxGetWidget(rc_plan_station), False);
		XtSetSensitive(UxGetWidget(om_plan_priority), False);
		XtSetSensitive(UxGetWidget(pb_plan_priority_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_priority_low), False);
		XtSetSensitive(UxGetWidget(pb_plan_priority_routine), False);
		XtSetSensitive(UxGetWidget(pb_plan_priority_high), False);
		XtSetSensitive(UxGetWidget(pb_plan_priority_urgent), False);
		XtSetSensitive(UxGetWidget(rc_plan_priority), False);
		XtSetSensitive(UxGetWidget(om_plan_data_direction), False);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_forward), False);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_reverse), False);
		XtSetSensitive(UxGetWidget(pb_plan_data_direction_unknown), False);
		XtSetSensitive(UxGetWidget(rc_plan_data_direction), False);
		XtSetSensitive(UxGetWidget(l_plan_rev), False);
		XtSetSensitive(UxGetWidget(tf_plan_rev), False);
		XtSetSensitive(UxGetWidget(l_plan_seq), False);
		XtSetSensitive(UxGetWidget(tf_plan_seq), False);
		XtSetSensitive(UxGetWidget(l_plan_job_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_job_id), False);
		XtSetSensitive(UxGetWidget(l_plan_order_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_order_id), False);
		XtSetSensitive(UxGetWidget(l_plan_item_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_item_id), False);
		XtSetSensitive(UxGetWidget(l_plan_frame_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_frame_id), False);
		XtSetSensitive(UxGetWidget(l_plan_subframe_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_subframe_id), False);
		XtSetSensitive(UxGetWidget(om_plan_product_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_standard), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_complex), False);
		XtSetSensitive(UxGetWidget(pb_plan_product_type_ccsd), False);
		XtSetSensitive(UxGetWidget(rc_plan_product_type), False);
		XtSetSensitive(UxGetWidget(om_plan_processor_mode), False);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_continuous), False);
		XtSetSensitive(UxGetWidget(pb_plan_processor_mode_scansar), False);
		XtSetSensitive(UxGetWidget(rc_plan_processor_mode), False);
		XtSetSensitive(UxGetWidget(om_plan_media_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_any), False);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_disk), False);
		XtSetSensitive(UxGetWidget(pb_plan_media_type_dcrsi), False);
		XtSetSensitive(UxGetWidget(rc_plan_media_type), False);
		XtSetSensitive(UxGetWidget(l_plan_media_id), False);
		XtSetSensitive(UxGetWidget(tf_plan_media_id), False);
		XtSetSensitive(UxGetWidget(pb_plan_query), False);
		XtSetSensitive(UxGetWidget(pb_plan_ready_select_all), False);
		XtSetSensitive(UxGetWidget(pb_plan_ready_deselect_all), False);
		XtSetSensitive(UxGetWidget(rc_plan_order_first), False);
		XtSetSensitive(UxGetWidget(om_plan_order_first), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_none), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_order_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_priority), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_media_id), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_mode), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_first_job_id), False);
		XtSetSensitive(UxGetWidget(rc_plan_order_second), False);
		XtSetSensitive(UxGetWidget(om_plan_order_second), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_none), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_order_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_priority), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_media_id), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_mode), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_second_job_id), False);
		XtSetSensitive(UxGetWidget(rc_plan_order_third), False);
		XtSetSensitive(UxGetWidget(om_plan_order_third), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_none), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_order_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_priority), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_media_id), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_mode), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_third_job_id), False);
		XtSetSensitive(UxGetWidget(rc_plan_order_fourth), False);
		XtSetSensitive(UxGetWidget(om_plan_order_fourth), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_none), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_order_type), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_priority), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_media_id), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_mode), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_plan_order_fourth_job_id), False);
	}
}

/* callback for plan_query - formats and adds a line to the plan screen
	ready list widget */
void add_row_to_ready_list()
{
	char buf[MAXBIGBUF];
	char order_type[20];
	char datatake_id[20];
	char order_item[20];
	XmString listitem;

	/* preformatting for combo fields */
	strcpy(order_type, plan_row.job_type);
	if (!strcasecmp(plan_row.quicklook_flag, "yes")) {
		strcat(order_type, " ");
		strcat(order_type, "QLK");
	}
	sprintf(order_item, "%08d %02d", plan_row.order_id, plan_row.item_id);
	sprintf(datatake_id, "%s %s %-5d %-2d", plan_row.platform,
		plan_row.sensor, plan_row.rev, plan_row.sequence);

	/* format the whole row */
        sprintf(buf, " %-10.10s %-8.8s %03d    %-16.16s    %-6.6s %8.3f   %-12.12s   %08d     %-15.15s %3.3s", 
	order_type, plan_row.priority, plan_row.age,
	 datatake_id, plan_row.mode, plan_row.pixel_spacing, 
	plan_row.media_id, plan_row.job_id, order_item,  
	plan_row.insert_top_flag);

	/* add it to the end of the list */
	listitem = XmStringCreateLocalized(buf);
	XmListAddItemUnselected(UxGetWidget(sw_plan_ready_list),
		listitem, 0);
	XmStringFree(listitem);

	/* kludge: motif keeps resetting the visibleItemCount */
	XtVaSetValues(UxGetWidget(sw_plan_ready_list),
				XmNvisibleItemCount, 10,
				0);

	/* remember it */
	if (ready_num_displayed < MAX_READY_JOBS) {
		ready_jobs[ready_num_displayed].job_id = plan_row.job_id;
		strcpy(ready_jobs[ready_num_displayed].insert_top_flag,
			plan_row.insert_top_flag);
		strcpy(ready_jobs[ready_num_displayed].quicklook_flag,
			plan_row.quicklook_flag);
	}

	/* count it */
	ready_num_displayed++;
}

/* callback for plan_available query - formats and adds a line to the plan
	screen available list widget */
void add_row_to_available_list()
{
	char buf[MAXBIGBUF];
	char order_type[20];
	char datatake_id[20];
	XmString listitem;
	char order_item[20];

	/* preformatting for combo fields */
	strcpy(order_type, plan_row.job_type);
	if (!strcasecmp(plan_row.quicklook_flag, "yes")) {
		strcat(order_type, " ");
		strcat(order_type, "QLK");
	}
	sprintf(order_item, "%08d %02d", plan_row.order_id, plan_row.item_id);
	sprintf(datatake_id, "%s %s %-5d %-2d", plan_row.platform,
		plan_row.sensor, plan_row.rev, plan_row.sequence);

	/* format the whole row */
        sprintf(buf, " %-10.10s %-8.8s %03d    %-16.16s    %-6.6s %8.3f   %-12.12s   %08d     %-15.15s %3.3s", 
	order_type, plan_row.priority, plan_row.age,
	datatake_id, plan_row.mode, plan_row.pixel_spacing, 
	plan_row.media_id, plan_row.job_id, order_item, 
	plan_row.insert_top_flag);

	/* add it to the end of the list */
	listitem = XmStringCreateLocalized(buf);
	XmListAddItemUnselected(UxGetWidget(sw_plan_available_list),
		listitem, 0);
	XmStringFree(listitem);

	/* kludge: motif keeps resetting the visibleItemCount */
	XtVaSetValues(UxGetWidget(sw_plan_available_list),
				XmNvisibleItemCount, 10,
				0);

	/* remember it */
	if (available_num_displayed < MAX_AVAILABLE_JOBS) {
		available_jobs[available_num_displayed].job_id =
			plan_row.job_id;
		strcpy(available_jobs[available_num_displayed].insert_top_flag,
			plan_row.insert_top_flag);
		strcpy(available_jobs[available_num_displayed].quicklook_flag,
			plan_row.quicklook_flag);
	}

	/* count it */
	available_num_displayed++;
}

/* code to build the section of a where clause dealing with L1, L1 QLK,
	Scan, and Scan QLK orders */
void build_plan_order(char *buf, int L1, int L1_QLK, int Scan, int Scan_QLK)
{
/*	
	What's all this, then?  Trust me, you really don't want to know.  But
	if you must, perhaps this logic table will help.

	L1	L1 QLK	Scan	Scan QLK
	--	------	----	--------

	0	0	0	0	not possible currently
	0	0	0	1	Scan && QLK
	0	0	1	0	Scan && !QLK
	0	0	1	1	Scan
	0	1	0	0	L1 && QLK
	0	1	0	1	QLK
	0	1	1	0	(L1 && QLK) || (Scan && !QLK)
	0	1	1	1	(L1 && QLK) || Scan
	1	0	0	0	(L1 && !QLK)
	1	0	0	1	(L1 && !QLK) || (Scan && QLK)
	1	0	1	0	!QLK
	1	0	1	1	(L1 && !QLK) || Scan
	1	1	0	0	L1
	1	1	0	1	L1 || (Scan && QLK)
	1	1	1	0	L1 || (Scan && !QLK)
	1	1	1	1	no where clause needed
*/
	if (L1) {
	  if (L1_QLK) {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "");
	      } else {
	        strcpy(buf, "(ppsgui_orders.job_type = \"L1\" or "
	          "(ppsgui_orders.job_type = \"SCAN\" and ppsgui_orders.quicklook_flag = \"NO\"))");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "(ppsgui_orders.job_type = \"L1\" or "
	          "(ppsgui_orders.job_type = \"SCAN\" and ppsgui_orders.quicklook_flag = \"YES\"))");
	      } else {
	        strcpy(buf, "(ppsgui_orders.job_type = \"L1\")");
	      }
	    }
	  } else {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "((ppsgui_orders.job_type = \"L1\" and ppsgui_orders.quicklook_flag = \"NO\")"
	          " or ppsgui_orders.job_type = \"SCAN\")");
	      } else {
		strcpy(buf, "(ppsgui_orders.quicklook_flag = \"NO\")");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "((ppsgui_orders.job_type = \"L1\" and ppsgui_orders.quicklook_flag = \"NO\")"
	          " or (ppsgui_orders.job_type = \"SCAN\" and ppsgui_orders.quicklook_flag = \"YES\"))");
	      } else {
		strcpy(buf, "(ppsgui_orders.job_type = \"L1\" and ppsgui_orders.quicklook_flag = \"NO\")");
	      }
	    }
	  }
	} else {
	  if (L1_QLK) {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "((ppsgui_orders.job_type = \"L1\" and ppsgui_orders.quicklook_flag = \"YES\")"
	          " or ppsgui_orders.job_type = \"SCAN\")");
	      } else {
	        strcpy(buf, "((ppsgui_orders.job_type = \"L1\" and ppsgui_orders.quicklook_flag = \"YES\")"
	          " or (ppsgui_orders.job_type = \"SCAN\" and ppsgui_orders.quicklook_flag = \"NO\"))");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "(ppsgui_orders.quicklook_flag = \"YES\")");
	      } else {
	        strcpy(buf, "(ppsgui_orders.job_type = \"L1\" and ppsgui_orders.quicklook_flag = \"YES\")");
	      }
	    }
	  } else {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "(ppsgui_orders.job_type = \"SCAN\")");
	      } else {
	        strcpy(buf, "(ppsgui_orders.job_type = \"SCAN\" and"
	          " ppsgui_orders.quicklook_flag = \"NO\")");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "(ppsgui_orders.job_type = \"SCAN\" and"
	          " ppsgui_orders.quicklook_flag = \"YES\")");
	      } else {
		/* should never get here! */
	        strcpy(buf, "");
	      }
	    }
	  }
	}
}

/* code to build and perform a plan query for the plan screen */
void do_plan_ready_query()
{
	int  retcode;
	char buf[MAXBIGBUF];
	char wherebuf[MAXBIGBUF];
	char satbuf[MAXSMALLBUF], sensbuf[MAXSMALLBUF], revbuf[MAXSMALLBUF];
	char seqbuf[MAXSMALLBUF], activitybuf[MAXSMALLBUF];
	char stationbuf[MAXSMALLBUF], prioritybuf[MAXSMALLBUF];
	char statebuf[MAXSMALLBUF], jobidbuf[MAXSMALLBUF];
	char orderidbuf[MAXSMALLBUF], itemidbuf[MAXSMALLBUF];
	char frameidbuf[MAXSMALLBUF], subframeidbuf[MAXSMALLBUF];
	char product_typebuf[MAXSMALLBUF], pixel_spacingbuf[MAXSMALLBUF];
	char projectionbuf[MAXSMALLBUF], proc_gainbuf[MAXSMALLBUF];
	char processor_modebuf[MAXSMALLBUF];
	char media_typebuf[MAXSMALLBUF];
	char media_idbuf[MAXSMALLBUF];
	char data_direction_buf[MAXSMALLBUF];
	int L1, L1_QLK, Scan, Scan_QLK;
	int num_wheres, num_orders;
	char orderbuf[MAXBIGBUF];
	char firstbuf[MAXSMALLBUF], secondbuf[MAXSMALLBUF];
	char thirdbuf[MAXSMALLBUF], fourthbuf[MAXSMALLBUF];
	char usecmd[MAXSMALLBUF] ;
	int i;

	intFields planIntFields[7];

	planIntFields[0].label = l_plan_rev;
	planIntFields[0].textField = tf_plan_rev;
	planIntFields[1].label = l_plan_seq;
	planIntFields[1].textField = tf_plan_seq;
	planIntFields[2].label = l_plan_job_id;
	planIntFields[2].textField = tf_plan_job_id;
	planIntFields[3].label = l_plan_order_id;
	planIntFields[3].textField = tf_plan_order_id;
	planIntFields[4].label = l_plan_item_id;
	planIntFields[4].textField = tf_plan_item_id;
	planIntFields[5].label = l_plan_frame_id;
	planIntFields[5].textField = tf_plan_frame_id;
	planIntFields[6].label = l_plan_subframe_id;
	planIntFields[6].textField = tf_plan_subframe_id;

	/* validate all integer fields */
	if ( ! validate_int_fields(pps_plan, planIntFields, 7))
		return;

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_plan_ready_list));

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* no items currently */
	ready_num_displayed = 0;
	ready_num_selected = 0;
	update_plan_totals();

	/* populate plan_query structure */
	plan_query.num_items = 0;
	plan_query.callback = add_row_to_ready_list;
        pps_db_bind_int(&plan_query, &plan_row.order_id);
        pps_db_bind_int(&plan_query, &plan_row.item_id);
	pps_db_bind_int(&plan_query, &plan_row.job_id);
	pps_db_bind_char(&plan_query, plan_row.job_type,
		sizeof(plan_row.job_type));
	pps_db_bind_char(&plan_query, plan_row.quicklook_flag,
		sizeof(plan_row.quicklook_flag));
	pps_db_bind_char(&plan_query, plan_row.priority,
		sizeof(plan_row.priority));
	pps_db_bind_int(&plan_query, &plan_row.priority_num);
	pps_db_bind_int(&plan_query, &plan_row.age);
	pps_db_bind_char(&plan_query, plan_row.platform,
		sizeof(plan_row.platform));
	pps_db_bind_char(&plan_query, plan_row.sensor,
		sizeof(plan_row.sensor));
	pps_db_bind_int(&plan_query, &plan_row.rev);
	pps_db_bind_int(&plan_query, &plan_row.sequence);
	pps_db_bind_char(&plan_query, plan_row.mode,
		sizeof(plan_row.mode));
	pps_db_bind_float(&plan_query, &plan_row.pixel_spacing);
	pps_db_bind_char(&plan_query, plan_row.media_id,
		sizeof(plan_row.media_id));
	pps_db_bind_char(&plan_query, plan_row.insert_top_flag,
		sizeof(plan_row.insert_top_flag));

	/* buffer the basic query */
	strcpy(buf, 
		"select order_id, item_id, job_id, job_type, quicklook_flag, "
		"priority, priority_num, "
		"age = datediff(day, state_date, getdate()), "
		"platform, sensor, rev, sequence, mode, pixel_spacing, "
		"media_id,insert_top_flag "
		"from ppsgui_orders ");

	/* init where clause */
	strcpy(wherebuf, "where job_state = \"READY\"");
	num_wheres = 1;

	/* almost always need a clause for the order type */
	L1 = XmToggleButtonGetState(UxGetWidget(tb_plan_L1_Orders));
	L1_QLK = XmToggleButtonGetState(UxGetWidget(tb_plan_L1_QLK));
	Scan = XmToggleButtonGetState(UxGetWidget(tb_plan_Scan_Orders));
	Scan_QLK = XmToggleButtonGetState(UxGetWidget(tb_plan_Scan_QLK));

	/* don't query if no types selected - possible because of GUI-
		originated queries */
	if (L1 == False && L1_QLK == False && Scan == False &&
			Scan_QLK == False) {
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
			XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	strcpy(orderbuf, "");
	build_plan_order(orderbuf, L1, L1_QLK, Scan, Scan_QLK);
	if (strlen(orderbuf)) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		num_wheres++;
		strcat(wherebuf, orderbuf);
	}
	
	/* Satellite (aka platform) */
	strcpy(satbuf, get_om_label(om_plan_sat));
	if (strcmp(satbuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.platform = \"");
		strcat(wherebuf, satbuf);
		strcat(wherebuf, "\"");
	}

	/* Sensor */
	strcpy(sensbuf, get_om_label(om_plan_sens));
	if (strcmp(sensbuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.sensor = \"");
		strcat(wherebuf, sensbuf);
		strcat(wherebuf, "\"");
	}

	/* Rev */
	strcpy(revbuf, UxGetText(tf_plan_rev));
	if (strcmp(revbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.rev = ");
		strcat(wherebuf, revbuf);
	}

	/* Seq */
	strcpy(seqbuf, UxGetText(tf_plan_seq));
	if (strcmp(seqbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.sequence = ");
		strcat(wherebuf, seqbuf);
	}

	/* Activity */
	strcpy(activitybuf, get_om_label(om_plan_activity));
	if (strcmp(activitybuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.activity_id = \"");
		strcat(wherebuf, activitybuf);
		strcat(wherebuf, "\"");
	}

	/* Station */
	strcpy(stationbuf, get_om_label(om_plan_station));
	if (strcmp(stationbuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.station_id = \"");
		strcat(wherebuf, stationbuf);
		strcat(wherebuf, "\"");
	}

	/* Priority */
	strcpy(prioritybuf, get_om_label(om_plan_priority));
	if (strcmp(prioritybuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.priority = \"");
		strcat(wherebuf, prioritybuf);
		strcat(wherebuf, "\"");
	}

	/* Data Direction */
	strcpy(data_direction_buf,
		get_om_label(om_plan_data_direction));
	if (strcmp(data_direction_buf, "Any") != 0)
	{
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.data_direction = \"");
		strcat(wherebuf, data_direction_buf);
		strcat(wherebuf, "\"");
	}

	/* Job ID */
	strcpy(jobidbuf, UxGetText(tf_plan_job_id));
	if (strcmp(jobidbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.job_id = ");
		strcat(wherebuf, jobidbuf);
	}

	/* Order ID */
	strcpy(orderidbuf, UxGetText(tf_plan_order_id));
	if (strcmp(orderidbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.order_id = ");
		strcat(wherebuf, orderidbuf);
	}

	/* Item ID */
	strcpy(itemidbuf, UxGetText(tf_plan_item_id));
	if (strcmp(itemidbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.item_id = ");
		strcat(wherebuf, itemidbuf);
	}

	/* Keywords supported only for L1 queries, not for Scan */
	if ((L1 || L1_QLK) && !Scan && !Scan_QLK) {

		/* Frame ID */
		strcpy(frameidbuf, UxGetText(tf_plan_frame_id));
		if (strcmp(frameidbuf, "")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "ppsgui_orders.frame_id = ");
			strcat(wherebuf, frameidbuf);
		}

		/* Subframe ID */
		strcpy(subframeidbuf, UxGetText(tf_plan_subframe_id));
		if (strcmp(subframeidbuf, "")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "ppsgui_orders.subframe_id = ");
			strcat(wherebuf, subframeidbuf);
		}

		/* Product Type */
		strcpy(product_typebuf, get_om_label(om_plan_product_type));
		if (strcmp(product_typebuf, "Any")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "ppsgui_orders.product_type = \"");
			strcat(wherebuf, product_typebuf);
			strcat(wherebuf, "\"");
		}
	}

	/* Processor Mode */
	strcpy(processor_modebuf,
		get_om_label(om_plan_processor_mode));
	if (strcmp(processor_modebuf, "Any") != 0)
	{
		if (num_wheres++)
			strcat(wherebuf, " and ");
		if (strcmp(processor_modebuf, "SCANSAR") == 0)
		{
			strcat(wherebuf,
"(platform = \"R1\" and mode in (\"SNA\",\"SNB\",\"SWA\",\"SWB\"))");
		}
		else
		{
		/* CONTINUOUS*/
			strcat(wherebuf,
"(platform = \"E1\" or platform = \"E2\" or platform = \"J1\" or (platform = \"R1\" and mode not in (\"SNA\",\"SNB\",\"SWA\",\"SWB\")))");
		}
	}

	/* Media Type */
	strcpy(media_typebuf, get_om_label(om_plan_media_type));
	if (strcmp(media_typebuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.media_type = \"");
		strcat(wherebuf, media_typebuf);
		strcat(wherebuf, "\"");
	}

	/* Media ID */
	strcpy(media_idbuf, UxGetText(tf_plan_media_id));
	if (strcmp(media_idbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "ppsgui_orders.media_id = \"");
		strcat(wherebuf, media_idbuf);
		strcat(wherebuf, "\"");
	}

	/* append the where clause if needed */
	if (num_wheres)
		strcat(buf, wherebuf);


	/* init order by clause */
	strcpy(orderbuf, " order by ");
	num_orders = 0;

	/* First sort */
	strcpy(firstbuf, get_om_label(om_plan_order_first));
	if (strcmp(firstbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; plan_order[i].label; i++)
			if (!(strcmp(firstbuf, plan_order[i].label)))
				break;
		strcat(orderbuf, plan_order[i].clause);
	}

	/* Second sort */
	strcpy(secondbuf, get_om_label(om_plan_order_second));
	if (strcmp(secondbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; plan_order[i].label; i++)
			if (!(strcmp(secondbuf, plan_order[i].label)))
				break;
		strcat(orderbuf, plan_order[i].clause);
	}

	/* Third sort */
	strcpy(thirdbuf, get_om_label(om_plan_order_third));
	if (strcmp(thirdbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; plan_order[i].label; i++)
			if (!(strcmp(thirdbuf, plan_order[i].label)))
				break;
		strcat(orderbuf, plan_order[i].clause);
	}

	/* Fourth sort */
	strcpy(fourthbuf, get_om_label(om_plan_order_fourth));
	if (strcmp(fourthbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; plan_order[i].label; i++)
			if (!(strcmp(fourthbuf, plan_order[i].label)))
				break;
		strcat(orderbuf, plan_order[i].clause);
	}

	/* check for Order By clause collisions */
	if ((!strcmp(firstbuf, secondbuf) && strcmp(firstbuf, "None")) ||
		(!strcmp(firstbuf, thirdbuf) && strcmp(firstbuf, "None")) ||
		(!strcmp(firstbuf, fourthbuf) && strcmp(firstbuf, "None")) ||
		(!strcmp(secondbuf, thirdbuf) && strcmp(secondbuf, "None")) ||
		(!strcmp(secondbuf, fourthbuf) && strcmp(secondbuf, "None")) ||
		(!strcmp(thirdbuf, fourthbuf) && strcmp(thirdbuf, "None"))) {
		sprintf(buf, "Duplicate Order By clause!\n\nNot useful, not supported.");
		do_error_dialog(pps_plan,buf);
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
			XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	/* append the order by clause if needed */
	if (num_orders)
		strcat(buf, orderbuf);

	/* problems with your select statement? */
	if (globalData.showSQL)
		printf("%s\n", buf);

	/* do the plan screen plan */
	retcode = db_exec(&query_connection, buf, &plan_query);
	if (retcode != ER_NO_ERROR) {
		do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
			XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	/* save the query string to be used by "print result..." */
	(void)strcpy(queryBuf, buf);

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* update the counters */
	update_plan_totals();

	/* check for context overflow */
	if (ready_num_displayed >= MAX_READY_JOBS) {
		sprintf(buf, "Can't handle more than %d jobs!\n\nPlease narrow query and re-submit.", MAX_READY_JOBS);
		do_error_dialog(pps_plan,buf);
	}
}

/* code to build and perform an available  for the plan screen */
void do_plan_available_query()
{
	int  retcode;
	char buf[MAXBIGBUF];

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_plan_available_list));

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* no items currently */
	available_num_displayed = 0;
	available_num_selected = 0;
	update_plan_totals();

	/* populate plan_query structure */
	plan_query.num_items = 0;
	plan_query.callback = add_row_to_available_list;
        pps_db_bind_int(&plan_query, &plan_row.order_id);
        pps_db_bind_int(&plan_query, &plan_row.item_id);
	pps_db_bind_int(&plan_query, &plan_row.job_id);
	pps_db_bind_char(&plan_query, plan_row.job_type,
		sizeof(plan_row.job_type));
	pps_db_bind_char(&plan_query, plan_row.quicklook_flag,
		sizeof(plan_row.quicklook_flag));
	pps_db_bind_char(&plan_query, plan_row.priority,
		sizeof(plan_row.priority));
	pps_db_bind_int(&plan_query, &plan_row.age);
	pps_db_bind_char(&plan_query, plan_row.platform,
		sizeof(plan_row.platform));
	pps_db_bind_char(&plan_query, plan_row.sensor,
		sizeof(plan_row.sensor));
	pps_db_bind_int(&plan_query, &plan_row.rev);
	pps_db_bind_int(&plan_query, &plan_row.sequence);
	pps_db_bind_char(&plan_query, plan_row.mode,
		sizeof(plan_row.mode));
	pps_db_bind_float(&plan_query, &plan_row.pixel_spacing);
	pps_db_bind_char(&plan_query, plan_row.media_id,
		sizeof(plan_row.media_id));
	pps_db_bind_char(&plan_query, plan_row.insert_top_flag,
		sizeof(plan_row.insert_top_flag));

	/* buffer the query */
	strcpy(buf, 
		"select order_id, item_id,"
		"ppsgui_orders.job_id,ppsgui_orders.job_type, "
		"ppsgui_orders.quicklook_flag, "
		"ppsgui_orders.priority, "
		"age = datediff(day, ppsgui_orders.state_date, getdate()), "
		"platform, sensor, rev, sequence,mode, pixel_spacing, media_id, "
		"ppsgui_orders.insert_top_flag "
		"from ppsgui_orders, schedule "
		"where ppsgui_orders.job_id = schedule.job_id "
		"order by ppsgui_orders.quicklook_flag desc, "
		"schedule.job_sequence");

	/* problems with your select statement? */
	if (globalData.showSQL)
		printf("%s\n", buf);

	/* do the plan screen plan */
	retcode = db_exec(&query_connection, buf, &plan_query);
	if (retcode != ER_NO_ERROR) {
		fprintf(stderr, "Failed to execute query for avail queue\n");
		do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
			XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* update the counters */
	update_plan_totals();
}

/* callback to update the screen */
void 
cb_update_screen(
Widget w,
XtPointer clientData,
XtPointer callData)
{
	do_plan_ready_query();
	do_plan_available_query();
	
} /* cb_update_screen */

/* routine to actually save a plan screen plan to a file */
void plan_save_query(char *fname)
{
	FILE *fp;
	char new_filename[MAXSMALLBUF];
	char swname[MAXSMALLBUF];
	char swmenu[MAXSMALLBUF];
	char swtext[MAXSMALLBUF];
	char tbstate[MAXSMALLBUF];
	int i;
	char buf[MAXBIGBUF];

	(void)strcpy(new_filename, fname);
	/* ensure it's a .ppf file - maybe add more intelligence here later,
		like making sure there isn't already an extension, or
		expanding the full path name? */
	if (strcmp(".ppf", &fname[strlen(fname) - strlen(".ppf")]))
		strcat(new_filename, ".ppf");

	/* open the file */
	fp = fopen(new_filename, "w");
	if (fp == NULL) {
		sprintf(buf, "Cannot write to File:\n\n%s", new_filename);
		do_error_dialog(pps_plan,buf);
		return;
	}

	/* for each swidget we care about... */
	for (i = 0; plan_settings[i].sw; i++) {
		/* get the ascii version of the name */
		strcpy(swname, UxGetName(*plan_settings[i].sw));
		/* possibly get and save the toggle state */
		if (plan_settings[i].tb) {
			strcpy(tbstate, UxGetSet(*plan_settings[i].sw));
			fprintf(fp, "%s.ToggleButton:\t%s\n", swname, tbstate);
		}
		/* possibly get and save the current option menu button */
		if (plan_settings[i].om) {
			strcpy(swmenu, UxGetMenuHistory(
					*plan_settings[i].sw));
			fprintf(fp, "%s.MenuHistory:\t%s\n", swname, swmenu);
		}
		/* possibly get and save the label text */
		if (plan_settings[i].tf) {
			strcpy(swtext, UxGetText(*plan_settings[i].sw));
			fprintf(fp, "%s.Text:\t%s\n", swname, swtext);
		}
	}

	/* close the file */
	fclose(fp);
}

/* routine to actually load a plan screen plan from a file */
void plan_load_query(char *fname)
{
	FILE *fp;
	char swname[MAXSMALLBUF];
	char swmenu[MAXSMALLBUF];
	char swtext[MAXSMALLBUF];
	int i;
	char buf[MAXBIGBUF];
	char *resource;

	/* zap saved filename */
	strcpy(plan_filename, "");

	/* make sure it's a .ppf file */
	if (strcmp(".ppf", &fname[strlen(fname) - strlen(".ppf")])) {
		sprintf(buf, "Not a .ppf file\n\n%s",
			fname);
		do_error_dialog(pps_plan,buf);
		return;
	}

	/* save filename */
	strcpy(plan_filename, fname);

	/* open the file, just to see if you can */
	fp = fopen(fname, "r");
	if (fp == NULL) {
		sprintf(buf, "Unable to load query from\n\n%s", fname);
		do_error_dialog(pps_plan,buf);
		return;
	}

	/* close the file */
	fclose(fp);

	/* load the file as resources */
	UxOverrideResources(fname);

	/* for each swidget we care about... */
	for (i = 0; plan_settings[i].sw; i++) {
		/* possibly get the toggle state */
		if (plan_settings[i].tb) 
		{
			if ((resource = UxGetResource
                                (UxGetName(*plan_settings[i].sw), "ToggleButton")) != NULL)
                        {
				if (strcmp(resource, "true"))
					XmToggleButtonSetState(UxGetWidget(
					*plan_settings[i].sw), False, True);
				else
					XmToggleButtonSetState(UxGetWidget(
					*plan_settings[i].sw), True, True);
			}
		}
		/* possibly get the current option menu button */
		if (plan_settings[i].om) 
		{
                        if ((resource = UxGetResource
                                (UxGetName(*plan_settings[i].sw), "MenuHistory")) != NULL)
                        {
				UxPutMenuHistory(*plan_settings[i].sw, resource);
			}
		}
		/* possibly get the label text */
		if (plan_settings[i].tf) 
		{
                        if ((resource = UxGetResource
                                (UxGetName(*plan_settings[i].sw), "Text")) != NULL)
                        {
				UxPutText(*plan_settings[i].sw, resource);
			}
		}
	}

}

/* callback for file selector dialog box OK button */
void cb_load_plan_file(Widget widget, XtPointer client_data,
	XtPointer call_data)
{
	char *filename;
	XmFileSelectionBoxCallbackStruct *cbs =
		(XmFileSelectionBoxCallbackStruct *) call_data;

	if (!XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
		return;

	/* make sure you got a file name */
	if (!*filename) {
		XtFree(filename);
		return;
	}

	/* pop down the file selector */
	UxDestroyInterface(pps_file_selector);

	/* do the load */
	plan_load_query(filename);

	/* free the file name */
	XtFree(filename);
}

/* callback for file selector dialog box OK button */
void cb_save_plan_file(Widget widget, XtPointer client_data,
	XtPointer call_data)
{
	char *filename;
	XmFileSelectionBoxCallbackStruct *cbs =
		(XmFileSelectionBoxCallbackStruct *) call_data;

	if (!XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &filename))
		return;

	/* make sure you got a file name */
	if (!*filename) {
		XtFree(filename);
		return;
	}

	/* pop down the file selector */
	UxDestroyInterface(pps_file_selector);

	/* do the save */
	plan_save_query(filename);

	/* free the file name */
	XtFree(filename);
}

/* callback to load a saved plan for the plan screen */
cb_plan_load_query(void)
{
	/* create and tweak the box */
	pps_file_selector = create_pps_file_selector(pps_plan);
	XtUnmanageChild(XmFileSelectionBoxGetChild(UxGetWidget(
		pps_file_selector), XmDIALOG_HELP_BUTTON));
	UxPutPattern(pps_file_selector, "*.ppf");
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNcancelCallback, cb_no_file_selected, NULL);
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNokCallback, cb_load_plan_file, NULL);
	UxPutDialogTitle(pps_file_selector, "Load plan...");

	/* pop it up */
	UxPopupInterface(pps_file_selector, nonexclusive_grab);
}

/* callback to save a previously loaded plan for the plan screen */
cb_plan_save_query(void)
{
	/* overwrite previously loaded file, if any, else ask */
	if (strlen(plan_filename))
		plan_save_query(plan_filename);
	else
		cb_plan_save_query_as();
}

/* callback to save a plan for the plan screen */
cb_plan_save_query_as(void)
{
	/* create and tweak the box */
	pps_file_selector = create_pps_file_selector(pps_plan);
	XtUnmanageChild(XmFileSelectionBoxGetChild(UxGetWidget(
		pps_file_selector), XmDIALOG_HELP_BUTTON));
	UxPutPattern(pps_file_selector, "*.ppf");
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNcancelCallback, cb_no_file_selected, NULL);
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNokCallback, cb_save_plan_file, NULL);
	UxPutDialogTitle(pps_file_selector, "Save plan As...");

	/* pop it up */
	UxPopupInterface(pps_file_selector, nonexclusive_grab);
}

/* callback to handle selection change in the Ready list */
cb_plan_ready_extsel(XmListCallbackStruct *cbs)
{
	int i;

	/* zero totals */
	ready_num_selected = 0;
	update_plan_totals();

	/* count selected */
	for (i = 0; i < cbs->selected_item_count; i++) {
		ready_num_selected++;
	}

	/* show new totals */
	update_plan_totals();
}

/* callback to handle selection change in the Available list */
cb_plan_available_extsel(XmListCallbackStruct *cbs)
{
	int i;

	/* zero totals */
	available_num_selected = 0;
	update_plan_totals();

	/* count selected */
	for (i = 0; i < cbs->selected_item_count; i++) {
		available_num_selected++;
	}

	/* show new totals */
	update_plan_totals();
}

/* callback to select all ready jobs */
cb_plan_ready_select_all()
{
	int i;
	char policy[MAXSMALLBUF];
	Widget listwid = UxGetWidget(sw_plan_ready_list);

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* zero totals */
	ready_num_selected = 0;
	update_plan_totals();

	/* deselect all ready items */
	XmListDeselectAllItems(listwid);

	/* hack the selection policy */
	strcpy(policy, UxGetSelectionPolicy(sw_plan_ready_list));
	UxPutSelectionPolicy(sw_plan_ready_list, "multiple_select");

	/* call ListKbdSelectAll action to select all ready items */
	if (selectAllActionProc == 0)
		get_select_all_action();
	(*selectAllActionProc) (listwid, 0, 0, 0);

	ready_num_selected = ready_num_displayed;

	/* restore the selection policy */
	UxPutSelectionPolicy(sw_plan_ready_list, policy);

	/* update totals */
	update_plan_totals();

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));
}

/* callback to deselect all ready jobs */
cb_plan_ready_deselect_all()
{
	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* deselect all ready items */
	XmListDeselectAllItems(UxGetWidget(sw_plan_ready_list));

	/* zero totals */
	ready_num_selected = 0;
	update_plan_totals();

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));
}

/* callback to select all available jobs */
cb_plan_available_select_all()
{
	int i;
	char policy[MAXSMALLBUF];
	Widget listwid = UxGetWidget(sw_plan_available_list);

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* zero totals */
	available_num_selected = 0;
	update_plan_totals();

	/* deselect all available items */
	XmListDeselectAllItems(listwid);

	/* hack the selection policy */
	strcpy(policy, UxGetSelectionPolicy(sw_plan_available_list));
	UxPutSelectionPolicy(sw_plan_available_list, "multiple_select");

	/* call ListKbdSelectAll action to select all available items */
	if (selectAllActionProc == 0)
		get_select_all_action();
	(*selectAllActionProc) (listwid, 0, 0, 0);

	available_num_selected = available_num_displayed;

	/* restore the selection policy */
	UxPutSelectionPolicy(sw_plan_available_list, policy);

	/* update totals */
	update_plan_totals();

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));
}

/* callback to deselect all available jobs */
cb_plan_available_deselect_all()
{
	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* deselect all available items */
	XmListDeselectAllItems(UxGetWidget(sw_plan_available_list));

	/* zero totals */
	available_num_selected = 0;
	update_plan_totals();

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));
}

/* common Insert Top routine for selected ready jobs */
do_plan_ready_IT(char *bool)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_ready_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* update the jobs */
	for (i = 0; i < position_count; i++) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_set_insert_top %d, \"%s\"",
			ready_jobs[position_list[i]-1].job_id, bool);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection,buf,&plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr,"Failed to execute sp_set_insert_top\n");
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode)); 
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_ready_query();
}

/* common Insert Top routine for selected available jobs */
do_plan_available_IT(char *bool)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_available_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* update the jobs */
	for (i = 0; i < position_count; i++) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_set_insert_top %d, \"%s\"",
			available_jobs[position_list[i]-1].job_id, bool);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection, buf, &plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_set_insert_top\n", retcode);
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_available_query();
}

/* callback to set Insert Top for selected ready jobs */
cb_plan_ready_IT_on()
{
	do_plan_ready_IT("YES");
}

/* callback to unset Insert Top for selected ready jobs */
cb_plan_ready_IT_off()
{
	do_plan_ready_IT("NO");
}

/* callback to set Insert Top for selected available jobs */
cb_plan_available_IT_on()
{
	do_plan_available_IT("YES");
}

/* callback to unset Insert Top for selected available jobs */
cb_plan_available_IT_off()
{
	do_plan_available_IT("NO");
}

/* callback to send ready jobs to the bottom of the plan */
cb_plan_ready_bottom(void)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;
	char error_msg[MAXBIGBUF];
	char job_type[MAX_JOB_TYPE_CHARS+1];
	XmString*	selectedItems=0;
	int			selectedItemCount=0;
	char		*selectedJobString=0;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_ready_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	error_msg[0] = '\0';
	/* check the job states */
	stat = check_job_state(READY, error_msg, position_list, position_count);

	/* if there was a problem getting the job state, return */
	if (stat != ER_NO_ERROR)
	{
		/* clean up */
		XtFree(position_list);
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	/* if the error message was not NULL,
		pop up the question dialog and return */
	else if (error_msg[0])
	{
		XppsCreateQuestionDialog(UxGetWidget(pps_plan),
					 error_msg,
					 True, 
					 cb_update_screen, NULL,
					 0, NULL);

		/* clean up */
		XtFree(position_list);
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	XtVaGetValues(UxGetWidget(sw_plan_ready_list),
				XmNselectedItems, &selectedItems,
				XmNselectedItemCount, &selectedItemCount,
				0);

	/* update the jobs */
	for (i = 0; i < position_count; i++) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_plan_ready_to_bottom %d, '%s'",
			ready_jobs[position_list[i]-1].job_id,
			ready_jobs[position_list[i]-1].quicklook_flag);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection,buf,&plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_plan_ready_to_bottom\n");
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
		XmStringGetLtoR(selectedItems[i], XmSTRING_DEFAULT_CHARSET,
				&selectedJobString);
		(void)sscanf(selectedJobString, "%s", job_type);
		send_new_status_to_ims(ready_jobs[position_list[i]-1].job_id,
				job_type, AVAILABLE);
		XtFree(selectedJobString);
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_ready_query();
	do_plan_available_query();
}

/* callback to send available jobs to the bottom of the plan */
cb_plan_available_bottom(void)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;
	char usecmd[MAXSMALLBUF] ;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_available_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* update the jobs */
	for (i = 0; i < position_count; i++) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_plan_available_to_bottom %d, '%s'",
			available_jobs[position_list[i]-1].job_id,
			available_jobs[position_list[i]-1].quicklook_flag);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection,buf,&plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_plan_available_to_bottom\n");
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_ready_query();
	do_plan_available_query();
}

/* callback to send ready jobs to the top of the plan */
cb_plan_ready_top(void)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;
	char error_msg[MAXBIGBUF];
	char job_type[MAX_JOB_TYPE_CHARS+1];
	XmString*	selectedItems=0;
	int			selectedItemCount=0;
	char		*selectedJobString=0;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_ready_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* check the job states */
	stat = check_job_state(READY, error_msg, position_list, position_count);

	/* if there was a problem getting the job state, return */
	if (stat != ER_NO_ERROR)
	{
		/* clean up */
		XtFree(position_list);
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	/* if the error message was not NULL, 
		pop up the question dialog and return */
	else if (error_msg[0])
	{
		XppsCreateQuestionDialog(UxGetWidget(pps_plan),
					 error_msg,
					 True, 
					 cb_update_screen, NULL,
					 0, NULL);

		/* clean up */
		XtFree(position_list);
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	XtVaGetValues(UxGetWidget(sw_plan_ready_list),
				XmNselectedItems, &selectedItems,
				XmNselectedItemCount, &selectedItemCount,
				0);

	/* update the jobs */
	for (i = position_count - 1; i >= 0; i--) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_plan_ready_to_top %d, '%s'",
			ready_jobs[position_list[i]-1].job_id,
			ready_jobs[position_list[i]-1].quicklook_flag);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection,buf,&plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_plan_ready_to_top\n");
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
			 /* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
		XmStringGetLtoR(selectedItems[i], XmSTRING_DEFAULT_CHARSET,
				&selectedJobString);
		(void)sscanf(selectedJobString, "%s", job_type);
		send_new_status_to_ims(ready_jobs[position_list[i]-1].job_id,
				job_type, AVAILABLE);
		XtFree(selectedJobString);
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_ready_query();
	do_plan_available_query();
}

/* callback to send available jobs to the top of the plan */
cb_plan_available_top(void)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;
	char usecmd[MAXSMALLBUF] ;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_available_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* update the jobs */
	for (i = position_count - 1; i >= 0; i--) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_plan_available_to_top %d, '%s'",
			available_jobs[position_list[i]-1].job_id,
			available_jobs[position_list[i]-1].quicklook_flag);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection,buf,&plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_plan_available_to_top\n");
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_ready_query();
	do_plan_available_query();
}

/* callback to remove available jobs from the plan */
cb_plan_available_remove(void)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;
	char job_state[MAXBIGBUF];
	char error_msg[MAXBIGBUF];
	char job_type[MAX_JOB_TYPE_CHARS+1];
	XmString*	selectedItems=0;
	int			selectedItemCount=0;
	char		*selectedJobString=0;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_plan_available_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_plan)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_plan)));

	/* check the job states */
	stat = check_job_state(AVAILABLE, error_msg, position_list, position_count);
	
	/* if there was a problem getting the job state, return */
	if (stat != ER_NO_ERROR)
	{
		/* clean up */
		XtFree(position_list);
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	/* if the error message was not NULL,
		pop up the question dialog and return */
	else if (error_msg[0])
	{
		XppsCreateQuestionDialog(UxGetWidget(pps_plan),
					 error_msg,
					 True, 
					 cb_update_screen, NULL,
					 0, NULL);

		/* clean up */
		XtFree(position_list);
		XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
		return;
	}

	XtVaGetValues(UxGetWidget(sw_plan_available_list),
				XmNselectedItems, &selectedItems,
				XmNselectedItemCount, &selectedItemCount,
				0);

	/* update the jobs */
	for (i = 0; i < position_count; i++) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_plan_available_remove %d",
			available_jobs[position_list[i]-1].job_id);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection, buf, &plan_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_plan_available_remove\n");
			do_error_dialog(pps_plan,(char *)get_pps_err_msg(retcode));
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
				XtWindow(UxGetWidget(pps_plan)));
			return;
		}
		XmStringGetLtoR(selectedItems[i], XmSTRING_DEFAULT_CHARSET,
				&selectedJobString);
		(void)sscanf(selectedJobString, "%s", job_type);
		send_new_status_to_ims(available_jobs[position_list[i]-1].job_id,
				job_type, READY);
		XtFree(selectedJobString);
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_plan)),
		XtWindow(UxGetWidget(pps_plan)));

	/* requery the database using the current settings */
	do_plan_ready_query();
	do_plan_available_query();
}

/* callback to print results */
void
cb_plan_print_results(void)
{
	Widget	ready_list;
	Widget	available_list;
	char**	items;
	int	numItems;
	int	i, k;
	FILE*	fp;
	char filename[MAXSMALLBUF];
	char buf[MAXBIGBUF];
	int  mypid;
	XmString      dialogTitle;
	static Widget printResultsDialog=0;
	char	noToBePlanned=True;

	ready_list = UxGetWidget(sw_plan_ready_list);
	available_list = UxGetWidget(sw_plan_available_list);

	/*--------------------------------------*/
	/* create a temp file for printing      */
	/*--------------------------------------*/
	mypid = getpid();
	sprintf(filename, "/tmp/%d_plan.ascii", mypid);
	if ((fp=fopen(filename, "w")) == NULL)
	{
		fprintf(stderr, "Cannot open temp file for writing\n");
		return;
	}

	if (list_get_all_items(ready_list, &items, &numItems))
	{
		/*------------------------------------*/
		/* print header                       */
		/*------------------------------------*/
		fprintf(fp, "\n\n");
		fprintf(fp, "Query Parameters:\n\n");
		fprintf(fp, queryBuf);
		fprintf(fp, "\n\n\n");
		fprintf(fp, "Ready to be planned:\n\n");
		fprintf(fp, "==================================================================================================================\n");
		fprintf(fp, " Order               Age      Datatake          Mode     Pixel       Media         Job        Order&Item      Inst\n");
		fprintf(fp, " Type      Priority (days)       ID                      Spacing       ID           ID           ID           Top\n");     
		fprintf(fp, "==================================================================================================================\n\n");
		/*------------------------------------*/
		/* print list contents                */
		/*------------------------------------*/
		for (k=0; k < numItems; k++)
		{
			fprintf(fp, "%s\n", items[k]);
			XtFree(items[k]);
		}
		XtFree(items);
	}
	else
	{
		/* nothing to print */
		noToBePlanned = True;
	}

	if (list_get_all_items(available_list, &items, &numItems))
	{
		/*------------------------------------*/
		/* print header                       */
		/*------------------------------------*/
		fprintf(fp, "\n\n\n");
		fprintf(fp, "Planned:\n\n");
		fprintf(fp, "==================================================================================================================\n");
		fprintf(fp, " Order               Age      Datatake          Mode     Pixel       Media         Job        Order&Item      Inst\n");
		fprintf(fp, " Type      Priority (days)       ID                      Spacing       ID           ID           ID           Top\n");     
		fprintf(fp, "==================================================================================================================\n\n");

		/*------------------------------------*/
		/* print list contents                */
		/*------------------------------------*/
		for (k=0; k < numItems; k++)
		{
			fprintf(fp, "%s\n", items[k]);
			XtFree(items[k]);
		}
		XtFree(items);
	}
	else
	{
		/* nothing to print */
		if (noToBePlanned)
		{
			fclose(fp);
			unlink(filename);
			XppsCreateErrorDialog(UxGetWidget(pps_plan),
				"Nothing to print", True, 0, 0);
			return;
		}
	}

	fclose(fp);

	dialogTitle = XmStringCreateLocalized("Plan: Print Results");
	if (printResultsDialog == 0)
		printResultsDialog =
			create_print_results_dialog(UxGetWidget(pps_plan),
				dialogTitle, filename);
	else
	{
		XtManageChild(printResultsDialog);
		XMapRaised(XtDisplay(XtParent(printResultsDialog)),
				XtWindow(XtParent(printResultsDialog)));
	}
	XmStringFree(dialogTitle);

}/* cb_plan_print_results */


/* callback code for clear query setting button */
void cb_plan_clear_query()
{
	char buf[10];

	/* clear the plan ready area */
	XmListDeleteAllItems(UxGetWidget(sw_plan_ready_list));

	/* deselect all order types */
	XmToggleButtonSetState(UxGetWidget(tb_plan_L1_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_plan_L1_QLK), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_plan_Scan_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_plan_Scan_QLK), False, True);

	/* update the ready jobs total area */
	ready_num_displayed = 0;
	ready_num_selected = 0;
        sprintf(buf, "%5d", ready_num_displayed);
        UxPutLabelString(l_plan_ready_num_displayed, buf);
        sprintf(buf, "%5d", ready_num_selected);
        UxPutLabelString(l_plan_ready_num_selected, buf);

	/* initialize the rest of the plan settings */
	UxPutMenuHistory(om_plan_sat, pb_plan_sat_any);
	UxPutMenuHistory(om_plan_sens, pb_plan_sens_any);
	UxPutText(tf_plan_rev, "");
	UxPutText(tf_plan_seq, "");
	UxPutMenuHistory(om_plan_activity, pb_plan_activity_any);
	UxPutMenuHistory(om_plan_station, pb_plan_station_any);
	UxPutText(tf_plan_job_id, "");
	UxPutText(tf_plan_order_id, "");
	UxPutText(tf_plan_item_id, "");
	UxPutMenuHistory(om_plan_priority, pb_plan_priority_any);
	UxPutMenuHistory(om_plan_data_direction, pb_plan_data_direction_any);
	UxPutText(tf_plan_frame_id, "");
	UxPutText(tf_plan_subframe_id, "");
	UxPutMenuHistory(om_plan_product_type, pb_plan_product_type_any);
	UxPutMenuHistory(om_plan_processor_mode, pb_plan_processor_mode_any);
	UxPutMenuHistory(om_plan_media_type, pb_plan_media_type_any);
	UxPutText(tf_plan_media_id, "");
	UxPutMenuHistory(om_plan_order_first, pb_plan_order_first_none);
	UxPutMenuHistory(om_plan_order_second, pb_plan_order_second_none);
	UxPutMenuHistory(om_plan_order_third, pb_plan_order_third_none);
	UxPutMenuHistory(om_plan_order_fourth, pb_plan_order_fourth_none);

}

/* function to check if the selected jobs are in the correct job state */
int check_job_state(char *required_state, 
		     char *error_msg, 
		     int *position_list, 
		     int position_count)
{
	int i;
	int stat;
	char job_state[MAXBIGBUF];
	char job_id_list[MAXBIGBUF];
	char buf[MAXBIGBUF];
	char buf2[MAXBIGBUF];

	/* start the job ID list as NULL */
	job_id_list[0] = '\0';

	for (i = 0; i < position_count; i++) 
	{
		/* get the job state */
		if (strcmp(required_state, READY) == 0)
			stat = get_job_state(ready_jobs[position_list[i]-1].job_id, 
					     job_state);
		else
			stat = get_job_state(available_jobs[position_list[i]-1].job_id, 
				     job_state);

		/* if there was an error, return stat and make error_msg NULL */
		if (stat != ER_NO_ERROR)
		{
			fprintf(stderr, "check_job_state: cannot get job state");
			strcpy(error_msg, "");
			return(stat);
		}
		
		/* 
		   if the job state was determined, check to see if it's correct
		   if it isn't, add it's job ID to the list 
		*/
		else if (strcmp(job_state, required_state) != 0)
		{
			if (strcmp(required_state, READY) == 0)
			{
				sprintf(buf, 
					"               %d\n", 
					ready_jobs[position_list[i]-1].job_id);
				strcat(job_id_list, buf);
			}
			else if (strcmp(required_state, AVAILABLE) == 0)
			{
				sprintf(buf, 
					"               %d\n", 
					available_jobs[position_list[i]-1].job_id);
				strcat(job_id_list, buf);
			}
		}
	}

	/* 
	   if the job ID list isn't empty, construct the error message and call 
	   the update dialog 
	*/
	if (job_id_list[0])
	{
		sprintf(buf, "The following is a list of Job ID's not in the %s state:\n", required_state);
		strcpy(error_msg, buf);
		strcat(error_msg, job_id_list);
		strcat(error_msg, "Do you want to update the \"Ready to be Planned\" and \"Planned\" lists?");
	}    
	/* if all the jobs were in the correct job state, make error_msg NULL and return */
	else
		strcpy(error_msg, "");
	
	return(ER_NO_ERROR);

} /* check_job_state() */

static void
send_new_status_to_ims(
int		job_id,
char		*job_type,
char		*new_state)
{
	Job_Rec		query_data;
	char 		msgbuf[MAXBIGBUF];

	/*---------------------------------------------------*/
	/* get all necessary info to perform IMS queries ,   */
	/* if failed, return immediately to the caller       */
	/*---------------------------------------------------*/
	if (get_pending_job_data(job_id, job_type, &query_data) 
							!= ER_NO_ERROR)
	{
		XppsCreateErrorDialog(UxGetWidget(pps_plan),
			"Get Job Info from DB failed.", True, 0, 0);
		return;
	}

	/*---------------------------------------------------*/
        /* inform IMS about the job state change             */
	/*---------------------------------------------------*/
	send_IMS_order_status(job_id, job_type, new_state, query_data);

} /* send_new_status_to_ims */

/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
static char SccsFileId[] = "@(#)pps_query_util.c	1.13    10/31/97";

/* support routines for various callbacks for PPS GUI */

/*---------------------------------------------------*/
/* 1/26/96: Sally Chou                               */
/*      added priority_num to sort priority          */
/*      by types instead of alphabets.               */
/*---------------------------------------------------*/

#include <unistd.h>
#include <string.h>

#include "UxLib.h"
#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <X11/cursorfont.h>
#include <Xm/TextF.h>

#include "defs.h"
#include "PPShdr.h"
#include "pps.h"
#include "pps_query.h"
#include "pps_file_selector.h"
#include "pps_util.h"
#include "pps_db.h"
#include "pps_check_params.h"
#include "resload.h"
#include "PPSerr.h"

extern GlobalData globalData;
extern CS_CONNECTION *query_connection;
extern CS_CONNECTION *exec_connection;

#define CHECK_RETURN_STATUS \
{ \
	if (retcode != ER_NO_ERROR) \
		do_error_dialog(pps_query,(char *)get_pps_err_msg(retcode)); \
}

/* defines for ppsgui_orders query items */
#define	MAX_JOB_TYPE_CHARS		4
#define	MAX_PRIORITY_CHARS		10
#define	MAX_MEDIA_ID_CHARS		12
#define	MAX_MODE_CHARS			3
#define	MAX_PLATFORM_CHARS		2
#define	MAX_SENSOR_CHARS		3
#define	MAX_JOB_STATE_CHARS		15
#define	MAX_INSERT_TOP_FLAG_CHARS	4

/*
** number of characters from the start of the string
** (needs to be redefined if display position is changed)
*/ 
#define MEDIA_ID_OFFSET			22

/*-------------------------------------------------*/
/* skip the blanks first, then skip the letters    */
/* attention: the ptr will be at a space           */
/*            to the left of the word              */
/*-------------------------------------------------*/
#define PPS_GO_ONE_WORD_TO_THE_LEFT(ptr) { while(*ptr == ' ') ptr--;\
                                            while(*ptr != ' ') ptr--; }

extern char	ProgName[];

typedef struct resubmit_struct
{
	int		jobId;
	char	quickLookFlag[LOGICAL_STRLEN + 1];
	char	jobType[MAX_JOB_TYPE_CHARS+1];
} Resubmit_struct;

/* number of items returned from a query */
int query_num_items;

/* buffer to hold the last successful query string */
char queryBuf[MAXBIGBUF];

/* query file selector context */
char query_filename[MAXSMALLBUF];

/* a prototype or two */
void query_load_query(char *fname);
static void send_new_status_to_ims(int job_id, char *job_type, char *new_state);
static void update_query_list( int new_state, int job_id, int job_id_offset,
				char* media_id, int media_id_offset,
				int param_status_offset,  char* param_status_str,
				int age_offset, int state_offset,
				char* state_str, char* previous_state_str);

/* structure for rows returned from ppsgui_orders query */
struct query_row_dcl {
	char		job_type[MAX_JOB_TYPE_CHARS+1];
	char		quicklook_flag[LOGICAL_STRLEN+1+1];
	char		priority[MAX_PRIORITY_CHARS+1];
	char		media_id[MAX_MEDIA_ID_CHARS+1];
	char		mode[MAX_MODE_CHARS+1];
	char		platform[MAX_PLATFORM_CHARS+1];
	char		sensor[MAX_SENSOR_CHARS+1];
	int		rev;
	int		sequence;
	int		frame_id;
	int		subframe_id;
	int		job_id;
	int		order_id;
	int		item_id;
	int		priority_num;
	char		job_state[MAX_JOB_STATE_CHARS+1];
	float		pixel_spacing;
	int		age;
	char		insert_top_flag[MAX_INSERT_TOP_FLAG_CHARS+1];
} query_row;

/* query context structure */
struct pps_db_exec_dcl query_query;

/* context structure for executing the sp_check_pending stored procedure */
struct pps_db_exec_dcl pending_query;

/* context structure for resubmitting a job */
struct pps_db_exec_dcl resubmit_query;

#define MAX_JOBS 9999
 
 
/* structure for Order By option menu strings and clauses */
struct order_by_dcl {
	char *label;
	char *clause;
};
struct order_by_dcl query_order[] = {
	"Order Type",		"job_type, quicklook_flag",
	"Priority",		"priority_num",
	"Media ID",		"media_id",
	"Mode",			"mode",
	"Datatake ID",		"platform, sensor, rev, sequence",
	"Frame ID",		"frame_id, subframe_id",
	"Job ID",		"job_id",
	"Order & Item ID",	"order_id, item_id",
	"State",		"job_state",
	"Age (Days)",		"state_date",
	NULL,			NULL
};

/* struct for defining which resources to save and load */
struct query_settings_dcl {

	swidget *sw;	/* swidget id */

	/* flags to select the resources to save - note that you don't
	   necessarily want to always save, say, the LabelString when
	   your widget is a label! */

	int tb;		/* Togglebutton?	XmToggleButtonGetState */
	int om;		/* OptionMenu?		UxGetMenuHistory */
	int tf;		/* TextField?		UxGetText */
};
struct query_settings_dcl query_settings[] = {
	&tb_query_L1_Orders,			1, 0, 0,
	&tb_query_L1_QLK,			1, 0, 0,
	&tb_query_Scan_Orders,			1, 0, 0,
	&tb_query_Scan_QLK,			1, 0, 0,
	&om_query_sat,				0, 1, 0,
	&om_query_sens,				0, 1, 0,
	&om_query_activity,			0, 1, 0,
	&om_query_station,			0, 1, 0,
	&om_query_priority,			0, 1, 0,
	&tf_query_rev,				0, 0, 1,
	&tf_query_seq,				0, 0, 1,
	&tf_query_job_id,			0, 0, 1,
	&tf_query_order_id,			0, 0, 1,
	&tf_query_item_id,			0, 0, 1,
	&tf_query_frame_id,			0, 0, 1,
	&tf_query_subframe_id,			0, 0, 1,
	&om_query_product_type,			0, 1, 0,
	&tf_query_pixel_spacing,		0, 0, 1,
	&om_query_projection,			0, 1, 0,
	&tf_query_proc_gain,			0, 0, 1,
	&om_query_processor_mode,		0, 1, 0,
	&om_query_data_direction,		0, 1, 0,
	&om_query_state,			0, 1, 0,
	&om_query_media_type,			0, 1, 0,
	&tf_query_media_id,			0, 0, 1,
	&om_query_order_first,			0, 1, 0,
	&om_query_order_second,			0, 1, 0,
	&om_query_order_third,			0, 1, 0,
	&om_query_order_fourth,			0, 1, 0,
	NULL,					0, 0, 0
};

extern Widget create_print_results_dialog(Widget parent,
				XmString dialogTitle, char* filename);

static int subquery_retcode;

/* callback code for main screen query button */
void cb_main_query()
{
	char buf[10];

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_query_results_list));

	/* deselect all order types */
	XmToggleButtonSetState(UxGetWidget(tb_query_L1_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_query_L1_QLK), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_query_Scan_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_query_Scan_QLK), False, True);

	/* no items currently */
	query_num_items = 0;
	sprintf(buf, "%5d", query_num_items);
	UxPutLabelString(l_query_num_items, buf);

	/* initialize the rest of the query settings */
	UxPutMenuHistory(om_query_sat, pb_query_sat_any);
	UxPutMenuHistory(om_query_sens, pb_query_sens_any);
	UxPutText(tf_query_rev, "");
	UxPutText(tf_query_seq, "");
	UxPutMenuHistory(om_query_activity, pb_query_activity_any);
	UxPutMenuHistory(om_query_station, pb_query_station_any);
	UxPutText(tf_query_job_id, "");
	UxPutText(tf_query_order_id, "");
	UxPutText(tf_query_item_id, "");
	UxPutMenuHistory(om_query_priority, pb_query_priority_any);
	UxPutMenuHistory(om_query_state, pb_query_state_any);
	UxPutText(tf_query_frame_id, "");
	UxPutText(tf_query_subframe_id, "");
	UxPutMenuHistory(om_query_product_type, pb_query_product_type_any);
	UxPutText(tf_query_pixel_spacing, "");
	UxPutMenuHistory(om_query_projection, pb_query_projection_any);
	UxPutText(tf_query_proc_gain, "");
	UxPutMenuHistory(om_query_processor_mode, pb_query_processor_mode_any);
	UxPutMenuHistory(om_query_data_direction, pb_query_data_direction_any);
	UxPutMenuHistory(om_query_media_type, pb_query_media_type_any);
	UxPutText(tf_query_media_id, "");
	UxPutMenuHistory(om_query_order_first, pb_query_order_first_none);
	UxPutMenuHistory(om_query_order_second, pb_query_order_second_none);
	UxPutMenuHistory(om_query_order_third, pb_query_order_third_none);
	UxPutMenuHistory(om_query_order_fourth, pb_query_order_fourth_none);

	/* pop up the interface */
	UxPopupInterface(pps_query, no_grab);

	XMapRaised(XtDisplay(XtParent(UxGetWidget(pps_query))),
			XtWindow(XtParent(UxGetWidget(pps_query))));

	/* load the default query, if any */
	query_load_query("default.pqf");
}

/* common code for query screen order type togglebutton callbacks */
void do_query_new_order_type()
{
	int L1, Scan;

	L1 = XmToggleButtonGetState(UxGetWidget(tb_query_L1_Orders)) ||
		XmToggleButtonGetState(UxGetWidget(tb_query_L1_QLK));
	Scan = XmToggleButtonGetState(UxGetWidget(tb_query_Scan_Orders)) ||
		XmToggleButtonGetState(UxGetWidget(tb_query_Scan_QLK));

	if (Scan) {
		XtSetSensitive(UxGetWidget(om_query_sat), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_any), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_e1), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_e2), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_j1), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_r1), True);
		XtSetSensitive(UxGetWidget(rc_query_sat), True);
		XtSetSensitive(UxGetWidget(om_query_sens), True);
		XtSetSensitive(UxGetWidget(pb_query_sens_any), True);
		XtSetSensitive(UxGetWidget(pb_query_sens_s), True);
		XtSetSensitive(UxGetWidget(rc_query_sens), True);
		XtSetSensitive(UxGetWidget(om_query_activity), True);
		XtSetSensitive(UxGetWidget(pb_query_activity_any), True);
		XtSetSensitive(UxGetWidget(pb_query_activity_rlt), True);
		XtSetSensitive(UxGetWidget(pb_query_activity_dmp), True);
		XtSetSensitive(UxGetWidget(rc_query_activity), True);
		XtSetSensitive(UxGetWidget(om_query_station), True);
		XtSetSensitive(UxGetWidget(pb_query_station_any), True);
		XtSetSensitive(UxGetWidget(pb_query_station_fa), True);
		XtSetSensitive(UxGetWidget(pb_query_station_mc), True);
		XtSetSensitive(UxGetWidget(rc_query_station), True);
		XtSetSensitive(UxGetWidget(om_query_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_any), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_low), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_routine), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_high), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_urgent), True);
		XtSetSensitive(UxGetWidget(rc_query_priority), True);
		XtSetSensitive(UxGetWidget(rc_query_state), True);
		XtSetSensitive(UxGetWidget(om_query_state), True);
		XtSetSensitive(UxGetWidget(pb_query_state_any), True);
		XtSetSensitive(UxGetWidget(pb_query_state_pending), True);
		XtSetSensitive(UxGetWidget(pb_query_state_ready), True);
		XtSetSensitive(UxGetWidget(pb_query_state_available), True);
		XtSetSensitive(UxGetWidget(pb_query_state_submitted), True);
		XtSetSensitive(UxGetWidget(l_query_rev), True);
		XtSetSensitive(UxGetWidget(tf_query_rev), True);
		XtSetSensitive(UxGetWidget(l_query_seq), True);
		XtSetSensitive(UxGetWidget(tf_query_seq), True);
		XtSetSensitive(UxGetWidget(l_query_job_id), True);
		XtSetSensitive(UxGetWidget(tf_query_job_id), True);
		XtSetSensitive(UxGetWidget(l_query_order_id), True);
		XtSetSensitive(UxGetWidget(tf_query_order_id), True);
		XtSetSensitive(UxGetWidget(l_query_item_id), True);
		XtSetSensitive(UxGetWidget(tf_query_item_id), True);
		XtSetSensitive(UxGetWidget(l_query_frame_id), False);
		XtSetSensitive(UxGetWidget(tf_query_frame_id), False);
		XtSetSensitive(UxGetWidget(l_query_subframe_id), False);
		XtSetSensitive(UxGetWidget(tf_query_subframe_id), False);
		XtSetSensitive(UxGetWidget(om_query_product_type), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_any), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_standard), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_complex), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_ccsd), False);
		XtSetSensitive(UxGetWidget(rc_query_product_type), False);
		XtSetSensitive(UxGetWidget(l_query_pixel_spacing), False);
		XtSetSensitive(UxGetWidget(tf_query_pixel_spacing), False);
		XtSetSensitive(UxGetWidget(om_query_projection), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_any), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_ground_range), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_slant_range), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_lambert), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_ps), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_utm), False);
		XtSetSensitive(UxGetWidget(rc_query_projection), False);
		XtSetSensitive(UxGetWidget(l_query_proc_gain), False);
		XtSetSensitive(UxGetWidget(tf_query_proc_gain), False);
		XtSetSensitive(UxGetWidget(om_query_processor_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_any), True);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_continuous), True);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_scansar), True);
		XtSetSensitive(UxGetWidget(rc_query_processor_mode), True);
		XtSetSensitive(UxGetWidget(om_query_data_direction), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_any), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_forward), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_reverse), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_unknown), True);
		XtSetSensitive(UxGetWidget(rc_query_data_direction), True);
		XtSetSensitive(UxGetWidget(om_query_media_type), True);
		XtSetSensitive(UxGetWidget(pb_query_media_type_any), True);
		XtSetSensitive(UxGetWidget(pb_query_media_type_disk), True);
		XtSetSensitive(UxGetWidget(pb_query_media_type_dcrsi), True);
		XtSetSensitive(UxGetWidget(rc_query_media_type), True);
		XtSetSensitive(UxGetWidget(l_query_media_id), True);
		XtSetSensitive(UxGetWidget(tf_query_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_query), True);
#if 0
		XtSetSensitive(UxGetWidget(pb_query_check_params), True);
#endif
		XtSetSensitive(UxGetWidget(rc_query_order_first), True);
		XtSetSensitive(UxGetWidget(om_query_order_first), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_state), True);
		XtSetSensitive(UxGetWidget(rc_query_order_second), True);
		XtSetSensitive(UxGetWidget(om_query_order_second), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_state), True);
		XtSetSensitive(UxGetWidget(rc_query_order_third), True);
		XtSetSensitive(UxGetWidget(om_query_order_third), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_state), True);
		XtSetSensitive(UxGetWidget(rc_query_order_fourth), True);
		XtSetSensitive(UxGetWidget(om_query_order_fourth), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_state), True);
	} else if (L1) {
		XtSetSensitive(UxGetWidget(om_query_sat), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_any), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_e1), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_e2), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_j1), True);
		XtSetSensitive(UxGetWidget(pb_query_sat_r1), True);
		XtSetSensitive(UxGetWidget(rc_query_sat), True);
		XtSetSensitive(UxGetWidget(om_query_sens), True);
		XtSetSensitive(UxGetWidget(pb_query_sens_any), True);
		XtSetSensitive(UxGetWidget(pb_query_sens_s), True);
		XtSetSensitive(UxGetWidget(rc_query_sens), True);
		XtSetSensitive(UxGetWidget(om_query_activity), True);
		XtSetSensitive(UxGetWidget(pb_query_activity_any), True);
		XtSetSensitive(UxGetWidget(pb_query_activity_rlt), True);
		XtSetSensitive(UxGetWidget(pb_query_activity_dmp), True);
		XtSetSensitive(UxGetWidget(rc_query_activity), True);
		XtSetSensitive(UxGetWidget(om_query_station), True);
		XtSetSensitive(UxGetWidget(pb_query_station_any), True);
		XtSetSensitive(UxGetWidget(pb_query_station_fa), True);
		XtSetSensitive(UxGetWidget(pb_query_station_mc), True);
		XtSetSensitive(UxGetWidget(rc_query_station), True);
		XtSetSensitive(UxGetWidget(om_query_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_any), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_low), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_routine), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_high), True);
		XtSetSensitive(UxGetWidget(pb_query_priority_urgent), True);
		XtSetSensitive(UxGetWidget(rc_query_priority), True);
		XtSetSensitive(UxGetWidget(rc_query_state), True);
		XtSetSensitive(UxGetWidget(om_query_state), True);
		XtSetSensitive(UxGetWidget(pb_query_state_any), True);
		XtSetSensitive(UxGetWidget(pb_query_state_pending), True);
		XtSetSensitive(UxGetWidget(pb_query_state_ready), True);
		XtSetSensitive(UxGetWidget(pb_query_state_available), True);
		XtSetSensitive(UxGetWidget(pb_query_state_submitted), True);
		XtSetSensitive(UxGetWidget(l_query_rev), True);
		XtSetSensitive(UxGetWidget(tf_query_rev), True);
		XtSetSensitive(UxGetWidget(l_query_seq), True);
		XtSetSensitive(UxGetWidget(tf_query_seq), True);
		XtSetSensitive(UxGetWidget(l_query_job_id), True);
		XtSetSensitive(UxGetWidget(tf_query_job_id), True);
		XtSetSensitive(UxGetWidget(l_query_order_id), True);
		XtSetSensitive(UxGetWidget(tf_query_order_id), True);
		XtSetSensitive(UxGetWidget(l_query_item_id), True);
		XtSetSensitive(UxGetWidget(tf_query_item_id), True);
		XtSetSensitive(UxGetWidget(l_query_frame_id), True);
		XtSetSensitive(UxGetWidget(tf_query_frame_id), True);
		XtSetSensitive(UxGetWidget(l_query_subframe_id), True);
		XtSetSensitive(UxGetWidget(tf_query_subframe_id), True);
		XtSetSensitive(UxGetWidget(om_query_product_type), True);
		XtSetSensitive(UxGetWidget(pb_query_product_type_any), True);
		XtSetSensitive(UxGetWidget(pb_query_product_type_standard), True);
		XtSetSensitive(UxGetWidget(pb_query_product_type_complex), True);
		XtSetSensitive(UxGetWidget(pb_query_product_type_ccsd), True);
		XtSetSensitive(UxGetWidget(rc_query_product_type), True);
		XtSetSensitive(UxGetWidget(l_query_pixel_spacing), True);
		XtSetSensitive(UxGetWidget(tf_query_pixel_spacing), True);
		XtSetSensitive(UxGetWidget(om_query_projection), True);
		XtSetSensitive(UxGetWidget(pb_query_projection_any), True);
		XtSetSensitive(UxGetWidget(pb_query_projection_ground_range), True);
		XtSetSensitive(UxGetWidget(pb_query_projection_slant_range), True);
		XtSetSensitive(UxGetWidget(pb_query_projection_lambert), True);
		XtSetSensitive(UxGetWidget(pb_query_projection_ps), True);
		XtSetSensitive(UxGetWidget(pb_query_projection_utm), True);
		XtSetSensitive(UxGetWidget(rc_query_projection), True);
		XtSetSensitive(UxGetWidget(l_query_proc_gain), True);
		XtSetSensitive(UxGetWidget(tf_query_proc_gain), True);
		XtSetSensitive(UxGetWidget(om_query_processor_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_any), True);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_continuous), True);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_scansar), True);
		XtSetSensitive(UxGetWidget(rc_query_processor_mode), True);
		XtSetSensitive(UxGetWidget(om_query_data_direction), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_any), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_forward), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_reverse), True);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_unknown), True);
		XtSetSensitive(UxGetWidget(rc_query_data_direction), True);
		XtSetSensitive(UxGetWidget(om_query_media_type), True);
		XtSetSensitive(UxGetWidget(pb_query_media_type_any), True);
		XtSetSensitive(UxGetWidget(pb_query_media_type_disk), True);
		XtSetSensitive(UxGetWidget(pb_query_media_type_dcrsi), True);
		XtSetSensitive(UxGetWidget(rc_query_media_type), True);
		XtSetSensitive(UxGetWidget(l_query_media_id), True);
		XtSetSensitive(UxGetWidget(tf_query_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_query), True);
#if 0
		XtSetSensitive(UxGetWidget(pb_query_check_params), True);
#endif
		XtSetSensitive(UxGetWidget(rc_query_order_first), True);
		XtSetSensitive(UxGetWidget(om_query_order_first), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_first_state), True);
		XtSetSensitive(UxGetWidget(rc_query_order_second), True);
		XtSetSensitive(UxGetWidget(om_query_order_second), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_second_state), True);
		XtSetSensitive(UxGetWidget(rc_query_order_third), True);
		XtSetSensitive(UxGetWidget(om_query_order_third), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_third_state), True);
		XtSetSensitive(UxGetWidget(rc_query_order_fourth), True);
		XtSetSensitive(UxGetWidget(om_query_order_fourth), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_none), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_order_type), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_priority), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_media_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_mode), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_sat_sens_rev), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_frame_subframe), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_job_id), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_order_item), True);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_state), True);
	} else {
		XtSetSensitive(UxGetWidget(om_query_sat), False);
		XtSetSensitive(UxGetWidget(pb_query_sat_any), False);
		XtSetSensitive(UxGetWidget(pb_query_sat_e1), False);
		XtSetSensitive(UxGetWidget(pb_query_sat_e2), False);
		XtSetSensitive(UxGetWidget(pb_query_sat_j1), False);
		XtSetSensitive(UxGetWidget(pb_query_sat_r1), False);
		XtSetSensitive(UxGetWidget(rc_query_sat), False);
		XtSetSensitive(UxGetWidget(om_query_sens), False);
		XtSetSensitive(UxGetWidget(pb_query_sens_any), False);
		XtSetSensitive(UxGetWidget(pb_query_sens_s), False);
		XtSetSensitive(UxGetWidget(rc_query_sens), False);
		XtSetSensitive(UxGetWidget(om_query_activity), False);
		XtSetSensitive(UxGetWidget(pb_query_activity_any), False);
		XtSetSensitive(UxGetWidget(pb_query_activity_rlt), False);
		XtSetSensitive(UxGetWidget(pb_query_activity_dmp), False);
		XtSetSensitive(UxGetWidget(rc_query_activity), False);
		XtSetSensitive(UxGetWidget(om_query_station), False);
		XtSetSensitive(UxGetWidget(pb_query_station_any), False);
		XtSetSensitive(UxGetWidget(pb_query_station_fa), False);
		XtSetSensitive(UxGetWidget(pb_query_station_mc), False);
		XtSetSensitive(UxGetWidget(rc_query_station), False);
		XtSetSensitive(UxGetWidget(om_query_priority), False);
		XtSetSensitive(UxGetWidget(pb_query_priority_any), False);
		XtSetSensitive(UxGetWidget(pb_query_priority_low), False);
		XtSetSensitive(UxGetWidget(pb_query_priority_routine), False);
		XtSetSensitive(UxGetWidget(pb_query_priority_high), False);
		XtSetSensitive(UxGetWidget(pb_query_priority_urgent), False);
		XtSetSensitive(UxGetWidget(rc_query_priority), False);
		XtSetSensitive(UxGetWidget(rc_query_state), False);
		XtSetSensitive(UxGetWidget(om_query_state), False);
		XtSetSensitive(UxGetWidget(pb_query_state_any), False);
		XtSetSensitive(UxGetWidget(pb_query_state_pending), False);
		XtSetSensitive(UxGetWidget(pb_query_state_ready), False);
		XtSetSensitive(UxGetWidget(pb_query_state_available), False);
		XtSetSensitive(UxGetWidget(pb_query_state_submitted), False);
		XtSetSensitive(UxGetWidget(l_query_rev), False);
		XtSetSensitive(UxGetWidget(tf_query_rev), False);
		XtSetSensitive(UxGetWidget(l_query_seq), False);
		XtSetSensitive(UxGetWidget(tf_query_seq), False);
		XtSetSensitive(UxGetWidget(l_query_job_id), False);
		XtSetSensitive(UxGetWidget(tf_query_job_id), False);
		XtSetSensitive(UxGetWidget(l_query_order_id), False);
		XtSetSensitive(UxGetWidget(tf_query_order_id), False);
		XtSetSensitive(UxGetWidget(l_query_item_id), False);
		XtSetSensitive(UxGetWidget(tf_query_item_id), False);
		XtSetSensitive(UxGetWidget(l_query_frame_id), False);
		XtSetSensitive(UxGetWidget(tf_query_frame_id), False);
		XtSetSensitive(UxGetWidget(l_query_subframe_id), False);
		XtSetSensitive(UxGetWidget(tf_query_subframe_id), False);
		XtSetSensitive(UxGetWidget(om_query_product_type), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_any), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_standard), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_complex), False);
		XtSetSensitive(UxGetWidget(pb_query_product_type_ccsd), False);
		XtSetSensitive(UxGetWidget(rc_query_product_type), False);
		XtSetSensitive(UxGetWidget(l_query_pixel_spacing), False);
		XtSetSensitive(UxGetWidget(tf_query_pixel_spacing), False);
		XtSetSensitive(UxGetWidget(om_query_projection), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_any), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_ground_range), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_slant_range), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_lambert), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_ps), False);
		XtSetSensitive(UxGetWidget(pb_query_projection_utm), False);
		XtSetSensitive(UxGetWidget(rc_query_projection), False);
		XtSetSensitive(UxGetWidget(l_query_proc_gain), False);
		XtSetSensitive(UxGetWidget(tf_query_proc_gain), False);
		XtSetSensitive(UxGetWidget(om_query_processor_mode), False);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_any), False);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_continuous), False);
		XtSetSensitive(UxGetWidget(pb_query_processor_mode_scansar), False);
		XtSetSensitive(UxGetWidget(rc_query_processor_mode), False);
		XtSetSensitive(UxGetWidget(om_query_data_direction), False);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_any), False);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_forward), False);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_reverse), False);
		XtSetSensitive(UxGetWidget(pb_query_data_direction_unknown), False);
		XtSetSensitive(UxGetWidget(rc_query_data_direction), False);
		XtSetSensitive(UxGetWidget(om_query_media_type), False);
		XtSetSensitive(UxGetWidget(pb_query_media_type_any), False);
		XtSetSensitive(UxGetWidget(pb_query_media_type_disk), False);
		XtSetSensitive(UxGetWidget(pb_query_media_type_dcrsi), False);
		XtSetSensitive(UxGetWidget(rc_query_media_type), False);
		XtSetSensitive(UxGetWidget(l_query_media_id), False);
		XtSetSensitive(UxGetWidget(tf_query_media_id), False);
		XtSetSensitive(UxGetWidget(pb_query_query), False);
#if 0
		XtSetSensitive(UxGetWidget(pb_query_check_params), False);
#endif
		XtSetSensitive(UxGetWidget(rc_query_order_first), False);
		XtSetSensitive(UxGetWidget(om_query_order_first), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_none), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_order_type), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_priority), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_media_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_mode), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_frame_subframe), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_job_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_order_item), False);
		XtSetSensitive(UxGetWidget(pb_query_order_first_state), False);
		XtSetSensitive(UxGetWidget(rc_query_order_second), False);
		XtSetSensitive(UxGetWidget(om_query_order_second), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_none), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_order_type), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_priority), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_media_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_mode), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_frame_subframe), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_job_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_order_item), False);
		XtSetSensitive(UxGetWidget(pb_query_order_second_state), False);
		XtSetSensitive(UxGetWidget(rc_query_order_third), False);
		XtSetSensitive(UxGetWidget(om_query_order_third), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_none), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_order_type), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_priority), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_media_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_mode), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_frame_subframe), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_job_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_order_item), False);
		XtSetSensitive(UxGetWidget(pb_query_order_third_state), False);
		XtSetSensitive(UxGetWidget(rc_query_order_fourth), False);
		XtSetSensitive(UxGetWidget(om_query_order_fourth), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_none), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_order_type), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_priority), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_media_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_mode), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_sat_sens_rev), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_frame_subframe), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_job_id), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_order_item), False);
		XtSetSensitive(UxGetWidget(pb_query_order_fourth_state), False);
	}
}

/* callback for query_query - formats and adds a line to the Query screen
	query results list widget */
void add_row_to_query_list()
{
	int		retcode;
	char 		buf[MAXBIGBUF]; 
	char		buf2[MAXBIGBUF];
	char 		cmdbuf[MAXBIGBUF];
	char 		order_type[20];
	char 		datatake_id[20];
	char 		frame_subframe[20];
	char 		order_item[20];
	XmString 	listitem;
        char            GHA_avail[LOGICAL_STRLEN+1];
        char            TCE_avail[LOGICAL_STRLEN+1];
        char            SV_avail[LOGICAL_STRLEN+1];
        char            Scan_File_avail[LOGICAL_STRLEN+1];
        char            Params_File_avail[LOGICAL_STRLEN+1];

	/* preformatting for combo fields */
	strcpy(order_type, query_row.job_type);
	if (!strcasecmp(query_row.quicklook_flag, "yes")) {
		strcat(order_type, " ");
		strcat(order_type, "QLK");
	}
	sprintf(datatake_id, "%s %s %-5d %-2d", query_row.platform,
		query_row.sensor, query_row.rev, query_row.sequence);
	sprintf(frame_subframe, "%03d %-02d", query_row.frame_id,
		query_row.subframe_id);
	sprintf(order_item, "%08d %02d", query_row.order_id, query_row.item_id);

        /* format the row with data read from ppsgui_orders */
        sprintf(buf, " %-10.10s %-9.9s %-13.13s %-6.6s %-16.16s %-7.7s "
		"%08d  %-15.15s %-13.13s %03d   ",
		order_type, query_row.priority, query_row.media_id, 
		query_row.mode, datatake_id, frame_subframe, 
		query_row.job_id, order_item,
		query_row.job_state, query_row.age);

	/* execute the sp_check_pending stored procedure to determine the
		reasons for pending jobs only */
	if (strcmp(query_row.job_state,PENDING) == 0) 
	{
		pending_query.num_items = 0;
		pending_query.callback = 0;

                strcpy(TCE_avail,"   ");
                strcpy(GHA_avail,"   ");
		strcpy(SV_avail,"   ");
                strcpy(Scan_File_avail,"   ");
                strcpy(Params_File_avail,"   ");

		pps_db_bind_char(&pending_query, GHA_avail,
                    		sizeof(GHA_avail));
        	pps_db_bind_char(&pending_query,TCE_avail,
                    		sizeof(TCE_avail));
	        pps_db_bind_char(&pending_query, SV_avail,
                    		sizeof(SV_avail));
        	pps_db_bind_char(&pending_query, Scan_File_avail, 
				sizeof(Scan_File_avail));
		pps_db_bind_char(&pending_query, Params_File_avail, 
				sizeof(Params_File_avail));
 
                /* make the actual db command line */
                sprintf(cmdbuf, "exec sp_check_pending %d, \"%s\"",
			query_row.job_id, query_row.job_type); 
 
                /* exec the stored procedure */
                retcode = db_exec(&exec_connection, cmdbuf, &pending_query);
		/* we want to report error when the subquery is finished,
			not here, so remember the error if any */ 
		if (subquery_retcode == ER_NO_ERROR)
			subquery_retcode = retcode;
	}
	else
	/* for non-pending jobs, all params are assumed to be available */
	{
		strcpy(GHA_avail,"YES");
		strcpy(SV_avail,"YES");
		if (strcmp(query_row.platform,"R1") == 0)
			strcpy(TCE_avail,"N/A");
		else
			strcpy(TCE_avail,"YES");
		if (strcmp(query_row.job_type,"SCAN") == 0)
		{
			strcpy(Scan_File_avail,"N/A");
			strcpy(Params_File_avail,"N/A");
		}
		else
		{
                        strcpy(Scan_File_avail,"YES");
                        strcpy(Params_File_avail,"YES");
		}
	}

	/* format the row with params avail status */
        sprintf(buf2, "   %-3.3s    %-3.3s    %-3.3s      %-3.3s       %-3.3s", 
	   TCE_avail, GHA_avail, SV_avail, Scan_File_avail, Params_File_avail);
	strcat(buf,buf2);

	/* add it to the end of the list */
	listitem = XmStringCreateLocalized(buf);
	XmListAddItemUnselected(UxGetWidget(sw_query_results_list),
		listitem, 0);
	XmStringFree(listitem);

	/* kludge: motif keeps resetting the visibleItemCount */
	XtVaSetValues(UxGetWidget(sw_query_results_list),
				XmNvisibleItemCount, 18,
				0);

	/* count it */
	query_num_items++;

}

/* code to build the section of a where clause dealing with L1, L1 QLK,
	Scan, and Scan QLK orders */
void build_query_order(char *buf, int L1, int L1_QLK, int Scan, int Scan_QLK)
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
	        strcpy(buf, "(job_type = \"L1\" or "
	          "(job_type = \"SCAN\" and quicklook_flag = \"NO\"))");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "(job_type = \"L1\" or "
	          "(job_type = \"SCAN\" and quicklook_flag = \"YES\"))");
	      } else {
	        strcpy(buf, "(job_type = \"L1\")");
	      }
	    }
	  } else {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "((job_type = \"L1\" and quicklook_flag = \"NO\")"
	          " or job_type = \"SCAN\")");
	      } else {
		strcpy(buf, "(quicklook_flag = \"NO\")");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "((job_type = \"L1\" and quicklook_flag = \"NO\")"
	          " or (job_type = \"SCAN\" and quicklook_flag = \"YES\"))");
	      } else {
		strcpy(buf, "(job_type = \"L1\" and quicklook_flag = \"NO\")");
	      }
	    }
	  }
	} else {
	  if (L1_QLK) {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "((job_type = \"L1\" and quicklook_flag = \"YES\")"
	          " or job_type = \"SCAN\")");
	      } else {
	        strcpy(buf, "((job_type = \"L1\" and quicklook_flag = \"YES\")"
	          " or (job_type = \"SCAN\" and quicklook_flag = \"NO\"))");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "(quicklook_flag = \"YES\")");
	      } else {
	        strcpy(buf, "(job_type = \"L1\" and quicklook_flag = \"YES\")");
	      }
	    }
	  } else {
	    if (Scan) {
	      if (Scan_QLK) {
	        strcpy(buf, "(job_type = \"SCAN\")");
	      } else {
	        strcpy(buf, "(job_type = \"SCAN\" and"
	          " quicklook_flag = \"NO\")");
	      }
	    } else {
	      if (Scan_QLK) {
	        strcpy(buf, "(job_type = \"SCAN\" and"
	          " quicklook_flag = \"YES\")");
	      } else {
		/* should never get here! */
	        strcpy(buf, "");
	      }
	    }
	  }
	}
}

/* code to build and perform a query for the Query screen */
void do_query_query()
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
	char data_direction_buf[MAXSMALLBUF];
	char media_typebuf[MAXSMALLBUF];
	char media_idbuf[MAXSMALLBUF];
	int L1, L1_QLK, Scan, Scan_QLK;
	int num_wheres, num_orders;
	char orderbuf[MAXBIGBUF];
	char firstbuf[MAXSMALLBUF], secondbuf[MAXSMALLBUF];
	char thirdbuf[MAXSMALLBUF], fourthbuf[MAXSMALLBUF];
	int i;

	intFields planIntFields[9];

	planIntFields[0].label = l_query_rev;
	planIntFields[0].textField = tf_query_rev;
	planIntFields[1].label = l_query_seq;
	planIntFields[1].textField = tf_query_seq;
	planIntFields[2].label = l_query_job_id;
	planIntFields[2].textField = tf_query_job_id;
	planIntFields[3].label = l_query_order_id;
	planIntFields[3].textField = tf_query_order_id;
	planIntFields[4].label = l_query_item_id;
	planIntFields[4].textField = tf_query_item_id;
	planIntFields[5].label = l_query_frame_id;
	planIntFields[5].textField = tf_query_frame_id;
	planIntFields[6].label = l_query_subframe_id;
	planIntFields[6].textField = tf_query_subframe_id;
	planIntFields[7].label = l_query_pixel_spacing;
	planIntFields[7].textField = tf_query_pixel_spacing;
	planIntFields[8].label = l_query_proc_gain;
	planIntFields[8].textField = tf_query_proc_gain;

	/* validate all integer fields */
	if ( ! validate_int_fields(pps_query, planIntFields, 9))
		return;

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_query_results_list));

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_query)),
		XtWindow(UxGetWidget(pps_query)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_query)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_query)));

	/* no items currently */
	query_num_items = 0;
	sprintf(buf, "%5d", query_num_items);
	UxPutLabelString(l_query_num_items, buf);

	/* populate query_query structure */
	query_query.num_items = 0;
	query_query.callback = add_row_to_query_list;

	pps_db_bind_char(&query_query, query_row.job_type,
		sizeof(query_row.job_type));
	pps_db_bind_char(&query_query, query_row.quicklook_flag,
		sizeof(query_row.quicklook_flag));
	pps_db_bind_char(&query_query, query_row.priority,
		sizeof(query_row.priority));
	pps_db_bind_char(&query_query, query_row.media_id,
		sizeof(query_row.media_id));
	pps_db_bind_char(&query_query, query_row.mode,
		sizeof(query_row.mode));
	pps_db_bind_char(&query_query, query_row.platform,
		sizeof(query_row.platform));
	pps_db_bind_char(&query_query, query_row.sensor,
		sizeof(query_row.sensor));
	pps_db_bind_int(&query_query, &query_row.rev);
	pps_db_bind_int(&query_query, &query_row.frame_id);
	pps_db_bind_int(&query_query, &query_row.subframe_id);
	pps_db_bind_int(&query_query, &query_row.job_id);
	pps_db_bind_int(&query_query, &query_row.order_id);
	pps_db_bind_int(&query_query, &query_row.item_id);
	pps_db_bind_int(&query_query, &query_row.priority_num);
	pps_db_bind_int(&query_query, &query_row.sequence);
	pps_db_bind_char(&query_query, query_row.job_state,
		sizeof(query_row.job_state));
	pps_db_bind_float(&query_query, &query_row.pixel_spacing);
	pps_db_bind_int(&query_query, &query_row.age);
	pps_db_bind_char(&query_query, query_row.insert_top_flag,
		sizeof(query_row.insert_top_flag));

	/* buffer the basic query */
	strcpy(buf, 
		"select job_type, quicklook_flag, priority, media_id, "
		"mode, platform, sensor, "
		"isnull(rev,0), isnull(frame_id,0), isnull(subframe_id,0), "
		"isnull(job_id,0), isnull(order_id,0), "
		"isnull(item_id,0), isnull(priority_num,0), "
		"isnull(sequence,0), job_state, "
		"pixel_spacing, age = datediff(day, state_date, getdate()), "
		"insert_top_flag from ppsgui_orders ");

	/* init where clause */
	strcpy(wherebuf, "where ");
	num_wheres = 0;

	/* almost always need a clause for the order type */
	L1 = XmToggleButtonGetState(UxGetWidget(tb_query_L1_Orders));
	L1_QLK = XmToggleButtonGetState(UxGetWidget(tb_query_L1_QLK));
	Scan = XmToggleButtonGetState(UxGetWidget(tb_query_Scan_Orders));
	Scan_QLK = XmToggleButtonGetState(UxGetWidget(tb_query_Scan_QLK));
	strcpy(orderbuf, "");
	build_query_order(orderbuf, L1, L1_QLK, Scan, Scan_QLK);
	if (strlen(orderbuf)) {
		num_wheres++;
		strcat(wherebuf, orderbuf);
	}
	
	/* Satellite (aka platform) */
	strcpy(satbuf, get_om_label(om_query_sat));
	if (strcmp(satbuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "platform = \"");
		strcat(wherebuf, satbuf);
		strcat(wherebuf, "\"");
	}

	/* Sensor */
	strcpy(sensbuf, get_om_label(om_query_sens));
	if (strcmp(sensbuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "sensor = \"");
		strcat(wherebuf, sensbuf);
		strcat(wherebuf, "\"");
	}

	/* Rev */
	strcpy(revbuf, UxGetText(tf_query_rev));
	if (strcmp(revbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "rev = ");
		strcat(wherebuf, revbuf);
	}

	/* Seq */
	strcpy(seqbuf, UxGetText(tf_query_seq));
	if (strcmp(seqbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "sequence = ");
		strcat(wherebuf, seqbuf);
	}

	/* Activity */
	strcpy(activitybuf, get_om_label(om_query_activity));
	if (strcmp(activitybuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "activity_id = \"");
		strcat(wherebuf, activitybuf);
		strcat(wherebuf, "\"");
	}

	/* Station */
	strcpy(stationbuf, get_om_label(om_query_station));
	if (strcmp(stationbuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "station_id = \"");
		strcat(wherebuf, stationbuf);
		strcat(wherebuf, "\"");
	}

	/* Priority */
	strcpy(prioritybuf, get_om_label(om_query_priority));
	if (strcmp(prioritybuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "priority = \"");
		strcat(wherebuf, prioritybuf);
		strcat(wherebuf, "\"");
	}

	/* State */
	strcpy(statebuf, get_om_label(om_query_state));
	if (strcmp(statebuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "job_state = \"");
		strcat(wherebuf, statebuf);
		strcat(wherebuf, "\"");
	}

	/* Job ID */
	strcpy(jobidbuf, UxGetText(tf_query_job_id));
	if (strcmp(jobidbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "job_id = ");
		strcat(wherebuf, jobidbuf);
	}

	/* Order ID */
	strcpy(orderidbuf, UxGetText(tf_query_order_id));
	if (strcmp(orderidbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "order_id = ");
		strcat(wherebuf, orderidbuf);
	}

	/* Item ID */
	strcpy(itemidbuf, UxGetText(tf_query_item_id));
	if (strcmp(itemidbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "item_id = ");
		strcat(wherebuf, itemidbuf);
	}

	/* Keywords supported only for L1 queries, not for Scan */
	if ((L1 || L1_QLK) && !Scan && !Scan_QLK) {

		/* Frame ID */
		strcpy(frameidbuf, UxGetText(tf_query_frame_id));
		if (strcmp(frameidbuf, "")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "frame_id = ");
			strcat(wherebuf, frameidbuf);
		}

		/* Subframe ID */
		strcpy(subframeidbuf, UxGetText(tf_query_subframe_id));
		if (strcmp(subframeidbuf, "")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "subframe_id = ");
			strcat(wherebuf, subframeidbuf);
		}

		/* Product Type */
		strcpy(product_typebuf, get_om_label(om_query_product_type));
		if (strcmp(product_typebuf, "Any")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "product_type = \"");
			strcat(wherebuf, product_typebuf);
			strcat(wherebuf, "\"");
		}

		/* Pixel Spacing */
		strcpy(pixel_spacingbuf, UxGetText(tf_query_pixel_spacing));
		if (strcmp(pixel_spacingbuf, "")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "pixel_spacing = ");
			strcat(wherebuf, pixel_spacingbuf);
		}

		/* Projection */
		strcpy(projectionbuf, get_om_label(om_query_projection));
		if (strcmp(projectionbuf, "Any")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "projection = \"");
			strcat(wherebuf, projectionbuf);
			strcat(wherebuf, "\"");
		}

		/* Proc Gain */
		strcpy(proc_gainbuf, UxGetText(tf_query_proc_gain));
		if (strcmp(proc_gainbuf, "")) {
			if (num_wheres++)
				strcat(wherebuf, " and ");
			strcat(wherebuf, "processing_gain = ");
			strcat(wherebuf, proc_gainbuf);
		}

	}

	/* Processor Mode */
	strcpy(processor_modebuf,
		get_om_label(om_query_processor_mode));
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

	/* Data Direction */
	strcpy(data_direction_buf,
		get_om_label(om_query_data_direction));
	if (strcmp(data_direction_buf, "Any") != 0)
	{
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "data_direction = \"");
		strcat(wherebuf, data_direction_buf);
		strcat(wherebuf, "\"");
	}

	/* Media Type */
	strcpy(media_typebuf, get_om_label(om_query_media_type));
	if (strcmp(media_typebuf, "Any")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "media_type = \"");
		strcat(wherebuf, media_typebuf);
		strcat(wherebuf, "\"");
	}

	/* Media ID */
	strcpy(media_idbuf, UxGetText(tf_query_media_id));
	if (strcmp(media_idbuf, "")) {
		if (num_wheres++)
			strcat(wherebuf, " and ");
		strcat(wherebuf, "media_id = \"");
		strcat(wherebuf, media_idbuf);
		strcat(wherebuf, "\"");
	}

	/* problems with your where clause? */
	if (globalData.showSQL)
		printf("%s\n", wherebuf);

	/* append the where clause if needed */
	if (num_wheres)
		strcat(buf, wherebuf);


	/* init order by clause */
	strcpy(orderbuf, " order by ");
	num_orders = 0;

	/* First sort */
	strcpy(firstbuf, get_om_label(om_query_order_first));
	if (strcmp(firstbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; query_order[i].label; i++)
			if (!(strcmp(firstbuf, query_order[i].label)))
				break;
		strcat(orderbuf, query_order[i].clause);
	}

	/* Second sort */
	strcpy(secondbuf, get_om_label(om_query_order_second));
	if (strcmp(secondbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; query_order[i].label; i++)
			if (!(strcmp(secondbuf, query_order[i].label)))
				break;
		strcat(orderbuf, query_order[i].clause);
	}

	/* Third sort */
	strcpy(thirdbuf, get_om_label(om_query_order_third));
	if (strcmp(thirdbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; query_order[i].label; i++)
			if (!(strcmp(thirdbuf, query_order[i].label)))
				break;
		strcat(orderbuf, query_order[i].clause);
	}

	/* Fourth sort */
	strcpy(fourthbuf, get_om_label(om_query_order_fourth));
	if (strcmp(fourthbuf, "None")) {
		if (num_orders++)
			strcat(orderbuf, ", ");
		for (i = 0; query_order[i].label; i++)
			if (!(strcmp(fourthbuf, query_order[i].label)))
				break;
		strcat(orderbuf, query_order[i].clause);
	}

	/* check for Order By clause collisions */
	if ((!strcmp(firstbuf, secondbuf) && strcmp(firstbuf, "None")) ||
		(!strcmp(firstbuf, thirdbuf) && strcmp(firstbuf, "None")) ||
		(!strcmp(firstbuf, fourthbuf) && strcmp(firstbuf, "None")) ||
		(!strcmp(secondbuf, thirdbuf) && strcmp(secondbuf, "None")) ||
		(!strcmp(secondbuf, fourthbuf) && strcmp(secondbuf, "None")) ||
		(!strcmp(thirdbuf, fourthbuf) && strcmp(thirdbuf, "None"))) {
		sprintf(buf, "Duplicate Order By clause!\n\nNot useful, not supported.");
		do_error_dialog(pps_query,buf);
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_query)),
			XtWindow(UxGetWidget(pps_query)));
		return;
	}

	/* problems with your order by clause? */
	if (globalData.showSQL)
		printf("%s\n", orderbuf);

	/* append the order by clause if needed */
	if (num_orders)
		strcat(buf, orderbuf);

	/* do the Query screen query */
	subquery_retcode = ER_NO_ERROR;
	retcode = db_exec(&query_connection, buf, &query_query);
	if ((retcode != ER_NO_ERROR) || (subquery_retcode != ER_NO_ERROR))
	{
		if (retcode != ER_NO_ERROR) 
	                do_error_dialog(pps_query,(char *)get_pps_err_msg(retcode));
		else if (subquery_retcode != ER_NO_ERROR)
			do_error_dialog(pps_query,(char *)get_pps_err_msg(subquery_retcode));
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_query)),
			XtWindow(UxGetWidget(pps_query)));
		return;
	}


	/* save the query string to be used by "print result..." */
	(void)strcpy(queryBuf, buf);

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_query)),
		XtWindow(UxGetWidget(pps_query)));

	/* update the item count */
	sprintf(buf, "%5d", query_num_items);
	UxPutLabelString(l_query_num_items, buf);
}

/* routine to actually save a query screen query to a file */
void query_save_query(char *fname)
{
	FILE *fp;
	char new_filename[MAXSMALLBUF];
	char swname[MAXSMALLBUF];
	char swmenu[MAXSMALLBUF];
	char swtext[MAXSMALLBUF];
	char tbstate[MAXSMALLBUF];
	int i;
	char buf[MAXBIGBUF];

	strcpy(new_filename, fname);
	/* ensure it's a .pqf file - maybe add more intelligence here later,
		like making sure there isn't already an extension, or
		expanding the full path name? */
	if (strcmp(".pqf", &fname[strlen(fname) - strlen(".pqf")]))
		strcat(new_filename, ".pqf");

	/* open the file */
	fp = fopen(new_filename, "w");
	if (fp == NULL) {
		sprintf(buf, "Cannot write to File:\n\n%s.", new_filename);
		do_error_dialog(pps_query,buf);
		return;
	}

	/* for each swidget we care about... */
	for (i = 0; query_settings[i].sw; i++) {
		/* get the ascii version of the name */
		strcpy(swname, UxGetName(*query_settings[i].sw));
		/* possibly get and save the toggle state */
		if (query_settings[i].tb) {
			strcpy(tbstate, UxGetSet(*query_settings[i].sw));
			fprintf(fp, "%s.ToggleButton:\t%s\n", swname, tbstate);
		}
		/* possibly get and save the current option menu button */
		if (query_settings[i].om) {
			strcpy(swmenu, UxGetMenuHistory(
					*query_settings[i].sw));
			fprintf(fp, "%s.MenuHistory:\t%s\n", swname, swmenu);
		}
		/* possibly get and save the label text */
		if (query_settings[i].tf) {
			strcpy(swtext, UxGetText(*query_settings[i].sw));
			fprintf(fp, "%s.Text:\t%s\n", swname, swtext);
		}
	}

	/* close the file */
	fclose(fp);
}

/* routine to actually load a query screen query from a file */
void query_load_query(char *fname)
{
	FILE *fp;
	char swname[MAXSMALLBUF];
	char swmenu[MAXSMALLBUF];
	char swtext[MAXSMALLBUF];
	int i;
	char buf[MAXBIGBUF];
	char *resource;

	/* zap saved filename */
	strcpy(query_filename, "");

	/* make sure it's a .pqf file */
	if (strcmp(".pqf", &fname[strlen(fname) - strlen(".pqf")])) {
		sprintf(buf, "Not a .pqf file\n\n%s",
			fname);
		do_error_dialog(pps_query,buf);
		return;
	}

	/* save the filename */
	strcpy(query_filename, fname);

	/* open the file, just to see if you can */
	fp = fopen(fname, "r");
	if (fp == NULL) {
		sprintf(buf, "Unable to load query from\n\n%s", fname);
		do_error_dialog(pps_query,buf);
		return;
	}

	/* close the file */
	fclose(fp);

	/* load the file as resources */
	UxOverrideResources(fname);

	/* for each swidget we care about... */
	for (i = 0; query_settings[i].sw; i++) 
	{
                if (query_settings[i].tb) 
		{
			if ((resource = UxGetResource
				(UxGetName(*query_settings[i].sw), "ToggleButton")) != NULL)
			{
				if (strcmp(resource, "true"))
                               		 XmToggleButtonSetState(UxGetWidget(
                                        *query_settings[i].sw), False, True);
	                        else
	                                XmToggleButtonSetState(UxGetWidget(
                                        *query_settings[i].sw), True, True);
			}
                }
                /* possibly get the current option menu button */
                if (query_settings[i].om) 
		{
                        if ((resource = UxGetResource
                                (UxGetName(*query_settings[i].sw), "MenuHistory")) != NULL)
			{
                        	UxPutMenuHistory(*query_settings[i].sw, resource);
			}
                }
                /* possibly get the label text */
                if (query_settings[i].tf) 
		{
                        if ((resource = UxGetResource
                                (UxGetName(*query_settings[i].sw), "Text")) != NULL)
			{
                        	UxPutText(*query_settings[i].sw, resource);
			}
                }
	}
}

/* callback for file selector dialog box OK button */
void cb_load_query_file(Widget widget, XtPointer client_data,
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
	query_load_query(filename);

	/* free the file name */
	XtFree(filename);
}

/* callback for file selector dialog box OK button */
void cb_save_query_file(Widget widget, XtPointer client_data,
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
	query_save_query(filename);

	/* free the file name */
	XtFree(filename);
}

/* callback to load a saved query for the query screen */
cb_query_load_query(void)
{
	/* create and tweak the box */
	pps_file_selector = create_pps_file_selector(pps_query);
	XtUnmanageChild(XmFileSelectionBoxGetChild(UxGetWidget(
		pps_file_selector), XmDIALOG_HELP_BUTTON));
	UxPutPattern(pps_file_selector, "*.pqf");
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNcancelCallback, cb_no_file_selected, NULL);
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNokCallback, cb_load_query_file, NULL);
	UxPutDialogTitle(pps_file_selector, "Load Query...");

	/*----------------------------------*/
	/* make the cursor position visible */
	/* this doesn't seem to have effect */
	/*----------------------------------*/
	XtVaSetValues(XmFileSelectionBoxGetChild(
		UxGetWidget(pps_file_selector), XmDIALOG_FILTER_TEXT),
		XmNcursorPositionVisible, True,
		0);
	XtVaSetValues(XmFileSelectionBoxGetChild(
		UxGetWidget(pps_file_selector), XmDIALOG_TEXT),
		XmNcursorPositionVisible, True,
		0);
		

	/* pop it up */
	UxPopupInterface(pps_file_selector, nonexclusive_grab);
}

/* callback to save a previously loaded query for the query screen */
cb_query_save_query(void)
{
	/* overwrite previously loaded file, if any, else ask */
	if (strlen(query_filename))
		query_save_query(query_filename);
	else
		cb_query_save_query_as();
}

/* callback to save a query for the query screen */
cb_query_save_query_as(void)
{
	/* create and tweak the box */
	pps_file_selector = create_pps_file_selector(pps_query);
	XtUnmanageChild(XmFileSelectionBoxGetChild(UxGetWidget(
		pps_file_selector), XmDIALOG_HELP_BUTTON));
	UxPutPattern(pps_file_selector, "*.pqf");
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNcancelCallback, cb_no_file_selected, NULL);
	XtAddCallback(UxGetWidget(pps_file_selector),
		XmNokCallback, cb_save_query_file, NULL);
	UxPutDialogTitle(pps_file_selector, "Save Query As...");

	/* pop it up */
	UxPopupInterface(pps_file_selector, nonexclusive_grab);
}


/* callback to print results */
void
cb_query_print_results(void)
{
	Widget	results_list;
	char**	items;
	int	numItems;
	int	i, k;
	FILE*	fp;
	char filename[MAXSMALLBUF];
	char buf[MAXBIGBUF];
	int  mypid;
	XmString      dialogTitle;
	static Widget printResultsDialog=0;

	results_list = UxGetWidget(sw_query_results_list);

	/*--------------------------------------*/
	/* create a temp file for printing      */
	/*--------------------------------------*/
	mypid = getpid();
	sprintf(filename, "/tmp/%d_query.ascii", mypid);
	if ((fp=fopen(filename, "w")) == NULL)
	{
		fprintf(stderr, "Cannot open temp file for writing\n");
		return;
	}

	if (list_get_all_items(results_list, &items, &numItems))
	{
		/*------------------------------------*/
		/* print header                       */
		/*------------------------------------*/
		fprintf(fp, "\n\n");
		fprintf(fp, "Query Parameters:\n\n");
		fprintf(fp, queryBuf);
		fprintf(fp, "\n\n\n");
		fprintf(fp, "Query Results:\n\n");
		fprintf(fp, "===========================================================================================================================\n");
		fprintf(fp, " Order                                     Datatake        Frame              Order & Item                   Age      \n");
		fprintf(fp, " Type       Priority  Media ID     Mode       ID             ID     Job ID        ID          State         (Days)  Params\n");
		fprintf(fp, "===========================================================================================================================\n\n");

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
		fclose(fp);
		unlink(filename);
		XppsCreateErrorDialog(UxGetWidget(pps_query),
			"Nothing to print", True, 0, 0);
		return;
	}

	fclose(fp);

	dialogTitle = XmStringCreateLocalized("Query: Print Results");
	if (printResultsDialog == 0)
		printResultsDialog =
			create_print_results_dialog(UxGetWidget(pps_query),
				dialogTitle, filename);
	else
	{
		XtManageChild(printResultsDialog);
		XMapRaised(XtDisplay(XtParent(printResultsDialog)),
				XtWindow(XtParent(printResultsDialog)));
	}
	XmStringFree(dialogTitle);

}/* cb_query_print_results */

/* callback routine to redo all IMS queries for a pending job */  
recheck_pending()
{  
    int            i;
    int            numSelectedPos;
    int           *selectedPosList;
    int            rc;
    char          *selectedString;
    Params_Avail   avail_indicators;
    int            numSelectedItems;
    XmStringTable  selectedXmStrings;
    Widget         listW = UxGetWidget(sw_query_results_list);
    Widget         pps_queryW = UxGetWidget(pps_query);
    Display*       display = XtDisplay(pps_queryW);
    char           *paramStatusPtr, *ageStartPtr, *stateStartPtr;
    int            j, k;
    char           *newStateStr=0;
    char           *statePtr;
    int            job_id, order_id, item_id;
    char           *jobIdPtr=0;
    char	   *orderIdPtr, *itemIdPtr;
    char	   *mediaIdPtr;
    char           order_type[MAXSMALLBUF];
    char           job_state[MAXSMALLBUF];
    char	   mediaId[MAXSMALLBUF];

    /* get list of selected items */
    rc = XmListGetSelectedPos(listW, &selectedPosList, &numSelectedPos);

    XtVaGetValues(listW,
                    XmNselectedItemCount, &numSelectedItems,
                    XmNselectedItems, &selectedXmStrings,
                    0);
 
    /* return if no selected items found */
    if (numSelectedItems <= 0)
    {
        do_error_dialog(pps_query, "Please select a PENDING/READY job"); 
        return;
    }
	
    /* display busy cursor */
    XDefineCursor(display, XtWindow(pps_queryW),
                             XCreateFontCursor(display, XC_watch));
    XFlush(display);

    /* convert the selected list item to a string */
    XmStringGetLtoR(selectedXmStrings[0],
                     XmFONTLIST_DEFAULT_TAG, &selectedString);

    /*------------------------------------------------------*/
    /* order type if the first word                         */
    /*------------------------------------------------------*/
    if (sscanf(selectedString, "%s", order_type) != 1)
    {
        fprintf(stderr, "recheck_pending: cannot get order type\n");
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));
        return;
    }

    /*------------------------------------------------------*/
    /* start of param status strings is 5 words to the left */
    /*------------------------------------------------------*/
    paramStatusPtr = selectedString + strlen(selectedString) - 1;
    for (j=0; j < 5; j++)
    {
        PPS_GO_ONE_WORD_TO_THE_LEFT(paramStatusPtr);
    }
    /* move one space to the right to point to the char */
    paramStatusPtr++;

    /*------------------------------------------------------*/
    /* skip one word to the left to find age                */
    /*------------------------------------------------------*/
    ageStartPtr = paramStatusPtr - 1;
    for (j=0; j < 1; j++)
    {
        PPS_GO_ONE_WORD_TO_THE_LEFT(ageStartPtr);
    }
    ageStartPtr++;

    /*------------------------------------------------------*/
    /* next word to the left is state                       */
    /*------------------------------------------------------*/
    statePtr = ageStartPtr - 1;
    PPS_GO_ONE_WORD_TO_THE_LEFT(statePtr);
    statePtr++;
    if (sscanf(statePtr, "%s", job_state) != 1)
    {
        fprintf(stderr, "recheck_pending: cannot get job state.\n");
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));
        return;
    }

    /*------------------------------------------------------*/
    /* next word to the left is item_id                     */
    /*------------------------------------------------------*/
    itemIdPtr = statePtr -1;
    PPS_GO_ONE_WORD_TO_THE_LEFT(itemIdPtr);
    itemIdPtr++;
    if (sscanf(itemIdPtr, "%d", &item_id) != 1)
    {
        fprintf(stderr, "recheck_pending: cannot get item id.\n");
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));
        return;
    }
    
    /*------------------------------------------------------*/
    /* next word to the left is order_id                    */
    /*------------------------------------------------------*/
    orderIdPtr = itemIdPtr - 1;
    PPS_GO_ONE_WORD_TO_THE_LEFT(orderIdPtr);
    orderIdPtr++;
    if (sscanf(orderIdPtr, "%d", &order_id) != 1)
    {
        fprintf(stderr, "recheck_pending: cannot get order id.\n");
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));
        return;
    }

    /*------------------------------------------------------*/
    /* next word to the left is job_id                      */
    /*------------------------------------------------------*/
    jobIdPtr = orderIdPtr - 1;
    PPS_GO_ONE_WORD_TO_THE_LEFT(jobIdPtr);
    jobIdPtr++;

    if (sscanf(jobIdPtr, "%d", &job_id) != 1)
    {
        fprintf(stderr, "recheck_pending:cannot get job id\n");
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));
        return;
    }

    /*------------------------------------------------------*/
    /* do the job in pending or ready only                  */
    /*------------------------------------------------------*/
    if (strcmp(job_state,PENDING) != 0 &&
 	strcmp(job_state,READY) != 0)
    {
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));

        do_error_dialog(pps_query,
                    "The selected job is not in PENDING/READY state");
        return;
    }

    /*------------------------------------------------------*/
    /* obtain parameter availability */
    /*------------------------------------------------------*/
    strcpy(avail_indicators.media_id,"");
    rc = check_params(order_id, item_id, job_id, order_type, 
		job_state, &avail_indicators);
						
    /*-------------------------------------------------*/
    /* modify selected item to reflect changes in      */
    /* parameter availability                          */
    /*-------------------------------------------------*/
    if (rc != ER_JOB_IS_STILL_PENDING && rc != ER_JOB_IS_NOW_AVAIL &&
        rc != ER_JOB_IS_NOW_READY && rc != ER_JOB_IS_NOW_PENDING &&
	rc != ER_JOB_IS_STILL_READY)
    {
        /* clean up */
        XtFree(selectedString);
        XtFree(selectedPosList);
        XUndefineCursor(display, XtWindow(pps_queryW));

        do_error_dialog(pps_query, (char *)get_pps_err_msg(rc));
		return;
    }
    sprintf(paramStatusPtr,
                 "%-3.3s    %-3.3s    %-3.3s      %-3.3s       %-3.3s", 
                            avail_indicators.TCE_avail,
                            avail_indicators.GHA_avail, 
                            avail_indicators.SV_avail,
                            avail_indicators.Scan_File_avail, 
                            avail_indicators.Params_File_avail);
			
    /*----------------------------------*/
    /* replace the state with new state */			
    /* go to the beginning of state     */
    /*----------------------------------*/
    stateStartPtr = ageStartPtr - 1;
    PPS_GO_ONE_WORD_TO_THE_LEFT(stateStartPtr);
    /* move one space to the right to point to the char */
    stateStartPtr++;

    if (rc == ER_JOB_IS_STILL_PENDING || rc == ER_JOB_IS_NOW_PENDING)
		newStateStr = PENDING;
    else if (rc == ER_JOB_IS_NOW_AVAIL)
		newStateStr = AVAILABLE;
    else if (rc == ER_JOB_IS_NOW_READY || rc == ER_JOB_IS_STILL_READY)
		newStateStr = READY;
	        

    update_query_list(rc,
                      job_id,
                      jobIdPtr - selectedString,
		      avail_indicators.media_id,
    		      MEDIA_ID_OFFSET,
                      paramStatusPtr - selectedString,
                      paramStatusPtr,
                      ageStartPtr - selectedString,
                      stateStartPtr - selectedString,
                      newStateStr, job_state);

    /* clean up */
    XtFree(selectedString);
    XtFree(selectedPosList);
    XUndefineCursor(display, XtWindow(pps_queryW));

} /*recheck_pending*/

/*----------------------------------------------*/
/* update all the items in the whole query list */
/* which has the same job id                    */
/*----------------------------------------------*/
static void
update_query_list(
int		new_state,
int		job_id,
int		job_id_offset,
char*		media_id,
int		media_id_offset,
int		param_status_offset,  /* # char from the start of string */
char*		param_status_str,
int		age_offset,
int		state_offset,
char*		state_str,
char*		previous_state_str)
{  
    char          *list_item;
    XmString       new_item;
    int            numItems;
    XmStringTable  listXmStrings;
    Widget         listW = UxGetWidget(sw_query_results_list);
    int            i, j;

    XtVaGetValues(listW,
                    XmNitemCount, &numItems,
                    XmNitems, &listXmStrings,
                    0);
 
    /* return if list is empty */
    if (numItems <= 0)
        return;
	
    /* call the routine to do the IMS queries for each selected job */
    for (i = 0; i < numItems; i++) 
    {
            int temp_job_id;

            /*-------------------------------------------------*/
            /* convert the current list item to a string       */
            /*-------------------------------------------------*/
            XmStringGetLtoR(listXmStrings[i],
                    XmFONTLIST_DEFAULT_TAG, &list_item);

            /*-------------------------------------------------*/
            /* if job_id doesn't match, go on to the next item */
            /*-------------------------------------------------*/
            if (sscanf(list_item + job_id_offset, "%d", &temp_job_id) != 1)
            {
                fprintf(stderr, "update_query_list: Internal Error\n");
                return;
            }
            if (temp_job_id != job_id)
            {
                XtFree(list_item);
                continue;
            }

            /*-------------------------------------------------*/
            /* update the media_id if available                */
            /*-------------------------------------------------*/
printf("media_id_offset=%d\n\n",media_id_offset);
	    if (strcmp(media_id,"") != 0)
	    {
		(void)strncpy(list_item+media_id_offset, media_id,
				strlen(media_id));
	    }
	
	    /*-------------------------------------------------*/
	    /* if the job state is changed, update the age and */
	    /* state fields                                    */
	    /*-------------------------------------------------*/ 	 

	    if (strcmp(previous_state_str,state_str) != 0)
	    {
		char *temp_str;
 
                /* update the age field */
                (void)strncpy(list_item + age_offset, "000", 3);

		/* update the state field */ 
                (void)strncpy(list_item + state_offset,
                                  state_str, strlen(state_str));
                temp_str = list_item + state_offset + strlen(state_str);
                for (j=0;
                     j < (age_offset - state_offset - (int)strlen(state_str));
                                              j++)
                        *temp_str++ = ' ';
	    }

            /*-------------------------------------------------*/
            /* update the param status sub string              */
            /* regardless of the new state                     */
            /*-------------------------------------------------*/

            (void)strcpy(list_item + param_status_offset, param_status_str);

	    /* convert the C string to an XmString */
            new_item = XmStringCreateLocalized(list_item);
			      
            /* replace selected list item with the updated version */
            XmListReplaceItemsPos(listW, &new_item, 1, i+1);
            XmStringFree(new_item);
            XmListSelectPos(listW, i+1, False);
            XtFree(list_item);
    }

    /* pop up information dialog */
    do_information_dialog(pps_query, (char *)get_pps_err_msg(new_state));
      
} /*update_query_list*/

static int
remove_from_submitted_queue(
int jobId)
{
	int		success=0;
	char 	cmdbuf[MAXBIGBUF];

	resubmit_query.num_items = 0;
	resubmit_query.callback = 0;
	pps_db_bind_int(&resubmit_query, &success);
	sprintf(cmdbuf, "exec sp_remove_submit %d", jobId);
 
	/* exec the stored procedure */
	if (db_exec(&query_connection, cmdbuf, &resubmit_query) == ER_NO_ERROR)
		return (success);
	else
		return 0;

} /* remove_from_submitted_queue */

void  
do_resubmit_to_topCB(
Widget		w, 
XtPointer	clientData,
XtPointer	callData)
{
	Resubmit_struct	*resubmitData = (Resubmit_struct*) clientData;
	char 	cmdbuf[MAXBIGBUF];
	char 	msgbuf[MAXBIGBUF];

	if (remove_from_submitted_queue(resubmitData->jobId))
	{
		sprintf(cmdbuf, "exec sp_plan_ready_to_top %d, '%s'",
					resubmitData->jobId, resubmitData->quickLookFlag);
 
		/* exec the stored procedure */
		if (db_exec(&query_connection, cmdbuf, &resubmit_query) == ER_NO_ERROR)
		{
            send_new_status_to_ims(resubmitData->jobId,
                                   resubmitData->jobType, AVAILABLE);
 
			(void)sprintf(msgbuf, "Job[%d] has been resubmitted to Top",
								resubmitData->jobId);
			pps_logMsg(ProgName, PPS_INFO, msgbuf);
			do_query_query();
			do_plan_available_query();
		}
		else
		{
			XppsCreateErrorDialog(	UxGetWidget(pps_query),
									"Resubmit to Top of Plan failed.",
									True, 0, 0);
		}
	}
	else
	{
		XppsCreateErrorDialog(	UxGetWidget(pps_query),
								"Cannot remove from submitted queue.",
								True, 0, 0);
	}

	XtFree((char*)resubmitData);
} /* do_resubmit_to_topCB */

static int
get_selected_job(
int*	jobId,		/* IN/OUT: job id */
int*	quickLook,	/* IN/OUT: True or False */
char*   jobType)    /* IN/OUT: L1 or SCAN */ 
{
	XmStringTable	xmStrings;
	int				numItems=0;
	char*			string;
	char*			ptr;
	char			jobState[MAXSMALLBUF];
	int				i;

	XtVaGetValues(	UxGetWidget(sw_query_results_list),
					XmNselectedItemCount, &numItems,
					XmNselectedItems, &xmStrings,
					0);
	if (numItems <= 0)
	{
		XppsCreateErrorDialog(UxGetWidget(pps_query),
			"Nothing selected for re-submission", True, 0, 0);
		return 0;
	}

	if (XmStringGetLtoR(xmStrings[0], XmSTRING_DEFAULT_CHARSET, &string))
	{
		/* job state is at column 95 */
		if (sscanf(string + 94, "%s", jobState) != 1)
		{
			fprintf(stderr, "get_selected_job: Cannot parse job state\n");
			return 0;
		}
		else
		{
			if (strcmp(jobState, SUBMITTED) != 0)
			{
				XppsCreateErrorDialog(	UxGetWidget(pps_query),
										"Job is not in SUBMITTED state",
										True, 0, 0);
				return 0;
			}
		}

		/* job type starts at column 0 */
		if (sscanf(string, "%s", jobType) != 1)
                return 0;
		 
		/* quickLook flag is n the 2nd word ("QLK") */
		ptr = (char*)strtok(string, " ");
		ptr=(char*)strtok(0, " ");
		if (ptr)
		{
			if (strcmp(ptr, "QLK") == 0)
				*quickLook = 1;
			else
				*quickLook = 0;
		}
		else
			return 0;

		/* job id starts at column 69 */
		if (ptr = string + 68)
		{
			if (sscanf(ptr, "%d", jobId) != 1)
				return 0;
		}
		else
			return 0;

#ifdef DEBUG
        printf("get_selected_job: job_id=%d,job_type=%s,job_state=%s\n",
				*jobId,jobType,jobState);
#endif

		XtFree(string);
		return 1;
	}
	else
		return 0;
	
} /* get_selected_job */

void  
cb_resubmit_to_top(
Widget		w, 
XtPointer	clientData, 
XtPointer	callData)
{
	int		jobId;
	int		quickLook;
	char 	msg[MAXBIGBUF];
	char	jobType[MAX_JOB_TYPE_CHARS+1];
	Resubmit_struct		*resubmitData;

	if ( ! get_selected_job(&jobId, &quickLook, jobType))
		return;

	resubmitData = (Resubmit_struct*)XtMalloc(sizeof(Resubmit_struct));
	resubmitData->jobId = jobId;
	if (quickLook)
		strcpy(resubmitData->quickLookFlag, "YES");
	else
		strcpy(resubmitData->quickLookFlag, "NO");
	strcpy(resubmitData->jobType,jobType);

	(void) sprintf(msg, "Do you really want to re-submit Job # %d?", jobId);
	XppsCreateQuestionDialog(	UxGetWidget(pps_query),
								msg,
								True,
								do_resubmit_to_topCB,
								(XtPointer)resubmitData, 0, 0);
} /* cb_resubmit_to_top */

void  
do_resubmit_to_bottomCB(
Widget		w, 
XtPointer	clientData,
XtPointer	callData)
{
	Resubmit_struct	*resubmitData = (Resubmit_struct*) clientData;
	char 	cmdbuf[MAXBIGBUF];
	char 	msgbuf[MAXBIGBUF];

	if (remove_from_submitted_queue(resubmitData->jobId))
	{
		sprintf(cmdbuf, "exec sp_plan_ready_to_bottom %d, '%s'",
					resubmitData->jobId, resubmitData->quickLookFlag);
 
		/* exec the stored procedure */
		if (db_exec(&query_connection, cmdbuf, &resubmit_query) == ER_NO_ERROR)
		{
	        send_new_status_to_ims(resubmitData->jobId,
   					               resubmitData->jobType, AVAILABLE);

			(void)sprintf(msgbuf, "Job[%d] has been resubmitted to Bottom",
								resubmitData->jobId);
			pps_logMsg(ProgName, PPS_INFO, msgbuf);
			do_query_query();
			do_plan_available_query();
		}
		else
		{
			XppsCreateErrorDialog(	UxGetWidget(pps_query),
									"Resubmit to Bottom of Plan failed.",
									True, 0, 0);
		}
	}
	else
	{
		XppsCreateErrorDialog(	UxGetWidget(pps_query),
								"Cannot remove from submitted queue.",
								True, 0, 0);
	}

	XtFree((char*)resubmitData);

} /* do_resubmit_to_bottomCB */

void  
cb_resubmit_to_bottom(
Widget		w, 
XtPointer	clientData, 
XtPointer	callData)
{
	int		jobId;
	int		quickLook;
	char 	msg[MAXBIGBUF];
	char	jobType[MAX_JOB_TYPE_CHARS+1];
	Resubmit_struct		*resubmitData;

	if ( ! get_selected_job(&jobId, &quickLook, jobType))
		return;

	resubmitData = (Resubmit_struct*)XtMalloc(sizeof(Resubmit_struct));
	resubmitData->jobId = jobId;
	if (quickLook)
		strcpy(resubmitData->quickLookFlag, "YES");
	else
		strcpy(resubmitData->quickLookFlag, "NO");
	strcpy(resubmitData->jobType,jobType);

	(void) sprintf(msg, "Do you really want to re-submit Job # %d?", jobId);
	XppsCreateQuestionDialog(UxGetWidget(pps_query),
		msg, True, do_resubmit_to_bottomCB, (XtPointer)resubmitData, 0, 0);

} /* cb_resubmit_to_bottom */

/* callback code for clear query setting button */
void cb_query_clear_search()
{
	char buf[10];

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_query_results_list));

	/* deselect all order types */
	XmToggleButtonSetState(UxGetWidget(tb_query_L1_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_query_L1_QLK), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_query_Scan_Orders), False, True);
	XmToggleButtonSetState(UxGetWidget(tb_query_Scan_QLK), False, True);

	/* no items currently */
	query_num_items = 0;
	sprintf(buf, "%5d", query_num_items);
	UxPutLabelString(l_query_num_items, buf);

	/* initialize the rest of the query settings */
	UxPutMenuHistory(om_query_sat, pb_query_sat_any);
	UxPutMenuHistory(om_query_sens, pb_query_sens_any);
	UxPutText(tf_query_rev, "");
	UxPutText(tf_query_seq, "");
	UxPutMenuHistory(om_query_activity, pb_query_activity_any);
	UxPutMenuHistory(om_query_station, pb_query_station_any);
	UxPutText(tf_query_job_id, "");
	UxPutText(tf_query_order_id, "");
	UxPutText(tf_query_item_id, "");
	UxPutMenuHistory(om_query_priority, pb_query_priority_any);
	UxPutMenuHistory(om_query_state, pb_query_state_any);
	UxPutText(tf_query_frame_id, "");
	UxPutText(tf_query_subframe_id, "");
	UxPutMenuHistory(om_query_product_type, pb_query_product_type_any);
	UxPutText(tf_query_pixel_spacing, "");
	UxPutMenuHistory(om_query_projection, pb_query_projection_any);
	UxPutText(tf_query_proc_gain, "");
	UxPutMenuHistory(om_query_processor_mode, pb_query_processor_mode_any);
	UxPutMenuHistory(om_query_data_direction, pb_query_data_direction_any);
	UxPutMenuHistory(om_query_media_type, pb_query_media_type_any);
	UxPutText(tf_query_media_id, "");
	UxPutMenuHistory(om_query_order_first, pb_query_order_first_none);
	UxPutMenuHistory(om_query_order_second, pb_query_order_second_none);
	UxPutMenuHistory(om_query_order_third, pb_query_order_third_none);
	UxPutMenuHistory(om_query_order_fourth, pb_query_order_fourth_none);
}


static void
send_new_status_to_ims(
int     job_id,
char    *job_type,
char    *new_state)
{
    Job_Rec     query_data;
    char        msgbuf[MAXBIGBUF];
 
    /*---------------------------------------------------*/
    /* get all necessary info to perform IMS queries ,   */
    /* if failed, return immediately to the caller       */
    /*---------------------------------------------------*/
    if (get_pending_job_data(job_id, job_type, &query_data)
                            != ER_NO_ERROR)
    {
        XppsCreateErrorDialog(UxGetWidget(pps_query),
            "Get Job Info from DB failed.", True, 0, 0);
        return;
    }
 
    /*---------------------------------------------------*/
    /* inform IMS about the job state change             */
    /*---------------------------------------------------*/
    send_IMS_order_status(job_id, job_type, new_state, query_data);
 
} /* send_new_status_to_ims */


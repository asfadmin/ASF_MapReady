/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
static char SccsFileId[] = "@(#)pps_policy_util.c	1.1    11/21/96";

/* support routines for various callbacks for PPS GUI */

#include <unistd.h>

#include "UxLib.h"
#include <Xm/Xm.h>
#include <X11/cursorfont.h>

#include "pps.h"
#include "pps_policy.h"
#include "pps_util.h"
#include "pps_db.h"
#include "resload.h"
#include "PPSerr.h"

extern GlobalData globalData;
extern CS_CONNECTION *query_connection;
extern CS_CONNECTION *exec_connection;

/* defines for ppsgui_orders policy items */
#define	MAX_JOB_TYPE_CHARS		4
#define	MAX_QUICKLOOK_FLAG_CHARS	4
#define	MAX_PRIORITY_CHARS		10
#define	MAX_MEDIA_ID_CHARS		12
#define	MAX_MODE_CHARS			3
#define	MAX_PLATFORM_CHARS		2
#define	MAX_SENSOR_CHARS		3
#define	MAX_JOB_STATE_CHARS		15
#define	MAX_INSERT_TOP_FLAG_CHARS	4
#define	MAX_AUTO_SUBMIT_CHARS		4

/* number of items returned from a query */
int policy_num_items;

/* job context */
struct job_context_dcl {
	int	job_id;
	int	prod_time;
	char	insert_top_flag[MAX_INSERT_TOP_FLAG_CHARS+1];
};
 
#define MAX_PENDING_JOBS	9999
 
struct job_context_dcl pending_jobs[MAX_PENDING_JOBS];

/* structure for rows returned from ppsgui_orders policy query */
struct query_row_dcl {
	char		job_type[MAX_JOB_TYPE_CHARS+1];
	char		quicklook_flag[MAX_QUICKLOOK_FLAG_CHARS+1];
	char		priority[MAX_PRIORITY_CHARS+1];
	char		media_id[MAX_MEDIA_ID_CHARS+1];
	char		mode[MAX_MODE_CHARS+1];
	char		platform[MAX_PLATFORM_CHARS+1];
	char		sensor[MAX_SENSOR_CHARS+1];
	int		rev;
	int		frame_id;
	int		subframe_id;
	int		job_id;
	int		order_id;
	int		item_id;
	char		job_state[MAX_JOB_STATE_CHARS+1];
	float		pixel_spacing;
	int		age;
	int		prod_time;	/* needs attention - jtg */
	char		insert_top_flag[MAX_INSERT_TOP_FLAG_CHARS+1];
} policy_row;

/* query context structure */
struct pps_db_exec_dcl policy_query;

/* buffer to hold the last successful query string */
char queryBuf[MAXBIGBUF];

/* policy exec context structure */
struct pps_db_exec_dcl policy_exec;

/* structure for rows returned from policy table query */
struct table_query_row_dcl {
	char		job_type[MAX_JOB_TYPE_CHARS+1];
	char		quicklook_flag[MAX_QUICKLOOK_FLAG_CHARS+1];
	char		priority[MAX_PRIORITY_CHARS+1];
	char		auto_submit[MAX_AUTO_SUBMIT_CHARS+1];
} policy_table_row;

/* query context structure */
struct pps_db_exec_dcl policy_table_query;

extern Widget create_print_results_dialog(Widget parent,
			XmString dialogTitle, char* filename);

/* callback for policy_table_query - sets the appropriate matrix button */
void set_policy_button()
{
	if (globalData.showSQL)
		printf("[%s] [%s] [%s] [%s]\n", policy_table_row.job_type, policy_table_row.quicklook_flag, policy_table_row.priority, policy_table_row.auto_submit);

	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "URGENT"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_L1), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "URGENT"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_L1_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "URGENT"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_Scan), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "URGENT"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_Scan_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "HIGH"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_High_L1), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "HIGH"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_High_L1_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "HIGH"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_High_Scan), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "HIGH"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_High_Scan_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "ROUTINE"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_L1), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "ROUTINE"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_L1_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "ROUTINE"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_Scan), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "ROUTINE"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_Scan_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "LOW"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Low_L1), True, True);
	if (!strcmp(policy_table_row.job_type, "L1") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "LOW"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Low_L1_QLK), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "NO ") &&
			!strcmp(policy_table_row.priority, "LOW"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Low_Scan), True, True);
	if (!strcmp(policy_table_row.job_type, "SCAN") &&
			!strcmp(policy_table_row.quicklook_flag, "YES") &&
			!strcmp(policy_table_row.priority, "LOW"))
		XmToggleButtonSetState(UxGetWidget(tb_policy_Low_Scan_QLK), True, True);
}

/* callback for policy_query - formats and adds a line to the Query screen
	query results list widget */
void add_row_to_policy_list()
{
	char buf[MAXBIGBUF];
	char order_type[20];
	char sat_sens_rev[20];
	char frame_subframe[20];
	char order_item[20];
	XmString listitem;

	/* preformatting for combo fields */
	strcpy(order_type, policy_row.job_type);
	if (!strcasecmp(policy_row.quicklook_flag, "yes")) {
		strcat(order_type, " ");
		strcat(order_type, "QLK");
	}
	sprintf(sat_sens_rev, "%s %s %d", policy_row.platform,
		policy_row.sensor, policy_row.rev);
	sprintf(frame_subframe, "%d %d", policy_row.frame_id,
		policy_row.subframe_id);
	sprintf(order_item, "%08d %02d", policy_row.order_id, policy_row.item_id);

	/* format the whole row */
	sprintf(buf, " %-10.10s %-9.9s %-12.12s %-7.7s %-13.13s %-7.7s %08d  %-15.15s %-10.10s %03d      %-03d  %3.3s", order_type, policy_row.priority, policy_row.media_id, policy_row.mode, sat_sens_rev, frame_subframe, policy_row.job_id, order_item, policy_row.job_state, policy_row.age, policy_row.prod_time,
policy_row.insert_top_flag);

	/* add it to the end of the list */
	listitem = XmStringCreateLocalized(buf);
	XmListAddItemUnselected(UxGetWidget(sw_policy_query_results_list),
		listitem, 0);
	XmStringFree(listitem);

	/* remember it */
	if (policy_num_items < MAX_PENDING_JOBS) {
		pending_jobs[policy_num_items].job_id = policy_row.job_id;
		pending_jobs[policy_num_items].prod_time =
			policy_row.prod_time;
		strcpy(pending_jobs[policy_num_items].insert_top_flag,
			policy_row.insert_top_flag);
	}

	/* count it */
	policy_num_items++;
}

/* code to build and perform a query for the Query screen */
void do_policy_query()
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
	char deskewbuf[MAXSMALLBUF], output_formatbuf[MAXSMALLBUF];
	char media_typebuf[MAXSMALLBUF];
	char media_idbuf[MAXSMALLBUF];
	int L1, L1_QLK, Scan, Scan_QLK;
	int i;
	int num_wheres;

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_policy_query_results_list));

	/* clear pending_jobs */
	policy_num_items = 0;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_policy)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_policy)));

	/* populate policy_query structure */
	policy_query.num_items = 0;
	policy_query.callback = add_row_to_policy_list;
	pps_db_bind_char(&policy_query, policy_row.job_type,
		sizeof(policy_row.job_type));
	pps_db_bind_char(&policy_query, policy_row.quicklook_flag,
		sizeof(policy_row.quicklook_flag));
	pps_db_bind_char(&policy_query, policy_row.priority,
		sizeof(policy_row.priority));
	pps_db_bind_char(&policy_query, policy_row.media_id,
		sizeof(policy_row.media_id));
	pps_db_bind_char(&policy_query, policy_row.mode,
		sizeof(policy_row.mode));
	pps_db_bind_char(&policy_query, policy_row.platform,
		sizeof(policy_row.platform));
	pps_db_bind_char(&policy_query, policy_row.sensor,
		sizeof(policy_row.sensor));
	pps_db_bind_int(&policy_query, &policy_row.rev);
	pps_db_bind_int(&policy_query, &policy_row.frame_id);
	pps_db_bind_int(&policy_query, &policy_row.subframe_id);
	pps_db_bind_int(&policy_query, &policy_row.job_id);
	pps_db_bind_int(&policy_query, &policy_row.order_id);
	pps_db_bind_int(&policy_query, &policy_row.item_id);
	pps_db_bind_char(&policy_query, policy_row.job_state,
		sizeof(policy_row.job_state));
	pps_db_bind_float(&policy_query, &policy_row.pixel_spacing);
	pps_db_bind_int(&policy_query, &policy_row.age);
	pps_db_bind_int(&policy_query, &policy_row.prod_time);
	pps_db_bind_char(&policy_query, policy_row.insert_top_flag,
		sizeof(policy_row.insert_top_flag));

	/* buffer the basic query */
	strcpy(buf, 
		"select job_type, quicklook_flag, priority, media_id, "
		"mode, platform, sensor, isnull(rev,0), isnull(frame_id,0), "
		"isnull(subframe_id,0), isnull(job_id,0), isnull(order_id,0), "
		"isnull(item_id,0), job_state, "
		"pixel_spacing, age = datediff(day, state_date, getdate()), "
		"prod_time, insert_top_flag "
		"from ppsgui_orders ");

	/* init where clause */
	strcpy(wherebuf, "where job_state = \"PENDING\" and (");
	num_wheres = 0;

	/* handle button matrix - what a pain! */
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Urgent_L1))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"NO\" and priority = \"URGENT\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Urgent_L1_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"YES\" and priority = \"URGENT\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Urgent_Scan))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"NO\" and priority = \"URGENT\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Urgent_Scan_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"YES\" and priority = \"URGENT\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_High_L1))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"NO\" and priority = \"HIGH\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_High_L1_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"YES\" and priority = \"HIGH\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_High_Scan))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"NO\" and priority = \"HIGH\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_High_Scan_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"YES\" and priority = \"HIGH\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Routine_L1))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"NO\" and priority = \"ROUTINE\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Routine_L1_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"YES\" and priority = \"ROUTINE\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Routine_Scan))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"NO\" and priority = \"ROUTINE\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Routine_Scan_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"YES\" and priority = \"ROUTINE\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Low_L1))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"NO\" and priority = \"LOW\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Low_L1_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"L1\" and quicklook_flag = \"YES\" and priority = \"LOW\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Low_Scan))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"NO\" and priority = \"LOW\") ");
	}
	if (XmToggleButtonGetState(UxGetWidget(tb_policy_Low_Scan_QLK))) {
		if (num_wheres++)
			strcat(wherebuf, "or ");
		strcat(wherebuf, "(job_type = \"SCAN\" and quicklook_flag = \"YES\" and priority = \"LOW\") ");
	}

	/* no point to querying if no buttons pushed */
	if (num_wheres == 0) {
		XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
			XtWindow(UxGetWidget(pps_policy)));
		return;
	}

	/* append the where(s) */
	strcat(buf, wherebuf);
	strcat(buf, ")");
	if (globalData.showSQL)
		printf("%s\n", buf);

	/* do the Policy screen query */
	retcode = db_exec(&exec_connection,buf,&policy_query);
	if (retcode != ER_NO_ERROR) {
		fprintf(stderr, "Failed to execute query for policy screen\n");
		do_error_dialog(pps_policy,(char *)get_pps_err_msg(retcode));
		/* restore default cursor */
		XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
			XtWindow(UxGetWidget(pps_policy)));
		return;
	}

	/* save the query string to be used by "print result..." */
	(void)strcpy(queryBuf, buf);

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)));
}

/* callback code for main screen query button */
void cb_main_policy()
{
	char buf[MAXBIGBUF];
	int  retcode;

	/* clear the query results area */
	XmListDeleteAllItems(UxGetWidget(sw_policy_query_results_list));

	/* no items currently */
	policy_num_items = 0;

	/* deselect all order types */
	XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_L1), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_L1_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_Scan), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Urgent_Scan_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_High_L1), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_High_L1_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_High_Scan), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_High_Scan_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_L1), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_L1_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_Scan), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Routine_Scan_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Low_L1), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Low_L1_QLK), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Low_Scan), False, False);
	XmToggleButtonSetState(UxGetWidget(tb_policy_Low_Scan_QLK), False, False);

	/* populate policy_table_query structure */
	policy_table_query.num_items = 0;
	policy_table_query.callback = set_policy_button;
	pps_db_bind_char(&policy_table_query, policy_table_row.job_type,
		sizeof(policy_table_row.job_type));
	pps_db_bind_char(&policy_table_query, policy_table_row.quicklook_flag,
		sizeof(policy_table_row.quicklook_flag));
	pps_db_bind_char(&policy_table_query, policy_table_row.priority,
		sizeof(policy_table_row.priority));
	pps_db_bind_char(&policy_table_query, policy_table_row.auto_submit,
		sizeof(policy_table_row.auto_submit));

	/* buffer the query - get only the ones that are turned on */
	strcpy(buf, 
		"select job_type, quicklook_flag, job_priority, auto_flag "
		"from policy where auto_flag = \"YES\"");
	if (globalData.showSQL)
		printf("%s\n", buf);

	/* do the Policy screen query */
	retcode = db_exec(&query_connection, buf, &policy_table_query);
	if (retcode != ER_NO_ERROR) {
		fprintf(stderr, "Failed to execute query for policy table\n");
		do_error_dialog(pps_policy,(char *)get_pps_err_msg(retcode));
		return;
	}

	/* pop up the interface */
	UxPopupInterface(pps_policy, no_grab);
	XMapRaised(XtDisplay(XtParent(UxGetWidget(pps_policy))),
				XtWindow(XtParent(UxGetWidget(pps_policy))));

	/* load the matching pending jobs */
	do_policy_query();
}

/* callback to select all ready jobs */
cb_policy_select_all()
{
	int i;
	char policy[MAXSMALLBUF];
	Widget listwid = UxGetWidget(sw_policy_query_results_list);

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_policy)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_policy)));

	/* deselect all items */
	XmListDeselectAllItems(listwid);

	/* hack the selection policy */
	strcpy(policy, UxGetSelectionPolicy(sw_policy_query_results_list));
	UxPutSelectionPolicy(sw_policy_query_results_list, "multiple_select");

	/* select all items */
	for (i = 0; i < policy_num_items; i++)
		XmListSelectPos(listwid, i+1, False);

	/* restore the selection policy */
	UxPutSelectionPolicy(sw_policy_query_results_list, policy);

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)));
}

/* callback to deselect all ready jobs */
cb_policy_deselect_all()
{
	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_policy)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_policy)));

	/* deselect all ready items */
	XmListDeselectAllItems(UxGetWidget(sw_policy_query_results_list));

	/* restore default cursor */
	XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)));
}

/* common Insert Top routine for selected ready jobs */
do_policy_IT(char *bool)
{
	char buf[MAXBIGBUF];
	int i;
	int retcode;
	int position_count;
	int *position_list;
	int stat;

	/* get list of selected items */
	stat = XmListGetSelectedPos(UxGetWidget(sw_policy_query_results_list),
		&position_list, &position_count);

	/* return if no selected items found */
	if (stat == FALSE)
		return;

	/* display busy cursor */
	XDefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)),
		XCreateFontCursor(XtDisplay(UxGetWidget(pps_policy)), XC_watch));

	/* display it now! */
	XFlush(XtDisplay(UxGetWidget(pps_policy)));

	/* update the jobs */
	for (i = 0; i < position_count; i++) {
		/* make the actual db command line */
		sprintf(buf, "exec sp_set_insert_top %d, \"%s\"",
			pending_jobs[position_list[i]-1].job_id, bool);
		if (globalData.showSQL)
			printf("%s\n", buf);
		/* exec the stored procedure */
		retcode = db_exec(&exec_connection,buf,&policy_exec);
		if (retcode != ER_NO_ERROR) {
			fprintf(stderr, "Failed to execute sp_set_insert_top\n");
			do_error_dialog(pps_policy,(char *)get_pps_err_msg(retcode));
			/* restore default cursor */
			XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
				XtWindow(UxGetWidget(pps_policy)));
			return;
		}
	}

	/* clean up */
	XtFree(position_list);
	XUndefineCursor(XtDisplay(UxGetWidget(pps_policy)),
		XtWindow(UxGetWidget(pps_policy)));

	/* requery the database using the current settings */
	do_policy_query();
}

/* callback to set Insert Top for selected ready jobs */
cb_policy_IT_on()
{
	do_policy_IT("YES");
}

/* callback to unset Insert Top for selected ready jobs */
cb_policy_IT_off()
{
	do_policy_IT("NO");
}

/* callback to handle changes to the policy matrix */
cb_policy_change(
char	*job_type,
char	*quicklook_flag,
char	*priority,
char	*auto_submit)
{
	char buf[MAXBIGBUF];
	int  retcode;

	do_policy_query();

	/* make the actual db command line */
	sprintf(buf, "exec sp_set_policy \"%s\", \"%s\", \"%s\", \"%s\"",
		job_type, quicklook_flag, priority, auto_submit) ;
	if (globalData.showSQL)
		printf("%s\n", buf);
	/* exec the stored procedure */
	retcode = db_exec(&exec_connection, buf, &policy_exec);
	if (retcode != ER_NO_ERROR) {
		fprintf(stderr, "Failed to execute sp_set_policy\n");
		do_error_dialog(pps_policy,(char *)get_pps_err_msg(retcode));
	}

	return;
}

/* callback to print results */
void
cb_policy_print_results(void)
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

	results_list = UxGetWidget(sw_policy_query_results_list);

	/*--------------------------------------*/
	/* create a temp file for printing      */
	/*--------------------------------------*/
	mypid = getpid();
	sprintf(filename, "/tmp/%d_policy.ascii", mypid);
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
		fprintf(fp, "Pending Jobs which match the Policy Matrix::\n\n");
		fprintf(fp, "******************************************************************************************************************\n");
		fprintf(fp, " Order                                      Sens        Frame              Order & Item              Age      Prod\n");
		fprintf(fp, " Type       Priority  Media ID     Mode   Sat   Rev      ID       Job ID        ID         State    (Days)    Time\n");
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
		fclose(fp);
		unlink(filename);
		XppsCreateErrorDialog(UxGetWidget(pps_policy),
			"Nothing to print", True, 0, 0);
		return;
	}

	fclose(fp);

	dialogTitle = XmStringCreateLocalized("Policy: Print Results");
	if (printResultsDialog == 0)
		printResultsDialog =
			create_print_results_dialog(UxGetWidget(pps_policy),
				dialogTitle, filename);
	else
	{
		XtManageChild(printResultsDialog);
		XMapRaised(XtDisplay(XtParent(printResultsDialog)),
				XtWindow(XtParent(printResultsDialog)));
	}
	XmStringFree(dialogTitle);

}/* cb_policy_print_results */

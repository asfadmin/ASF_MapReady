/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
#ifndef _PPS_UTIL_H_INCLUDED
#define _PPS_UTIL_H_INCLUDED

/* defines for PPS GUI utility routines */

#pragma ident	"@(#)pps_util.h	1.2  12/18/96"

#include <UxLib.h>

#define MAXBIGBUF       2000
#define MAXSMALLBUF     100

typedef struct
{
	swidget		label;
	swidget		textField;

} intFields;

char *get_om_label(swidget sw);
void cb_no_file_selected(Widget widget, XtPointer client_data,
	XtPointer call_data);

void do_error_dialog(swidget parent, char* buf);
void do_information_dialog(swidget parent, char* buf);
void do_warning_dialog(swidget parent, char* buf);

int get_job_state (int job_id, char *job_state);

#endif /*_PPS_UTIL_H_INCLUDED*/

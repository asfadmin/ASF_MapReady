/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*******************************************************************************
       pps_policy.h
       This header file is included by pps_policy.c

*******************************************************************************/

#ifndef	_PPS_POLICY_INCLUDED
#define	_PPS_POLICY_INCLUDED

#pragma ident	"@(#)pps_policy.h	1.1  11/21/96"

#include <stdio.h>
#include "UxLib.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	swidget	Uxmb_policy;
	swidget	Uxpb_policy_file;
	swidget	Uxpb_policy_print_results;
	swidget	Uxpb_policy_print_screen;
	swidget	Uxpb_policy_exit;
	swidget	Uxcb_policy;
	swidget	Uxl_policy_matrix;
	swidget	Uxf_policy_matrix;
	swidget	Uxbb_policy_matrix;
	swidget	Uxrc_policy_matrix;
	swidget	Uxlabel9;
	swidget	Uxlabel10;
	swidget	Uxlabel11;
	swidget	Uxlabel12;
	swidget	Uxlabel13;
	swidget	Uxlabel14;
	swidget	Uxlabel15;
	swidget	Uxlabel16;
	swidget	Uxf_policy_query_results;
	swidget	Uxbb_policy_query_results;
	swidget	Uxl_policy_priority;
	swidget	Uxl_policy_media_id;
	swidget	Uxl_policy_mode;
	swidget	Uxl_policy_sat_sens_rev;
	swidget	Uxl_policy_frame_id;
	swidget	Uxl_policy_job_id;
	swidget	Uxl_policy_order_item;
	swidget	Uxl_policy_state;
	swidget	Uxl_policy_age;
	swidget	Uxl_policy_prod_time;
	swidget	Uxl_policy_order_type;
	swidget	Uxsw_policy_query_results;
	swidget	UxlabelGadget1;
	swidget	Uxpb_policy_IT_on;
	swidget	Uxl_policy_pending_jobs;
	swidget	Uxpb_policy_IT_off;
	swidget	Uxf_policy_bear;
	swidget	Uxl_policy_bear;
	swidget	UxUxParent;
} _UxCpps_policy;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCpps_policy          *UxPps_policyContext;
#define mb_policy               UxPps_policyContext->Uxmb_policy
#define pb_policy_file          UxPps_policyContext->Uxpb_policy_file
#define pb_policy_print_results UxPps_policyContext->Uxpb_policy_print_results
#define pb_policy_print_screen  UxPps_policyContext->Uxpb_policy_print_screen
#define pb_policy_exit          UxPps_policyContext->Uxpb_policy_exit
#define cb_policy               UxPps_policyContext->Uxcb_policy
#define l_policy_matrix         UxPps_policyContext->Uxl_policy_matrix
#define f_policy_matrix         UxPps_policyContext->Uxf_policy_matrix
#define bb_policy_matrix        UxPps_policyContext->Uxbb_policy_matrix
#define rc_policy_matrix        UxPps_policyContext->Uxrc_policy_matrix
#define label9                  UxPps_policyContext->Uxlabel9
#define label10                 UxPps_policyContext->Uxlabel10
#define label11                 UxPps_policyContext->Uxlabel11
#define label12                 UxPps_policyContext->Uxlabel12
#define label13                 UxPps_policyContext->Uxlabel13
#define label14                 UxPps_policyContext->Uxlabel14
#define label15                 UxPps_policyContext->Uxlabel15
#define label16                 UxPps_policyContext->Uxlabel16
#define f_policy_query_results  UxPps_policyContext->Uxf_policy_query_results
#define bb_policy_query_results UxPps_policyContext->Uxbb_policy_query_results
#define l_policy_priority       UxPps_policyContext->Uxl_policy_priority
#define l_policy_media_id       UxPps_policyContext->Uxl_policy_media_id
#define l_policy_mode           UxPps_policyContext->Uxl_policy_mode
#define l_policy_sat_sens_rev   UxPps_policyContext->Uxl_policy_sat_sens_rev
#define l_policy_frame_id       UxPps_policyContext->Uxl_policy_frame_id
#define l_policy_job_id         UxPps_policyContext->Uxl_policy_job_id
#define l_policy_order_item     UxPps_policyContext->Uxl_policy_order_item
#define l_policy_state          UxPps_policyContext->Uxl_policy_state
#define l_policy_age            UxPps_policyContext->Uxl_policy_age
#define l_policy_prod_time      UxPps_policyContext->Uxl_policy_prod_time
#define l_policy_order_type     UxPps_policyContext->Uxl_policy_order_type
#define sw_policy_query_results UxPps_policyContext->Uxsw_policy_query_results
#define labelGadget1            UxPps_policyContext->UxlabelGadget1
#define pb_policy_IT_on         UxPps_policyContext->Uxpb_policy_IT_on
#define l_policy_pending_jobs   UxPps_policyContext->Uxl_policy_pending_jobs
#define pb_policy_IT_off        UxPps_policyContext->Uxpb_policy_IT_off
#define f_policy_bear           UxPps_policyContext->Uxf_policy_bear
#define l_policy_bear           UxPps_policyContext->Uxl_policy_bear
#define UxParent                UxPps_policyContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern swidget	pps_policy;
extern swidget	tb_policy_Urgent_L1;
extern swidget	tb_policy_Urgent_L1_QLK;
extern swidget	tb_policy_Urgent_Scan;
extern swidget	tb_policy_Urgent_Scan_QLK;
extern swidget	tb_policy_High_L1;
extern swidget	tb_policy_High_L1_QLK;
extern swidget	tb_policy_High_Scan;
extern swidget	tb_policy_High_Scan_QLK;
extern swidget	tb_policy_Routine_L1;
extern swidget	tb_policy_Routine_L1_QLK;
extern swidget	tb_policy_Routine_Scan;
extern swidget	tb_policy_Routine_Scan_QLK;
extern swidget	tb_policy_Low_L1;
extern swidget	tb_policy_Low_L1_QLK;
extern swidget	tb_policy_Low_Scan;
extern swidget	tb_policy_Low_Scan_QLK;
extern swidget	sw_policy_query_results_list;
extern swidget	pb_policy_select_all;
extern swidget	pb_policy_deselect_all;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_pps_policy( swidget _UxUxParent );

#endif	/* _PPS_POLICY_INCLUDED */

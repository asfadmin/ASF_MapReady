
/*******************************************************************************
       pps_query.h
       This header file is included by pps_query.c

*******************************************************************************/

#ifndef	_PPS_QUERY_INCLUDED
#define	_PPS_QUERY_INCLUDED

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
	swidget	Uxmb_query;
	swidget	Uxpb_query_file;
	swidget	Uxpb_query_print_results;
	swidget	Uxpb_query_print_screen;
	swidget	Uxpb_query_exit;
	swidget	Uxmb_query_file;
	swidget	Uxmb_query_edit;
	swidget	Uxpb_query_check_params;
	swidget	Uxmb_query_resubmit;
	swidget	Uxmb_query_resubmit_top;
	swidget	Uxmb_query_resubmit_bottom;
	swidget	Uxpb_query_resubmit;
	swidget	Uxmb_query_top_b1;
	swidget	Uxpb_query_setting;
	swidget	Uxpb_query_load_query;
	swidget	Uxpb_query_save_query;
	swidget	Uxpb_query_save_query_as;
	swidget	Uxpb_query_clear;
	swidget	Uxmb_query_top_b2;
	swidget	Uxf_query_query_settings;
	swidget	Uxbb_query_query_settings;
	swidget	Uxrc_query_sens_o;
	swidget	Uxrc_query_sens_v;
	swidget	Uxrc_query_sens_z;
	swidget	Uxpb_query_station_gh;
	swidget	Uxpb_query_station_ph;
	swidget	Uxpb_query_station_as;
	swidget	Uxpb_query_station_be;
	swidget	Uxpb_query_station_co;
	swidget	Uxpb_query_station_cu;
	swidget	Uxpb_query_station_es;
	swidget	Uxpb_query_station_fs;
	swidget	Uxpb_query_station_ha;
	swidget	Uxpb_query_station_ho;
	swidget	Uxpb_query_station_hy;
	swidget	Uxpb_query_station_is;
	swidget	Uxpb_query_station_in;
	swidget	Uxpb_query_station_jo;
	swidget	Uxpb_query_station_ks;
	swidget	Uxpb_query_station_ku;
	swidget	Uxpb_query_station_ma;
	swidget	Uxpb_query_station_ms;
	swidget	Uxpb_query_station_pp;
	swidget	Uxpb_query_station_sa;
	swidget	Uxpb_query_station_se;
	swidget	Uxpb_query_station_sy;
	swidget	Uxpb_query_station_tf;
	swidget	Uxpb_query_station_tg;
	swidget	Uxpb_query_station_th;
	swidget	Uxpb_query_station_to;
	swidget	Uxpb_query_station_ts;
	swidget	Uxpb_query_station_wf;
	swidget	Uxpb_query_station_wh;
	swidget	Uxrc_query_product_type_calset;
	swidget	Uxrc_query_product_type_ramp;
	swidget	Uxrc_query_product_type_scansar;
	swidget	Uxpb_query_state_retry;
	swidget	Uxrc_query_state_completed;
	swidget	Uxrc_query_state_fail;
	swidget	Uxrc_query_media_type_id1;
	swidget	Uxf_query_query_results;
	swidget	Uxbb_query_query_results;
	swidget	Uxl_query_priority;
	swidget	Uxl_query_media;
	swidget	Uxl_query_mode;
	swidget	Uxl_query_sat_sens_rev;
	swidget	Uxl_query_frame;
	swidget	Uxl_query_job;
	swidget	Uxl_query_order;
	swidget	Uxl_query_state;
	swidget	Uxl_query_age;
	swidget	Uxl_query_order_type;
	swidget	UxscrolledWindowList2;
	swidget	Uxl_query_tce;
	swidget	Uxl_query_gha;
	swidget	Uxl_query_sv;
	swidget	Uxl_query_scan_results;
	swidget	Uxl_query_calib_params;
	swidget	Uxl_query_query_results;
	swidget	Uxf_query_items_found;
	swidget	Uxbb_query_items_found;
	swidget	Uxl_query_items_found;
	swidget	Uxl_query_query_settings;
	swidget	Uxframe2;
	swidget	UxbulletinBoard1;
	swidget	Uxpb_query_order_first_age;
	swidget	Uxpb_query_order_second_age;
	swidget	Uxpb_query_order_third_age;
	swidget	Uxpb_query_order_fourth_age;
	swidget	Uxl_query_order_by;
	swidget	UxUxParent;
} _UxCpps_query;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCpps_query           *UxPps_queryContext;
#define mb_query                UxPps_queryContext->Uxmb_query
#define pb_query_file           UxPps_queryContext->Uxpb_query_file
#define pb_query_print_results  UxPps_queryContext->Uxpb_query_print_results
#define pb_query_print_screen   UxPps_queryContext->Uxpb_query_print_screen
#define pb_query_exit           UxPps_queryContext->Uxpb_query_exit
#define mb_query_file           UxPps_queryContext->Uxmb_query_file
#define mb_query_edit           UxPps_queryContext->Uxmb_query_edit
#define pb_query_check_params   UxPps_queryContext->Uxpb_query_check_params
#define mb_query_resubmit       UxPps_queryContext->Uxmb_query_resubmit
#define mb_query_resubmit_top   UxPps_queryContext->Uxmb_query_resubmit_top
#define mb_query_resubmit_bottom UxPps_queryContext->Uxmb_query_resubmit_bottom
#define pb_query_resubmit       UxPps_queryContext->Uxpb_query_resubmit
#define mb_query_top_b1         UxPps_queryContext->Uxmb_query_top_b1
#define pb_query_setting        UxPps_queryContext->Uxpb_query_setting
#define pb_query_load_query     UxPps_queryContext->Uxpb_query_load_query
#define pb_query_save_query     UxPps_queryContext->Uxpb_query_save_query
#define pb_query_save_query_as  UxPps_queryContext->Uxpb_query_save_query_as
#define pb_query_clear          UxPps_queryContext->Uxpb_query_clear
#define mb_query_top_b2         UxPps_queryContext->Uxmb_query_top_b2
#define f_query_query_settings  UxPps_queryContext->Uxf_query_query_settings
#define bb_query_query_settings UxPps_queryContext->Uxbb_query_query_settings
#define rc_query_sens_o         UxPps_queryContext->Uxrc_query_sens_o
#define rc_query_sens_v         UxPps_queryContext->Uxrc_query_sens_v
#define rc_query_sens_z         UxPps_queryContext->Uxrc_query_sens_z
#define pb_query_station_gh     UxPps_queryContext->Uxpb_query_station_gh
#define pb_query_station_ph     UxPps_queryContext->Uxpb_query_station_ph
#define pb_query_station_as     UxPps_queryContext->Uxpb_query_station_as
#define pb_query_station_be     UxPps_queryContext->Uxpb_query_station_be
#define pb_query_station_co     UxPps_queryContext->Uxpb_query_station_co
#define pb_query_station_cu     UxPps_queryContext->Uxpb_query_station_cu
#define pb_query_station_es     UxPps_queryContext->Uxpb_query_station_es
#define pb_query_station_fs     UxPps_queryContext->Uxpb_query_station_fs
#define pb_query_station_ha     UxPps_queryContext->Uxpb_query_station_ha
#define pb_query_station_ho     UxPps_queryContext->Uxpb_query_station_ho
#define pb_query_station_hy     UxPps_queryContext->Uxpb_query_station_hy
#define pb_query_station_is     UxPps_queryContext->Uxpb_query_station_is
#define pb_query_station_in     UxPps_queryContext->Uxpb_query_station_in
#define pb_query_station_jo     UxPps_queryContext->Uxpb_query_station_jo
#define pb_query_station_ks     UxPps_queryContext->Uxpb_query_station_ks
#define pb_query_station_ku     UxPps_queryContext->Uxpb_query_station_ku
#define pb_query_station_ma     UxPps_queryContext->Uxpb_query_station_ma
#define pb_query_station_ms     UxPps_queryContext->Uxpb_query_station_ms
#define pb_query_station_pp     UxPps_queryContext->Uxpb_query_station_pp
#define pb_query_station_sa     UxPps_queryContext->Uxpb_query_station_sa
#define pb_query_station_se     UxPps_queryContext->Uxpb_query_station_se
#define pb_query_station_sy     UxPps_queryContext->Uxpb_query_station_sy
#define pb_query_station_tf     UxPps_queryContext->Uxpb_query_station_tf
#define pb_query_station_tg     UxPps_queryContext->Uxpb_query_station_tg
#define pb_query_station_th     UxPps_queryContext->Uxpb_query_station_th
#define pb_query_station_to     UxPps_queryContext->Uxpb_query_station_to
#define pb_query_station_ts     UxPps_queryContext->Uxpb_query_station_ts
#define pb_query_station_wf     UxPps_queryContext->Uxpb_query_station_wf
#define pb_query_station_wh     UxPps_queryContext->Uxpb_query_station_wh
#define rc_query_product_type_calset UxPps_queryContext->Uxrc_query_product_type_calset
#define rc_query_product_type_ramp UxPps_queryContext->Uxrc_query_product_type_ramp
#define rc_query_product_type_scansar UxPps_queryContext->Uxrc_query_product_type_scansar
#define pb_query_state_retry    UxPps_queryContext->Uxpb_query_state_retry
#define rc_query_state_completed UxPps_queryContext->Uxrc_query_state_completed
#define rc_query_state_fail     UxPps_queryContext->Uxrc_query_state_fail
#define rc_query_media_type_id1 UxPps_queryContext->Uxrc_query_media_type_id1
#define f_query_query_results   UxPps_queryContext->Uxf_query_query_results
#define bb_query_query_results  UxPps_queryContext->Uxbb_query_query_results
#define l_query_priority        UxPps_queryContext->Uxl_query_priority
#define l_query_media           UxPps_queryContext->Uxl_query_media
#define l_query_mode            UxPps_queryContext->Uxl_query_mode
#define l_query_sat_sens_rev    UxPps_queryContext->Uxl_query_sat_sens_rev
#define l_query_frame           UxPps_queryContext->Uxl_query_frame
#define l_query_job             UxPps_queryContext->Uxl_query_job
#define l_query_order           UxPps_queryContext->Uxl_query_order
#define l_query_state           UxPps_queryContext->Uxl_query_state
#define l_query_age             UxPps_queryContext->Uxl_query_age
#define l_query_order_type      UxPps_queryContext->Uxl_query_order_type
#define scrolledWindowList2     UxPps_queryContext->UxscrolledWindowList2
#define l_query_tce             UxPps_queryContext->Uxl_query_tce
#define l_query_gha             UxPps_queryContext->Uxl_query_gha
#define l_query_sv              UxPps_queryContext->Uxl_query_sv
#define l_query_scan_results    UxPps_queryContext->Uxl_query_scan_results
#define l_query_calib_params    UxPps_queryContext->Uxl_query_calib_params
#define l_query_query_results   UxPps_queryContext->Uxl_query_query_results
#define f_query_items_found     UxPps_queryContext->Uxf_query_items_found
#define bb_query_items_found    UxPps_queryContext->Uxbb_query_items_found
#define l_query_items_found     UxPps_queryContext->Uxl_query_items_found
#define l_query_query_settings  UxPps_queryContext->Uxl_query_query_settings
#define frame2                  UxPps_queryContext->Uxframe2
#define bulletinBoard1          UxPps_queryContext->UxbulletinBoard1
#define pb_query_order_first_age UxPps_queryContext->Uxpb_query_order_first_age
#define pb_query_order_second_age UxPps_queryContext->Uxpb_query_order_second_age
#define pb_query_order_third_age UxPps_queryContext->Uxpb_query_order_third_age
#define pb_query_order_fourth_age UxPps_queryContext->Uxpb_query_order_fourth_age
#define l_query_order_by        UxPps_queryContext->Uxl_query_order_by
#define UxParent                UxPps_queryContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern swidget	pps_query;
extern swidget	tb_query_L1_Orders;
extern swidget	tb_query_L1_QLK;
extern swidget	tb_query_Scan_Orders;
extern swidget	tb_query_Scan_QLK;
extern swidget	rc_query_sat;
extern swidget	pb_query_sat_any;
extern swidget	pb_query_sat_e1;
extern swidget	pb_query_sat_e2;
extern swidget	pb_query_sat_j1;
extern swidget	pb_query_sat_r1;
extern swidget	om_query_sat;
extern swidget	rc_query_sens;
extern swidget	pb_query_sens_any;
extern swidget	pb_query_sens_s;
extern swidget	om_query_sens;
extern swidget	rc_query_activity;
extern swidget	pb_query_activity_any;
extern swidget	pb_query_activity_rlt;
extern swidget	pb_query_activity_dmp;
extern swidget	om_query_activity;
extern swidget	rc_query_station;
extern swidget	pb_query_station_any;
extern swidget	pb_query_station_fa;
extern swidget	pb_query_station_mc;
extern swidget	om_query_station;
extern swidget	rc_query_priority;
extern swidget	pb_query_priority_any;
extern swidget	pb_query_priority_low;
extern swidget	pb_query_priority_routine;
extern swidget	pb_query_priority_high;
extern swidget	pb_query_priority_urgent;
extern swidget	om_query_priority;
extern swidget	l_query_rev;
extern swidget	tf_query_rev;
extern swidget	l_query_seq;
extern swidget	tf_query_seq;
extern swidget	l_query_job_id;
extern swidget	tf_query_job_id;
extern swidget	l_query_order_id;
extern swidget	tf_query_order_id;
extern swidget	l_query_item_id;
extern swidget	tf_query_item_id;
extern swidget	l_query_frame_id;
extern swidget	tf_query_frame_id;
extern swidget	l_query_subframe_id;
extern swidget	tf_query_subframe_id;
extern swidget	rc_query_product_type;
extern swidget	pb_query_product_type_any;
extern swidget	pb_query_product_type_standard;
extern swidget	pb_query_product_type_complex;
extern swidget	pb_query_product_type_ccsd;
extern swidget	om_query_product_type;
extern swidget	l_query_pixel_spacing;
extern swidget	tf_query_pixel_spacing;
extern swidget	rc_query_projection;
extern swidget	pb_query_projection_any;
extern swidget	pb_query_projection_ground_range;
extern swidget	pb_query_projection_slant_range;
extern swidget	pb_query_projection_lambert;
extern swidget	pb_query_projection_ps;
extern swidget	pb_query_projection_utm;
extern swidget	om_query_projection;
extern swidget	l_query_proc_gain;
extern swidget	tf_query_proc_gain;
extern swidget	rc_query_state;
extern swidget	pb_query_state_any;
extern swidget	pb_query_state_pending;
extern swidget	pb_query_state_ready;
extern swidget	pb_query_state_available;
extern swidget	pb_query_state_submitted;
extern swidget	om_query_state;
extern swidget	rc_query_media_type;
extern swidget	pb_query_media_type_any;
extern swidget	pb_query_media_type_disk;
extern swidget	pb_query_media_type_dcrsi;
extern swidget	om_query_media_type;
extern swidget	l_query_media_id;
extern swidget	tf_query_media_id;
extern swidget	rc_query_processor_mode;
extern swidget	pb_query_processor_mode_any;
extern swidget	pb_query_processor_mode_continuous;
extern swidget	pb_query_processor_mode_scansar;
extern swidget	om_query_processor_mode;
extern swidget	rc_query_data_direction;
extern swidget	pb_query_data_direction_any;
extern swidget	pb_query_data_direction_forward;
extern swidget	pb_query_data_direction_reverse;
extern swidget	pb_query_data_direction_unknown;
extern swidget	om_query_data_direction;
extern swidget	sw_query_results_list;
extern swidget	l_query_num_items;
extern swidget	pb_query_query;
extern swidget	rc_query_order_first;
extern swidget	pb_query_order_first_none;
extern swidget	pb_query_order_first_order_type;
extern swidget	pb_query_order_first_priority;
extern swidget	pb_query_order_first_media_id;
extern swidget	pb_query_order_first_mode;
extern swidget	pb_query_order_first_sat_sens_rev;
extern swidget	pb_query_order_first_frame_subframe;
extern swidget	pb_query_order_first_job_id;
extern swidget	pb_query_order_first_order_item;
extern swidget	pb_query_order_first_state;
extern swidget	om_query_order_first;
extern swidget	rc_query_order_second;
extern swidget	pb_query_order_second_none;
extern swidget	pb_query_order_second_order_type;
extern swidget	pb_query_order_second_priority;
extern swidget	pb_query_order_second_media_id;
extern swidget	pb_query_order_second_mode;
extern swidget	pb_query_order_second_sat_sens_rev;
extern swidget	pb_query_order_second_frame_subframe;
extern swidget	pb_query_order_second_job_id;
extern swidget	pb_query_order_second_order_item;
extern swidget	pb_query_order_second_state;
extern swidget	om_query_order_second;
extern swidget	rc_query_order_third;
extern swidget	pb_query_order_third_none;
extern swidget	pb_query_order_third_order_type;
extern swidget	pb_query_order_third_priority;
extern swidget	pb_query_order_third_media_id;
extern swidget	pb_query_order_third_mode;
extern swidget	pb_query_order_third_sat_sens_rev;
extern swidget	pb_query_order_third_frame_subframe;
extern swidget	pb_query_order_third_job_id;
extern swidget	pb_query_order_third_order_item;
extern swidget	pb_query_order_third_state;
extern swidget	om_query_order_third;
extern swidget	rc_query_order_fourth;
extern swidget	pb_query_order_fourth_none;
extern swidget	pb_query_order_fourth_order_type;
extern swidget	pb_query_order_fourth_priority;
extern swidget	pb_query_order_fourth_media_id;
extern swidget	pb_query_order_fourth_mode;
extern swidget	pb_query_order_fourth_sat_sens_rev;
extern swidget	pb_query_order_fourth_frame_subframe;
extern swidget	pb_query_order_fourth_job_id;
extern swidget	pb_query_order_fourth_order_item;
extern swidget	pb_query_order_fourth_state;
extern swidget	om_query_order_fourth;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_pps_query( swidget _UxUxParent );

#endif	/* _PPS_QUERY_INCLUDED */

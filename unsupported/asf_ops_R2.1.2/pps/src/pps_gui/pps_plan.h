
/*******************************************************************************
       pps_plan.h
       This header file is included by pps_plan.c

*******************************************************************************/

#ifndef	_PPS_PLAN_INCLUDED
#define	_PPS_PLAN_INCLUDED

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
	swidget	Uxmb_plan;
	swidget	Uxpb_plan_file;
	swidget	Uxpb_plan_print_results;
	swidget	Uxpb_plan_print_screen;
	swidget	Uxpb_plan_exit;
	swidget	Uxmb_plan_file;
	swidget	Uxpb_plan_query_setting;
	swidget	Uxpb_plan_load_query;
	swidget	Uxpb_plan_save_query;
	swidget	Uxpb_plan_save_query_as;
	swidget	Uxpb_plan_clear_query;
	swidget	Uxmb_plan_top_b1;
	swidget	Uxf_plan_query_settings;
	swidget	Uxbb_plan_query_settings;
	swidget	Uxrc_plan_sens_o;
	swidget	Uxrc_plan_sens_v;
	swidget	Uxrc_plan_sens_z;
	swidget	Uxpb_plan_station_gh;
	swidget	Uxpb_plan_station_ph;
	swidget	Uxpb_plan_station_as;
	swidget	Uxpb_plan_station_be;
	swidget	Uxpb_plan_station_co;
	swidget	Uxpb_plan_station_cu;
	swidget	Uxpb_plan_station_es;
	swidget	Uxpb_plan_station_fs;
	swidget	Uxpb_plan_station_ha;
	swidget	Uxpb_plan_station_ho;
	swidget	Uxpb_plan_station_hy;
	swidget	Uxpb_plan_station_is;
	swidget	Uxpb_plan_station_in;
	swidget	Uxpb_plan_station_jo;
	swidget	Uxpb_plan_station_ks;
	swidget	Uxpb_plan_station_ku;
	swidget	Uxpb_plan_station_ma;
	swidget	Uxpb_plan_station_ms;
	swidget	Uxpb_plan_station_pp;
	swidget	Uxpb_plan_station_sa;
	swidget	Uxpb_plan_station_se;
	swidget	Uxpb_plan_station_sy;
	swidget	Uxpb_plan_station_tf;
	swidget	Uxpb_plan_station_tg;
	swidget	Uxpb_plan_station_th;
	swidget	Uxpb_plan_station_to;
	swidget	Uxpb_plan_station_ts;
	swidget	Uxpb_plan_station_wf;
	swidget	Uxpb_plan_station_wh;
	swidget	Uxrc_plan_product_type_calset;
	swidget	Uxrc_plan_product_type_ramp;
	swidget	Uxrc_plan_product_type_scansar;
	swidget	Uxrc_plan_media_type_id1;
	swidget	Uxl_plan_query_settings;
	swidget	Uxl_plan_planned;
	swidget	Uxl_plan_ready;
	swidget	Uxframe6;
	swidget	UxbulletinBoard15;
	swidget	UxscrolledWindowList12;
	swidget	Uxl_plan_ready_order_type;
	swidget	Uxl_plan_ready_priority;
	swidget	Uxl_plan_ready_age;
	swidget	Uxl_plan_ready_sat_sens_rev;
	swidget	Uxl_plan_ready_job;
	swidget	Uxl_plan_ready_insert_top;
	swidget	Uxl_plan_ready_mode;
	swidget	Uxl_plan_ready_pixel_spacing;
	swidget	Uxl_plan_ready_media_id;
	swidget	Uxl_plan_available_job2;
	swidget	Uxf_plan_ready;
	swidget	Uxbb_plan_ready;
	swidget	Uxpb_plan_ready_top;
	swidget	Uxpb_plan_ready_IT_on;
	swidget	Uxpb_plan_ready_IT_off;
	swidget	Uxpb_plan_ready_bottom;
	swidget	Uxframe7;
	swidget	UxbulletinBoard2;
	swidget	UxscrolledWindowList1;
	swidget	Uxl_plan_available_order_type;
	swidget	Uxl_plan_available_priority;
	swidget	Uxl_plan_available_age;
	swidget	Uxl_plan_available_sat_sens_rev;
	swidget	Uxl_plan_available_mode;
	swidget	Uxl_plan_available_pixel_spacing;
	swidget	Uxl_plan_available_media_id;
	swidget	Uxl_plan_available_job;
	swidget	Uxl_plan_available_insert_top;
	swidget	Uxl_plan_available_job1;
	swidget	Uxframe3;
	swidget	UxbulletinBoard4;
	swidget	Uxpb_plan_order_first_age;
	swidget	Uxpb_plan_order_first_order_item;
	swidget	Uxpb_plan_order_second_age;
	swidget	Uxpb_plan_order_second_order_item;
	swidget	Uxpb_plan_order_third_age;
	swidget	Uxpb_plan_order_third_order_item;
	swidget	Uxpb_plan_order_fourth_age;
	swidget	Uxpb_plan_order_fourth_order_item;
	swidget	Uxl_plan_order_by;
	swidget	Uxf_plan_items_ready;
	swidget	Uxbb_plan_items_ready;
	swidget	Uxl_query_items_found1;
	swidget	Uxl_query_items_found4;
	swidget	Uxf_plan_available;
	swidget	Uxbb_plan_available;
	swidget	Uxpb_plan_available_top;
	swidget	Uxpb_plan_available_IT_on;
	swidget	Uxpb_plan_available_IT_off;
	swidget	Uxpb_plan_available_remove;
	swidget	Uxpb_plan_available_bottom;
	swidget	Uxf_plan_items_found;
	swidget	Uxbb_plan_items_available;
	swidget	Uxl_query_items_found2;
	swidget	Uxl_query_items_found7;
	swidget	Uxl_plan_modify_ready;
	swidget	Uxl_plan_modify_available;
	swidget	UxUxParent;
} _UxCpps_plan;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCpps_plan            *UxPps_planContext;
#define mb_plan                 UxPps_planContext->Uxmb_plan
#define pb_plan_file            UxPps_planContext->Uxpb_plan_file
#define pb_plan_print_results   UxPps_planContext->Uxpb_plan_print_results
#define pb_plan_print_screen    UxPps_planContext->Uxpb_plan_print_screen
#define pb_plan_exit            UxPps_planContext->Uxpb_plan_exit
#define mb_plan_file            UxPps_planContext->Uxmb_plan_file
#define pb_plan_query_setting   UxPps_planContext->Uxpb_plan_query_setting
#define pb_plan_load_query      UxPps_planContext->Uxpb_plan_load_query
#define pb_plan_save_query      UxPps_planContext->Uxpb_plan_save_query
#define pb_plan_save_query_as   UxPps_planContext->Uxpb_plan_save_query_as
#define pb_plan_clear_query     UxPps_planContext->Uxpb_plan_clear_query
#define mb_plan_top_b1          UxPps_planContext->Uxmb_plan_top_b1
#define f_plan_query_settings   UxPps_planContext->Uxf_plan_query_settings
#define bb_plan_query_settings  UxPps_planContext->Uxbb_plan_query_settings
#define rc_plan_sens_o          UxPps_planContext->Uxrc_plan_sens_o
#define rc_plan_sens_v          UxPps_planContext->Uxrc_plan_sens_v
#define rc_plan_sens_z          UxPps_planContext->Uxrc_plan_sens_z
#define pb_plan_station_gh      UxPps_planContext->Uxpb_plan_station_gh
#define pb_plan_station_ph      UxPps_planContext->Uxpb_plan_station_ph
#define pb_plan_station_as      UxPps_planContext->Uxpb_plan_station_as
#define pb_plan_station_be      UxPps_planContext->Uxpb_plan_station_be
#define pb_plan_station_co      UxPps_planContext->Uxpb_plan_station_co
#define pb_plan_station_cu      UxPps_planContext->Uxpb_plan_station_cu
#define pb_plan_station_es      UxPps_planContext->Uxpb_plan_station_es
#define pb_plan_station_fs      UxPps_planContext->Uxpb_plan_station_fs
#define pb_plan_station_ha      UxPps_planContext->Uxpb_plan_station_ha
#define pb_plan_station_ho      UxPps_planContext->Uxpb_plan_station_ho
#define pb_plan_station_hy      UxPps_planContext->Uxpb_plan_station_hy
#define pb_plan_station_is      UxPps_planContext->Uxpb_plan_station_is
#define pb_plan_station_in      UxPps_planContext->Uxpb_plan_station_in
#define pb_plan_station_jo      UxPps_planContext->Uxpb_plan_station_jo
#define pb_plan_station_ks      UxPps_planContext->Uxpb_plan_station_ks
#define pb_plan_station_ku      UxPps_planContext->Uxpb_plan_station_ku
#define pb_plan_station_ma      UxPps_planContext->Uxpb_plan_station_ma
#define pb_plan_station_ms      UxPps_planContext->Uxpb_plan_station_ms
#define pb_plan_station_pp      UxPps_planContext->Uxpb_plan_station_pp
#define pb_plan_station_sa      UxPps_planContext->Uxpb_plan_station_sa
#define pb_plan_station_se      UxPps_planContext->Uxpb_plan_station_se
#define pb_plan_station_sy      UxPps_planContext->Uxpb_plan_station_sy
#define pb_plan_station_tf      UxPps_planContext->Uxpb_plan_station_tf
#define pb_plan_station_tg      UxPps_planContext->Uxpb_plan_station_tg
#define pb_plan_station_th      UxPps_planContext->Uxpb_plan_station_th
#define pb_plan_station_to      UxPps_planContext->Uxpb_plan_station_to
#define pb_plan_station_ts      UxPps_planContext->Uxpb_plan_station_ts
#define pb_plan_station_wf      UxPps_planContext->Uxpb_plan_station_wf
#define pb_plan_station_wh      UxPps_planContext->Uxpb_plan_station_wh
#define rc_plan_product_type_calset UxPps_planContext->Uxrc_plan_product_type_calset
#define rc_plan_product_type_ramp UxPps_planContext->Uxrc_plan_product_type_ramp
#define rc_plan_product_type_scansar UxPps_planContext->Uxrc_plan_product_type_scansar
#define rc_plan_media_type_id1  UxPps_planContext->Uxrc_plan_media_type_id1
#define l_plan_query_settings   UxPps_planContext->Uxl_plan_query_settings
#define l_plan_planned          UxPps_planContext->Uxl_plan_planned
#define l_plan_ready            UxPps_planContext->Uxl_plan_ready
#define frame6                  UxPps_planContext->Uxframe6
#define bulletinBoard15         UxPps_planContext->UxbulletinBoard15
#define scrolledWindowList12    UxPps_planContext->UxscrolledWindowList12
#define l_plan_ready_order_type UxPps_planContext->Uxl_plan_ready_order_type
#define l_plan_ready_priority   UxPps_planContext->Uxl_plan_ready_priority
#define l_plan_ready_age        UxPps_planContext->Uxl_plan_ready_age
#define l_plan_ready_sat_sens_rev UxPps_planContext->Uxl_plan_ready_sat_sens_rev
#define l_plan_ready_job        UxPps_planContext->Uxl_plan_ready_job
#define l_plan_ready_insert_top UxPps_planContext->Uxl_plan_ready_insert_top
#define l_plan_ready_mode       UxPps_planContext->Uxl_plan_ready_mode
#define l_plan_ready_pixel_spacing UxPps_planContext->Uxl_plan_ready_pixel_spacing
#define l_plan_ready_media_id   UxPps_planContext->Uxl_plan_ready_media_id
#define l_plan_available_job2   UxPps_planContext->Uxl_plan_available_job2
#define f_plan_ready            UxPps_planContext->Uxf_plan_ready
#define bb_plan_ready           UxPps_planContext->Uxbb_plan_ready
#define pb_plan_ready_top       UxPps_planContext->Uxpb_plan_ready_top
#define pb_plan_ready_IT_on     UxPps_planContext->Uxpb_plan_ready_IT_on
#define pb_plan_ready_IT_off    UxPps_planContext->Uxpb_plan_ready_IT_off
#define pb_plan_ready_bottom    UxPps_planContext->Uxpb_plan_ready_bottom
#define frame7                  UxPps_planContext->Uxframe7
#define bulletinBoard2          UxPps_planContext->UxbulletinBoard2
#define scrolledWindowList1     UxPps_planContext->UxscrolledWindowList1
#define l_plan_available_order_type UxPps_planContext->Uxl_plan_available_order_type
#define l_plan_available_priority UxPps_planContext->Uxl_plan_available_priority
#define l_plan_available_age    UxPps_planContext->Uxl_plan_available_age
#define l_plan_available_sat_sens_rev UxPps_planContext->Uxl_plan_available_sat_sens_rev
#define l_plan_available_mode   UxPps_planContext->Uxl_plan_available_mode
#define l_plan_available_pixel_spacing UxPps_planContext->Uxl_plan_available_pixel_spacing
#define l_plan_available_media_id UxPps_planContext->Uxl_plan_available_media_id
#define l_plan_available_job    UxPps_planContext->Uxl_plan_available_job
#define l_plan_available_insert_top UxPps_planContext->Uxl_plan_available_insert_top
#define l_plan_available_job1   UxPps_planContext->Uxl_plan_available_job1
#define frame3                  UxPps_planContext->Uxframe3
#define bulletinBoard4          UxPps_planContext->UxbulletinBoard4
#define pb_plan_order_first_age UxPps_planContext->Uxpb_plan_order_first_age
#define pb_plan_order_first_order_item UxPps_planContext->Uxpb_plan_order_first_order_item
#define pb_plan_order_second_age UxPps_planContext->Uxpb_plan_order_second_age
#define pb_plan_order_second_order_item UxPps_planContext->Uxpb_plan_order_second_order_item
#define pb_plan_order_third_age UxPps_planContext->Uxpb_plan_order_third_age
#define pb_plan_order_third_order_item UxPps_planContext->Uxpb_plan_order_third_order_item
#define pb_plan_order_fourth_age UxPps_planContext->Uxpb_plan_order_fourth_age
#define pb_plan_order_fourth_order_item UxPps_planContext->Uxpb_plan_order_fourth_order_item
#define l_plan_order_by         UxPps_planContext->Uxl_plan_order_by
#define f_plan_items_ready      UxPps_planContext->Uxf_plan_items_ready
#define bb_plan_items_ready     UxPps_planContext->Uxbb_plan_items_ready
#define l_query_items_found1    UxPps_planContext->Uxl_query_items_found1
#define l_query_items_found4    UxPps_planContext->Uxl_query_items_found4
#define f_plan_available        UxPps_planContext->Uxf_plan_available
#define bb_plan_available       UxPps_planContext->Uxbb_plan_available
#define pb_plan_available_top   UxPps_planContext->Uxpb_plan_available_top
#define pb_plan_available_IT_on UxPps_planContext->Uxpb_plan_available_IT_on
#define pb_plan_available_IT_off UxPps_planContext->Uxpb_plan_available_IT_off
#define pb_plan_available_remove UxPps_planContext->Uxpb_plan_available_remove
#define pb_plan_available_bottom UxPps_planContext->Uxpb_plan_available_bottom
#define f_plan_items_found      UxPps_planContext->Uxf_plan_items_found
#define bb_plan_items_available UxPps_planContext->Uxbb_plan_items_available
#define l_query_items_found2    UxPps_planContext->Uxl_query_items_found2
#define l_query_items_found7    UxPps_planContext->Uxl_query_items_found7
#define l_plan_modify_ready     UxPps_planContext->Uxl_plan_modify_ready
#define l_plan_modify_available UxPps_planContext->Uxl_plan_modify_available
#define UxParent                UxPps_planContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern swidget	pps_plan;
extern swidget	tb_plan_L1_Orders;
extern swidget	tb_plan_L1_QLK;
extern swidget	tb_plan_Scan_Orders;
extern swidget	tb_plan_Scan_QLK;
extern swidget	rc_plan_sat;
extern swidget	pb_plan_sat_any;
extern swidget	pb_plan_sat_e1;
extern swidget	pb_plan_sat_e2;
extern swidget	pb_plan_sat_j1;
extern swidget	pb_plan_sat_r1;
extern swidget	om_plan_sat;
extern swidget	rc_plan_sens;
extern swidget	pb_plan_sens_any;
extern swidget	pb_plan_sens_s;
extern swidget	om_plan_sens;
extern swidget	rc_plan_activity;
extern swidget	pb_plan_activity_any;
extern swidget	pb_plan_activity_rlt;
extern swidget	pb_plan_activity_dmp;
extern swidget	om_plan_activity;
extern swidget	rc_plan_station;
extern swidget	pb_plan_station_any;
extern swidget	pb_plan_station_fa;
extern swidget	pb_plan_station_mc;
extern swidget	om_plan_station;
extern swidget	rc_plan_priority;
extern swidget	pb_plan_priority_any;
extern swidget	pb_plan_priority_low;
extern swidget	pb_plan_priority_routine;
extern swidget	pb_plan_priority_high;
extern swidget	pb_plan_priority_urgent;
extern swidget	om_plan_priority;
extern swidget	l_plan_rev;
extern swidget	tf_plan_rev;
extern swidget	l_plan_seq;
extern swidget	tf_plan_seq;
extern swidget	l_plan_job_id;
extern swidget	tf_plan_job_id;
extern swidget	l_plan_order_id;
extern swidget	tf_plan_order_id;
extern swidget	l_plan_item_id;
extern swidget	tf_plan_item_id;
extern swidget	l_plan_frame_id;
extern swidget	tf_plan_frame_id;
extern swidget	l_plan_subframe_id;
extern swidget	tf_plan_subframe_id;
extern swidget	rc_plan_product_type;
extern swidget	pb_plan_product_type_any;
extern swidget	pb_plan_product_type_standard;
extern swidget	pb_plan_product_type_complex;
extern swidget	pb_plan_product_type_ccsd;
extern swidget	om_plan_product_type;
extern swidget	rc_plan_media_type;
extern swidget	pb_plan_media_type_any;
extern swidget	pb_plan_media_type_disk;
extern swidget	pb_plan_media_type_dcrsi;
extern swidget	om_plan_media_type;
extern swidget	l_plan_media_id;
extern swidget	tf_plan_media_id;
extern swidget	pb_plan_query;
extern swidget	rc_plan_processor_mode;
extern swidget	pb_plan_processor_mode_any;
extern swidget	pb_plan_processor_mode_continuous;
extern swidget	pb_plan_processor_mode_scansar;
extern swidget	om_plan_processor_mode;
extern swidget	rc_plan_data_direction;
extern swidget	pb_plan_data_direction_any;
extern swidget	pb_plan_data_direction_forward;
extern swidget	pb_plan_data_direction_reverse;
extern swidget	pb_plan_data_direction_unknown;
extern swidget	om_plan_data_direction;
extern swidget	sw_plan_ready_list;
extern swidget	sw_plan_available_list;
extern swidget	rc_plan_order_first;
extern swidget	pb_plan_order_first_none;
extern swidget	pb_plan_order_first_order_type;
extern swidget	pb_plan_order_first_priority;
extern swidget	pb_plan_order_first_sat_sens_rev;
extern swidget	pb_plan_order_first_mode;
extern swidget	pb_plan_order_first_pixel_spacing;
extern swidget	pb_plan_order_first_media_id;
extern swidget	pb_plan_order_first_job_id;
extern swidget	pb_plan_order_first_insert_top;
extern swidget	om_plan_order_first;
extern swidget	rc_plan_order_second;
extern swidget	pb_plan_order_second_none;
extern swidget	pb_plan_order_second_order_type;
extern swidget	pb_plan_order_second_priority;
extern swidget	pb_plan_order_second_sat_sens_rev;
extern swidget	pb_plan_order_second_mode;
extern swidget	pb_plan_order_second_pixel_spacing;
extern swidget	pb_plan_order_second_media_id;
extern swidget	pb_plan_order_second_job_id;
extern swidget	pb_plan_order_second_insert_top;
extern swidget	om_plan_order_second;
extern swidget	rc_plan_order_third;
extern swidget	pb_plan_order_third_none;
extern swidget	pb_plan_order_third_order_type;
extern swidget	pb_plan_order_third_priority;
extern swidget	pb_plan_order_third_sat_sens_rev;
extern swidget	pb_plan_order_third_mode;
extern swidget	pb_plan_order_third_pixel_spacing;
extern swidget	pb_plan_order_third_media_id;
extern swidget	pb_plan_order_third_job_id;
extern swidget	pb_plan_order_third_insert_top;
extern swidget	om_plan_order_third;
extern swidget	rc_plan_order_fourth;
extern swidget	pb_plan_order_fourth_none;
extern swidget	pb_plan_order_fourth_order_type;
extern swidget	pb_plan_order_fourth_priority;
extern swidget	pb_plan_order_fourth_sat_sens_rev;
extern swidget	pb_plan_order_fourth_mode;
extern swidget	pb_plan_order_fourth_pixel_spacing;
extern swidget	pb_plan_order_fourth_media_id;
extern swidget	pb_plan_order_fourth_job_id;
extern swidget	pb_plan_order_fourth_insert_top;
extern swidget	om_plan_order_fourth;
extern swidget	l_plan_ready_num_displayed;
extern swidget	l_plan_ready_num_selected;
extern swidget	l_plan_available_num_displayed;
extern swidget	l_plan_available_num_selected;
extern swidget	pb_plan_ready_select_all;
extern swidget	pb_plan_ready_deselect_all;
extern swidget	pb_plan_available_select_all;
extern swidget	pb_plan_available_deselect_all;
extern swidget	pb_plan_available_refresh;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

swidget	create_pps_plan( swidget _UxUxParent );

#endif	/* _PPS_PLAN_INCLUDED */

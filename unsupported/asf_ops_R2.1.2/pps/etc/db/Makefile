#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	makefile for etc
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#               @(#)Makefile	1.5  04/23/97
#===============================================================================

SUB_PATH      =   etc/db
LOCAL_PPS_DIR =   /LOCAL/$(ASFVER)/pps
DB_DIR        =   db
SOURCE_FILES = \
		PPS_db_build.csh		\
		L1_orders.crt			\
		L1_procparms.crt		\
		cleanup.crt			\
		counters.crt			\
		empty.crt			\
		init_counter.crt		\
		init_policy.crt			\
		insert_test.crt			\
		jobs.crt			\
		policy.crt			\
		ppsgui_orders.crt		\
		rules.crt			\
		scan_orders.crt			\
		schedule.crt			\
		sp_auto_avail.crt		\
		sp_check_pending.crt		\
		sp_check_policy.crt		\
		sp_get_data_for_ims_queries.crt	\
		sp_get_data_for_ims_status.crt  \
		sp_get_insert_top_flag.crt	\
		sp_get_l1_order_jobid.crt	\
		sp_get_next_frame_job.crt	\
		sp_get_next_jobid.crt		\
		sp_get_next_scan_job.crt	\
		sp_get_order_id.crt		\
		sp_get_scan_order_jobid.crt	\
		sp_insert_schedule.crt		\
		sp_is_job_pending.crt		\
		sp_is_job_submitted.crt		\
		sp_match_l1_order.crt		\
		sp_plan_available_remove.crt	\
		sp_plan_available_to_bottom.crt	\
		sp_plan_available_to_top.crt	\
		sp_plan_ready_to_bottom.crt	\
		sp_plan_ready_to_top.crt	\
		sp_remove_l1_cancel.crt		\
		sp_remove_l1_complete.crt	\
		sp_remove_l1_order.crt		\
		sp_remove_scan.crt			\
		sp_remove_submit.crt		\
		sp_resequence_schedule.crt	\
		sp_set_insert_top.crt		\
		sp_set_policy.crt		\
		sp_test_get_next_job.crt	\
		sp_test_get_num_jobs.crt	\
		sp_test_print_jobs.crt		\
		sp_test_sva.crt			\
		sp_update_L1_using_scan.crt	\
		sp_update_status_from_cp.crt	\
		sp_update_job_state.crt		\
		tr_L1orders_delete.crt		\
		tr_L1orders_insert.crt		\
		tr_jobs_delete.crt		\
		tr_jobs_insert.crt		\
		tr_jobs_update.crt		\
		tr_ppsgui_insert.crt		\
		tr_schedule_update.crt

default: $(SOURCE_FILES)
	chmod a+x *.csh

source:
	$(MAKE) default

all:
	$(MAKE) default

install:
	-/bin/mkdir $(LOCAL_PPS_DIR)
	-/bin/mkdir $(LOCAL_PPS_DIR)/$(DB_DIR)
	-/bin/rm -f $(LOCAL_PPS_DIR)/$(DB_DIR)/*
	/bin/cp -p * $(LOCAL_PPS_DIR)/$(DB_DIR)

%.crt: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.crt
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.crt

%.csh: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.csh
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.csh

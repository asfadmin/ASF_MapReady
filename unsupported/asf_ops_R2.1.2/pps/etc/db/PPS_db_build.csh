#! /bin/csh -f

#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:     PPS_db_build.csh
# Description:  PPS common environment variables script
# Creator:      Nadia Adhami
# Notes:
#
# SCCS Info:
#  		@(#)PPS_db_build.csh	1.5  04/23/97
#===============================================================================
 

echo ""
date
echo "I.    Create PPS types, rules, tables"
echo "II.   Declare Stored Procedures & Triggers"
echo "III.  Initialize Counters and Policy tables"
echo ""
echo "Usage:"
echo "      PPS_db_build.csh <sybase_userid>  <sybase_password>"
echo ""


set USERNAME = $1
set PASSWD   = $2
if ("${USERNAME}" == "") then
	set USERNAME = $user
	echo "Set username to " ${USERNAME}
endif
if ("${PASSWD}" == "") then
	echo "Have to set password - Bye"
	exit
endif

if ($?LOCAL) then
        echo LOCAL = $LOCAL
else
        echo LOCAL not set, exiting...
        exit 1
endif

echo ""
echo "1. Cleanup "
echo ""
sleep 1 
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/cleanup.crt

echo ""
echo "2. Create TYPES and RULES"
echo ""
sleep 1
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/rules.crt

echo ""
echo "3. Create TABLES"
echo ""
sleep 1 
echo "L1_orders"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/L1_orders.crt
echo "scan_orders"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/scan_orders.crt
echo "L1_procparms"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/L1_procparms.crt
echo "jobs"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/jobs.crt
echo "schedule"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/schedule.crt
echo "policy"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/policy.crt
echo "counters"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/counters.crt
echo "ppsgui_orders"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/ppsgui_orders.crt

echo ""
echo "4. Declare Stored Procedures "
echo ""
echo "used by PPS Server:"
echo ""
sleep 1 
echo "sp_insert_schedule"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_insert_schedule.crt
echo "sp_auto_avail"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_auto_avail.crt
echo "sp_get_insert_top_flag"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_insert_top_flag.crt
echo "sp_get_l1_order_jobid"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_l1_order_jobid.crt
echo "sp_get_next_scan_job"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_next_scan_job.crt
echo "sp_get_next_frame_job"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_next_frame_job.crt
echo "sp_get_next_jobid"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_next_jobid.crt
echo "sp_get_scan_order_jobid"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_scan_order_jobid.crt
echo "sp_is_job_pending"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_is_job_pending.crt
echo "sp_is_job_submitted"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_is_job_submitted.crt
echo "sp_match_l1_order"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_match_l1_order.crt
echo "sp_remove_l1_complete"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_remove_l1_complete.crt
echo "sp_remove_l1_cancel"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_remove_l1_cancel.crt
echo "sp_remove_l1_order"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_remove_l1_order.crt
echo "sp_remove_scan"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_remove_scan.crt
echo "sp_resequence_schedule"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_resequence_schedule.crt
echo "sp_update_job_state"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_update_job_state.crt
echo "sp_update_status_from_cp"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_update_status_from_cp.crt
echo "sp_remove_submit"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_remove_submit.crt
echo "sp_update_L1_using_scan"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_update_L1_using_scan.crt

echo ""
echo "used by PPS GUI:"
echo ""
echo "sp_check_pending"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_check_pending.crt
echo "sp_get_data_for_ims_queries"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_data_for_ims_queries.crt
echo "sp_get_order_id"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_order_id.crt
echo "sp_plan_available_remove"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_plan_available_remove.crt
echo "sp_plan_available_to_bottom"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_plan_available_to_bottom.crt
echo "sp_plan_available_to_top"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_plan_available_to_top.crt
echo "sp_plan_ready_to_bottom"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_plan_ready_to_bottom.crt
echo "sp_plan_ready_to_top"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_plan_ready_to_top.crt
echo "sp_set_insert_top"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_set_insert_top.crt
echo "sp_set_policy"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_set_policy.crt
echo "sp_get_data_for_ims_status"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_get_data_for_ims_status.crt

echo ""
echo "used for Testing by PPS Server :"
echo ""
echo "sp_test_print_jobs"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_test_print_jobs.crt
echo "sp_test_sva"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_test_sva.crt
echo "sp_test_get_next_job"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_test_get_next_job.crt
echo "sp_test_get_num_jobs"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/sp_test_get_num_jobs.crt

echo ""
echo "5. Declare Triggers"
echo ""
echo "tr_jobs_insert"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_jobs_insert.crt
echo "tr_jobs_delete"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_jobs_delete.crt
echo "tr_jobs_update"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_jobs_update.crt
echo "tr_L1orders_delete"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_L1orders_delete.crt
echo "tr_L1orders_insert"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_L1orders_insert.crt
echo "tr_ppsgui_insert"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_ppsgui_insert.crt
echo "tr_schedule_update"
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/tr_schedule_update.crt

echo ""
echo "6. Initialize  Counters Table"
echo ""
sleep 1 
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/init_counter.crt

echo ""
echo "7. Initialize  Policy Table"
echo ""
sleep 1 
isql -U ${USERNAME} -P ${PASSWD} -i $LOCAL/pps/db/init_policy.crt

echo ""
echo "Completed."
echo ""

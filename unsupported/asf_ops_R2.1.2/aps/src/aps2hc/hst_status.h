/******************************************************************************/
/*                                                                            */
/*  Filename:    hst_status.h                                                 */
/*                                                                            */
/*  Abstract:    Host controller error code definitions.                      */
/*                                                                            */
/*  Author:      Philip Munts                                                 */
/*                                                                            */
/*  History:     25 August 1995 -- PM  -- Initial version.                    */
/*               29 August 1995 -- NJC -- Added new error messages.           */
/*               16 Oct    1995 -- hat -- Added new error message.	      */
/*       	 11 Mar	   1996 -- hat -- Added new error message.            */
/*       	 15 April  1996 -- hat -- Added new error message.            */
/*                                                                            */
/*  Every possible Host Controller error code should have a symbol and string */
/*  defined here.  Use the DefineError() macro to define both at once.        */
/*                                                                            */
/******************************************************************************/

#if !defined(HST_STATUS_INCLUDED) || defined(ALLOCATESTRINGS)

#ifdef ALLOCATESTRINGS
#define DefineError(symbol, string) string,
static char const * const hst_errlist[] =
{
#else
#define DefineError(symbol, string) symbol,
enum hst_errorcodes { HCERRLOLIMIT = 1000,
#endif

/* Host controller specific errors go here */

DefineError(hst_s_bad_command, "Bad command message")
DefineError(hst_s_unexp_data, "Unexpected data in input stream")
DefineError(hst_s_TCP_lost, "TCP connection lost")
DefineError(hst_s_echo_mismatch, "Command echo does not match command")
DefineError(hst_s_stat_mismatch, "Status does not match command")

DefineError(hst_s_null_file_ptr, "File was not opened")

DefineError(hst_s_unknown_frequency, "The frequency is unknown")

DefineError(hst_s_odl_unknown_keyword, "ODL message contains unknown keyword")
DefineError(hst_s_odl_wrong_message, "ODL message contains the wrong type of message")
DefineError(hst_s_odl_empty_header, "ODL message contains an empty header")
DefineError(hst_s_odl_bad_parse, "ODL message contains an empty header")
DefineError(hst_s_odl_bad_value, "ODL message contains a bad value")

DefineError(hst_s_wos_no_wos_record, "WOS has no WOS Record")
DefineError(hst_s_wos_too_big, "WOS is too large")
DefineError(hst_s_wos_premature_eof, "WOS file has premature EOF")
DefineError(hst_s_wos_bad_header, "WOS file has bad header")
DefineError(hst_s_wos_bad_record, "WOS file has bad WOS_RECORD")
DefineError(hst_s_wos_unknown_satellite, "WOS has unknown satellite")
DefineError(hst_s_wos_unknown_antenna, "WOS has unknown antenna")
DefineError(hst_s_wos_unknown_freq_sat_combo, "WOS has unknown freq sat combo")
DefineError(hst_s_wos_unknown_data_dir, "WOS has unknown data direction")
DefineError(hst_s_wos_unknown_customer, "WOS has unknown customer")
DefineError(hst_s_wos_rejected, "The WOS was rejected")
DefineError(hst_s_wos_unknown_customer_sat_combo, "WOS has unknown customer satellite combo")
DefineError(hst_s_wos_unknown_data_dir_sat_combo, "WOS has unknown data direction and satellite combo")
DefineError(hst_s_wos_record_times_overlap, "WOS has overlapping record times for transmitter")
DefineError(hst_s_wos_inverted_record_times, "WOS has record start time >= record stop time")
DefineError(hst_s_wos_inverted_aoslos_times, "WOS has aos time >= los time")
DefineError(hst_s_wos_inverted_record_aoslos_times, "WOS has record times outside of aos/los time")
DefineError(hst_s_wos_aoslos_to_large, "WOS has aos-to-los greater than maximum pass duration")

DefineError(hst_s_time_conversion_failed, "The time conversion failed")

DefineError(hst_s_operator_timeout, "Operator did not respond in time")

DefineError(hst_s_bad_syntax, "Syntax problem with data")
DefineError(hst_s_syntax, "Syntax problem with data")

DefineError(hst_s_bad_semantic, "Semantic problem with data")
DefineError(hst_s_semantic, "Semantic problem with data")

DefineError(hst_s_bad_miss_comp, "Missing component in data")
DefineError(hst_s_misscomp, "Missing component in data")

DefineError(hst_s_data_transfer_failed, "The data transfered failed")

DefineError(hst_s_pps_djr_q_empty, "Dub Job Request queue is empty")
DefineError(hst_s_pps_djr_no_match, "HDDR type is not in queue")

DefineError(hst_s_write_protect, "Tape is write protected")
DefineError(hst_s_no_tape, "No tape mounted on drive")
DefineError(hst_s_tape_timeout, "Tape operation timeout")
DefineError(hst_s_device_busy, "Device operation in progress")

DefineError(hst_s_rule_nomatch, "Rule cannot be satisfied")

DefineError(hst_s_ROS_too_big, "ROS is too large")
DefineError(hst_s_ROS_bad_job, "ROS entry has invalid job type")
DefineError(hst_s_ROS_wrong_device, "ROS entry is for wrong device")
DefineError(hst_s_ROS_too_late, "ROS entry too late for acquisition")

DefineError(hst_s_not_ready, "Missed acquisition: system not ready")

DefineError(hst_s_env_var_not_set, "The environment variables are not set")

DefineError(hst_s_DCRSi_scanner, "DCRSi scanner is off")

DefineError(hst_s_jit_bad_write, "Error occured writing jit to a file")

DefineError(hst_s_device_already_scheduled, "Device is already scheduled")

DefineError(hst_s_list_smaller_than_table_data, "List is smaller than table data")

DefineError(hst_s_bad_parm, "Illegal parameter value")

DefineError(hst_s_unauth_client, "Unauthorized client")
DefineError(hst_s_bad_net_addr, "Bad network address")
DefineError(hst_s_device_offline, "Device is unavailable")

DefineError(hst_s_malloc_failed, "Malloc failed")
DefineError(hst_s_illegal_job_type, "Job Type is not known or implemented")
DefineError(hst_s_unknown_device, "The device name is not known")
DefineError(hst_s_different_devices_online, "Different devices are now online")

DefineError(hst_s_unknown_satellite, "Unknown satellite identifier")

DefineError(hst_s_dss_bad_write, "Error occured writing ds schedule to a file")

DefineError(hst_s_worker_mismatch, "Workers have changed since scheduling started")

DefineError(hst_s_operator_cancel, "Operator canceled operation")

DefineError(hst_s_no_window_available, "A window was not found")
DefineError(hst_s_no_job_available, "A job was not found")
DefineError(hst_s_failsafe_engaged, "The finite loop failsafe was engaged")

DefineError(hst_s_unknown_dogtag, "Unknown dog-tag number")
DefineError(hst_s_illegal_dogtag, "Illegal dog-tag number")
DefineError(hst_s_unknown_tape_series, "Unknown tape series")
DefineError(hst_s_unknown_drive_type, "Unknown drive type")
DefineError(hst_s_unknown_media_type, "Unknown media type")
DefineError(hst_s_bad_Sybase_status, "A database error occurred")
DefineError(hst_s_duplicate_db_entries, "More than one db row was affected")
DefineError(hst_s_already_mounted, "Recorder already has a tape mounted")
DefineError(hst_s_already_loaded, "Recorder already has a tape loaded")

DefineError(hst_s_ephemeris_premature_eof, "Ephemeris file has premature EOF")
DefineError(hst_s_ephemeris_bad_header, "Ephemeris file has bad header")
DefineError(hst_s_ephemeris_bad_record, "Ephemeris file has bad State Vector")
DefineError(hst_s_ephemeris_bad_metadata, "Ephemeris file has bad metadata")

DefineError(hst_s_cannot_resolve, "Cannot resolve host address")
DefineError(hst_s_connect_failed, "TCP connection failed")
DefineError(hst_s_login_failed, "Login failed")
DefineError(hst_s_control_failed, "Control upgrade failed")
DefineError(hst_s_pathq_failed, "Pathname query failed")
DefineError(hst_s_11s_bad_write, "Error occured writing 11m schedule to a file")
DefineError(hst_s_10s_bad_write, "Error occured writing 10m schedule to a file")
DefineError(hst_s_ftp_failed, "ftp file transfer failed")

DefineError(hst_s_IMS_warning, "IMS_WARNING")
DefineError(hst_s_IMS_error, "IMS_ERROR")
DefineError(hst_s_IMS_fatal, "IMS_FATAL")

DefineError(hst_s_ephemeris_too_big, "Wrong number of Ephemeris records")
DefineError(hst_s_ephemeris_no_ephemeris_record, "Ephemeris has no Ephemeris Record")
DefineError(hst_s_ephemeris_no_svmd, "Ephemeris has no State Vector Metadata")
DefineError(hst_s_ephemeris_no_svd, "Ephemeris has no State Vector Data")
DefineError(hst_s_ephemeris_rejected, "The Ephemeris was rejected")
DefineError(hst_s_ephemeris_time_regresses, "The Ephemeris time is older than the previous state vectors")

DefineError(hst_s_10e_bad_write, "Error occured writing 10m ephemeris to a file")

DefineError(hst_s_no_matching_data_found, "A match for the specified data was not found")

DefineError(hst_s_no_schedule_for_resource, "The WOS contains no activities for the resource")

DefineError(hst_s_server_locked, "Server is locked by another client.")

DefineError(hst_s_wos_antenna_gap_violation, "Antenna gap violation.")
DefineError(hst_s_wos_recorder_gap_violation, "Recorder gap violation.")

#ifdef ALLOCATESTRINGS
"Last message"
};
#else
HCERRHILIMIT };
#endif

#undef DefineError
#define HST_STATUS_INCLUDED
#endif

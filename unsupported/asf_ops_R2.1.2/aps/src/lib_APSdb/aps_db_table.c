#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:    aps_db_table.c
 
Description:  global variables and tables used for APS database access

Notes:
          This file was created by the program: 
          /home/aps/r2.1.2/etc/install/sh_scripts/Create_aps_db_table_source.csh
          Fri Jan 30 15:52:11 PST 1998 
 
==============================================================================*/
#pragma ident   "@(#)aps_db_table.c	5.5 98/01/30 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_APSdb/SCCS/s.aps_db_table.c"

#include "db_sybint.h"

#include "db_activ_conf.h"
COLUMN_DEFS activ_conf_columns_[NUM_ACTIV_CONF_COLS+1] =
{
  {"activ_conf.n_acty",           2,       SMALLBIND,     "%hd"},
  {"activ_conf.m_acty",           2,       SMALLBIND,     "%hd"},
  {"activ_conf.conf_status",      2,       SMALLBIND,     "%hd"},
    {NULL, 0, 0, NULL}
} ;

#include "db_active_dar_activities.h"
COLUMN_DEFS active_dar_activities_columns_[NUM_ACTIVE_DAR_ACTIVITIES_COLS+1] =
{
  {"active_dar_activities.node",            10+1,     NTBSTRINGBIND, "%s"},
  {"active_dar_activities.process_id",       8+1,     NTBSTRINGBIND, "%s"},
  {"active_dar_activities.kpid",             4,       INTBIND,       "%d"},
  {"active_dar_activities.command_name",    16+1,     NTBSTRINGBIND, "%s"},
  {"active_dar_activities.permission_id",    4,       INTBIND,       "%d"},
  {"active_dar_activities.mu_activity_id",  21+1,     NTBSTRINGBIND, "%s"},
  {"active_dar_activities.userid",           8+1,     NTBSTRINGBIND, "%s"},
  {"active_dar_activities.darid",            4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_active_planning_activities.h"
COLUMN_DEFS active_planning_activities_columns_[NUM_ACTIVE_PLANNING_ACTIVITIES_COLS+1] =
{
  {"active_planning_activities.node",            10+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.process_id",       8+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.kpid",             4,       INTBIND,       "%d"},
  {"active_planning_activities.command_name",    16+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.permission_id",    4,       INTBIND,       "%d"},
  {"active_planning_activities.mu_activity_id",  21+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.userid",           8+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"active_planning_activities.station_id",       3+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_active_single_activities.h"
COLUMN_DEFS active_single_activities_columns_[NUM_ACTIVE_SINGLE_ACTIVITIES_COLS+1] =
{
  {"active_single_activities.node",            10+1,     NTBSTRINGBIND, "%s"},
  {"active_single_activities.process_id",       8+1,     NTBSTRINGBIND, "%s"},
  {"active_single_activities.kpid",             4,       INTBIND,       "%d"},
  {"active_single_activities.command_name",    16+1,     NTBSTRINGBIND, "%s"},
  {"active_single_activities.permission_id",    4,       INTBIND,       "%d"},
  {"active_single_activities.mu_activity_id",  21+1,     NTBSTRINGBIND, "%s"},
  {"active_single_activities.userid",           8+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_activities.h"
COLUMN_DEFS activities_columns_[NUM_ACTIVITIES_COLS+1] =
{
  {"activities.obj",              2+1,     NTBSTRINGBIND, "%s"},
  {"activities.acty",             3+1,     NTBSTRINGBIND, "%s"},
  {"activities.transid",          2+1,     NTBSTRINGBIND, "%s"},
  {"activities.n_acty",           2,       SMALLBIND,     "%hd"},
    {NULL, 0, 0, NULL}
} ;

#include "db_antenna.h"
COLUMN_DEFS antenna_columns_[NUM_ANTENNA_COLS+1] =
{
  {"antenna.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"antenna.antenna_id",       2,       SMALLBIND,     "%hd"},
  {"antenna.pre_pass_time_sec",   4,       INTBIND,       "%d"},
  {"antenna.pre_dtk_track_pad_sec",              4,       INTBIND,       "%d"},
  {"antenna.post_dtk_track_pad_sec",             4,       INTBIND,       "%d"},
  {"antenna.post_pass_time_sec",  4,       INTBIND,       "%d"},
  {"antenna.comment",         30+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_antenna_down_times.h"
COLUMN_DEFS antenna_down_times_columns_[NUM_ANTENNA_DOWN_TIMES_COLS+1] =
{
  {"antenna_down_times.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"antenna_down_times.antenna_id",       2,       SMALLBIND,     "%hd"},
  {"antenna_down_times.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"antenna_down_times.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"antenna_down_times.comments",        60+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_antenna_pref.h"
COLUMN_DEFS antenna_pref_columns_[NUM_ANTENNA_PREF_COLS+1] =
{
  {"antenna_pref.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"antenna_pref.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"antenna_pref.antenna_id",       2,       SMALLBIND,     "%hd"},
  {"antenna_pref.preference",       2,       SMALLBIND,     "%hd"},
    {NULL, 0, 0, NULL}
} ;

#include "db_antenna_priority.h"
COLUMN_DEFS antenna_priority_columns_[NUM_ANTENNA_PRIORITY_COLS+1] =
{
  {"antenna_priority.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"antenna_priority.antenna_id",       2,       SMALLBIND,     "%hd"},
  {"antenna_priority.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"antenna_priority.priority",         2,       SMALLBIND,     "%hd"},
    {NULL, 0, 0, NULL}
} ;

#include "db_csa_data.h"
COLUMN_DEFS csa_data_columns_[NUM_CSA_DATA_COLS+1] =
{
  {"csa_data.unavail_event_counter",              4,       INTBIND,       "%d"},
  {"csa_data.rar_daily_counter",   4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_cvrg.h"
COLUMN_DEFS cvrg_columns_[NUM_CVRG_COLS+1] =
{
  {"cvrg.marker",           4,       INTBIND,       "%d"},
  {"cvrg.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"cvrg.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"cvrg.mjdate",           8,       FLT8BIND,      "%f"}, 
  {"cvrg.rev",              4,       INTBIND,       "%d"},
  {"cvrg.sublat",           4,       REALBIND,      "%f"},
  {"cvrg.sublon",           4,       REALBIND,      "%f"},
  {"cvrg.nrlat",            4,       REALBIND,      "%f"},
  {"cvrg.nrlon",            4,       REALBIND,      "%f"},
  {"cvrg.farlat",           4,       REALBIND,      "%f"},
  {"cvrg.farlon",           4,       REALBIND,      "%f"},
  {"cvrg.satalt",           4,       REALBIND,      "%f"},
  {"cvrg.sun",              1,       CHARBIND,      "%c"},
  {"cvrg.sunang",           4,       REALBIND,      "%f"},
  {"cvrg.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"cvrg.masks",            4+1,     NTBSTRINGBIND, "%s"},
  {"cvrg.opermode",         1,       TINYBIND,      "%d"},
  {"cvrg.crossflag",        1,       CHARBIND,      "%c"},
  {"cvrg.ascdsc",           1,       CHARBIND,      "%c"},
    {NULL, 0, 0, NULL}
} ;

#include "db_cvrgmdat.h"
COLUMN_DEFS cvrgmdat_columns_[NUM_CVRGMDAT_COLS+1] =
{
  {"cvrgmdat.marker",           4,       INTBIND,       "%d"},
  {"cvrgmdat.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.start_time",      21+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.stop_time",       21+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.gen_time",        21+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.runmask",          3+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.repflag",          1,       CHARBIND,      "%c"},
  {"cvrgmdat.version",          5+1,     NTBSTRINGBIND, "%s"},
  {"cvrgmdat.ephemeris",      100+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_dar.h"
COLUMN_DEFS dar_columns_[NUM_DAR_COLS+1] =
{
  {"dar.darid",            4,       INTBIND,       "%d"},
  {"dar.userid",          15+1,     NTBSTRINGBIND, "%s"},
  {"dar.reqtime",         21+1,     NTBSTRINGBIND, "%s"},
  {"dar.reqstat",          3+1,     NTBSTRINGBIND, "%s"},
  {"dar.prvdarid",         4,       INTBIND,       "%d"},
  {"dar.prvreqstat",       3+1,     NTBSTRINGBIND, "%s"},
  {"dar.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"dar.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"dar.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"dar.endtime",         21+1,     NTBSTRINGBIND, "%s"},
  {"dar.sitename",        32+1,     NTBSTRINGBIND, "%s"},
  {"dar.shape",            1,       CHARBIND,      "%c"},
  {"dar.radius",           4,       REALBIND,      "%f"},      
  {"dar.nwlat",            4,       REALBIND,      "%f"},
  {"dar.nwlon",            4,       REALBIND,      "%f"},
  {"dar.nelat",            4,       REALBIND,      "%f"},
  {"dar.nelon",            4,       REALBIND,      "%f"},
  {"dar.selat",            4,       REALBIND,      "%f"},
  {"dar.selon",            4,       REALBIND,      "%f"},
  {"dar.swlat",            4,       REALBIND,      "%f"},
  {"dar.swlon",            4,       REALBIND,      "%f"},
  {"dar.nobs",             1,       TINYBIND,      "%d"},
  {"dar.fobs",            30+1,     NTBSTRINGBIND, "%s"},
  {"dar.rev",              4,       INTBIND,       "%d"},
  {"dar.ascdsc",           1,       CHARBIND,      "%c"},
  {"dar.usercmnt",       255+1,     NTBSTRINGBIND, "%s"},
  {"dar.plnrcmnt",       255+1,     NTBSTRINGBIND, "%s"},
  {"dar.quicklook",        1,       CHARBIND,      "%c"},
  {"dar.J1_obs_freq",      4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_dl2obs.h"
COLUMN_DEFS dl2obs_columns_[NUM_DL2OBS_COLS+1] =
{
  {"dl2obs.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"dl2obs.rev_obs",          4,       INTBIND,       "%d"},
  {"dl2obs.dtkid_obs",        1,       TINYBIND,      "%d"},
  {"dl2obs.rev_dl",           4,       INTBIND,       "%d"},
  {"dl2obs.dtkid_dl",         1,       TINYBIND,      "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_dtk.h"
COLUMN_DEFS dtk_columns_[NUM_DTK_COLS+1] =
{
  {"dtk.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"dtk.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"dtk.rev",              4,       INTBIND,       "%d"},
  {"dtk.dtkid",            1,       TINYBIND,      "%d"},
  {"dtk.fadtkid",         20+1,     NTBSTRINGBIND, "%s"},
  {"dtk.darid",            4,       INTBIND,       "%d"},
  {"dtk.actid",            6+1,     NTBSTRINGBIND, "%s"},
  {"dtk.ascdsc",           1,       CHARBIND,      "%c"},
  {"dtk.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"dtk.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"dtk.strtlat",          4,       REALBIND,      "%f"},
  {"dtk.stoplat",          4,       REALBIND,      "%f"},
  {"dtk.nrlat1",           4,       REALBIND,      "%f"},
  {"dtk.nrlon1",           4,       REALBIND,      "%f"},
  {"dtk.farlat1",          4,       REALBIND,      "%f"},
  {"dtk.farlon1",          4,       REALBIND,      "%f"},
  {"dtk.nrlat2",           4,       REALBIND,      "%f"},
  {"dtk.nrlon2",           4,       REALBIND,      "%f"},
  {"dtk.farlat2",          4,       REALBIND,      "%f"},
  {"dtk.farlon2",          4,       REALBIND,      "%f"},
  {"dtk.lookangl",         4,       REALBIND,      "%f"},
  {"dtk.dtkstat",          3+1,     NTBSTRINGBIND, "%s"},
  {"dtk.proposed_dtkstat", 3+1,     NTBSTRINGBIND, "%s"},
  {"dtk.transid",          2+1,     NTBSTRINGBIND, "%s"},
  {"dtk.sitename",        32+1,     NTBSTRINGBIND, "%s"},
  {"dtk.notes",           40+1,     NTBSTRINGBIND, "%s"},
  {"dtk.dtkdate",         21+1,     NTBSTRINGBIND, "%s"},
  {"dtk.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"dtk.fa_schedule_link",   20+1,     NTBSTRINGBIND, "%s"},
  {"dtk.planner_quicklook",   1,       CHARBIND,      "%c"},
  {"dtk.science_quicklook",   1,       CHARBIND,      "%c"},
  {"dtk.submit_time",     21+1,     NTBSTRINGBIND, "%s"},
  {"dtk.antenna_id",       2,       SMALLBIND,     "%hd"},
  {"dtk.fa_strttime",     21+1,     NTBSTRINGBIND, "%s"},
  {"dtk.fa_stoptime",     21+1,     NTBSTRINGBIND, "%s"},
  {"dtk.fa_duration_min",  4,       REALBIND,      "%f"},
  {"dtk.asf_reduction_min",   4,       REALBIND,      "%f"},
    {NULL, 0, 0, NULL}
} ;

#include "db_dtkid_lock.h"
COLUMN_DEFS dtkid_lock_columns_[NUM_DTKID_LOCK_COLS+1] =
{
  {"dtkid_lock.semaphore",        4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_ephemeris.h"
COLUMN_DEFS ephemeris_columns_[NUM_EPHEMERIS_COLS+1] =
{
  {"ephemeris.filename",       100+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.starttime",       21+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.endtime",         21+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.startrev",         4,       INTBIND,       "%d"},
  {"ephemeris.endrev",           4,       INTBIND,       "%d"},
  {"ephemeris.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.epoch",            8,       FLT8BIND,      "%f"}, 
  {"ephemeris.phase_name",       1,       CHARBIND,      "%c"},
  {"ephemeris.sv_filename",    100+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_type",         10+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_rev",           5+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_time",         21+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_r_x",          11+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_r_y",          11+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_r_z",          11+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_v_x",          11+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_v_y",          11+1,     NTBSTRINGBIND, "%s"},
  {"ephemeris.sv_v_z",          11+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_framegen_calls.h"
COLUMN_DEFS framegen_calls_columns_[NUM_FRAMEGEN_CALLS_COLS+1] =
{
  {"framegen_calls.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"framegen_calls.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"framegen_calls.rev",              4,       INTBIND,       "%d"},
  {"framegen_calls.dtkid",            1,       TINYBIND,      "%d"},
  {"framegen_calls.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"framegen_calls.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"framegen_calls.dtkstat",          3+1,     NTBSTRINGBIND, "%s"},
  {"framegen_calls.dtkdate",         21+1,     NTBSTRINGBIND, "%s"},
  {"framegen_calls.station_id",       3+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_j1_dn_times.h"
COLUMN_DEFS j1_dn_times_columns_[NUM_J1_DN_TIMES_COLS+1] =
{
  {"j1_dn_times.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"j1_dn_times.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"j1_dn_times.sar_status",       1,       CHARBIND,      "%c"},
  {"j1_dn_times.ops_status",       1,       CHARBIND,      "%c"},
  {"j1_dn_times.mdr_status",       1,       CHARBIND,      "%c"},
  {"j1_dn_times.mdr_op_time",      2,       SMALLBIND,     "%hd"},
  {"j1_dn_times.mdt_status",       1,       CHARBIND,      "%c"},
    {NULL, 0, 0, NULL}
} ;

#include "db_maskinout.h"
COLUMN_DEFS maskinout_columns_[NUM_MASKINOUT_COLS+1] =
{
  {"maskinout.stationid",        3+1,     NTBSTRINGBIND, "%s"},
  {"maskinout.ejdate",           8,       FLT8BIND,      "%f"}, 
  {"maskinout.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"maskinout.rev",              4,       INTBIND,       "%d"},
  {"maskinout.inout",            3+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_maskinout_mdat.h"
COLUMN_DEFS maskinout_mdat_columns_[NUM_MASKINOUT_MDAT_COLS+1] =
{
  {"maskinout_mdat.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"maskinout_mdat.start_time",      21+1,     NTBSTRINGBIND, "%s"},
  {"maskinout_mdat.stop_time",       21+1,     NTBSTRINGBIND, "%s"},
  {"maskinout_mdat.gen_time",        21+1,     NTBSTRINGBIND, "%s"},
  {"maskinout_mdat.version",          5+1,     NTBSTRINGBIND, "%s"},
  {"maskinout_mdat.ephemeris",      100+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_multi_user_wait.h"
COLUMN_DEFS multi_user_wait_columns_[NUM_MULTI_USER_WAIT_COLS+1] =
{
  {"multi_user_wait.status",          11+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_permission_counter.h"
COLUMN_DEFS permission_counter_columns_[NUM_PERMISSION_COUNTER_COLS+1] =
{
  {"permission_counter.permission_id",    4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_phase.h"
COLUMN_DEFS phase_columns_[NUM_PHASE_COLS+1] =
{
  {"phase.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"phase.phase_name",       1,       CHARBIND,      "%c"},
  {"phase.phase_start",     21+1,     NTBSTRINGBIND, "%s"},
  {"phase.phase_lon",        8,       FLT8BIND,      "%f"},
  {"phase.phase_days",       4,       INTBIND,       "%d"},
  {"phase.phase_orbits",     4,       INTBIND,       "%d"},
  {"phase.last_rev",         4,       INTBIND,       "%d"},
  {"phase.cycle_days",       4,       INTBIND,       "%d"},
  {"phase.cycle_revs",       4,       INTBIND,       "%d"},
  {"phase.orb_a",            8,       FLT8BIND,      "%f"},
  {"phase.orb_e",            8,       FLT8BIND,      "%f"},
  {"phase.orb_i",            8,       FLT8BIND,      "%f"},
  {"phase.orb_arg_peri",     8,       FLT8BIND,      "%f"},
  {"phase.rsp_0_lon",        8,       FLT8BIND,      "%f"},
  {"phase.n_rows",           4,       INTBIND,       "%d"},
  {"phase.min_row",          4,       INTBIND,       "%d"},
  {"phase.max_row",          4,       INTBIND,       "%d"},
  {"phase.antarctic_mode",   1,       CHARBIND,      "%c"},
  {"phase.EW_tol_km",        8,       FLT8BIND,      "%f"},
  {"phase.NS_tol_min",       8,       FLT8BIND,      "%f"},
    {NULL, 0, 0, NULL}
} ;

#include "db_reqq_phase.h"
COLUMN_DEFS reqq_phase_columns_[NUM_REQQ_PHASE_COLS+1] =
{
  {"reqq_phase.reqq_id",          1,       TINYBIND,      "%d"},
  {"reqq_phase.due_date",        21+1,     NTBSTRINGBIND, "%s"},
  {"reqq_phase.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"reqq_phase.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_rgs_down_times.h"
COLUMN_DEFS rgs_down_times_columns_[NUM_RGS_DOWN_TIMES_COLS+1] =
{
  {"rgs_down_times.station_id",       3+1,     NTBSTRINGBIND, "%s"},
  {"rgs_down_times.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"rgs_down_times.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"rgs_down_times.disposition",      1,       CHARBIND,      "%c"},
  {"rgs_down_times.fa_notification",  1,       CHARBIND,      "%c"},
  {"rgs_down_times.utype",            1,       CHARBIND,      "%c"},
  {"rgs_down_times.ureason",          1,       CHARBIND,      "%c"},
  {"rgs_down_times.remarks",         60+1,     NTBSTRINGBIND, "%s"},
  {"rgs_down_times.unavail_event_counter",              4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_sat_inclusion_period.h"
COLUMN_DEFS sat_inclusion_period_columns_[NUM_SAT_INCLUSION_PERIOD_COLS+1] =
{
  {"sat_inclusion_period.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"sat_inclusion_period.length",           4,       INTBIND,       "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_sat_rev_dtkid.h"
COLUMN_DEFS sat_rev_dtkid_columns_[NUM_SAT_REV_DTKID_COLS+1] =
{
  {"sat_rev_dtkid.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"sat_rev_dtkid.rev",              4,       INTBIND,       "%d"},
  {"sat_rev_dtkid.dtkid",            1,       TINYBIND,      "%d"},
    {NULL, 0, 0, NULL}
} ;

#include "db_satsensor.h"
COLUMN_DEFS satsensor_columns_[NUM_SATSENSOR_COLS+1] =
{
  {"satsensor.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"satsensor.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"satsensor.opermode",         4,       INTBIND,       "%d"},
  {"satsensor.chopermode",      15+1,     NTBSTRINGBIND, "%s"},
  {"satsensor.beamradiusd",      4,       REALBIND,      "%f"},
  {"satsensor.lookangled",       4,       REALBIND,      "%f"},
  {"satsensor.cvrg_allowed",     1,       CHARBIND,      "%c"},
  {"satsensor.low_bit_rate_flag",   1,       CHARBIND,      "%c"},
    {NULL, 0, 0, NULL}
} ;

#include "db_schstat.h"
COLUMN_DEFS schstat_columns_[NUM_SCHSTAT_COLS+1] =
{
  {"schstat.status",           3+1,     NTBSTRINGBIND, "%s"},
  {"schstat.pap",              1,       CHARBIND,      "%c"},
  {"schstat.wos",              1,       CHARBIND,      "%c"},
  {"schstat.eurf",             1,       CHARBIND,      "%c"},
  {"schstat.reqq",             1,       CHARBIND,      "%c"},
  {"schstat.reqw",             1,       CHARBIND,      "%c"},
  {"schstat.ople",             1,       CHARBIND,      "%c"},
    {NULL, 0, 0, NULL}
} ;

#include "db_schunavail.h"
COLUMN_DEFS schunavail_columns_[NUM_SCHUNAVAIL_COLS+1] =
{
  {"schunavail.facid",            2+1,     NTBSTRINGBIND, "%s"},
  {"schunavail.prareid",         11+1,     NTBSTRINGBIND, "%s"},
  {"schunavail.syssubstat",       6+1,     NTBSTRINGBIND, "%s"},
  {"schunavail.utype",            1,       CHARBIND,      "%c"},
  {"schunavail.ureason",          1,       CHARBIND,      "%c"},
  {"schunavail.non_esa",          1,       CHARBIND,      "%c"},
  {"schunavail.reserved",         2+1,     NTBSTRINGBIND, "%s"},
  {"schunavail.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"schunavail.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"schunavail.remarks",         60+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_seg.h"
COLUMN_DEFS seg_columns_[NUM_SEG_COLS+1] =
{
  {"seg.darid",            4,       INTBIND,       "%d"},
  {"seg.segid",            2,       SMALLBIND,     "%hd"},
  {"seg.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"seg.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"seg.rev",              4,       INTBIND,       "%d"},
  {"seg.dtkid",           15+1,     NTBSTRINGBIND, "%s"},
  {"seg.ascdsc",           1,       CHARBIND,      "%c"},
  {"seg.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"seg.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"seg.strtlat",          4,       REALBIND,      "%f"},
  {"seg.stoplat",          4,       REALBIND,      "%f"},
  {"seg.nrlat1",           4,       REALBIND,      "%f"},
  {"seg.nrlon1",           4,       REALBIND,      "%f"},
  {"seg.farlat1",          4,       REALBIND,      "%f"},
  {"seg.farlon1",          4,       REALBIND,      "%f"},
  {"seg.nrlat2",           4,       REALBIND,      "%f"},
  {"seg.nrlon2",           4,       REALBIND,      "%f"},
  {"seg.farlat2",          4,       REALBIND,      "%f"},
  {"seg.farlon2",          4,       REALBIND,      "%f"},
  {"seg.lookangl",         4,       REALBIND,      "%f"},
  {"seg.segstat",          3+1,     NTBSTRINGBIND, "%s"},
  {"seg.segdate",          8+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_site.h"
COLUMN_DEFS site_columns_[NUM_SITE_COLS+1] =
{
  {"site.sitename",        32+1,     NTBSTRINGBIND, "%s"},
  {"site.shape",            1,       CHARBIND,      "%c"},
  {"site.radius",           4,       REALBIND,      "%f"},      
  {"site.nwlat",            4,       REALBIND,      "%f"},
  {"site.nwlon",            4,       REALBIND,      "%f"},
  {"site.nelat",            4,       REALBIND,      "%f"},
  {"site.nelon",            4,       REALBIND,      "%f"},
  {"site.selat",            4,       REALBIND,      "%f"},
  {"site.selon",            4,       REALBIND,      "%f"},
  {"site.swlat",            4,       REALBIND,      "%f"},
  {"site.swlon",            4,       REALBIND,      "%f"},
  {"site.comments",        50+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_sscvrg.h"
COLUMN_DEFS sscvrg_columns_[NUM_SSCVRG_COLS+1] =
{
  {"sscvrg.darid",            4,       INTBIND,       "%d"},
  {"sscvrg.sitename",        32+1,     NTBSTRINGBIND, "%s"},
  {"sscvrg.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"sscvrg.sensor",           3+1,     NTBSTRINGBIND, "%s"},
  {"sscvrg.rev",              4,       INTBIND,       "%d"},
  {"sscvrg.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"sscvrg.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"sscvrg.strtet",           8,       FLT8BIND,      "%f"}, 
  {"sscvrg.stopet",           8,       FLT8BIND,      "%f"}, 
  {"sscvrg.strtlat",          4,       REALBIND,      "%f"},
  {"sscvrg.stoplat",          4,       REALBIND,      "%f"},
  {"sscvrg.nrlat1",           4,       REALBIND,      "%f"},
  {"sscvrg.nrlon1",           4,       REALBIND,      "%f"},
  {"sscvrg.farlat1",          4,       REALBIND,      "%f"},
  {"sscvrg.farlon1",          4,       REALBIND,      "%f"},
  {"sscvrg.nrlat2",           4,       REALBIND,      "%f"},
  {"sscvrg.nrlon2",           4,       REALBIND,      "%f"},
  {"sscvrg.farlat2",          4,       REALBIND,      "%f"},
  {"sscvrg.farlon2",          4,       REALBIND,      "%f"},
  {"sscvrg.ascdsc",           1,       CHARBIND,      "%c"},
    {NULL, 0, 0, NULL}
} ;

#include "db_station.h"
COLUMN_DEFS station_columns_[NUM_STATION_COLS+1] =
{
  {"station.stationid",        3+1,     NTBSTRINGBIND, "%s"},
  {"station.stationname",     20+1,     NTBSTRINGBIND, "%s"},
  {"station.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"station.stlat",            8,       FLT8BIND,      "%f"},
  {"station.stlon",            8,       FLT8BIND,      "%f"},
  {"station.staltm",           8,       FLT8BIND,      "%f"},
  {"station.stradiuskm",       8,       FLT8BIND,      "%f"},
  {"station.runbuffkm",        8,       FLT8BIND,      "%f"},
  {"station.elevangle",        8,       FLT8BIND,      "%f"},
  {"station.fa_station",       4+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

#include "db_stats_calls.h"
COLUMN_DEFS stats_calls_columns_[NUM_STATS_CALLS_COLS+1] =
{
  {"stats_calls.type",             3+1,     NTBSTRINGBIND, "%s"},
  {"stats_calls.condition",        3+1,     NTBSTRINGBIND, "%s"},
  {"stats_calls.sat",              2+1,     NTBSTRINGBIND, "%s"},
  {"stats_calls.rev",              4,       INTBIND,       "%d"},
  {"stats_calls.dtkid",            1,       TINYBIND,      "%d"},
  {"stats_calls.strttime",        21+1,     NTBSTRINGBIND, "%s"},
  {"stats_calls.stoptime",        21+1,     NTBSTRINGBIND, "%s"},
  {"stats_calls.antenna_id",       2,       SMALLBIND,     "%hd"},
  {"stats_calls.dtkstat",          3+1,     NTBSTRINGBIND, "%s"},
  {"stats_calls.call_time",       21+1,     NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;

DB_TABLES aps_table[] = 
{
  {"activ_conf",	 activ_conf_columns_}, /* 0 */
  {"active_dar_activities",	 active_dar_activities_columns_}, /* 1 */
  {"active_planning_activities",	 active_planning_activities_columns_}, /* 2 */
  {"active_single_activities",	 active_single_activities_columns_}, /* 3 */
  {"activities",	 activities_columns_}, /* 4 */
  {"antenna",	 antenna_columns_}, /* 5 */
  {"antenna_down_times",	 antenna_down_times_columns_}, /* 6 */
  {"antenna_pref",	 antenna_pref_columns_}, /* 7 */
  {"antenna_priority",	 antenna_priority_columns_}, /* 8 */
  {"csa_data",	 csa_data_columns_}, /* 9 */
  {"cvrg",	 cvrg_columns_}, /* 10 */
  {"cvrgmdat",	 cvrgmdat_columns_}, /* 11 */
  {"dar",	 dar_columns_}, /* 12 */
  {"dl2obs",	 dl2obs_columns_}, /* 13 */
  {"dtk",	 dtk_columns_}, /* 14 */
  {"dtkid_lock",	 dtkid_lock_columns_}, /* 15 */
  {"ephemeris",	 ephemeris_columns_}, /* 16 */
  {"framegen_calls",	 framegen_calls_columns_}, /* 17 */
  {"j1_dn_times",	 j1_dn_times_columns_}, /* 18 */
  {"maskinout",	 maskinout_columns_}, /* 19 */
  {"maskinout_mdat",	 maskinout_mdat_columns_}, /* 20 */
  {"multi_user_wait",	 multi_user_wait_columns_}, /* 21 */
  {"permission_counter",	 permission_counter_columns_}, /* 22 */
  {"phase",	 phase_columns_}, /* 23 */
  {"reqq_phase",	 reqq_phase_columns_}, /* 24 */
  {"rgs_down_times",	 rgs_down_times_columns_}, /* 25 */
  {"sat_inclusion_period",	 sat_inclusion_period_columns_}, /* 26 */
  {"sat_rev_dtkid",	 sat_rev_dtkid_columns_}, /* 27 */
  {"satsensor",	 satsensor_columns_}, /* 28 */
  {"schstat",	 schstat_columns_}, /* 29 */
  {"schunavail",	 schunavail_columns_}, /* 30 */
  {"seg",	 seg_columns_}, /* 31 */
  {"site",	 site_columns_}, /* 32 */
  {"sscvrg",	 sscvrg_columns_}, /* 33 */
  {"station",	 station_columns_}, /* 34 */
  {"stats_calls",	 stats_calls_columns_}, /* 35 */
  {NULL,         NULL}   /* End of Table  */
} ;

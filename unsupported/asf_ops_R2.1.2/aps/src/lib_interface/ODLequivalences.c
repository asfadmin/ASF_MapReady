#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		ODLequivalences.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)ODLequivalences.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.ODLequivalences.c"

/*==============================================================================
Function:       EQUIVALENCE TABLES.

Description:    the tables are used by the driver to help translate fields.
                see  the VALUE_DEFS below.

Creator:        Lawrence Stevens

Creation Date:  Tue Aug  8 19:17:45 PDT 1995

Notes:      
==============================================================================*/
#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absolute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h>         /* for getopt                           */

#include <sys/types.h>      /* for read                 */
#include <sys/uio.h>        /* for read                 */
#include <sys/unistd.h>     /* for read                 */
#include <timeconv.h>          /* for tc_asf2odl() etc.    */
#include <mu_utilities.h>   /* for permissions          */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */

#include "fa_defs.h"        /* for APS file types:  _KEYWORD_VALUE_DEFS  */
#include "ODL_defs.h"       /* for APS file types:  ODL_FILEDEF  */
#include "dtkm_utilities.h" 

/* FOR DATABASE TABLES        */
#include "db_dtk.h"         /* for dtk table             */
#include "db_antenna.h"         /* for dtk table             */

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

/* for debugging purposes */
#define   PRINT_DIAG 1 
#undef    PRINT_DIAG

/* FOR ODL_CREATOR */
#include "GENconversions.h"  /* for fa_* globals, gen_string2str() etc.   */
#include "ODLconversions.h"
/* EQUIVALENCE TABLES    */
 
/*
--        filetype = {AWOS | MWOS | AREQ}
--                AWOS  = ASF Weekly Operations Schedule
--                MWOS  = McMurdo Weekly Operations Schedule
--                AREQ  = McMurdo Request for Availability
*/

/*    fa_string                 aps_string   */
EQUIV_TABLE   WOS_object[]=
{   {"WOS",                    "AWOS"},
    {"WOS",                    "MWOS"},
    {"AVAILABILITY_REQUEST",   "AREQ"},
    {"DL_TO_DTKS_MAP",         "ADDM"},
    {"DL_TO_DTKS_MAP",         "MDDM"},
    {NULL,  NULL}
};

EQUIV_TABLE   WOS_record[]=
{   {"WOS_record",                    "AWOS"},
    {"WOS_record",                    "MWOS"},
    {"AVAILABILITY_REQUEST_RECORD",   "AREQ"},
    {NULL,  NULL}
};

/* status for the downlink-to-dtk-map file   */
EQUIV_TABLE   ODL_dmap_dtkstat[]=
{   {"PLANNED",   "PLN"},
    {"SCHEDULED", "SCH"},
    {NULL,  NULL}
};


EQUIV_TABLE   WOS_satellite[]=
{   {"E1",      "E1"},
    {"E2",      "E2"},
    {"J1",      "J1"},
    {"A1",      "A1"},
    {"R1",      "R1"},
    {NULL,  NULL}
};

/*
-- The following table supports both the creation of the AWOS and MWOS
-- only downlinks are currently in these files, so there is no 
-- sensor information, only Z.  
*/
EQUIV_TABLE   WOS_sensor[]=
{   {"Z",       DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {"Z",       DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {NULL, NULL}
};

/*
-- The following table supports both the creation of the ADDM and MDDM
-- downlink-to-data-take map files.  
*/
EQUIV_TABLE   ODL_dmap_sensor[]=
{   {"Z",       DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {"Z",       DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {"S",       "S"},       /* any SAR will match this.  */

    {"O",       "OPS"},     /* J1 OPS              */
    {"V",       "VNR"},     /* J1 VNR              */

    {"A",       "AVN"},     /* ADEOS sensor AVNIR  (not in sensor relation)  */
    {"C",       "OCT"},     /* ADEOS sensor OCTS   */
    {"E",       "EDA"},     /* ADEOS sensor TEDA   */
    {"I",       "IMG"},     /* ADEOS sensor IMG    */
    {"L",       "LAS"},     /* ADEOS sensor ILAS   */
    {"N",       "NSC"},     /* ADEOS sensor NSCAT  */
    {"P",       "POL"},     /* ADEOS sensor POLDER */
    {"T",       "TOM"},     /* ADEOS sensor TOMS   */

    {NULL, NULL}
};

EQUIV_TABLE   WOS_mode[]=
{   { "STD",    DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE },
    { "STD",    DTKM_SENSOR_REALTIME_DOWNLINK_CODE },
    { NULL,     NULL }
};

EQUIV_TABLE   ODL_dmap_mode[]=
{   {"STD",    DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, /* downlink  */
    {"STD",    DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, /* downlink  */

    {"STD",    "SAR"},     /* no mode for these fields; STD is used.  */
    {"STD",    "OPS"},     /* J1 sensor only     */
    {"STD",    "VNR"},     /* J1 sensor only     */
    {"STD",    "AVN"},     /* ADEOS sensor AVNIR  (not in sensor relation)  */
    {"STD",    "OCT"},     /* ADEOS sensor OCTS   */
    {"STD",    "EDA"},     /* ADEOS sensor TEDA   */
    {"STD",    "IMG"},     /* ADEOS sensor IMG    */
    {"STD",    "LAS"},     /* ADEOS sensor ILAS   */
    {"STD",    "NSC"},     /* ADEOS sensor NSCAT  */
    {"STD",    "POL"},     /* ADEOS sensor POLDER */
    {"STD",    "TOM"},     /* ADEOS sensor TOMS   */

    {"ST1",    "ST1"},     /* here are 26 Radarsat SAR modes:     */
    {"ST2",    "ST2"},
    {"ST3",    "ST3"},
    {"ST4",    "ST4"},
    {"ST5",    "ST5"},
    {"ST6",    "ST6"},
    {"ST7",    "ST7"},
    {"WD1",    "SW1"},
    {"WD2",    "SW2"},
    {"WD3",    "SW3"},
    {"FN1",    "SF1"},
    {"FN2",    "SF2"},
    {"FN3",    "SF3"},
    {"FN4",    "SF4"},
    {"FN5",    "SF5"},
    {"EL1",    "SL1"},
    {"EH1",    "SH1"},
    {"EH2",    "SH2"},
    {"EH3",    "SH3"},
    {"EH4",    "SH4"},
    {"EH5",    "SH5"},
    {"EH6",    "SH6"},
    {"SWA",    "SWA"},
    {"SWB",    "SWB"},
    {"SNA",    "SNA"},
    {"SNB",    "SNB"},
    {NULL,     NULL}
};

/*
--  There are 3 types of data-take activities:  
--
--  type 1.  real-time sensing 
--  type 2.  real-time downlink 
--  type 3.  recording:  satellite senses and records data on onboard tape.
--  type 4.  tape dump:  downlink of a tape playback from an earlier recording.
--
-- NOTE:  there are NO recording operations in 
-- the WOS, only real-time and dump.
--
-- NOTE:  when frame generator is called with a data-take, only 
-- sensing activities are reported:  real-time observations or recording 
-- activities.  The external field values are to be RTL if the observation 
-- it a real time, and DMP if the observation is a recording to be 
-- tape dumped later.  
--
-- The WOS will have these types of activities:  
--  type 2.  real-time downlink 
--  type 4.  tape dump:  downlink of a tape playback from an earlier recording.
--
-- and the frame generator call will report only these types of activities:
-- type 1.  real-time sensing 
-- type 3.  recording:  satellite senses and records data on onboard tape.
--          (recording:) "the sensing data is later played back and 
--          downlinked (DMP) in a later data-take, whose information is 
--          not currently provided."  
--
-- There are 2 tables:  WOS_activity[] and 
-- Framegen_activity[] to translate values for one field in the 
-- data dictionary:  ACTIVITY_ID
--
--
--    ACTIVITY_ID   dtk.actid
*/
EQUIV_TABLE   WOS_activity[]=
/*   external       APS internal value.  */
{   {"RLT",         DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {"DMP",         DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {NULL, NULL}
};

EQUIV_TABLE   Framegen_activity[]=
/*   external       APS internal value.  */
{   {"RLT",         DTKM_ACTID_REALTIME_OBSERVATION_CODE},   
    {"DMP",         DTKM_ACTID_RECORDING_OBSERVATION_CODE}, 
    {NULL, NULL}
};


/*
-- NOTE:  there are no record operations in 
-- the WOS, only real-time and dump.
-- Also, we just want to look at characters 4-6, and 
-- so we need to work around once again the problem 
-- created by concatenating the activity value field with the 
-- agency value (ASF/NSF/CSA/etc.) field.  
-- Note that the SENSOR codes are good for the DTK_ACTID field as well.  
-- IMPORTANT:
-- A syntax like this:  "STR" "ING", in this context, 
-- is identical to a syntax like this:  "STRING".  
-- If there is no comma (,), the strings are concatenated.  
*/
EQUIV_TABLE   WOS_agency[]=
{   {"ASF",         DTKM_SENSOR_REALTIME_DOWNLINK_CODE "ASF"},   
    {"ESA",         DTKM_SENSOR_REALTIME_DOWNLINK_CODE "ESA"},
    {"ESA-ASF",     DTKM_SENSOR_REALTIME_DOWNLINK_CODE "ESF"},
    {"CSA",         DTKM_SENSOR_REALTIME_DOWNLINK_CODE "CSA"},
    {"CSA-ASF",     DTKM_SENSOR_REALTIME_DOWNLINK_CODE "CEF"},
    {"NASDA",       DTKM_SENSOR_REALTIME_DOWNLINK_CODE "NAS"},
    {"NASDA-ASF",   DTKM_SENSOR_REALTIME_DOWNLINK_CODE "NSF"},

    {"ASF",         DTKM_ACTID_REALTIME_OBSERVATION_CODE "ASF"},
    {"ESA",         DTKM_ACTID_REALTIME_OBSERVATION_CODE "ESA"},
    {"ESA-ASF",     DTKM_ACTID_REALTIME_OBSERVATION_CODE "ESF"},
    {"CSA",         DTKM_ACTID_REALTIME_OBSERVATION_CODE "CSA"},
    {"CSA-ASF",     DTKM_ACTID_REALTIME_OBSERVATION_CODE "CEF"},
    {"NASDA",       DTKM_ACTID_REALTIME_OBSERVATION_CODE "NAS"},
    {"NASDA-ASF",   DTKM_ACTID_REALTIME_OBSERVATION_CODE "NSF"},

    {"ASF",         DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "ASF"},
    {"ESA",         DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "ESA"},
    {"ESA-ASF",     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "ESF"},
    {"CSA",         DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "CSA"},
    {"CSA-ASF",     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "CEF"},
    {"NASDA",       DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "NAS"},
    {"NASDA-ASF",   DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "NSF"},

    {"ASF",         DTKM_ACTID_RECORDING_OBSERVATION_CODE "ASF"},
    {"ESA",         DTKM_ACTID_RECORDING_OBSERVATION_CODE "ESA"},
    {"ESA-ASF",     DTKM_ACTID_RECORDING_OBSERVATION_CODE "ESF"},
    {"CSA",         DTKM_ACTID_RECORDING_OBSERVATION_CODE "CSA"},
    {"CSA-ASF",     DTKM_ACTID_RECORDING_OBSERVATION_CODE "CEF"},
    {"NASDA",       DTKM_ACTID_RECORDING_OBSERVATION_CODE "NAS"},
    {"NASDA-ASF",   DTKM_ACTID_RECORDING_OBSERVATION_CODE "NSF"},

    {NULL, NULL}
};

EQUIV_TABLE   WOS_transid[]=
{   {"ERS-1_8140",          "00"},  /* must change to ERS-2_8140 if sat = E2 */
    {"JERS-1_8150",         "F1"},
    {"JERS-1_8350",         "F2"},
    {"RADARSAT-1_8105",     "F3"},
    {"RADARSAT-1_8230",     "F4"},
    {"ADEOS-1_8150",        "F5"},
    {"ADEOS-1_8350",        "F6"},
    {"ADEOS-1_8250",        "F7"},
    {NULL, NULL}
};

EQUIV_TABLE   WOS_quicklook[]=
{   {"YES",     "Y"},
    {"NO",      "N"},
    {NULL, NULL}
};

EQUIV_TABLE   WOS_msg_type[]=
{   {"AVAILABILITY_REQUEST",        "AREQ"},
    {"WOS",                         "AWOS"},
    {"WOS",                         "MWOS"},
    {"DL_TO_DTKS_MAP",              "ADDM"},
    {"DL_TO_DTKS_MAP",              "MDDM"},
    {NULL, NULL}
};

EQUIV_TABLE   ODL_dmap_station_id[]=
{                                   /* translate DTK_STATION_ID -> ODL value */
    {"FA",                          "ASF" },
    {"MC",                          "MCM" },
    {NULL, NULL }
};


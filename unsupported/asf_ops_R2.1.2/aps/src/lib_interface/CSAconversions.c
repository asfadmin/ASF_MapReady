#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   CSAconversions.c

Description:CSA conversion routines, which are used to decode/translate
            CSA File string values.

External Functions Defined:
    int CSAc_time2asftime   convert a CSA time value string to asf time format
    int CSAc_append_station use facility id to complete global fa_file_type
    int CSAc_get_activity   translate global fa_file_type to get activity type
    int CSAc_aps_report     translate a CSA string value to its ASF equivalent
                            and then into its APS report equivalent
    int CSAc_default_sensor returns the CSA default sensor. Requires no input.
    int CSAc_default_darid  returns the CSA default darid. Requires no input.

File Scope Functions:
    
External Variables Defined:
    EQUIV_TABLE   CSA_transmission_id[]     contains tranmission id values
    EQUIV_TABLE   CSA_activity_id[]         contains activity id values
    EQUIV_TABLE   CSA_facility_id[]         contains facility id values
    EQUIV_TABLE   CSA_sat_id[]          contains satellite id values
    EQUIV_TABLE   CSA_dtk_status[]      contains datatake status values
    EQUIV_TABLE   CSA_write_recording_flags[]=
    EQUIV_TABLE   CSA_dtk_filetype[]= 
    EQUIV_TABLE   CSA_activity_type[]= 
    EQUIV_TABLE   CSA_mu_station[]= 
    EQUIV_TABLE   CSA_facility_to_suffix[]= 
    EQUIV_TABLE   CSA_sensor[]= 
    EQUIV_TABLE   CSA_FLG_to_suffix

File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)CSAconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.CSAconversions.c"

#include <string.h>         /* for strcpy(), strlen()         */
#include <timeconv.h>       /* for tc_csa2asf()               */
#include "CSAconversions.h"
#include "GENconversions.h"

/* for dtk relation   */
#include "db_dtk.h"
 
/* for APS basic definitions */   
#include "dapps_defs.h"    

/* for DTKM definitions, codes */ 
#include "dtkm_utilities.h"    

/* for permissions */
#include <mu_utilities.h>

/* for debugging purposes  */
#define PRINT_DIAG 1
#undef  PRINT_DIAG

EQUIV_TABLE   CSA_write_recording_flags[]=
{
    {"PLAYBACK", "RITE"}, /* if a playback, also write recording */
    {"REALTIME", "SKIP"}, /* if real time, no recording.      */
    {NULL, NULL}
};
 
EQUIV_TABLE   CSA_dtk_status[]= 
{   {"RECEPTION_REQUEST",   "PLN"},     
    {"RECEPTION_SCHEDULE",  "SCH"}, 
    {NULL, NULL}
};

/*
-- This value is completed by CSAc_append_station.
*/
EQUIV_TABLE   CSA_dtk_filetype[]= 
{   {"RECEPTION_REQUEST",       "CRR_"},    
    {"RECEPTION_SCHEDULE",      "CRS_"}, 
    {NULL, NULL}
};

EQUIV_TABLE   CSA_activity_type[]= 
{   {"CRRA",        MU_CRRA},   
    {"CRRM",        MU_CRRM}, 
    {"CRSA",        MU_CRSA}, 
    {"CRSM",        MU_CRSM}, 
    {NULL, NULL}
};

EQUIV_TABLE   CSA_sat_id[]= 
{   {"RADARSAT_1", "R1"}, 
    {NULL, NULL}
};

EQUIV_TABLE   CSA_facility_id[]= 
{   {"F", "ASF"}, 
    {"M", "MCM"}, 
    {NULL, NULL}
};

EQUIV_TABLE   CSA_mu_station[]= 
{   {"F", MU_ASF_STATIONID}, 
    {"M", MU_MCM_STATIONID}, 
    {NULL, NULL}
};

EQUIV_TABLE   CSA_facility_to_suffix[]= 
{   {"F", "A"}, 
    {"M", "M"}, 
    {NULL, NULL}
};

    EQUIV_TABLE   CSA_FLG_to_suffix[]=
{   {"REALTIME", "_1"}, 
    {"PLAYBACK", "_2"}, 
    {NULL, NULL}
};


EQUIV_TABLE   CSA_activity_id[]= 
{   {"REALTIME", DTKM_ACTID_REALTIME_OBSERVATION_CODE}, 
    {"PLAYBACK", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {NULL, NULL}
};

EQUIV_TABLE   CSA_transmission_id[]= 
{   {"REALTIME", "F3"}, 
    {"PLAYBACK", "F4"}, 
    {NULL, NULL}
};
 
EQUIV_TABLE   CSA_sensor[]= 
{   {"REALTIME", "SAR"}, 
    {"PLAYBACK", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {NULL, NULL}
};
 

CSA_VALUEDEF sch_req_file[] =  
{
    /*
    -- Use global variable fa_number_of_records to store number of receptions
    */
    /*  0  */
    {FILE_HEADER, REPORT_CONTROL, 
        "NB_OF_RECEPTIONS",     
        gen_string2int,     NULL,               (int) &fa_number_of_records},

    {FILE_HEADER, REPORT_HEADER,    
        "FILE_CREATION_TIME",   
        CSAc_time2asftime,  NULL,               (int) fa_creation_date},

    /*  2  */
    {FILE_HEADER, REPORT_HEADER,    
        "FILE_TYPE",            
        table_lookupFA2APS, CSA_dtk_filetype,           (int) fa_file_type},

    {FILE_HEADER, REPORT_HEADER,    
        "RECEPTION_FACILITY_ID",            
        CSAc_append_station, CSA_facility_to_suffix,    (int) fa_file_type},

    {FILE_HEADER, REPORT_RECORD,    
        "FILE_TYPE",            
        table_lookupFA2APS, CSA_dtk_status,     DTK_DTKSTAT},

    /*
    -- NOTE: we no longer need AOS_TIME/LOS_TIME. Any antenna-specific
    -- procedures are now handled during conflict analysis of each 
    -- individual datatake proposal, and during creation of the WOS file
    -- which contains the instructions for the antenna.
    --
                < 2 VALUE_DEFS entry has been ommitted here >
    */

    /*  5  */
    {FILE_HEADER, REPORT_RECORD,    
        "SPACECRAFT_IDENTIFIER",    
        table_lookupFA2APS, CSA_sat_id,     DTK_SAT},

    {FILE_HEADER, REPORT_RECORD,    
        "RECEPTION_ORBIT_NUMBER",   
        gen_string2int, NULL,               DTK_REV},

    {FILE_HEADER, REPORT_RECORD,    
        "RECEPTION_FACILITY_ID",    
        table_lookupFA2APS, CSA_facility_id,DTK_STATION_ID},

    {FILE_HEADER, REPORT_HEADER,    
        "RECEPTION_FACILITY_ID",    
        table_lookupFA2APS, CSA_mu_station, (int)fa_station_id},

    /*
    -- DTK_FA_SCHEDULE_LINK = 1234567_1  indicating 'Realtime' downlink
    -- or                   = 1234567_2  indicating 'Tapedump' downlink
    -- where 1234567 is the 7-digit Reception Schedule id 
    -- NOTE:
    --  Since the RECORDING records are derived from the TAPE DUMP ('PLAYBACK')
    --  record, they will also inherit the new DTK_FA_SCHEDULE_LINK field.
    --  Remember that RECORDINGs will only be created from 'PLAYBACK' records.
    --  And the Realtime Downlinks are created with a time bracket that 
    --  includes all of the real time observations for that rev.  
    */
    /*  9 */
    {FILE_HEADER, REPORT_RECORD,    
        "RECEPTION_SCHEDULE_ID",    
        gen_string2str, NULL,               DTK_FA_SCHEDULE_LINK},

    {FILE_RECORD, REPORT_RECORD,    
        "REALTIME_PLAYBACK_FLG",    
        table_lookupFA2APScat, CSA_FLG_to_suffix, DTK_FA_SCHEDULE_LINK},

    /*
    -- The following conversion uses the global variable fa_file_type
    -- and does not require a keyword.  But we use "RECEPTION_SCHEDULE_ID"
    -- for standardization purposes.
    */
    {FILE_HEADER, REPORT_HEADER,    
        "RECEPTION_SCHEDULE_ID",                        
        CSAc_get_activity, CSA_activity_type,   (int) fa_activity_type},

    /* 12 */
    {FA_DEFAULT,  REPORT_RECORD, 
        NULL,                       
        CSAc_default_darid, NULL,           DTK_DARID},

    /* 13 */
    {FA_DEFAULT,  REPORT_RECORD, 
        NULL,                       
        CSAc_default_quicklook, NULL,       DTK_SCIENCE_QUICKLOOK},

    {FA_DEFAULT,	REPORT_HEADER,
		NULL,	gen_set_trigger,				NULL,
											(int) &fa_trigger_CSA_dl_mapping},
    {FILE_RECORD, REPORT_RECORD,    
        "IMG_DATA_DL_START",        
        CSAc_time2asftime,  NULL,           DTK_STRTTIME},

    {FILE_RECORD, REPORT_RECORD,    
        "IMG_DATA_DL_STOP",         
        CSAc_time2asftime,  NULL,           DTK_STOPTIME},

    /*  16 */
    {FILE_RECORD, REPORT_RECORD,
        "REALTIME_PLAYBACK_FLG",
        table_lookupFA2APS, CSA_activity_id,DTK_ACTID},

    {FILE_RECORD, REPORT_RECORD, 
        "REALTIME_PLAYBACK_FLG",    
        table_lookupFA2APS, CSA_transmission_id, DTK_TRANSID},

    {FILE_RECORD,  REPORT_RECORD, 
        "REALTIME_PLAYBACK_FLG",                        
        table_lookupFA2APS, CSA_sensor,     DTK_SENSOR},

    /*  19 */
    {FILE_RECORD, GLOBAL_VARIABLE,  
        "IMG_DATA_ACQ_START",   
        CSAc_time2asftime,      NULL,       (int)fa_recording_start_time},

    {FILE_RECORD, GLOBAL_VARIABLE,  
        "IMG_DATA_ACQ_STOP",    
        CSAc_time2asftime,      NULL,       (int)fa_recording_stop_time},

    /*  21 */
    {FILE_RECORD, GLOBAL_VARIABLE, 
        "REALTIME_PLAYBACK_FLG",    
        table_lookupFA2APS,     CSA_write_recording_flags, 
                                            (int) fa_record_write_flag },

    {0, 0,  NULL, 0, NULL, 0}
};                                                           


CSA_FILENAME CSA_files[] =
{
    {"CSA", "CRRA", sch_req_file},
    {"CSA", "CRRM", sch_req_file},
    {"CSA", "CRSA", sch_req_file},
    {"CSA", "CRSM", sch_req_file},
    {NULL, NULL, NULL}
} ;


/*==============================================================================
Function:       CSAc_append_station

Description:    Append the station identifier (ie:'A' for ASF, 'M' for McMurdo)
                to the last position of the fa_file_type
Parameters:     

Returns:        
Type          Name              Definition
int                     TRUE = success. Station identifier was appended.
int                     FALSE = error. Station identifier not found.

Creator:        Miguel Siu

Creation Date:  Fri Jan 10 11:49:26 PST 1997

Notes:      
==============================================================================*/
int CSAc_append_station(
    EQUIV_TABLE *facility_to_suffix, 
    char        *facility, 
    char        *file_type )
{
    char file_type_updater[10] ;

    /*
    -- translate the facility identifier into a single character station_id
    */
    if (!table_lookupFA2APS(facility_to_suffix, facility, file_type_updater) )
        return FALSE ;

    /*
    -- Now, copy the update character to the end of the file_type
    -- Here is an example:
    --                         1     2     3     4
    --                        ---   ---   ---   ---
    --          Facility    :  F     F     M     M
    --          Updater char:  A     A     M     M
    --          actid before: CRR_  CRS_  CRR_  CRS_
    --          actid after : CRRA  CRSA  CRRM  CRSM

    */
    strcpy(file_type+3, file_type_updater ) ;
    
    return TRUE ;
}

/*==============================================================================
Function:       CSAc_get_activity

Description:    Translate the file type into the single-activity identifier
                required by the permissions library.
Parameters:     

Returns:        
Type          Name              Definition
int                     TRUE = success. Equivalent string was found.
int                     FALSE = error. An equivalence was not found.

Creator:        Miguel Siu

Creation Date:  Fri Jan 10 11:49:16 PST 1997

Notes:      This routine uses the global variable fa_file_type
==============================================================================*/
int CSAc_get_activity(
    EQUIV_TABLE *CSA_activity_type, 
    char        *unused_station, 
    char        *activity_type )
{
    if (!table_lookupFA2APS(CSA_activity_type, fa_file_type, activity_type) )
        return FALSE ;
    else
        return TRUE ;
}

/*==============================================================================
Function:       CSAc_time2asftime  

Description:    translate input CSA time string into asf time string

Parameters:     
Type        Name                Definition
char        *source_string      CSA time string 
char        *result_string      asf time string

Returns:        
Type          Name              Definition
int                             TRUE = success. Equivalent string was found.
int                             FALSE = error. An equivalence was not found.

Creator:        Miguel Siu

Creation Date:  Fri May 19 14:26:39 PDT 1995

Notes:  This call allows us to access the routine tc_csa2asf() while at the same
        time maintaining the function prototype for CSAc_* conversion routines.
==============================================================================*/
int CSAc_time2asftime(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string )
{

    if ( strlen(source_string ) )
        return ( tc_csa2asf(source_string,result_string) );
    else
    {
        strcpy( result_string, source_string ) ;
        return TRUE ;
    }

}



/*==============================================================================
Function:       CSAc_default_sensor

Description:    returns the CSA default sensor.

Parameters:     NO input required.

Returns:        
Type          Name              Definition
int                             TRUE = success. CSA default sensor returned.

Creator:        Miguel Siu

Creation Date:  Tue Jun 20 16:14:42 PDT 1995

Notes:      
==============================================================================*/
int CSAc_default_sensor(
    void        *unused_pointer, 
    char        *unused_pointr2,
    char        *sensor)
{
    strcpy(sensor,"SAR");
    return(TRUE);
}


/*==============================================================================
Function:       CSAc_default_quicklook
Description:    returns the CSA default quicklook:  'N' for no.
Creator:        L Stevens
Creation Date:  Wed Dec 27 14:15:34 PST 1995 
==============================================================================*/
int CSAc_default_quicklook(
    void        *unused_pointer, 
    char        *unused_pointr2,
    char        *quicklook )
{
    *quicklook = 'N' ;
    return(TRUE);
}


/*==============================================================================
Function:      CSAc_default_darid 

Description:    returns the CSA default darid.

Parameters:     NO input required.

Returns:        
Type          Name              Definition
int                             TRUE = success. CSA default darid returned.

Creator:        Miguel Siu

Creation Date:  Tue Jun 20 16:14:52 PDT 1995

Notes:      
==============================================================================*/
int CSAc_default_darid(
    void        *unused_pointer, 
    char        *unused_pointr2,
    int         *darid)
{
    *darid = 0;
    return(TRUE);
}

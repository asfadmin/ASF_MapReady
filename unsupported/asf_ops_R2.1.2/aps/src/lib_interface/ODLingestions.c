#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   ODLingestions.c

Description:    source file for conversion routines and Conversion tables.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:

File Scope Variables:
    
NOTE:
==============================================================================*/
#pragma ident   "@(#)ODLingestions.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.ODLingestions.c"

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


/*==============================================================================
Function:       WOS_get_antenna_id

Description:    Translate the antenna_id from the WOS format into an
                integer value.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Oct  8 08:02:40 PDT 1996

Notes:      The incoming string is 'ANTENNA_x' where x = number 
==============================================================================*/
int WOS_get_antenna_id(
    void        *unused_pointer,
    char        *string_value,
    short int   *smallint_value)
{
    char    *underscore_loc ;

    underscore_loc = strchr(string_value, '_') ;

    if ( !gen_field_is_numeric( underscore_loc+1, strlen(underscore_loc+1) ) )
        return FALSE ;
    *smallint_value = atoi(underscore_loc+1) ;

    return (TRUE) ;
}

/*==============================================================================
Function:       WOS_default_SCH_status

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Oct  8 10:05:50 PDT 1996

Notes:      
==============================================================================*/
int WOS_default_SCH_status(
    void    *unused_pointer,
    char    *unused_character_pointer,
    char    *insert_here )
{
    strcpy(insert_here, "SCH" ) ;
    return (TRUE) ;
}

/*==============================================================================
Function:       WOS_determine_sensor

Description:    This routine expands the single-character sensor value in the
                WOS file to a 3-character value. But this is not a single
                translation. Here are the rules to follow:

                  1) If the sensor is 'Z', we must translate the ACTIVITY 
                     value from the WOS. This value is currently stored in 
                     global fa_acquisition_mode

                  2) If the sensor is 'S', we must use the MODE value from the
                     WOS. This value is currently stored in 
					 global fa_downlink_mode.

                  3) Otherwise, we can go ahead and do a straight translation
                     of the single-character sensor value.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Apr 10 15:50:23 PDT 1997

Notes:          
    This routine uses global variable fa_acquisition_mode 
==============================================================================*/
int WOS_determine_sensor(
    EQUIV_TABLE *equiv_table,
    char        *sensor_character,
    char        *sensor_string )
{
    if (strncmp(sensor_character, "Z", 1) !=0)
		return (FALSE) ;
	if (!table_lookupFA2APS(WOS_activity, fa_acquisition_mode, sensor_string ))
        return (FALSE) ;
 
    return (TRUE) ;
}

/*==============================================================================
Function:       ODL EQUIVALENCES

Description:    
   The following are used in the ingestion process for ODL files
   (ie: the AVAILABILITY RESPONSE--ARES file)

   This is different than the ODL generation process, which also uses
   instructions, but uses them in a different manner.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Oct  2 11:16:32 PDT 1996

Notes:      
==============================================================================*/
EQUIV_TABLE   WOS_destination[]=
/*  EXTERNAL         APS INTERNAL VALUE  */
{   {"HC",          "ASF"},
    {"WFF",         "MCM"},
    {NULL, NULL}
};

EQUIV_TABLE   WOS_flight_agency[]=
{   {"HC",          "ASF"},
    {"WFF",         "WFF"},
    {NULL, NULL}
};

/*
-- For AWOS and MWOS:
-- The following table exists because there is a many_to-one relationship 
-- between the MSG_TYPE keyword and the file_type. So we are forced to use
-- the DESTINATION keyword to make the file_type determination.
-- The prefered location for the translation would be table ODL_msg_type
*/
EQUIV_TABLE   WOS_file_type[]=
{   {"HC",          "AWOS"},
    {"WFF",         "MWOS"},
    {NULL, NULL}
};

EQUIV_TABLE   WOS_activity_type[]=
{   {"HC",          MU_AWOS},
    {"WFF",         MU_MWOS},
    {NULL, NULL}
};

/*
-- The following is used to ensure that we are operating on a WOS file.
-- This is necessary, since some calling programs use valuedefs
-- generically, without checking the file_type.
*/
EQUIV_TABLE   WOS_certification[]=
{   {"WOS",          "XWOS"},
    {NULL, NULL}
};

/*
EQUIV_TABLE   WOS_expanded_sensor[]= This array no longer needed, 
									 only valid sensor value in WOS is 'Z'
									 Use routine WOS_determine_sensor()
*/

EQUIV_TABLE   RES_transmission_id[]= 
{   {"ERS-1_8140",          "00"},
    {"ERS-2_8140",          "00"},
    {"JERS-1_8150",         "F1"},
    {"JERS-1_8350",         "F2"},
    {"RADARSAT-1_8105",     "F3"},
    {"RADARSAT-1_8230",     "F4"},
    {"ADEOS-1_8150",        "F5"},
    {"ADEOS-1_8350",        "F6"},
    {"ADEOS-1_8250",        "F7"},
    {NULL, NULL}
};

EQUIV_TABLE   RES_status[]=
{   {"ACCEPTED",    "PLN"},
    {"REJECTED",    "REJ"},
    {NULL, NULL}
};

EQUIV_TABLE   RES_station_id[]=     /* McMurdo Groundstation is supported */
{   {"MGS", "MCM"},
    /*
    {"WPS", "XXX"},                   Wallops and Poker Flats not supported
    {"PKR", "XXX"},
    */
    {NULL, NULL}
};

EQUIV_TABLE   RES_facility_id[]=     /* McMurdo Groundstation is supported */
{   {"MGS", MU_MCM_STATIONID},
    {NULL, NULL}
};


EQUIV_TABLE   ODL_msg_type[]=           /* translate MSG_TYPE -> filetype */
{   {"AVAILABILITY_RESPONSE",        "ARES"},
    /*
    {"WOS",                          "AWOS"},  These cases do not work. There
    {"WOS",                          "MWOS"},   is a one-to-many ambiguity.
                                                Use WOS_file_type table.
    */
    {NULL, NULL}
};

EQUIV_TABLE   ODL_activity_type[]=
{                                   /* translate MSG_TYPE -> fa_activity_type */
    {"AVAILABILITY_RESPONSE",        MU_ARES},
    {NULL, NULL}
};

    /* external value                APS INTERNAL value.  */

/*==============================================================================
Function:      ODL_VALUEDEFS  res_file_valuedefs

Description:   Set of instructions for ingesting the AVAILABILITY RESPONSE
                file (or ARES file)

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Sep 18 15:37:59 PDT 1996

Notes:      
==============================================================================*/
ODL_VALUEDEFS res_file_valuedefs[] = 
{
/*
-- HEADER records
*/
    /*  0 ares */
    {FILE_HEADER, REPORT_HEADER,    
        "MSG_TYPE",         
        table_lookupFA2APS, ODL_msg_type,   (int) fa_file_type},

    {FILE_HEADER, REPORT_HEADER,    
        "MSG_TYPE",         
        table_lookupFA2APS, ODL_activity_type,(int) fa_activity_type},

    {FILE_HEADER, REPORT_CONTROL, 
        "NUMBER_OF_RECORDS",        
        NULL,       NULL,           (int) &fa_number_of_records},

    /*  3 ares */
    {FILE_HEADER, REPORT_CONTROL,
        "TIME",
        NULL,   NULL,                   (int) fa_creation_date},

    {FA_DEFAULT,  REPORT_CONTROL, 
        NULL,                       
        gen_set_trigger,    NULL,   (int) &fa_trigger_RES_rej_downtime},

    {FA_DEFAULT,  REPORT_CONTROL, 
        NULL,                       
        gen_set_trigger,    NULL,   (int)&fa_trigger_RES_fillin_response},

/* 
-- DATA records
*/
    /*  6 ares */
    {FILE_RECORD, REPORT_RECORD,    
        "PLATFORM", 
        NULL,               NULL,           DTK_SAT},

    /*
    -- We no longer decode a value for DTK_SENSOR. We will allow the routine
    -- RES_fillin_response to fill it in.  For more info, see notes attached
    -- to table RES_sensor.
            < a ODL_VALUEDEFS definition is omitted here >          
    */

    {FILE_RECORD, REPORT_RECORD,    
        "REVOLUTION",   
        NULL,               NULL,           DTK_REV},

    /*
    -- Store dtkid in global variable, for later reassignment
    */
    /*  8 ares */
    {FILE_RECORD, REPORT_RECORD,    
        "SEQUENCE", 
        gen_int2tinyint,    NULL,           DTK_DTKID},

    {FILE_RECORD, REPORT_RECORD,    
        "TIME_ON",      
        NULL,               NULL,           DTK_STRTTIME},

    {FILE_RECORD, REPORT_RECORD,    
        "TIME_OFF",         
        NULL,               NULL,           DTK_STOPTIME},

    /* 11 ares */
    {FILE_RECORD, REPORT_RECORD, 
        "TRANSMITTER_ID",   
        table_lookupFA2APS, RES_transmission_id, DTK_TRANSID},

    {FILE_RECORD, REPORT_RECORD,    
        "FA_SCHEDULE_LINK", 
        NULL,               NULL,           DTK_FA_SCHEDULE_LINK},

    {FILE_RECORD, REPORT_RECORD, 
        "STATUS",   
        table_lookupFA2APS, RES_status,     DTK_DTKSTAT},

    /* 14 ares */
    {FILE_RECORD, REPORT_RECORD, 
        "DATA_RECEPTION_LOCATION",  
        table_lookupFA2APS, RES_station_id, DTK_STATION_ID},

    {FILE_RECORD, GLOBAL_VARIABLE, 
        "DATA_RECEPTION_LOCATION",  
        table_lookupFA2APS, RES_facility_id,(int) fa_station_id},

    {0, 0,  NULL, 0, NULL, 0}
};



/*==============================================================================
Function:      ODL_FILEDEF res_file 

Description:   Describe object aggregates for ARES (AVAILABILITY RESPONSE) file
                as well as a set of instructions for parsing the file.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Oct  2 08:53:40 PDT 1996

Notes:      
==============================================================================*/
ODL_FILEDEF res_file = 
{   "AVAILABILITY_RESPONSE",            /* file object */
    "COMMON_HEADER",                    /* header object */
    "AVAILABILITY_RESPONSE_RECORD",     /* data record object */
    res_file_valuedefs
};

/*==============================================================================
Function:      ODL_VALUEDEFS  wos_file_valuedefs

Description:   Set of instructions for ingesting the WOS file from McMurdo

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Oct  1 08:30:49 PDT 1996

Notes:      
==============================================================================*/
ODL_VALUEDEFS wos_file_valuedefs[] = 
{
/*
-- HEADER records
*/
    /*
    -- The following ensures that we are operating on a WOS file.
    -- This is necessary, since some calling programs use these valuedefs
    -- generically, without checking the file_type.
    */
/*  0 incoming WOS */
    {FILE_HEADER, REPORT_CONTROL,
        "MSG_TYPE",
        table_lookupFA2APS,         WOS_certification,(int) fa_file_type},

/*  1 incoming WOS */
    {FILE_HEADER, REPORT_CONTROL,
        "TIME",
        NULL,                       NULL,           (int) fa_creation_date},

    /*
    -- The Wallops Flight Facility is considered a flight agency, because
    -- it generates files that we must ingest.
    -- We also consider Alaska Sar Facility a flight agency only for
    -- consistency, and because there are functions which consider it so.
    */
/*  2 incoming WOS */
    {FILE_HEADER, REPORT_CONTROL,
        "DESTINATION",
        table_lookupFA2APS,         WOS_flight_agency,(int) fa_flight_agency},

    /*
    -- We now use  DESTINATION keyword to get the type of file we are ingesting.
    -- AWOS, MWOS files have DESTINATION  'HC' and 'WFF' respectively.
    -- We do not use the MSG_TYPE keyword because it is ambiguous.
    -- (MSG_TYPE 'WOS' could refer to AWOS or MWOS)
    */
/*  3 incoming WOS */
    {FILE_HEADER, REPORT_CONTROL,
        "DESTINATION",
        table_lookupFA2APS,         WOS_file_type,  (int) fa_file_type},

/*  4 incoming WOS */
    {FILE_HEADER, REPORT_CONTROL,
        "DESTINATION",
        table_lookupFA2APS,     WOS_activity_type,  (int) fa_activity_type},

/*  5 incoming WOS */
    {FA_DEFAULT,  REPORT_RECORD, 
        NULL,                       
        WOS_default_SCH_status,     NULL,           DTK_DTKSTAT},

/*  6 incoming WOS */
    {FILE_HEADER, REPORT_RECORD,   
        "DESTINATION",  table_lookupFA2APS, WOS_destination,
                                                    DTK_STATION_ID },
/*  7 incoming WOS */
    {FILE_HEADER, REPORT_CONTROL,   
        "DESTINATION",  table_lookupFA2APS, WOS_destination,
                                                    (int) fa_station_id },
/*  8 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,   
        "PLATFORM",     NULL,               NULL,           DTK_SAT},

/*  9 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "ACTIVITY_ID",  table_lookupFA2APS, WOS_activity,   DTK_ACTID},

/* 10 incoming WOS */
/*  global fa_acquisition_mode may be needed by WOS_determine_sensor() routine */
    {FILE_RECORD, GLOBAL_VARIABLE, 
        "ACTIVITY_ID",  NULL,               NULL, (int) fa_acquisition_mode},

/*  global fa_downlink_mode may be needed by WOS_determine_sensor() routine */
/* 11 incoming WOS */
    {FILE_RECORD, GLOBAL_VARIABLE, 
        "MODE",  NULL,  NULL,                       (int) fa_downlink_mode},


/* 12 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,   
        "SENSOR",    WOS_determine_sensor,  NULL,			DTK_SENSOR},
/* 13 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,   
        "REVOLUTION",   NULL,               NULL,           DTK_REV},

/* 14 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "SEQUENCE",     gen_int2tinyint,    NULL,           DTK_DTKID},

/* 15 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "TIME_ON",  NULL,                   NULL,       DTK_STRTTIME},

/* 16 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "TIME_OFF", NULL,                   NULL,       DTK_STOPTIME},

/* 17 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "TRANSMITTER_ID", table_lookupFA2APS,
                                    RES_transmission_id,DTK_TRANSID},
/* 18 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "ANTENNA_ID",
                    WOS_get_antenna_id,     NULL,       DTK_ANTENNA_ID},
/* 19 incoming WOS */
    {FILE_RECORD, REPORT_RECORD,
        "FA_SCHEDULE_LINK",         NULL,   NULL,       DTK_FA_SCHEDULE_LINK},
 
    {0, 0,  NULL, 0, NULL, 0}
};



/*==============================================================================
Function:      ODL_FILEDEF wos_file 

Description:   Describe object aggregates for WOS file
                as well as a set of instructions for parsing the file.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Oct  2 08:53:40 PDT 1996

Notes:      
==============================================================================*/
ODL_FILEDEF wos_file = 
{   "WOS",                          /* file object */
    "COMMON_HEADER",                /* header object */
    "WOS_RECORD",                   /* data record object */
    wos_file_valuedefs
};

/*==============================================================================
Function:       ODL_FILENAME  ODL_files

Description:    These are all the files that use ODL format
                (not to be confused with the old CSA OLD format)    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Oct 10 10:33:36 PDT 1996

Notes:      
==============================================================================*/
ODL_FILENAME ODL_files[] =
{
    {"WFF", "ARES", &res_file}, /* currently used for ARES ingestion */
    {"WFF", "MWOS", &wos_file}, /* currently used for WOS comparison */
    {"ASF", "AWOS", &wos_file}, /* currently used for WOS comparison */
    {NULL, NULL, NULL}
} ;

ODL_FILENAME ARES_file = {"WFF", "ARES", &res_file} ;
                                /* currently used for ARES ingestion */


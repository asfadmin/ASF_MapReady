#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       IMS_interface.c

Description:    Handles IMS interface.  

External Functions Defined:
    int IMS_interface()
    
File Scope Functions:
    static int write_AOS_PMF()
    static int write_DLK_PMF()
    static int write_MSK_PMF()
    static int write_SAR_PMF()
    static int write_PMF()
    static void print_pmf_values()
    static void print_ims_info()

    
External Variables Defined:
    
File Scope Variables:
==============================================================================*/
#pragma ident   "@(#)IMS_interface.c	1.12 98/03/25 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.IMS_interface.c"

#include <stdlib.h>          /* for malloc() and free() etc.             */
#include <aps_log_msg.h>     /* for APS_CRITICAL, aps_log_msg() etc.     */
#include "aps_Statistics.h"  /* for APS_STATS_IMS_INFO, tc_asf2odl()     */

extern char msg[];           /* defined in main.c                        */


/*==============================================================================
Function:       print_odl_time()

Description:    If the string value is not "", then the input 
                is used as an ASF time and converted to ODL time.  
                Then it is printed with the input keyword.  

                This routine was created to shrink the code; there 
                were a lot of repeated lines of code.  In addition, 
                the asftime variable had to be printed twice in the 
                old code, once in the strcmp() call and once in the 
                tc_asf2odl() call.  One bug had occurred by mistyping the 
                2nd occurrence; three more bugs were found when 
                I re-coded using this function call.  

Creator:        Lawrence Stevens

Creation Date:  Tue Feb 10 20:17:55 PST 1998

==============================================================================*/
static int print_PMF_odl_time(
    FILE    *fp,           /* file for printing.            */
    char    *keyword,      /* input PMF keyword to use      */
    char    *asftime  )    /* input ASF time                */
{
    char    odl_time[ASF_TIME_STR_LENGTH+1] ;

    if( strcmp(asftime, "" ) != 0 )
    {
        if( tc_asf2odl( asftime, odl_time ) != TRUE )
            return STATS_ERROR_CONVERTING_ASFTIME_TO_ODL_TIME ;
        (void) fprintf( fp, "%s = %s\n", keyword, odl_time ) ;
    }

    return TRUE ;
}

/*==============================================================================
Function:       write_AOS_PMF()

Description:    write PMF describing AOS-LOS reporting.  

Creator:        Lawrence Stevens

Creation Date:  Mon Feb  9 15:47:48 PST 1998

Notes:          
==============================================================================*/
static int 
write_AOS_PMF( 
    FILE                 *fp, 
    APS_STATS_PMF_VALUES *pmf_values ) 
{

    int     return_code ;

    (void) fprintf( fp, "OBJECT = AOS_LOS_METADATA\n" ) ;

    (void) fprintf( fp, "OBJECT = COMMON_HEADER\n" ) ;

    return_code = print_PMF_odl_time( fp, "TIME", pmf_values->asftime_now ) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "MSG_TYPE = 'STATISTICS_METADATA'\n" ) ;

    (void) fprintf( fp, "NUMBER_OF_RECORDS = 1\n" ) ;

    (void) fprintf( fp, "SOURCE = 'APS'\n" ) ;

    (void) fprintf( fp, "DESTINATION = 'IMS'\n" ) ;

    (void) fprintf( fp, "END_OBJECT = COMMON_HEADER\n" ) ;

    (void) fprintf( fp, "OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "PLATFORM = '%s'\n", pmf_values->sat ) ;

    (void) fprintf( fp, "REVOLUTION = %d\n", pmf_values->rev ) ;

    (void) fprintf( fp, "ANTENNA_ID = 'ANTENNA_%d'\n", 
        pmf_values->antenna_id ) ;

    if( strcmp(pmf_values->condition, "CAN") == 0 )
    {
        (void) fprintf( fp, "STATS_STATUS = 'CANCELLED'\n" ) ;
        /* 
        -- a cancelled AOS-LOS should 
        -- report only FA times.  
        */
        if( strcmp(pmf_values->asftime_aos_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_los_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_aos, "" )    != 0  
        ||  strcmp(pmf_values->asftime_los, "" )    != 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_AOS_LOS ;
    }
    else if (strcmp(pmf_values->condition, "RED") == 0 )
    {
        (void) fprintf( fp, "STATS_STATUS = 'REDUCED'\n" ) ;
        /* 
        -- a reduced AOS-LOS should 
        -- report both FA and regular times:  
        */
        if( strcmp(pmf_values->asftime_aos_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_los_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_aos, "" )    == 0  
        ||  strcmp(pmf_values->asftime_los, "" )    == 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_REDUCED_AOS_LOS ;
    }
    else if (strcmp(pmf_values->condition, "SCH") == 0 )
    {
        (void) fprintf( fp, "STATS_STATUS = 'SCHEDULED'\n" ) ;
        /* 
        -- a scheduled AOS-LOS should 
        -- report only regular times:  
        */
        if( strcmp(pmf_values->asftime_aos_fa, "" ) != 0 
        ||  strcmp(pmf_values->asftime_los_fa, "" ) != 0 
        ||  strcmp(pmf_values->asftime_aos, "" )    == 0  
        ||  strcmp(pmf_values->asftime_los, "" )    == 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_SCHEDULED_AOS_LOS ;
    }
    else
        return STATS_ERROR_IN_CONDITION_PMF_VALUE ;

    return_code = print_PMF_odl_time( fp, "TIME_AOS_FA", 
        pmf_values->asftime_aos_fa ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_LOS_FA", 
        pmf_values->asftime_los_fa ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_AOS", pmf_values->asftime_aos) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_LOS", pmf_values->asftime_los) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "END_OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "END_OBJECT = AOS_LOS_METADATA\n" ) ;

    (void) fprintf( fp, "END\n" ) ;

    return TRUE ;

}



/*==============================================================================
Function:       write_DLK_PMF()

Description:    write PMF describing Downlink reporting.  

Creator:        Lawrence Stevens

Creation Date:  Mon Feb  9 15:47:48 PST 1998

Notes:          
==============================================================================*/
static int 
write_DLK_PMF( 
    FILE                 *fp, 
    APS_STATS_PMF_VALUES *pmf_values ) 
{

    int     return_code ;

    (void) fprintf( fp, "OBJECT = DOWNLINK_METADATA\n" ) ;

    (void) fprintf( fp, "OBJECT = COMMON_HEADER\n" ) ;

    return_code = print_PMF_odl_time( fp, "TIME", pmf_values->asftime_now ) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "MSG_TYPE = 'STATISTICS_METADATA'\n" ) ;

    (void) fprintf( fp, "NUMBER_OF_RECORDS = 1\n" ) ;

    (void) fprintf( fp, "SOURCE = 'APS'\n" ) ;

    (void) fprintf( fp, "DESTINATION = 'IMS'\n" ) ;

    (void) fprintf( fp, "END_OBJECT = COMMON_HEADER\n" ) ;

    (void) fprintf( fp, "OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "PLATFORM = '%s'\n", pmf_values->sat ) ;

    (void) fprintf( fp, "REVOLUTION = %d\n", pmf_values->rev ) ;

    (void) fprintf( fp, "SEQUENCE = %d\n", pmf_values->dtkid ) ;

    (void) fprintf( fp, "ANTENNA_ID = 'ANTENNA_%d'\n", 
        pmf_values->antenna_id ) ;

    if(strcmp(pmf_values->activity_id, DTKM_SENSOR_REALTIME_DOWNLINK_CODE) == 0)
    {
        (void) fprintf( fp, "ACTIVITY_ID = 'RLT'\n" ) ;
    }
    else if( strcmp( pmf_values->activity_id,
                     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0 )
    {
        (void) fprintf( fp, "ACTIVITY_ID = 'DMP'\n" ) ;
    }
    else
    {
        return STATS_ERROR_IN_PMF_DOWNLINK_ACTIVITY_ID ;
    }


    if( strcmp(pmf_values->condition, "CAN") == 0 )
    {
        (void) fprintf( fp, "STATS_STATUS = 'CANCELLED'\n" ) ;
        /* 
        -- a cancelled downlink should 
        -- report only FA times:  
        */
        if( strcmp(pmf_values->asftime_on_fa, "" )  == 0 
        ||  strcmp(pmf_values->asftime_off_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_on, "" )     != 0  
        ||  strcmp(pmf_values->asftime_off, "" )    != 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_DOWNLINK ;
    }
    else if (strcmp(pmf_values->condition, "RED") == 0 )
    {
        (void) fprintf( fp, "STATS_STATUS = 'REDUCED'\n" ) ;
        /* 
        -- a reduced downlink should 
        -- report both FA and regular times:  
        */
        if( strcmp(pmf_values->asftime_on_fa, "" )  == 0 
        ||  strcmp(pmf_values->asftime_off_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_on, "" )     == 0  
        ||  strcmp(pmf_values->asftime_off, "" )    == 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_REDUCED_DOWNLINK ;
    }
    else
        return STATS_ERROR_IN_CONDITION_PMF_VALUE ;

    return_code = print_PMF_odl_time( fp, "TIME_ON_FA", 
        pmf_values->asftime_on_fa ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_OFF_FA", 
        pmf_values->asftime_off_fa ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_ON", 
        pmf_values->asftime_on ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_OFF", 
        pmf_values->asftime_off ) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "END_OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "END_OBJECT = DOWNLINK_METADATA\n" ) ;

    (void) fprintf( fp, "END\n" ) ;

    return TRUE ;

}



/*==============================================================================
Function:       write_MSK_PMF()

Description:    write PMF describing Mask entry and exit reporting.  

Creator:        Lawrence Stevens

Creation Date:  Mon Feb  9 15:47:48 PST 1998

==============================================================================*/
static int 
write_MSK_PMF( 
    FILE                 *fp, 
    APS_STATS_PMF_VALUES *pmf_values ) 
{

    int     return_code ;

    (void) fprintf( fp, "OBJECT = MASK_METADATA\n" ) ;

    (void) fprintf( fp, "OBJECT = COMMON_HEADER\n" ) ;

    return_code = print_PMF_odl_time( fp, "TIME", 
        pmf_values->asftime_now ) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "MSG_TYPE = 'STATISTICS_METADATA'\n" ) ;

    (void) fprintf( fp, "NUMBER_OF_RECORDS = 1\n" ) ;

    (void) fprintf( fp, "SOURCE = 'APS'\n" ) ;

    (void) fprintf( fp, "DESTINATION = 'IMS'\n" ) ;

    (void) fprintf( fp, "END_OBJECT = COMMON_HEADER\n" ) ;

    (void) fprintf( fp, "OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "PLATFORM = '%s'\n", pmf_values->sat ) ;

    (void) fprintf( fp, "REVOLUTION = %d\n", pmf_values->rev ) ;

    (void) fprintf( fp, "ANTENNA_ID = 'ANTENNA_%d'\n", 
        pmf_values->antenna_id ) ;

    if( strcmp(pmf_values->condition, "CAN") == 0 )
        (void) fprintf( fp, "STATS_STATUS = 'CANCELLED'\n" ) ;
    else if (strcmp(pmf_values->condition, "SCH") == 0 )
        (void) fprintf( fp, "STATS_STATUS = 'SCHEDULED'\n" ) ;
    else
        return STATS_ERROR_IN_CONDITION_PMF_VALUE ;

    return_code = print_PMF_odl_time( fp, "TIME_MASK_ENTRY", 
        pmf_values->asftime_mask_entry ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_MASK_EXIT", 
        pmf_values->asftime_mask_exit ) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "END_OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "END_OBJECT = MASK_METADATA\n" ) ;

    (void) fprintf( fp, "END\n" ) ;

    return TRUE ;

}


/*==============================================================================
Function:       write_SAR_PMF()

Description:    write PMF describing SAR activity reporting.  

Creator:        Lawrence Stevens

Creation Date:  Mon Feb  9 15:47:48 PST 1998

Notes:          
==============================================================================*/
/* for interface info, functions:   */
#include <GENconversions.h>    /* for table_lookupAPS2FA()              */
#include <ODLconversions.h>    /* for ODL_dmap_mode[]                   */
static int 
write_SAR_PMF( 
    FILE                 *fp, 
    APS_STATS_PMF_VALUES *pmf_values ) 
{

    char    pmf_sensor_mode[10] ;
    int     return_code ;

    (void) fprintf( fp, "OBJECT = SAR_METADATA\n" ) ;

    (void) fprintf( fp, "OBJECT = COMMON_HEADER\n" ) ;

    return_code = print_PMF_odl_time( fp, "TIME", pmf_values->asftime_now ) ;
    if( return_code < 0 )
        return return_code ;

    (void) fprintf( fp, "MSG_TYPE = 'STATISTICS_METADATA'\n" ) ;

    (void) fprintf( fp, "NUMBER_OF_RECORDS = 1\n" ) ;

    (void) fprintf( fp, "SOURCE = 'APS'\n" ) ;

    (void) fprintf( fp, "DESTINATION = 'IMS'\n" ) ;

    (void) fprintf( fp, "END_OBJECT = COMMON_HEADER\n" ) ;

    (void) fprintf( fp, "OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "PLATFORM = '%s'\n", pmf_values->sat ) ;

    (void) fprintf( fp, "REVOLUTION = %d\n", pmf_values->rev ) ;

    (void) fprintf( fp, "SEQUENCE = %d\n", pmf_values->dtkid ) ;

    (void) fprintf( fp, "SENSOR = 'S'\n" ) ;

    /*
    -- Translate Sensor value to sensor mode for the mode field.
    -- Use EQUIV_TABLE   ODL_dmap_mode[]
    -- from ODLequivalences.c in lib_interface.
    -- Use table_lookupAPS2FA() from GENconversions.c in lib_interface.
    --
    -- the following call translates the input SENSOR to
    -- the output sensor mode.
    -- the include files are:   
    --     <GENconversions.h> for table_lookupAPS2FA()
    --     <ODLconversions.h> for ODL_dmap_mode[] 
    */
    return_code = table_lookupAPS2FA( ODL_dmap_mode,
        pmf_values->sensor, 
        pmf_sensor_mode ) ;
    if( return_code != TRUE )
        return STATS_ERROR_TRANSLATING_SENSOR_TO_SENSOR_MODE ;

    (void) fprintf( fp, "MODE = '%s'\n", pmf_sensor_mode ) ;

    /* 
    -- check the condition input vs times provided:  
    */
    if( strcmp(pmf_values->condition, "CAN") == 0 
    ||  strcmp(pmf_values->condition, "RED") == 0 )
    {
        /* 
        -- downlink was cancelled or reduced, and we 
        -- are printing one of its SAR activities.  
        -- print DOWNLINK_STATUS later.  NOT here.  
        -- 
        -- a cancelled or reduced SAR should 
        -- report only FA times:  
        */
        if( strcmp(pmf_values->asftime_on_fa, "" )  == 0 
        ||  strcmp(pmf_values->asftime_off_fa, "" ) == 0 
        ||  strcmp(pmf_values->asftime_on, "" )     != 0  
        ||  strcmp(pmf_values->asftime_off, "" )    != 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_SAR ;
    }
    else if (strcmp(pmf_values->condition, "PLN") == 0 )
    {
        /* 
        -- print status here, for a planned SAR.  
        */
        (void) fprintf( fp, "STATS_STATUS = 'PLANNED'\n" ) ;

        /* 
        -- a planned SAR activity should 
        -- report only regular times  
        -- and time planned:
        */
        if( strcmp(pmf_values->asftime_on_fa, "" )   != 0 
        ||  strcmp(pmf_values->asftime_off_fa, "" )  != 0 
        ||  strcmp(pmf_values->asftime_on, "" )      == 0  
        ||  strcmp(pmf_values->asftime_off, "" )     == 0 
        ||  strcmp(pmf_values->asftime_planned, "" ) == 0 )
            return STATS_ERROR_IN_PMF_TIMES_IN_PLANNED_SAR ;
    }
    else
        return STATS_ERROR_IN_CONDITION_PMF_VALUE ;

    return_code = print_PMF_odl_time( fp, "TIME_ON_FA", 
        pmf_values->asftime_on_fa ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_OFF_FA", 
        pmf_values->asftime_off_fa ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_ON", 
        pmf_values->asftime_on ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_OFF", 
        pmf_values->asftime_off ) ;
    if( return_code < 0 )
        return return_code ;

    return_code = print_PMF_odl_time( fp, "TIME_PLANNED", 
        pmf_values->asftime_planned ) ;
    if( return_code < 0 )
        return return_code ;

    if( strcmp(pmf_values->condition, "CAN") == 0 
    ||  strcmp(pmf_values->condition, "RED") == 0 )
    {
        /* print the 3 DOWNLINK values:  */
        if( strcmp(pmf_values->condition, "CAN") == 0 )
            (void) fprintf( fp, "DOWNLINK_STATUS = 'CANCELLED'\n" ) ;
        else if (strcmp(pmf_values->condition, "RED") == 0 )
            (void) fprintf( fp, "DOWNLINK_STATUS = 'REDUCED'\n" ) ;

        (void) fprintf( fp, "DOWNLINK_REVOLUTION = %d\n", 
            pmf_values->downlink_rev ) ;

        (void) fprintf( fp, "DOWNLINK_SEQUENCE = %d\n", 
            pmf_values->downlink_dtkid ) ;
    }

    (void) fprintf( fp, "END_OBJECT = CATALOG_METADATA\n" ) ;

    (void) fprintf( fp, "END_OBJECT = SAR_METADATA\n" ) ;

    (void) fprintf( fp, "END\n" ) ;

    return TRUE ;

}


/*==============================================================================
Function:       create_pmf_name()

Description:    create Name of PMF, without path or extension.
Creator:        Lawrence Stevens

Creation Date:  Fri Feb 20 18:22:59 PST 1998

==============================================================================*/
static void 
create_pmf_name( char *pmf_name, APS_STATS_PMF_VALUES* pmf_values )
{

    (void) sprintf( pmf_name, "stats.%s.%s.%s.%d.%d",
        pmf_values->type,
        pmf_values->condition,
        pmf_values->sat,
        pmf_values->rev,
        pmf_values->dtkid ) ;

    return ;

}

/*==============================================================================
Function:       write_PMF()

Description:    write PMF.  PMF = P. Metadata File

Creator:        Lawrence Stevens

Creation Date:  Tue Feb 10 17:53:49 PST 1998

Notes:          
==============================================================================*/
static int 
write_PMF( 
    APS_STATS_IMS_INFO   *ims_info,
    APS_STATS_PMF_VALUES *pmf_values ) 
{
    char    pmf_name[50] ;
    char    full_pmf_filename[256] ;
    int     return_code ;
    FILE    *PMF_fp ;

    /* 
    -- create complete pmf name:  
    */

    /* Name of PMF, without path or extension */
    create_pmf_name( pmf_name, pmf_values ) ;

    (void) sprintf( full_pmf_filename, "%s/%s.M",
        ims_info->sourceDir,     /* directory path for output PMF's.  */
        pmf_name ) ;             /* Name of PMF, without path or extension */

    (void) fprintf( ims_info->logfile_ptr, "PMF filename:  %s\n", 
        full_pmf_filename ) ;

#ifdef DO_NOT_CREATE_FILES
    /* 
    -- Assign PMF_fp to point to the output file:  
    -- write the PMF contents into the 
    -- output file we are already using:   
    */
    (void) fprintf( ims_info->logfile_ptr, 
        "PMF NOT CREATED; CONTENTS LISTED HERE INSTEAD:\n");
    PMF_fp = ims_info->logfile_ptr ;

#else 
    /* 
    -- Assign PMF_fp to point to a new file:
    -- create files to write contents into them 
    */
    /* open the file:   */
    PMF_fp = fopen(full_pmf_filename, "w") ;
    if ( PMF_fp == NULL  )
    {
        (void)sprintf(msg,
"Unable to open PMF file %s; check for directory permissions or full disk.",
            full_pmf_filename ) ;
        aps_log_msg( ims_info->programName, APS_CRITICAL, msg, 
            DO_SYSLOG, DO_PRINT);
        return STATS_ERROR_COULD_NOT_OPEN_PMF_FILE ;
    }

    (void) fprintf(ims_info->logfile_ptr, "PMF CREATED OK.\n" ) ;

#endif /* DO_NOT_CREATE_FILES  */


    /*
    -- write the file contents:  
    -- PMF_fp is set OK.  
    */
    if(      strcmp( pmf_values->type, "AOS" ) == 0 )
    {
        return_code = write_AOS_PMF( PMF_fp, pmf_values ) ;
    }
    else if( strcmp( pmf_values->type, "DLK" ) == 0 )
    {
        return_code = write_DLK_PMF( PMF_fp, pmf_values ) ;
    }
    else if( strcmp( pmf_values->type, "SAR" ) == 0 )
    {
        return_code = write_SAR_PMF( PMF_fp, pmf_values ) ;
    }
    else if( strcmp( pmf_values->type, "MSK" ) == 0 )
    {
        return_code = write_MSK_PMF( PMF_fp, pmf_values ) ;
    }
    else 
    {
        #ifndef DO_NOT_CREATE_FILES
        /* close the file that we just opened:    */
        (void) fclose( PMF_fp ) ;
        #endif  

        return STATS_ERROR_UNKNOWN_PMF_TYPE ;
    }


#ifndef DO_NOT_CREATE_FILES
    /* close the file that we just wrote to:    */
    (void) fclose( PMF_fp ) ;
#endif  

    return return_code ;

}
#ifdef PRINT_DIAG

/*==============================================================================
Function:       print_pmf_values()

Description:    Print values in PMF values structure

Creator:        Lawrence Stevens

Creation Date:  Mon Feb  9 14:00:37 PST 1998

Notes:          
==============================================================================*/
static void 
print_pmf_values(
    FILE                 *fp, 
    APS_STATS_PMF_VALUES *pmf_values ) 
{


    (void) fprintf( fp, 
        "    APS_STATS_PMF_VALUES: \n" ) ;
    (void) fprintf( fp, 
        "        type           = %s\n", pmf_values->type ) ;
    (void) fprintf( fp, 
        "        condition      = %s\n", pmf_values->condition     ) ;
    (void) fprintf( fp, 
        "        sat            = %s\n", pmf_values->sat           ) ;
    (void) fprintf( fp, 
        "        rev            = %d\n", pmf_values->rev           ) ;
    (void) fprintf( fp, 
        "        dtkid          = %d\n", pmf_values->dtkid         ) ;
    (void) fprintf( fp, 
        "        sensor         = %s\n", pmf_values->sensor        ) ;
    (void) fprintf( fp, 
        "        antenna_id     = %d\n", pmf_values->antenna_id    ) ;
    (void) fprintf( fp, 
        "        activity_id    = %s\n", pmf_values->activity_id   ) ;
    (void) fprintf( fp, 
        "        asftime_now    = %s\n", pmf_values->asftime_now   ) ;
    (void) fprintf( fp, 
        "        asftime_aos    = %s\n", pmf_values->asftime_aos   ) ;
    (void) fprintf( fp, 
        "        asftime_los    = %s\n", pmf_values->asftime_los   ) ;
    (void) fprintf( fp, 
        "        asftime_aos_fa = %s\n", pmf_values->asftime_aos_fa) ;
    (void) fprintf( fp, 
        "        asftime_los_fa = %s\n", pmf_values->asftime_los_fa) ;
    (void) fprintf( fp, 
        "        asftime_mask_entry = %s\n", pmf_values->asftime_mask_entry) ;
    (void) fprintf( fp, 
        "        asftime_mask_exit  = %s\n", pmf_values->asftime_mask_exit ) ;
    (void) fprintf( fp, 
        "        asftime_on     = %s\n", pmf_values->asftime_on    ) ;
    (void) fprintf( fp, 
        "        asftime_off    = %s\n", pmf_values->asftime_off   ) ;
    (void) fprintf( fp, 
        "        asftime_on_fa  = %s\n", pmf_values->asftime_on_fa ) ;
    (void) fprintf( fp, 
        "        asftime_off_fa = %s\n", pmf_values->asftime_off_fa) ;
    (void) fprintf( fp, 
        "        asftime_planned= %s\n", pmf_values->asftime_planned) ;
    (void) fprintf( fp, 
        "        downlink_rev   = %d\n", pmf_values->downlink_rev  ) ;
    (void) fprintf( fp, 
        "        downlink_dtkid = %d\n", pmf_values->downlink_dtkid) ;

    return ;

}
#endif /*  PRINT_DIAG   */

#ifdef PRINT_DIAG

/*==============================================================================
Function:       print_ims_info()

Description:    

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 12:18:00 PST 1998

==============================================================================*/
static void
print_ims_info( 
    FILE                 *fp,
    APS_STATS_IMS_INFO   *aps_stats_ims_info ) 
{

    (void) fprintf( fp, 
        "    APS_STATS_IMS_INFO: \n" ) ;
    (void) fprintf( fp, 
        "        username    = %s\n", aps_stats_ims_info->username ) ;
    (void) fprintf( fp, 
        "        password    = %s\n", aps_stats_ims_info->password ) ;
    (void) fprintf( fp, 
        "        accountId   = %s\n", aps_stats_ims_info->accountId) ;
    (void) fprintf( fp, 
        "        sourceDir   = %s\n", aps_stats_ims_info->sourceDir) ;
    (void) fprintf( fp, 
        "        programName = %s\n",aps_stats_ims_info->programName);
    (void) fprintf( fp, 
        "        catSrvName  = %s\n", aps_stats_ims_info->catSrvName);
    (void) fprintf( fp, 
        "        catDbName   = %s\n", aps_stats_ims_info->catDbName );

    return ;
}
#endif /*  PRINT_DIAG   */


/*==============================================================================
Function:       assign_dataset()

Description:    

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Feb 20 18:43:22 PST 1998

Notes:          
==============================================================================*/
static char *assign_dataset( APS_STATS_PMF_VALUES *pmf_values )
{

    if( strcmp(pmf_values->type, "AOS") == 0 )
        return ("APS STATISTICS FOR AOS-LOS TIMES") ;
    if( strcmp(pmf_values->type, "DLK") == 0 )
        return ("APS STATISTICS FOR DOWNLINKS") ;
    if( strcmp(pmf_values->type, "MSK") == 0 )
        return ("APS STATISTICS FOR MASK ENTRY/EXITS") ;
    if( strcmp(pmf_values->type, "SAR") == 0 )
        return ("APS STATISTICS FOR SAR ACTIVITIES") ;
    else
        return NULL ;

}


/*==============================================================================
Function:       fill_ims_event()

Description:    fill IMS structure from APS info
Creator:        Lawrence Stevens
Creation Date:  Fri Feb 20 18:07:35 PST 1998
==============================================================================*/
static void
fill_ims_event(
    APS_STATS_IMS_INFO      *ims_info,      /* input info, for IMS.      */
    APS_STATS_PMF_VALUES    *pmf_values,    /* input info, for IMS.      */
    IMS_CLNT_EVENT          *ims_event )    /* fill this structure.      */
{
    static char     *extensions[] = {"M"} ;
    static char     pmf_name[50] ;

    /*   IMS_CLNT_REQUEST_TYPE requestType;     */
    ims_event->requestType = IMS_REPLACE ;

    /*   char *username;            */
    ims_event->username = ims_info->username ;

    /*   char *password;            */
    ims_event->password = ims_info->password ;

    /*   char *accountId;          */
    ims_event->accountId = ims_info->accountId ;

    /*   char *platform;    */
    ims_event->platform = "APS" ;

    /*   char *sensor;    */
    ims_event->sensor = NULL ;

    /*   char *dataset;    */
    ims_event->dataset = assign_dataset( pmf_values ) ;

    /*   char *name;    Name of PMF, without path or extension.  */
    ims_event->name = pmf_name ;
    create_pmf_name( ims_event->name, pmf_values ) ;

    /*   char *format;    */
    ims_event->format = "PMF" ;

    /*   short version;    */
    ims_event->version = -1 ;

    /*   short fileCount;    */
    ims_event->fileCount = 1 ;

    /*   char **extensions;    */
    ims_event->extensions = extensions ;

    /*   char *sourceDir;    */
    ims_event->sourceDir = ims_info->sourceDir ;

    /*   char localArchiveFlag;    */
    ims_event->localArchiveFlag = 'N' ;

    /*   char *programName;          */
    ims_event->programName = ims_info->programName ;

    /*   char *catSrvName;           */
    ims_event->catSrvName = ims_info->catSrvName ;

    /*   char *catDbName;           */
    ims_event->catDbName = ims_info->catDbName ;

    /*   
    -- char *ftsSrvName;         
    -- Mon Mar 23 11:07:30 PST 1998
    -- Tim Roberts says to call with a NULL, so that 
    -- the IMS will determine the right server name.  
    */
    ims_event->ftsSrvName = NULL ;

    /*   IMS_MSG_STRUCT *msgDesc;    */
    ims_event->msgDesc = ims_msgStructAlloc() ;

    return ;

}

#ifdef DUMMY_IMS_ROUTINE
static int ims_archive( IMS_CLNT_EVENT  *ims_event )
{
    (void) fprintf(stderr, 
        "DUMMY_IMS_ROUTINE CALLED INSTEAD OF REAL ROUTINE\n" ) ;
    return 1 ;
}
#endif   /* DUMMY_IMS_ROUTINE  */


/*==============================================================================
Function:       IMS_interface()

Description:    Handles the IMS interface.  Takes input info and reports 
                to IMS.  

Creator:        Lawrence Stevens

Creation Date:  Fri Feb  6 11:13:03 PST 1998

==============================================================================*/
int 
IMS_interface(                          /* interfaces with IMS.      */
    APS_STATS_IMS_INFO      *ims_info,      /* input info, for IMS.      */
    APS_STATS_PMF_VALUES    *pmf_values )   /* input info, for IMS.      */
{
    int             return_code = 0 ;

    static          IMS_MSG_QUEUE *msgQueue;

    IMS_CLNT_EVENT  *ims_event ;

    /* 
    -- for APS_STATS_IMS_INFO definition:  
    --     see aps_Statistics.h in current directory.  
    --
    -- for APS_STATS_PMF_VALUES definition:  
    --     see aps_Statistics.h in current directory.  
    --
    -- for IMS_CLNT_EVENT definition:  
    --     see ims_archive.h in .../include/imsdads directory.  
    */
#ifdef PRINT_DIAG
    print_ims_info( ims_info->logfile_ptr, ims_info ) ;
    print_pmf_values( ims_info->logfile_ptr, pmf_values ) ;
#endif 

    ims_event = malloc( sizeof(IMS_CLNT_EVENT) ) ;
    fill_ims_event( ims_info, pmf_values, ims_event ) ;
    print_ims_event( ims_info->logfile_ptr, ims_event ) ;

    return_code = write_PMF(ims_info, pmf_values) ;
    if( return_code < 0 )
    {
        /* allocated in fill_ims_event()    */
        (void) ims_msgStructFree (ims_event->msgDesc);
        free(ims_event) ;
        return return_code ;
    }
    (void) fprintf( ims_info->logfile_ptr, 
        "PMF DIRECTORY:  %s\n",  ims_info->sourceDir ) ;

    if( ims_info->ims_call_flag != TRUE )
    {
        (void) fprintf( ims_info->logfile_ptr, 
            "\n\nIMS INTERFACE PRINTING ONLY, NO CALL.\n" ) ;
        (void) fprintf( ims_info->logfile_ptr, 
            "===============================================\n" ) ;

        #ifdef RANDOM_CODES
        /* test the different returns from ims_archive().  */
        j = rand() ;
        if (!( j % 4 ))
        {
            (void) fprintf( ims_info->logfile_ptr, 
                "IMS return code randomly set to IMS_OK for testing.\n" ) ;
            return_code = IMS_OK ;
        }
        if (!( -1 + j % 4 ))
        {
            (void) fprintf( ims_info->logfile_ptr, 
                "IMS return code randomly set to IMS_WARNING for testing.\n" ) ;
            return_code = IMS_WARNING ;
        }
        if (!( -2 + j % 4 ))
        {
            (void) fprintf( ims_info->logfile_ptr, 
                "IMS return code randomly set to IMS_ERROR for testing.\n" ) ;
            return_code = IMS_ERROR ;
        }
        if (!( -3 + j % 4 ))
        {
            (void) fprintf( ims_info->logfile_ptr, 
                "IMS return code randomly set to IMS_FATAL for testing.\n" ) ;
            return_code = IMS_FATAL ;
        }
        #else
        (void) fprintf( ims_info->logfile_ptr, 
        "IMS return code is always set to IMS_OK for testing in this run.\n" ) ;
        return_code = IMS_OK ;
        #endif    /*  RANDOM_CODES    */

    } /* do not make the REAL CALL to IMS.  */
    else
    {   
        /* if( ims_info->ims_call_flag == TRUE )    */
        /* do make the REAL CALL to IMS.  */

        /*
        -- Making the real call to IMS.  
        -- Specify the syslog facility for logging of the messages generated
        -- by the IMS Message Facility.
        */
        (void) ims_msgOpenSyslog (ims_event->msgDesc, "IMS:", LOG_LOCAL1);
        /*
        -- Specify the severity of messages:
        */
        (void) ims_msgSetSyslogSeverity(ims_event->msgDesc, IMS_INFO ) ;
 
        /*
        -- Specify the program name to be displayed in the syslog message.
        */
        (void)ims_msgProgramName(ims_event->msgDesc, ims_event->programName ) ;
 
        /*
        -- Use the Sybase error and message handlers provided by IMS.
        */
        (void) ims_msgSybErrHndlFlag (ims_event->msgDesc, IMS_ON);
        (void) ims_msgSybMsgHndlFlag (ims_event->msgDesc, IMS_ON);

        /* turn stderr off, direct log file output to logfp.  */
        (void) ims_msgStderrFlag(ims_event->msgDesc, IMS_OFF) ;
        (void) ims_msgLogFilePtr(ims_event->msgDesc, ims_info->logfile_ptr) ;

        (void) fprintf( ims_info->logfile_ptr, 
            "\nCalling ims_archive() for PMF:  %s.%s.%s.%d.%d.M\n",  
            pmf_values->type,
            pmf_values->condition,
            pmf_values->sat,
            pmf_values->rev,
            pmf_values->dtkid ) ;

        return_code = ims_archive( ims_event ) ;
        /* Extract messages from the message queue. */
        while ((msgQueue = ims_msgQueueExtract (ims_event->msgDesc)) 
                  != (IMS_MSG_QUEUE *) NULL)
        {
            (void) fprintf( ims_info->logfile_ptr, "%s\n", msgQueue->msg);
            (void) ims_msgQueueFree(msgQueue);
        }
    } /* do make the REAL CALL to IMS.  */

    /* always do this step:   */
    switch( return_code )
    {
    case IMS_OK :
        (void) fprintf(ims_info->logfile_ptr, 
            "\nIMS_OK code IMS_OK returned from ims_archive() call.\n" ) ;
        return_code = TRUE ;
        break ;
    case IMS_WARNING :
        (void) fprintf(ims_info->logfile_ptr, 
          "\nIMS_WARNING code IMS_WARNING returned from ims_archive() call.\n");
        return_code = TRUE ;
        break ;
    case IMS_ERROR :
        (void) fprintf(ims_info->logfile_ptr, 
            "\nIMS_ERROR code IMS_ERROR returned from ims_archive() call.\n" ) ;
        return_code = STATS_ERROR_IMS_ERROR ;
        break ;
    case IMS_FATAL :
        (void) fprintf(ims_info->logfile_ptr, 
            "\nIMS_FATAL code IMS_FATAL returned from ims_archive() call.\n" ) ;
        return_code = STATS_ERROR_IMS_FATAL ;
        break ;
    default:
        (void) fprintf(ims_info->logfile_ptr, 
            "\nUnknown code %d returned from ims_archive() call.\n",
            return_code ) ;
        return_code = STATS_ERROR_IMS_UNKNOWN_RETURN_CODE ;
        break ;
    }

    if( ims_info->ims_call_flag )
    {   /* 
        -- the ims was called. 
        -- Restore the syslog config info:
        */
        (void) ims_msgOpenSyslog (ims_event->msgDesc, "APS:", LOG_LOCAL1);
     
        /*
        -- Restore the Sybase message and error handlers provided by APS.
        */
        db_install_message_handler(db_default_message_handler);
        db_install_error_handler(error_handler_exit);
    }

    /* Free the message descriptor, allocated in fill_ims_event(). */
    (void) ims_msgStructFree (ims_event->msgDesc);

    /* free the ims structure.  */
    free(ims_event) ;

    /* return_code was set above.  */
    return return_code ;

}

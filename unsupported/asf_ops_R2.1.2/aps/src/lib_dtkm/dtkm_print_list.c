#define PRINT_EXTRA_TIMES


#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_print_list.c

Description:    

External Functions Defined:

int dtkm_antenna_down_times_rec2str( DB_RECORD **antenna_down_times_record )

int dtkm_print_antenna_down_times(FILE *fp, DB_RECORD **antenna_down_times_rec )

int dtkm_print_antenna_down_times_list(FILE *fp, llist *antenna_down_times_list)

int dtkm_rec2str( DB_RECORD **dtk_rec )

int dtkm_print( FILE *fp, DB_RECORD **dtk_rec )

int dtkm_print_list(FILE *fp, llist *dtk_list )

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_print_list.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_print_list.c"

/* INCLUDES  */

#include <string.h>       /* for strcmp, strncmp argument checks  */
#include <stdio.h>        /* for fprintf etc...                   */

#include "dtkm.h"   /* for error messages     */
/* 
-- GLOBALLY declared variable used for output:  
*/
char dtk_string[512] ;              /* this must go after dtkm.h    */


/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "dapps_list.h"     /* for APS linked list macros           */

/* FOR DATABASE TABLES        */
#include "db_dtk.h"                 /* for dtk table                  */
#include "db_antenna_down_times.h"  /* for antenna_down_times table   */

/* FOR LAT/LON FIELDS        */
#include "check_lat_lon.h"  

#include "timeconv.h"       /* for tc_et_ASF_datetime_diff() */

/* codes used to decode fields to display values, in strings:  */
static char     DOWN_CODE[] = "*DOWN*" ;
static char     TIME_CODE[] = "*TIME*" ;

/* 
-- globally declared string:  dtk_string
-- This string can be used when calling this routine; you will not 
-- have to declare a string in the calling routine.  
-- it will always have enough bytes.  
*/


/*==============================================================================
Function:   dtkm_antenna_down_times_rec2str

Description:  puts into a character string values from a 
antenna_down_times DB_RECORD.  Used when printing out a list 
of antenna_down_times one line per record. 

Parameters:     
    DB_RECORD    **antenna_down_times_record  input antenna_down_times record.  

    globally declared here, not a parameter; use dtkm_utilities.h   
    char         *dtk_string     output string to receive the data.  Must 
                                 be at least 78 bytes long.  

Returns:    
    0       no error.
    != 0    error

Creator:    Lawrence Stevens

Creation Date:  Sat Nov 11 19:37:29 PST 1995

Notes:      
==============================================================================*/
/* must be declared GLOBALLY:  */
int dtkm_antenna_down_times_rec2str( DB_RECORD **antenna_down_times_record )
{
    /*
    --     station
    --     |   antenna_id
    --     |   |      start time
    --     |   |      |                     stop time
    --     |   |      |                     |
    --     |   |      |                     |
    --     ASF 1 down 1995:090:20:52:50.999 1995:090:20:53:50.999
    */
    sprintf(dtk_string, "%3.3s%2d down %21s %21s",
        CAST_ANTENNA_DOWN_TIMES_STATION_ID 
            antenna_down_times_record[ANTENNA_DOWN_TIMES_STATION_ID],
        CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID 
            antenna_down_times_record[ANTENNA_DOWN_TIMES_ANTENNA_ID],
        CAST_ANTENNA_DOWN_TIMES_STRTTIME 
            antenna_down_times_record[ANTENNA_DOWN_TIMES_STRTTIME],
        CAST_ANTENNA_DOWN_TIMES_STOPTIME 
            antenna_down_times_record[ANTENNA_DOWN_TIMES_STOPTIME] ) ;

    return 0 ;
}

/*==============================================================================
Function:   dtkm_print_antenna_down_times

Description:  fprints the values of a antenna_down_times record from a 
                antenna_down_times DB_RECORD.

NOTE:  if the file pointer is NULL, there is no print.  In this way, you 
can easily turned the print on or off.  

Parameters:     
    FILE *fp, 
    DB_RECORD    **antenna_down_times_rec  record to fprint.

Returns:    
    0       no error.
    != 0    DTKM_ERROR_NULL_RECORD

Creator:    Lawrence Stevens

Creation Date:  Sat Nov 11 19:46:47 PST 1995

Notes:      
==============================================================================*/
/* must be declared GLOBALLY:  */
int dtkm_print_antenna_down_times( 
    FILE *fp, 
    DB_RECORD **antenna_down_times_rec )
{
    int     return_code ;

    if ( fp == NULL )
        return 0 ;

    if (antenna_down_times_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    return_code = dtkm_antenna_down_times_rec2str( antenna_down_times_rec ) ;

    if (return_code != 0)
        return (return_code) ;

    /* dtk_string is globally declared; see dtkm_utilities.h  */
    fprintf(fp, "%s\n", dtk_string ) ;

    return 0 ;
}

/*==============================================================================
Function:   dtkm_print_antenna_down_times_list

Description:  fprints the values of antenna_down_times record from a 
                linked list of data-takes.  if the file pointer is NULL, 
                there is no print.

Parameters:     
    FILE    *fp                         file pointer to output file.
    llist   *antenna_down_times_list       records to fprint.

Returns:    
    >=0     the number of records fprinted.  could be 0.

Creator:    Lawrence Stevens

Creation Date:  Sat Nov 11 19:50:32 PST 1995

Notes:      
==============================================================================*/
int dtkm_print_antenna_down_times_list(
    FILE    *fp, 
    llist   *antenna_down_times_list )
{
    DB_RECORD   **antenna_down_times_rec ;
    cursor      antenna_down_times_list_ptr = NULL ;
    int         record_number = 0 ;

    if (fp == NULL)
        return 0 ;

    if (antenna_down_times_list == NULL)
        return DTKM_ERROR_INPUT_ANTENNA_LIST_NOT_INITIALIZED ;

    /* get the first of the list of data-takes.  */
    antenna_down_times_rec = 
        (DB_RECORD **) 
        FIRST(antenna_down_times_list, antenna_down_times_list_ptr) ;

    /* fprint each record and increment the counter.  */
    while ( antenna_down_times_rec != NULL )
    {
        record_number ++ ;
        fprintf( fp, "#%3d:     ", record_number ) ;
        dtkm_print_antenna_down_times( fp,  antenna_down_times_rec ) ;

        /* get the next record in the list  */
        antenna_down_times_rec = 
            (DB_RECORD **) 
            NEXT(antenna_down_times_list, antenna_down_times_list_ptr) ;
    }

    return ( NUMELTS(antenna_down_times_list) ) ;
}

/*==============================================================================
Function:   dtkm_rec2str

Description:    puts into a string selected values from a 
                dtk DB_RECORD.  Used when printing out a list of 
                data-takes.  

    char         *dtk_string     output string to receive the data.  Must 
                                 be at least 78 bytes long.  
                dtk_string is globally declared in this file.  

Parameters:     
    DB_RECORD    **dtk_record    input dtk record.  

Returns:    
    0       no error.
    != 0    error

Creator:    Lawrence Stevens

Creation Date:  02/09/1995

Notes:      
    SAMPLE:  dtkm_rec2str(dtk_record ) ;
             printf("DTK:  %s\n", dtk_string ) ;

==============================================================================*/

int dtkm_rec2str( DB_RECORD **dtk_rec )
{
    char    activity[12] ;
    char    agencies[4] ;
    double  duration_days ;
    float   duration_minutes ;
    float   duration_seconds ;
    int     int_minutes ;
    int     int_seconds ;

    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    /* 
    -- write in the activity:  
    -- TAPE DUMP 
    -- RECORDING
    -- RT DOWNLK
    -- RT OBSERV.    
    -- *DOWN*       (satellite equipment is down)
    -- TAPE LENGTH  ( if this is a conflict activity )
    */

    if ( dtkm_is_a_realtime_downlink( dtk_rec ) == TRUE )
        strcpy(activity, "RT DOWNLK " ) ;
    else if ( dtkm_is_a_realtime_observation( dtk_rec ) == TRUE )
        strcpy(activity, "RT OBSERV " ) ;
    else if ( dtkm_is_a_tape_dump( dtk_rec ) == TRUE )
        strcpy(activity, "TAPE DUMP" ) ;
    else if ( dtkm_is_a_recording( dtk_rec ) == TRUE )
        strcpy(activity, "RECORDING" ) ;
    else if ( strncmp( DOWN_CODE, CAST_DTK_ACTID dtk_rec[DTK_ACTID],
            strlen(DOWN_CODE)) == 0 )
    {
        strcpy(activity, DOWN_CODE) ;
    }
    else if ( strncmp( TIME_CODE, CAST_DTK_ACTID dtk_rec[DTK_ACTID],
            strlen(TIME_CODE)) == 0 )
    {
        strcpy(activity, "TAPE LENGTH" ) ;
    }
    else
        strcpy(activity, "") ;

    /* determine the agency:  CEF, NSF, ASF, etc.     */
    if ( (int) strlen(CAST_DTK_ACTID dtk_rec[DTK_ACTID]) < 6 )
    {
        /*  agencies not available.     */
        strcpy(agencies, "" ) ;
    }
    else if ( strncmp( DOWN_CODE, CAST_DTK_ACTID dtk_rec[DTK_ACTID],
            strlen(DOWN_CODE)) == 0 
         ||   strncmp( TIME_CODE, CAST_DTK_ACTID dtk_rec[DTK_ACTID],
            strlen(TIME_CODE)) == 0 )
    {
        /* 
        -- if there is a down time or time problem, there 
        -- is no agency value.  
        */
        strcpy(agencies, "" ) ;
    }
    else
    {
        strcpy(agencies, (CAST_DTK_ACTID dtk_rec[DTK_ACTID])+3 ) ;
    }

    /* compute the time duration in minutes.      */
    if ( !tc_et_ASF_datetime_diff(
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
        &duration_days ) )
        return DTKM_ERROR_COMPUTING_TIME_DIFF ;

    /* 
    -- add 1/2 second to this duration and then truncate to the second.  
    -- the net effect is a roundoff to the nearest second.  
    */
    duration_days += (0.5 / 24.0 / 3600.0)  ;
    duration_minutes = duration_days * 24.0 * 60.0 ;
    int_minutes = (int) duration_minutes ;

    duration_seconds = 60.0*( duration_minutes - int_minutes) ;
    int_seconds = (int) duration_seconds ;

    /*
    -- example of string:
    --
status
|   station
|   |   antenna_id
|   |   | satellite
|   |   | |  sensor
|   |   | |  |   rev number
|   |   | |  |   |     dtkid
|   |   | |  |   |     |  activity  
|   |   | |  |   |     |  |         downlink frequency
|   |   | |  |   |     |  |         |  agencies
|   |   | |  |   |     |  |         |  |    science quicklook flag
|   |   | |  |   |     |  |         |  |    |  ascending-descending flag
|   |   | |  |   |     |  |         |  |    |  |  fa_dtkid
|   |   | |  |   |     |  |         |  |    |  |  |
PLN ASF 1 R1/SS1/00001.01 TAPE DUMP F4 CEF  Y  A  S1234501 darid=20001
        DTK LENGTH:  1:00      1995:090:20:52:50.999 1995:090:20:53:50.999
                     |         |                     |
                     |         start time            stop time
                     dtk length in minutes
    --
    */

    sprintf(dtk_string, "%3.3s %3.3s", 
        CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],
        CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) ;

    /* print the antenna_id if > 0   */
    if ( CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] > 0 )
        sprintf(dtk_string, "%s%2d", 
            dtk_string, CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ) ;
    else
        strcat(dtk_string, "  " ) ;

    sprintf(dtk_string, 
        "%s %2.2s/%3.3s/%05ld.%02d %-9.9s %2.2s %3.3s %1c %1c %s",
        dtk_string, 
        CAST_DTK_SAT dtk_rec[DTK_SAT],
        CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
        CAST_DTK_REV dtk_rec[DTK_REV],
        CAST_DTK_DTKID dtk_rec[DTK_DTKID],
        activity,
        CAST_DTK_TRANSID dtk_rec[DTK_TRANSID],
        agencies,
        CAST_DTK_SCIENCE_QUICKLOOK dtk_rec[DTK_SCIENCE_QUICKLOOK],
        CAST_DTK_ASCDSC dtk_rec[DTK_ASCDSC],
        CAST_DTK_FADTKID dtk_rec[DTK_FADTKID] ) ;

    /* print the darid if > 0   */
    if ( CAST_DTK_DARID dtk_rec[DTK_DARID] > 0 )
        sprintf(dtk_string, "%s darid=%ld", dtk_string, 
            CAST_DTK_DARID dtk_rec[DTK_DARID] ) ;
        
    sprintf(dtk_string, "%s\n        DTK LENGTH:%3d:%02d     %21s %21s",
        dtk_string, 
        int_minutes,
        int_seconds,
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME], 
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;

#ifdef PRINT_EXTRA_TIMES
    /* 
    -- add another line to show the 
    -- submit time: 
    */
    sprintf(dtk_string, "%s\n sub %s",
        dtk_string, 
        CAST_DTK_SUBMIT_TIME dtk_rec[DTK_SUBMIT_TIME] ) ;
#endif

    return 0 ;

}

/*==============================================================================
Function:   dtkm_print

Description:  fprints the values of a data-take record from a dtk DB_RECORD.

NOTE:  if the file pointer is NULL, there is no print.  In this way, the 
print can be easily turned on or off.  

Parameters:     
    FILE *fp, 
    DB_RECORD    **dtk_record  record to fprint.

Returns:    
    0       no error.
    != 0    DTKM_ERROR_NULL_RECORD

Creator:    Lawrence Stevens

Creation Date:  01/31/1995

Notes:      
    SAMPLE:  dtkm_print(FILE *fp, dtk_record ) ;
==============================================================================*/
int dtkm_print( 
    FILE *fp, 
    DB_RECORD **dtk_rec )
{
int     return_code ;

    if ( fp == NULL )
        return 0 ;

    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    return_code = dtkm_rec2str( dtk_rec ) ;

    if (return_code != 0)
        return (return_code) ;

    /* dtk_string is globally declared; see dtkm_utilities.h  */
    fprintf(fp, "%s\n", dtk_string ) ;

    return 0 ;
}

/*==============================================================================
Function:   dtkm_print_list

Description:  fprints the values of data-take record from a linked list 
of data-takes.  if the file pointer is NULL, there is no print.

Parameters:     
    FILE    *fp             file pointer to output file.
    llist   *dtk_list       records to fprint.

Returns:    
    >=0     the number of dtk records fprinted.  could be 0.

Creator:    Lawrence Stevens

Creation Date:  01/31/1995

Notes:      
    SAMPLE:  dtkm_print_list(fp, dtk_list ) ;
==============================================================================*/
int dtkm_print_list(FILE *fp, llist *dtk_list )
{
    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr = NULL ;
    int         record_number = 0 ;

    if (fp == NULL)
        return 0 ;

    if (dtk_list == NULL)
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    /* get the first of the list of data-takes.  */
    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

    /* fprint each record and increment the counter.  */
    while ( dtk_rec != NULL )
    {
        record_number ++ ;
        fprintf( fp, "#%3d: ", record_number ) ;
        dtkm_print( fp,  dtk_rec ) ;

        /* get the next record in the list  */
        dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr) ;
    }

    return ( NUMELTS(dtk_list) ) ;
}

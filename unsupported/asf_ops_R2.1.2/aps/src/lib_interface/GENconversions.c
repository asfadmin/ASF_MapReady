#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#undef  PRINT_DIAG

#ifdef MIGUEL_COMMENT_OUT
extern int gen_field_is_numeric(
    char        *field,     /* input numeric field to test.    */
    int         length ) ;  /* how many bytes we are to test.  */
extern int gen_string2str(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string);
#endif
/*==============================================================================
Filename:   GENconversions.c

Description:
        Generic string conversions.

External Functions Defined:
    int gen_rev2trackstart  calculate the track_start time in asftime format
    int gen_rev2trackend    calculate the track_end time in asftime format
    int itoa                convert integer into equivalent string (ie. 2 ->'2')
    int table_lookupFA2APS
    int table_lookupFA2APScat  convert and concatenate to a value
    int table_lookupAPS2FAquoted
    int gen_time2asftime    convert a time value string to asf time format
    int gen_string2str      copy a string      
    int gen_string2zero_filled_str
                            copy string into a zero-filled string
    int gen_string2strcat   copy a string and concatenate to a value 
    int gen_string2qstr     copy a string and put single quotes around it. 
    int gen_string2int      convert a string value to integer value
    int gen_string2tinyint  convert a string value to one-byte integer value
    int gen_string2float    convert a string value to float value
    int gen_string2malloc_str
                            create storage for destination string,initialize it 
    int gen_string2zero_filled_str3_cat
    int gen_string2_insert_zero_filled_str
    int gen_int2tinyint     convert an integer into a tiny integer value
    int gen_int2smallint    convert an integer into a small integer value
    int gen_int2zero_filled_str   
    int gen_int2blank_filled_str   
    int gen_int2str   
    int gen_intplus1_2str   
    int gen_tinyint2str   
    int gen_smallint2str   
    int gen_field_is_numeric    returns TRUE if all characters are numeric.   
    int gen_field_is_float      returns TRUE if a float.  
    int gen_get_asftime     get the current time in asf time format.
    int gen_get_odltime     get the current time in odl time format.
    int gen_asftime2odl
    int gen_asftime2odlcat  convert asf time and strcat() the result to dest.

File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes: These conversions do not necessitate the use of the CAST_* mechanism.    

==============================================================================*/
#pragma ident   "@(#)GENconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.GENconversions.c"

#include <stdio.h>
#include <ctype.h>       /* for isspace()   */
#include <stdlib.h>
#include <string.h>
#include <math.h>        /* for pow, which computes a power.  */

/* for permission definitions */
#include <mu_utilities.h>

#include "GENconversions.h"
#include "dapps_defs.h"     /* for APS basic definitions...                 */ 
#include "db_sybint.h"      /* for APS sybase interface routines            */
#include "dapps_list.h"     /* for APS linked list macros                   */
#include "timeconv.h"       /* for tc_asf2odl(), tc_systime2asf()           */
#include "aps_db_table.h"   /* for DTK     etc.                             */
#include "db_dtk.h"         /* for DTK_SAT etc.                             */
#include "db_antenna.h"     /* for ANTENNA_STATION_ID etc.                  */
#include "dtkm_utilities.h" /* for DTKM_SENSOR_REALTIME_DOWNLINK_CODE etc.  */


/* 
-- REPORT_HEADER, REPORT_CONTROL  element declarations 
    These variables are used by the various Flight Agency processors
    (ie: the ingestion processors), and they are available for use by any
    processes that deal with Flight Agency files.
*/

int             fa_trigger_REQQ_add_grs_records ;
int             fa_trigger_shaq_observation_generation ;
int             fa_trigger_dtk_create_FA_downlinks ;
int             fa_trigger_dtk_delete_range ;
int             fa_trigger_dtk_status_by_blanking ;
int             fa_trigger_dtk_fillin_FA_response ;
int             fa_trigger_skip_default_values = 0 ; 
                        /* initial value: DO NOT SKIP */
int             fa_trigger_dtk_list_handling_for_STGS ;
int             fa_trigger_always_create_response_file = 0 ;
                        /* initial value:DO NOT FORCE RESPONSE FILE CREATION */
int             fa_trigger_reqm_secondary_record_ingestion = 0 ;
int             fa_trigger_MPSG_station_mask_check = 0 ;
int             fa_trigger_read_variable_SHAQP_header = 0 ;
int             fa_trigger_SHAQP_dtk_consolidation = 0 ;
int             fa_trigger_RES_rej_downtime = 0 ;
int             fa_trigger_RES_fillin_response = 0 ;
int             fa_trigger_CSA_dl_mapping = 0 ;
int             fa_antenna_id ;
int             fa_cyclic_counter ;
int             fa_dtkid ;
int             fa_counter ;
int             fa_number_of_records ;
int             fa_number_of_primary_recs ;
int             fa_number_of_secondary_recs ;
int             fa_number_of_observations ;
int             fa_number_of_error_records ;
int             fa_number_of_subrecords ;
int             fa_record_size ;    /* informational, not used by utilities */
int             fa_subrecord_size ; /* informational, not used by utilities */
int             *fa_subrecord_control ;
char            *fa_filename    = NULL ;
char            *fa_file_dest   = NULL ;
char            fa_flight_agency[]="XXX" ;
char            fa_station_id[]  = "XXX" ;
char            fa_file_type[] = "XXXX" ;   /* values like MWOS, AREQ, etc. */
char            fa_activity_type[] = "XXXX_XXXX" ;
char            fa_file_id[]    =     "000000" ;
char            fa_sat_id[] =         "XX" ;
char            fa_creation_date[] =        "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_first_track_start[] =    "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_last_track_end[] =       "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_argument_start_time[] =  "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_argument_stop_time[] =   "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_aos_track_start[] =  	"yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_los_track_end[] =    	"yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_recording_start_time[] = "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_recording_stop_time[] =  "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_file_start_time[] =      "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_file_stop_time[] =       "yyyy:ddd:hh:mm:ss.ccc" ;
char            fa_file_start_rev[] =  "12345" ;
char            fa_file_stop_rev[]  =  "12345" ;
char            fa_start_yyyymmdd[]=  "yyyymmdd" ;
char            fa_stop_yyyymmdd[]=   "yyyymmdd" ;
char            fa_processing_flag[]=   "READ" ;    /* values: SKIP / READ */
char            fa_record_write_flag[]= "RITE" ;    /* values: SKIP / RITE */
char            fa_acquisition_mode[]=  "XXX" ;     /* values: ROB  / REC  */
char            fa_downlink_mode[]=  "XXX" ;		/* values: RLT  / DMP  */
char            fa_dtk_quicklook_flag = 'N' ;       /* values: Y or N      */
char            fa_schedule_link[]=     "                    " ; /* it varies */
FA_KEY_STRUCT   *fa_sch_link_control ;
FILE            *fa_output_file_fp = NULL ;   
llist           *fa_dtk_report_list = NULL ;


/*==============================================================================
Function:       gen_set_trigger

Description:    used to initialize a trigger to TRUE

Parameters:     

Returns:        Always returns TRUE

Creator:        Miguel Siu

Creation Date:  Tue Oct 24 10:25:36 PDT 1995

Notes:		
==============================================================================*/
int gen_set_trigger(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	int 		*trigger)
{
	*trigger = TRUE  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:      gen_default_ASF_station 

Description:   Returns a value of 'ALL', indicating support for both
				the Alaska SAR Facility (ASF) and McMurdo (MCM) 
				groundstations.
Parameters:     

Returns:        Always TRUE

Creator:        Miguel Siu

Creation Date:  Sun Dec 15 08:17:39 PST 1996

Notes for NASDA files:
	By agreement, the OPL1 describes scheduled datatakes at
	the ASF ground station (e.g. datatakes at Hatoyama 'HEOC'
	or at McMurdo 'MCM' are ignored)
	By agreement, the REQM describes scheduled datatakes at
	the ASF ground station (we don't do planning for the
	datatakes at Hatoyama 'HEOC', which is the other possible
	groundstation value)
	The REQA describes requested datatakes accepted by the
	flight agency. We only request datatakes at 
	the ASF ground station.
==============================================================================*/
int gen_default_ASF_station(
	void	*unused_pointer,
	char	*unused_character_pointer,
	char	*insert_here ) 
{
	strcpy(insert_here, MU_ASF_STATIONID ) ;
	return (TRUE) ;
}

/*==============================================================================
Function:       gen_default_DMP

Description:    insert default value of DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE 

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 17:14:28 PDT 1995

Notes:		
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int gen_default_DMP(
	void	*unused_pointer,
	char	*unused_character_pointer,
	char	*insert_here ) 
{
	strcpy(insert_here, DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) ;
	return (TRUE) ;
}


/*==============================================================================
Function:      gen_default_ALL_stations
 
Description:   Returns a value of 'ALL', indicating support for both
                the Alaska SAR Facility (ASF) and McMurdo (MCM)
                groundstations.
 
Parameters:
 
Returns:        Always TRUE
 
Creator:        Miguel Siu
 
Creation Date:  Sun Dec 15 08:17:39 PST 1996

Notes:		
==============================================================================*/
int gen_default_ALL_stations(
	void	*unused_pointer,
	char	*unused_character_pointer,
	char	*insert_here ) 
{
	strcpy(insert_here, MU_ALL_STATIONID ) ;
	return (TRUE) ;
}

/*==============================================================================
Function:       gen_rev2trackstart()

Description:    given a rev, calculate the track_start time in asftime format
                (earliest starttime in rev - antenna.pre_dtk_track_pad_sec)

Creator:        Miguel Siu 

Creation Date:  Wed Jul 17 18:25:42 PDT 1996

NOTES:  This routine uses the global variables 
        fa_sat_id, fa_antenna_id and fa_station_id

==============================================================================*/
int gen_rev2trackstart(
    void        *unused_pointer,
    int         *rev,
    char        *track_start )
{
    int         pre_track_pad_seconds;
    cursor      dtk_list_ptr ;
    DB_RECORD   **dtk_rec ;
    int         found ;
    char        earliest_strttime[ASF_TIME_STR_LENGTH+1] ;
    DB_RECORD   **earliest_dtk_rec ;

    cursor      antenna_list_ptr ;
    DB_RECORD   **antenna_rec ;
    llist       *antenna_list = NULL ;


    /*
    -- get the first downlink record in the global list fa_dtk_report_list
    -- that matches the rev and fa_sat and fa_station_id and fa_antenna_id.  
    -- calculate the tracking start for this downlink data-take.  
    --
    -- At the start of processing, all of the data-takes for the 
    -- report were placed into the global list pointer:  fa_dtk_report_list
    */
    found = FALSE ;
    strcpy( earliest_strttime, "9999:365:12:02:03.444" ) ;
    for (   dtk_rec = (DB_RECORD **) FIRST(fa_dtk_report_list, dtk_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(fa_dtk_report_list, dtk_list_ptr)  
        )
    {
        if( strcmp( fa_sat_id, CAST_DTK_SAT dtk_rec[DTK_SAT] ) != 0 )
            continue ;
        if( *rev != CAST_DTK_REV dtk_rec[DTK_REV] )
            continue ;
        if( dtkm_is_a_downlink( dtk_rec ) != TRUE )
            continue ;
        if( strcmp( fa_station_id, 
                    CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) != 0 )
        {
            continue ;
        }
        if( fa_antenna_id != CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] )
            continue ;
        /* 
        -- this dtk is the first downlink dtk in the list 
        -- with the desired sat and *rev values, and also 
        -- is a downlink with the desired ground station 
        -- and antenna ID.  
        */
        found = TRUE ;
        if( strcmp( earliest_strttime, 
                    CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) > 0 )
        {
            /* this is an earlier data-take.  */
            strcpy( earliest_strttime, CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME]);
            earliest_dtk_rec = dtk_rec ;
        }
    }

    if( found != TRUE )
        return FALSE ;

    dtk_rec = earliest_dtk_rec ;

    /*
    -- the desired data-take has been found. 
    -- get the pre-pass-pad, there should be a single record returned.
    */
    sprintf(where_clause,
            "where %s = '%s' and %s = %d", 
            APS_COL(ANTENNA, ANTENNA_STATION_ID), fa_station_id,
            APS_COL(ANTENNA, ANTENNA_ANTENNA_ID), fa_antenna_id ) ;
    antenna_list = db_get_records
        (DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(ANTENNA),
        where_clause, NULL, APS_CDEFS(ANTENNA), ALL_COLS) ;
    if (NUMELTS(antenna_list) != 1)
	{
		DEL_LIST (antenna_list) ;
        return FALSE ;
	}
    antenna_rec = (DB_RECORD **) FIRST(antenna_list, antenna_list_ptr);
    pre_track_pad_seconds = CAST_ANTENNA_PRE_DTK_TRACK_PAD_SEC 
                            antenna_rec[ANTENNA_PRE_DTK_TRACK_PAD_SEC] ;
	DEL_LIST (antenna_list) ;

    /*
    -- calculate the track start time
    */
    if ( !tc_asf_add_ndays( CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
            -(pre_track_pad_seconds/60.0/60.0/24.0), track_start )    )
        return FALSE ;

    return TRUE ;
}


/*==============================================================================
Function:       gen_rev2trackend()

Description:    given a rev, calculate the track_end time in asftime format
                (latest stoptime in rev + antenna.post_dtk_track_pad_sec)

Creator:        Miguel Siu 

Creation Date:  Wed Jul 17 18:25:42 PDT 1996

NOTES:  This routine uses the global variables 
        fa_sat_id, fa_antenna_id and fa_station_id
==============================================================================*/
int gen_rev2trackend(
    void        *unused_pointer,
    int         *rev,
    char        *track_end )
{
    int         post_track_pad_seconds;
    cursor      dtk_list_ptr ;
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **latest_dtk_rec ;
    char        latest_stoptime[ASF_TIME_STR_LENGTH+1] ;
    int         found ;

    cursor      antenna_list_ptr ;
    DB_RECORD   **antenna_rec ;
    llist       *antenna_list = NULL ;
 
    /*
    -- get the downlink record in the global list fa_dtk_report_list
    -- that matches the *rev and fa_sat and fa_antenna_id and fa_station_id
    -- and has the latest stoptime.  
    -- calculate the tracking end for this downlink data-take.  
    --
    -- At the start of processing, all of the data-takes for the 
    -- report were placed into the global list pointer:  fa_dtk_report_list
    */
    strcpy( latest_stoptime, "0000:365:23:59:01.234" ) ;
    found = FALSE ;
    for (   dtk_rec = (DB_RECORD **) FIRST(fa_dtk_report_list, dtk_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(fa_dtk_report_list, dtk_list_ptr)  
        )
    {
        if( strcmp( fa_sat_id, CAST_DTK_SAT dtk_rec[DTK_SAT] ) != 0 )
            continue ;
        if( *rev != CAST_DTK_REV dtk_rec[DTK_REV] )
            continue ;
        if( dtkm_is_a_downlink( dtk_rec ) != TRUE )
            continue ;
        if( strcmp( fa_station_id, 
                    CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) != 0 )
        {
            continue ;
        }
        if( fa_antenna_id != CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] )
            continue ;
        /* 
        -- this dtk has the desired sat and *rev values, and also 
        -- is a downlink with the desired ground station 
        -- and antenna ID.  
        */
        found = TRUE ;
        if( strcmp( latest_stoptime, 
                    CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) < 0 )
        {
            /* 
            -- this dtk is a candidate for latest stop time
            */
            strcpy( latest_stoptime, CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
            latest_dtk_rec = dtk_rec ;
        }
    }

    if( found != TRUE )
        return FALSE ;

    dtk_rec = latest_dtk_rec ;

    /*
    -- the desired data-take has been found:  dtk_rec 
    -- get the post-pass-pad, there should be a single record returned.
    */
    sprintf(where_clause,
            "where %s = '%s' and %s = %d", 
            APS_COL(ANTENNA, ANTENNA_STATION_ID), fa_station_id,
            APS_COL(ANTENNA, ANTENNA_ANTENNA_ID), fa_antenna_id ) ;
    antenna_list = db_get_records
        (DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(ANTENNA),
        where_clause, NULL, APS_CDEFS(ANTENNA), ALL_COLS) ;
    if (NUMELTS(antenna_list) != 1)
	{
		DEL_LIST (antenna_list) ;
        return FALSE ;
	}
    antenna_rec = (DB_RECORD **) FIRST(antenna_list, antenna_list_ptr);
    post_track_pad_seconds = CAST_ANTENNA_POST_DTK_TRACK_PAD_SEC
        antenna_rec[ANTENNA_POST_DTK_TRACK_PAD_SEC] ;
	DEL_LIST (antenna_list) ;
 
    /*
    -- calculate the track end time for this data-take.  
    */
    if ( !tc_asf_add_ndays( CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
            (post_track_pad_seconds/60.0/60.0/24.0), track_end )    )
        return FALSE ;
 
    return TRUE ;
}
 

/*==============================================================================
Function:       gen_field_is_float()

Description:    returns TRUE if the field is a valid float.   Leading 
                blanks are OK; imbedded or trailing blanks are not OK.  

Creator:        Miguel Siu      

Creation Date:  Fri Feb  2 09:24:24 PST 1996

==============================================================================*/
int gen_field_is_float(
    char        *field,     /* input numeric field to test.    */
    int         length )    /* how many bytes we are to test.  */
{
    char    *field_end = field + (length - 1) ;
    int     number_of_dots = 0 ;

    while( field <= field_end )
    {
        /* 
        -- skip the starting 
        -- blanks:  
        */
        if ( ! isspace( *field )  )
        {
            /* 
            -- the first non-blank; this character 
            -- and all the rest are supposed to 
            -- be decimal digits or a dot.  
            */
            while( field <= field_end )
            {
                if ( ! isdigit( *field ) )
                    if ( *field == '.')
                        number_of_dots++ ;
                    else
                        return FALSE ;

                field++ ;
            }
        }

        field++ ;
    }

    if (number_of_dots > 1) return FALSE ;
    return TRUE ;

}

/*==============================================================================
Function:       gen_asftime2odlcat

Description:    change asftime format string to ODL format.  
                this is easy, just a couple of character switches.  
                and then use strcat() instead of strcpy() to 
                place the result.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Apr  7 19:09:02 PDT 1997

Notes:      
==============================================================================*/
int gen_asftime2odlcat(
    void        *unused_pointer,
    void        *asftime,             /* yyyy:ddd:hh:mm:ss.sss   */
    char        *result_odltime)      /* yyyy-dddThh:mm:ss.sss   */
{
    char    odltime[ASF_TIME_STR_LENGTH+1] ;
    int     return_code ;

    return_code = tc_asf2odl(asftime, result_odltime) ; 
    if( return_code != TRUE )
        return return_code ;

    strcat( result_odltime, odltime ) ;

    return return_code ;

}


/*==============================================================================
Function:       gen_asftime2odl

Description:    change asftime format string to ODL format.  
                this is easy, just a couple of character switches.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Wed Aug  9 22:23:58 PDT 1995

Notes:      
==============================================================================*/
int gen_asftime2odl(
    void        *unused_pointer,
    void        *asftime,             /* yyyy:ddd:hh:mm:ss.sss   */
    char        *result_odltime)      /* yyyy-dddThh:mm:ss.sss   */
{

    return tc_asf2odl(asftime, result_odltime) ; 

}


/*==============================================================================
Function:       gen_get_odltime

Description:    get the current time in the format used in the 
                interface files between Wallops Flight Facility and ASF:
                yyyy-dddThh:mm:ss.sss
                1996-123T23:59:59.999
                Note that this is close to ASF format:
                1996:123:23:59:59.999

Creator:        Lawrence Stevens

Creation Date:  Wed Aug  9 21:19:28 PDT 1995

Notes:      
==============================================================================*/
int gen_get_odltime (
    void        *unused_pointer,
    void        *another_unused_pointer,
    char        *destination_current_odltime)
{

    if (! tc_systime2asf(destination_current_odltime) )
        return FALSE ;

    /* 
    -- now alter the asftime format to 
    -- odl format by substituting - and T   
    -- gen_asftime2odl() is robust, source and destination can
    -- be the same.  
    */
    return ( 
        gen_asftime2odl(
            unused_pointer, 
            destination_current_odltime, 
            destination_current_odltime )   
           )   ;
}


/*==============================================================================
Function:       gen_get_asftime()

Description:    gets the current time in ASF format.

Creator:        Lawrence Stevens

Creation Date:  Tue Aug  8 18:20:02 PDT 1995

Notes:      
==============================================================================*/
int gen_get_asftime (
    void        *unused_pointer,
    void        *another_unused_pointer,
    char        *destination_asftime)
{
    return ( tc_systime2asf(destination_asftime) ) ;
}


/*==============================================================================
Function:       gen_tinyint2str

Description:    sprintf a Sybase tinyint into a string.  this is a 
                single-byte integer.  In C, this is a character cast
                as an integer.  

Creator:        Lawrence Stevens

Creation Date:  Wed Aug  9 19:38:59 PDT 1995

Notes:      
==============================================================================*/
int gen_tinyint2str(
    void    *unused_pointer,
    char    *tinyintvalue,
    char    *destination )
{
    /*
    -- NOTE:  sprintf() in this installation returns the number 
    -- of characters printed.  If there is an error, then a negative
    -- number is returned.  
    */
    if (  sprintf(destination, "%d", (int)*tinyintvalue ) > 0 )
        return TRUE ;
    else
        return FALSE ;
}

/*==============================================================================
Function:       gen_smallint2str

Description:    sprintf a Sybase smallint into a string.  this is a 
                two-byte integer.  In C, this is a short int.  

Creator:        Lawrence Stevens

Creation Date:  Tue Jan  9 11:34:51 PST 1996 

Notes:      
==============================================================================*/
int gen_smallint2str(
    void        *unused_pointer,
    short int   *smallintvalue,
    char        *destination )
{
    /*
    -- NOTE:  sprintf() in this installation returns the number 
    -- of characters printed.  If there is an error, then a negative
    -- number is returned.  So if (sprintf > 0) return TRUE
    */
    if ( sprintf(destination,"%hd",*smallintvalue )    > 0  )
        return TRUE ;
    else
        return FALSE ;
}

/*==============================================================================
Function:       gen_int2str()

Description:    sprintf an int into a string.  

Creator:        Lawrence Stevens

Creation Date:  Tue Aug  8 18:50:37 PDT 1995

Notes:      
==============================================================================*/
int gen_int2str(
    void    *unused_pointer,
    int     *intvalue,
    char    *destination )
{
    /*
    -- NOTE:  sprintf() in this installation returns the number 
    -- of characters printed.  If there is an error, then a negative
    -- number is returned.  
    */
    if (  sprintf(destination, "%d", *intvalue ) > 0 )
        return TRUE ;
    else
        return FALSE ;
}

/*==============================================================================
Function:       gen_intplus1_2str()

Description:    sprintf an int, adding 1, into a string.  

Creator:        Lawrence Stevens

Creation Date:  Sat Apr 12 12:47:20 PDT 1997

Notes:      required since the DDM file has an extra object in it, 
            the CATALOG_METADATA object, in addition to the data-takes.  
==============================================================================*/
int gen_intplus1_2str(
    void    *unused_pointer,
    int     *intvalue,
    char    *destination )
{
    /*
    -- NOTE:  sprintf() in this installation returns the number 
    -- of characters printed.  If there is an error, then a negative
    -- number is returned.  
    */
    if (  sprintf(destination, "%d", (*intvalue) + 1 ) > 0 )
        return TRUE ;
    else
        return FALSE ;
}

/*==============================================================================
Function:       gen_int2zero_filled_str

Description:    writes an int to a 0-filled ascii field.  Note that this 
                output is not a string, only a field of a certain number 
                of bytes.  
Returns:        FALSE is the field with is too narrow, TRUE if OK.  

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 24 16:09:53 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int gen_int2zero_filled_str(
    int     intvalue,
    int     field_width,
    char    *destination )
{
    char    format[] = "%0llllld" ;
    char    buf[1024] ;

    /* 
    -- check to see if intvalue needs more digits than 
    -- field_width allows
    -- we compare intvalue with 10 to the field_width power
    -- intvalue must be smaller.  
    -- thus a field width of 2 allows numbers up to 99.  
    -- and intvalue must be smaller than 100.  
    */
    if ( intvalue >= (int) pow( (double) 10.0, (double) field_width ) )
    {
        /* field is too narrow; fill it with *** and return FALSE.  */
        memset( destination, '*', field_width ) ; 
        return FALSE ;
    }

    /* set up format to use in the sprintf:  */
    sprintf( format, "%%0%dd", field_width ) ;

    sprintf(buf, format, intvalue ) ;

    /* 
    -- copy only the desired field width to output; do not 
    -- include the null string terminator.  
    */
    strncpy(destination, buf, field_width ) ;

    return TRUE ;
}

/*==============================================================================
Function:       gen_int2blank_filled_str

Description:    writes an int to a blank-filled ascii field.  Note that this 
                output is not a string, only a field of a certain number 
                of bytes.  
Returns:        FALSE is the field with is too narrow, TRUE if OK.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 24 16:09:53 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int gen_int2blank_filled_str(
    int     intvalue,
    int     field_width,
    char    *destination )
{
    char    format[] = "%llllld" ;
    char    buf[1024] ;

    /* 
    -- check to see if intvalue needs more digits than 
    -- field_width allows
    -- we compare intvalue with 10 to the field_width power
    -- intvalue must be smaller.  
    -- thus a field width of 2 allows numbers up to 99.  
    -- and intvalue must be smaller than 100.  
    */
    if ( intvalue >= (int) pow( (double) 10.0, (double) field_width ) )
    {
        /* field is too narrow; fill it with *** and return FALSE.  */
        memset( destination, '*', field_width ) ; 
        return FALSE ;
    }

    sprintf( format, "%%%dd", field_width ) ;

    sprintf(buf, format, intvalue ) ;

    /* 
    -- copy only the desired field width to output; do not 
    -- include the null string terminator.  
    */
    strncpy(destination, buf, field_width ) ;

    return TRUE ;
}


/*==============================================================================
Function:       gen_string2malloc_str

Description:    Allocate memory for filename variable, and populate it 
                with the given string.

Parameters:     
Type        Name                    Definition
char        *source_string          string to be converted
char        *destination_filename   resulting float value

Returns:        
Type          Name              Definition
int                             TRUE = successful conversion.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Thu Jul 20 15:42:27 PDT 1995

Notes:      
==============================================================================*/
int gen_string2malloc_str (
    void        *unused_pointer,
    char        *source_string,
    char        **destination_filename)
{
    char *local_string ;

    local_string = (char *) malloc(strlen(source_string) + 1)  ;
    strcpy (local_string, source_string ) ;

    *destination_filename = local_string ;

    return (TRUE) ;
}


/*==============================================================================
Function:       gen_string2float

Description:    translate input string into a float

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *float_value        resulting float value

Returns:        
Type          Name              Definition
int                             TRUE = successful conversion.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Fri May 19 14:26:39 PDT 1995

Notes:      
==============================================================================*/
int gen_string2float(
    void        *unused_pointer, 
    char        *source_string, 
    float       *float_value)
{
    if ( !gen_field_is_float( source_string, strlen(source_string) ) )
        return FALSE ;
    *float_value = atof(source_string);

#ifdef PRINT_DIAG
    printf("%s(%d):  gen_string2float() value = %f\n", __FILE__, __LINE__, 
        *float_value ) ;
#endif

        return(TRUE);
}


/*==============================================================================
Function:       gen_string2int

Description:    translate input string into an integer

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *int_value          resulting integer value

Returns:        
Type          Name              Definition
int                             TRUE = successful conversion.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Fri May 19 14:26:39 PDT 1995

Notes:      
==============================================================================*/
int gen_string2int(
    void        *unused_pointer, 
    char        *source_string, 
    int         *int_value)
{
    if ( !gen_field_is_numeric( source_string, strlen(source_string) ) )
        return FALSE ;
    *int_value = atoi(source_string);

#ifdef PRINT_DIAG
    printf("%s(%d):  gen_string2int() value = %d\n", __FILE__, __LINE__, 
        *int_value ) ;
#endif

    return(TRUE);
}

/*==============================================================================
Function:      gen_int2tinyint 

Description:   Convert an integer value into a tiny integer value 

Parameters:     
Type        Name                Definition
char        *integer_value      integer to be converted
char        *tinyint_value      resulting tinyint value

Returns:        
int                             TRUE = successful conversion.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Mon Sep 23 17:26:00 PDT 1996

Notes:      
==============================================================================*/
int gen_int2tinyint(
    void        *unused_pointer, 
    int         *integer_value, 
    char        *tinyint_value)
{
    *tinyint_value = (char) *integer_value ;
    return(TRUE);
}

/*==============================================================================
Function:      gen_int2smallint 

Description:   Convert an integer value into a small integer value 

Parameters:     
Type        Name                Definition
char        *integer_value      integer to be converted
char        *smallint_value     resulting tinyint value

Returns:        
int                             TRUE = successful conversion.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Tue Oct  1 10:50:04 PDT 1996

Notes:      
==============================================================================*/
int gen_int2smallint(
    void        *unused_pointer, 
    int         *integer_value, 
    char        *smallint_value)
{
    *smallint_value = (char) *integer_value ;
    return(TRUE);
}

/*==============================================================================
Function:       gen_string2tinyint

Description:    translate input string into a single-byte integer

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *tinyint_value      resulting tinyint value

Returns:        
Type          Name              Definition
int                             TRUE = successful conversion.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Fri May 19 14:26:39 PDT 1995

Notes:      
==============================================================================*/
int gen_string2tinyint(
    void        *unused_pointer, 
    char        *source_string, 
    char        *tinyint_value)
{
    int     int_value ;

    if ( !gen_field_is_numeric( source_string, strlen(source_string) ) )
        return FALSE ;
    int_value = atoi(source_string);

    *tinyint_value = (char) int_value ;

#ifdef PRINT_DIAG
        printf("%s(%d):  gen_string2tinyint() value = %d\n", 
            __FILE__, __LINE__, (int) *tinyint_value ) ;
#endif

    return(TRUE);
}



/*==============================================================================
Function:       gen_string2qstr

Description:    copy a string, putting single quotes around it.  

Creator:        Lawrence Stevens

Creation Date:  Wed Aug  9 20:37:59 PDT 1995

Notes:      
==============================================================================*/
int gen_string2qstr(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string)
{
    strcpy(result_string, "'");
    strcat(result_string, source_string);
    strcat(result_string, "'") ;

#ifdef PRINT_DIAG
        printf("%s(%d):  gen_string2qstr() value = %s\n", 
            __FILE__, __LINE__, result_string ) ;
#endif

    return(TRUE);
}

/*==============================================================================
Function:       gen_string2strcat

Description:    translate input string into a duplicate string and 
                append to the existing value.  

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *result_string      string with possibly a value already.  

Returns:        
Type          Name              Definition
int                             TRUE = successful string duplication.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Fri May 19 14:26:39 PDT 1995

Notes:      
==============================================================================*/
int gen_string2strcat(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string)
{
    strcat(result_string, source_string);

#ifdef PRINT_DIAG
        printf("%s(%d):  gen_string2str() value = %s\n", 
            __FILE__, __LINE__, result_string ) ;
#endif

    return(TRUE);

}


/*==============================================================================
Function:       gen_string2zero_filled_str

Description:    translate input string into a duplicate string, zero filled

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *result_string      duplicate of string

Returns:        
Type          Name              Definition
int                             TRUE = successful string duplication.
int                             FALSE = error. 

Creator:        Miguel Siu

Creation Date:  Wed Sep 11 14:16:23 PDT 1996

Notes:      
==============================================================================*/
int gen_string2zero_filled_str (
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string)
{
    int     j ;
    int     leading_blank ;
    int     strlength_source ;

    strlength_source = strlen(source_string) ;

    if ( strlength_source <= 0  ||  strlength_source > 40 )
        return FALSE ;

    /* copy the leading zeros and the source string:  */
    leading_blank = 1 ;
    for ( j = 0 ; j < strlength_source ; j ++ )
    {
        if ( source_string[j] == ' '  &&  leading_blank )
            result_string[j] = '0' ;
        else
        {
            result_string[j] = source_string[j] ;
            leading_blank = 0 ; 
        }
    }

    return (TRUE) ;
}



/*==============================================================================
Function:       gen_string2zero_filled_str3_cat

Description:    take input string, right justify into a 3-character
                string, zero filled on the left.  concatenate a '_' then
                this string to the destination string.  
                Example:  
                source_string = "1",
                result_string = "1234567"
                the final, changed, result_string would then be:
                "1234567_001"

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *result_string      result of processing

Returns:        
Type          Name              Definition
int                             TRUE = successful string operation.
int                             FALSE = error. 

Creator:        Lawrence Stevens

Creation Date:  Wed Dec 13 16:12:30 PST 1995

Notes:      
==============================================================================*/
int gen_string2zero_filled_str3_cat(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string)
{
    int     strlength ;
    int     j ;
    char    buf[] = "000" ;

    strlength = strlen(source_string) ;

    if ( strlength <= 0  ||  strlength > 3)
        return FALSE ;

    /* 
    -- copy the source_string, last characters first, to the 
    -- right edge of the buffer:  like "12"  --> "012"
    -- the field is then left as zero-filled.  
    */
    for ( j = 0 ; j < strlength ; j ++ )
    {
        buf[ 2 - j ]  =  source_string[ strlength - 1 - j ] ;
    }

    strcat(result_string, "_" ) ;
    strcat(result_string, buf ) ;

    return TRUE ;

}

/*==============================================================================
Function:       gen_string2_insert_zero_filled_str

Description:    take input string, replace leading blanks by 0,
                then insert, with a '_', at the start of a string.   
                Example:  
                source_string = " 1234",
                result_string = "AF"
                the final, changed, result_string would then be:
                "01234_AF"

Parameters:     
Type        Name                Definition
char        *source_string      string to be converted
char        *result_string      result of processing

Returns:        
Type          Name              Definition
int                             TRUE = successful string operation.
int                             FALSE = error. 

Creator:        Lawrence Stevens

Creation Date:  Wed Dec 13 16:12:30 PST 1995

Notes:      
==============================================================================*/
int gen_string2_insert_zero_filled_str(
    void        *unused_pointer, 
    char        *source_string, 
    char        *result_string)
{
    int     strlength_source ;
    int     strlength_result ;

    int     j ;
    int     leading_blank ;

    strlength_source = strlen(source_string) ;
    if ( strlength_source <= 0  ||  strlength_source > 40 )
        return FALSE ;

    /* 
    -- copy the result_string strlength+1 bytes down 
    -- in memory to make room for the inserted string 
    -- and '_'
    */

    strlength_result = strlen(result_string) ;
    /* 
    -- memmove is a robust, slower version of memcpy, 
    -- where source and destination can overlap OK.  
    -- move the result_string + null character terminator.  
    */
    memmove( result_string+strlength_source+1, result_string, 
        strlength_result+1 ) ;

    /* copy the leading zeros and the source string:  */
    leading_blank = 1 ;
    for ( j = 0 ; j < strlength_source ; j ++ )
    {
        if ( source_string[j] == ' '  &&  leading_blank )
            result_string[j] = '0' ;
        else
        {
            result_string[j] = source_string[j] ;
            leading_blank = 0 ; 
        }
    }

    /* finally, copy the '_' to connect the 2 strings.  */
    result_string[strlength_source] = '_' ;

    return TRUE ;

}



/*==============================================================================
Function:       table_lookupFA2APScat

Description:    Same as table_lookupFA2APS, but the results are concatenated
                to the destination.
                Translate Flight Agency string into APS equivalent string,
                using the indicated conversion table, then concatenate 
                to destination.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Mon Oct  7 15:42:22 PDT 1996

Notes:      
==============================================================================*/
int table_lookupFA2APScat(
    EQUIV_TABLE         *equiv_table,
    char                *source_string, 
    char                *result_string)
{
    char    temp_result[100] ;
    int     equiv_found;

    /* 
    -- put the translation into temp_result, 
    -- then strcat() to result_string:  
    */
    equiv_found = table_lookupFA2APS( equiv_table, source_string, 
        temp_result ) ;

    strcat( result_string, temp_result ) ;

    return(equiv_found);
}


/*==============================================================================
Function:       table_lookupFA2APS

Description:    translate Flight Agency string into APS equivalent string, 
                using the indicated conversion  table           

Parameters:     
Type        Name                Definition
struct EQUIV_TABLE 
            *conversion_table   table holding the equivalences
char        *source_string      source string   
char        *result_string      equivalent result string 

Returns:        
Type          Name              Definition
int                             TRUE = success. Equivalent string was found.
int                             FALSE = error. An equivalence was not found.

Creator:        Miguel Siu

Creation Date:  Tue Jun 20 11:39:48 PDT 1995

Notes:  ASSUMPTION: equivalence tables will always list the Flight Agency 
        string first, followed by the equivalent APS string.
==============================================================================*/
int table_lookupFA2APS(
    EQUIV_TABLE         *equiv_table,
    char                *source_string, 
    char                *result_string)
{
    int i;
    int equiv_found;


    equiv_found = FALSE;
    for ( i = 0 ; equiv_table[i].fa_string ; i++ )
    {
        if (strncmp(source_string, equiv_table[i].fa_string, 
                strlen(equiv_table[i].fa_string)) == 0)
        {
            /* found.  */
            strcpy(result_string, equiv_table[i].aps_string);
            equiv_found = TRUE;     
#ifdef PRINT_DIAG
            printf ("table_lookupFA2APS: translated |%s|to|%s|\n",
                equiv_table[i].fa_string,equiv_table[i].aps_string);
#endif
            break;
        }
    }

    return(equiv_found);
}


/*==============================================================================
Function:       table_lookupAPS2FA 

Description:    translate APS string into its Flight Agency string, 
                using the indicated conversion  table           

Parameters:     
Type        Name                Definition
struct EQUIV_TABLE 
            *conversion_table   table holding the equivalences
char        *source_string      source string   
char        *result_string      equivalent result string 

Returns:        
Type          Name              Definition
int                             TRUE = success. Equivalent string was found.
int                             FALSE = error. An equivalence was not found.

Creator:        Miguel Siu

Creation Date:  Tue Jun 20 11:39:48 PDT 1995

Notes:  ASSUMPTION: equivalence tables will always list the Flight Agency 
        string first, followed by the equivalent APS string.
==============================================================================*/
int table_lookupAPS2FA(
    EQUIV_TABLE         *equiv_table,
    char                *source_string, 
    char                *result_string)
{
    int i;
    int equiv_found;


    equiv_found = FALSE;
    for (i = 0 ; equiv_table[i].fa_string ; i++ )
    {
        if (strncmp(source_string, equiv_table[i].aps_string, 
                strlen(equiv_table[i].aps_string)) == 0)
        {
            /* found.  */
            strcpy(result_string, equiv_table[i].fa_string);
            equiv_found = TRUE;     
#ifdef PRINT_DIAG
            printf ("table_lookupAPS2FA: translated |%s|to|%s|\n",
                equiv_table[i].aps_string,equiv_table[i].fa_string);
#endif
            break;
        }
    }

    return(equiv_found);
}

/*==============================================================================
Function:       table_lookupAPS2FAquoted 

Description:    translate APS string into its Flight Agency string, 
                using the indicated conversion  table           
                AND putting quotes (') around it.  

Parameters:     
Type        Name                Definition
struct EQUIV_TABLE 
            *conversion_table   table holding the equivalences
char        *source_string      source string   
char        *result_string      equivalent result string 

Returns:        
Type          Name              Definition
int                             TRUE = success. Equivalent string was found.
int                             FALSE = error. An equivalence was not found.

Creator:        Miguel Siu

Creation Date:  Tue Jun 20 11:39:48 PDT 1995

Notes:  ASSUMPTION: equivalence tables will always list the Flight Agency 
        string first, followed by the equivalent APS string.
==============================================================================*/
int table_lookupAPS2FAquoted(
    EQUIV_TABLE         *equiv_table,
    char                *source_string, 
    char                *result_string )
{
    int     len ;

    if ( !table_lookupAPS2FA(equiv_table, source_string, result_string ) )
        return FALSE ;

    /* 
    -- now surround the string by 
    -- quotes; convert   "abd" to "'abd'"
    */
    len = strlen(result_string) ; 

    /* ending quote and null byte:  */
    strcpy(result_string+len+1, "'") ;

    /* 
    -- slide the non-null characters to the 
    -- right one byte using the robust memmove(), in 
    -- which the source and destination areas can overlap
    */
    memmove( result_string+1, result_string, len ) ;

    /* 
    -- put in the quote at the start of the string.  
    */
    *result_string = '\'' ;

    return TRUE ;
}

/*=============================================================================
Function:    itoa.c

Description: This function converts a given decimal integer to its equivalent
             character representation returned in a user's string variable.

Input arguments: 
        int:
        = a four byte decimal integer.

Returns:
        char *s  a character string containing the character representation
                 of the input decimal integer.

Creator:  Ted Pavlovitch

Creation date: 03/27/95

===========================================================================*/
void itoa(int n, char *s)
{
int q;

   if( n <0 )
   {
   n = -n;
   *s = '-';
   s++;
   }

   q = n;
   --s;

   do
   {
   s++;
   }
   while( (n/=10) > 0 );

   *(++s) = '\0';

   do
   {
   *(--s) = q%10 + '0';
   }
   while( (q/=10) > 0 );

return;

}  /* end function itoa */

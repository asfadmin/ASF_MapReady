#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   GENconversions.c
Description:    
        Function prototypes for generic string conversions.

Creator:    Miguel Siu

Notes:      
==============================================================================*/
#pragma ident   "@(#)GENconversions.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/global/SCCS/s.GENconversions.h"

#ifndef _GENconversions_
#define _GENconversions_

#include <fa_defs.h>    /* for FA basic definitions... */
#include <stdio.h>

int gen_set_trigger(
	void    *unused_pointer,
	char    *unused_pointer2,
	int		*trigger  ) ;

int gen_default_DMP(
	void    *unused_pointer,
	char    *unused_character_pointer,
	char    *reqm_actid ) ;

int gen_default_ALL_stations(
	void    *unused_pointer,
	char    *unused_character_pointer,
	char    *groundstation ) ;

int gen_default_ASF_station(
	void    *unused_pointer,
	char    *unused_character_pointer,
	char    *groundstation ) ;

int gen_rev2trackstart(
    void        *unused_pointer,
    int         *rev,
    char        *track_start ) ;
 
int gen_rev2trackend(
    void        *unused_pointer,
    int         *rev,
    char        *track_end ) ;
 
int table_lookupFA2APScat(
    EQUIV_TABLE *table,
    char        *source_string, 
    char        *equiv_string);

int table_lookupFA2APS(
    EQUIV_TABLE *table,
    char        *source_string, 
    char        *equiv_string);

int table_lookupAPS2FA(
    EQUIV_TABLE *table,
    char        *source_string, 
    char        *equiv_string);

int table_lookupAPS2FAquoted(
    EQUIV_TABLE *table,
    char        *source_string, 
    char        *equiv_string);    /* surrounds the result with quotes (') */

int gen_asftime2odlcat (
    void        *unused_pointer,
    void        *asftime,        /* 1996:123:23:59:59.999  */
    char        *dest2strcat) ;  /* strcat result - 1996-123T23:59:59.999  */

int gen_asftime2odl (
    void        *unused_pointer,
    void        *asftime,    /* 1996:123:23:59:59.999  */
    char        *odltime) ;  /* 1996-123T23:59:59.999  */

int gen_get_odltime (
    void        *unused_pointer,
    void        *another_unused_pointer,
    char        *destination_current_odltime) ;  /* 1996-123T23:59:59.999  */

int gen_get_asftime (
    void        *unused_pointer,
    void        *another_unused_pointer,
    char        *destination_current_asftime) ;

int gen_tinyint2str(
    void    *unused_pointer,
    char    *tinyintvalue,
    char    *destination ) ;

int gen_int2tinyint(
    void    *unused_pointer,
    int     *integer_value,
    char    *destination ) ;

int gen_int2smallint(
    void    *unused_pointer,
    int     *integer_value,
    char    *destination ) ;

int gen_smallint2str(
    void        *unused_pointer,
    short int   *smallintvalue,
    char        *destination ) ;

int gen_int2str(
    void    *unused_pointer,
    int     *intvalue,
    char    *destination ) ;

int gen_intplus1_2str(
    void    *unused_pointer,
    int     *intvalue,
    char    *destination ) ;

int gen_int2zero_filled_str(
    int     field_width,
    int     intvalue,
    char    *destination ) ;

int gen_int2blank_filled_str(
    int     intvalue,
    int     field_width,
    char    *destination ) ;

int gen_string2float(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    float       *float_value);

int gen_string2int(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    int         *int_value);

int gen_string2tinyint(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    char        *tinyint_value);

int gen_string2qstr(
    void        *unused_pointer,  
    char        *source_string,
    char        *single_quoted_result_string) ;

int gen_string2str(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    char        *result_string);

int gen_string2strcat(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    char        *result_string);

int gen_string2_insert_zero_filled_str(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    char        *result_string ) ;

int gen_string2zero_filled_str(
    void        *unused_pointer,
    char        *source_string,
    char        *result_string) ;

int gen_string2zero_filled_str3_cat(
    void        *unused_pointer,
    char        *source_string,
    char        *result_string) ;

int gen_string2malloc_str(
    void        *table,   /* equivalence table not used by this routine */
    char        *source_string,
    char        **result_string);  /* address of a character pointer   */

int gen_field_is_numeric(
    char        *field,     /* input numeric field to test.    */
    int         length ) ;  /* how many bytes we are to test.  */

int gen_set_trigger(
    void        *unused_pointer,
    char        *unused_pointr2,
    int         *trigger) ;


/* 
-- REPORT_HEADER, REPORT_CONTROL  element declarations 
    These variables are used by the various Flight Agency processors
    (ie: the ingestion processors), and they are available for use by any
    processes that deal with Flight Agency files.
*/

extern int    fa_trigger_REQQ_add_grs_records ;
extern int    fa_trigger_shaq_observation_generation ;
extern int    fa_trigger_dtk_create_FA_downlinks ;
extern int    fa_trigger_dtk_delete_range ;
extern int    fa_trigger_dtk_status_by_blanking ;
extern int    fa_trigger_dtk_fillin_FA_response ;
extern int    fa_trigger_skip_default_values ;
extern int    fa_trigger_dtk_list_handling_for_STGS ;
extern int    fa_trigger_SHAQP_dtk_consolidation ;
extern int    fa_trigger_read_variable_SHAQP_header ;
extern int    fa_trigger_always_create_response_file ;
extern int    fa_trigger_reqm_secondary_record_ingestion ;
extern int    fa_trigger_MPSG_station_mask_check ;
extern int    fa_trigger_RES_rej_downtime ;
extern int    fa_trigger_RES_fillin_response ;
extern int    fa_trigger_CSA_dl_mapping ;
extern int    fa_antenna_id ;
extern int    fa_cyclic_counter ;
extern int    fa_counter ;
extern int    fa_dtkid ;
extern int    fa_number_of_records ;
extern int    fa_number_of_primary_recs ;
extern int    fa_number_of_secondary_recs ;
extern int    fa_number_of_observations ;
extern int    fa_number_of_error_records ;
extern int    fa_number_of_subrecords ;
extern int    fa_record_size ;
extern int    fa_subrecord_size ;
extern int    *fa_subrecord_control ;
extern char   *fa_filename ;
extern char   *fa_file_dest ;
extern char   fa_flight_agency[] ;
extern char   fa_station_id[] ;
extern char   fa_file_type[] ;      /*   values like MWOS, AREQ, etc.     */
extern char   fa_activity_type[] ;  /*   used to get permissions          */
extern char   fa_file_id[] ;
extern char   fa_sat_id[] ;
extern char   fa_creation_date[] ;
extern char   fa_first_track_start[] ;
extern char   fa_last_track_end[] ;
extern char   fa_argument_start_time [] ;
extern char   fa_argument_stop_time[] ;
extern char   fa_aos_track_start[] ;
extern char   fa_los_track_end[] ;
extern char   fa_recording_start_time[] ;
extern char   fa_recording_stop_time[] ;
extern char   fa_file_start_rev[] ;
extern char   fa_file_stop_rev[] ;
extern char   fa_start_yyyymmdd[] ;
extern char   fa_stop_yyyymmdd[] ;
extern char   fa_file_start_time[] ;
extern char   fa_file_stop_time[] ;
extern char   fa_file_start_rev[] ;
extern char   fa_file_stop_rev[] ;
extern char   fa_start_yyyymmdd[] ;
extern char   fa_stop_yyyymmdd[] ;
extern char   fa_processing_flag[] ;
extern char   fa_record_write_flag[] ;
extern char   fa_acquisition_mode[] ;
extern char   fa_downlink_mode[] ;
extern char   fa_dtk_quicklook_flag ;   /* Y or N.  */
extern char   fa_schedule_link[] ;
extern FA_KEY_STRUCT   *fa_sch_link_control ;
extern FILE   *fa_output_file_fp ;
extern llist  *fa_dtk_report_list ;



#endif /* _GENconversions_ */

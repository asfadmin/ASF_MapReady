#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	ESAconversions.c

Description:ESA conversion routines, which are used to decode/translate
			ESA File string values.

External Functions Defined:
	int ESAc_set_READ_TO_EOF sets flag to read to end of file.
	int ESAc_deFAUlt_sensor	 returns the ESA default sensor. Requires no input.
	int	ESAc_default_darid	 returns the ESA default darid.  Requires no input.
	int	ESAc_default_transid returns the ESA default transid.Requires no input.
	int	ESAc_default_actid	 returns the ESA default actid.  Requires no input.
	int ESAc_default_shaqp_stat 	returns the ESA shaqp default status 'SCH'.
							 requires no input.
	int ESAc_default_shaqp_type 	returns the ESA shaqp default activity type
							'SHAQP'. Used for permissions in multi-user.
	int ESAc_get_shaqp_sat	 extracts the satellite from SHAQP filename.
	int ESAc_get_shaqp_creation_date
							 extracts the creation date from SHAQP filename.
	int ESAc_dd_mmm_yy_hh_mm_ss_ccc2asftime
							 convert a ESA time value string to asf time format
	int ESAc_yymmdd_pad_hh_mm_ss2asftime 
							 convert embedded ESA time format to asf time format
	int ESAc_yyyymmddhhmmssccc2asftime
							 convert embedded ESA time format to asf time format
	int ESAc_time_duration2asftime
							 convert ESA time + duration into asf time
	int ESAc_default_subrecords
							returns zero (0) subrecords. Requires no input.
	int ESAc_default_subrec_size
							 sets the default subrecord size (0 bytes)
	int ESAc_shaqp_time_duration2strttime
			convert embedded ESA time + duration format to asf start time format
	int ESAc_shaqp_time_duration2stoptime
			convert embedded ESA time + duration format to asf stop time format

File Scope Functions:
	
External Variables Defined:
	EQUIV_TABLE   ESA_facility_id[] 	contains facility id values
	EQUIV_TABLE   ESA_dtk_status[] 		contains datatake status values
	
File Scope Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)ESAconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.ESAconversions.c"

#include <stdlib.h>          
#include <libgen.h>           /* for strfind()         */
#include <string.h>           /* for strncpy(), strlen(), strcpy()        */
#include <timeconv.h>            /* for tc_esadaytime2asf(), tc_esa2asf()    */
#include "GENconversions.h" 
#include "ESAconversions.h"
/* for APS basic definitions */   
#include "dapps_defs.h"    

/* FOR DATABASE TABLES        */
#include "db_dtk.h"				/* for dtk table             */

/* FOR permission definitions */
#include <mu_utilities.h>

/* for pre-defined DTKM values */
#include "dtkm_utilities.h"

/* for debugging purposes  */
#define PRINT_DIAG 1
#undef  PRINT_DIAG


EQUIV_TABLE   ESA_dtk_status[]= 
{   {"SHAQ_", "SCH"},
	{"MPSG_", "PLN"},
	/*SHQP     use routine ESAc_default_shaqp_stat */
	{NULL, NULL}
} ;

EQUIV_TABLE   ESA_activity_type[]= 
{   {"SHAQ_", MU_SHAQ },
	{"MPSG_", MU_MPSG },
	/*SHQP     use routine ESAc_default_shaqp_type */
	{NULL, NULL}
} ;

EQUIV_TABLE   ESA_facility_id[]= 
{   {"AF", "ASF"}, 
	{"MM", "MCM"}, 
	{NULL, NULL}
} ;

EQUIV_TABLE   ESA_mu_facility_id[]= 
{   {"AF", MU_ASF_STATIONID }, 
	{"MM", MU_MCM_STATIONID }, 
	{NULL, NULL}
} ;


EQUIV_TABLE	ESA_plan_flags[]=
{	{"S", "READ"},
	{"M", "SKIP"},
	{"D", "SKIP"},
	{NULL, NULL}
} ;


/*==============================================================================
Function:      array of VALUE_DEFS ESA_valuedefs_shaqp 

Description:    This array is used to describe the field values in the ESA SHAQP
				file that can be processed.  The array describes each field's

					source 		(the type of record which produced it)
					use			(value placed in a report or in a DB record)
					offset		(from the beginning of the record)
					length		(number of bytes in field)
					conversion_function		(if needed)
					equivalence_table		(if needed by conversion_function)
					destination (either an indexed location in DB record or 
								 the address of a variable)

Creator:        Miguel Siu

Creation Date:  Wed Sep  4 13:19:46 PDT 1996

Notes:	See file fa_defs.h for more information regarding available field source
		and field use values.


-- Sample Header:

In case of problems please contact 0039-6-9416710   (primary)
                                   0039-6-94180-692 (back-up)
 
             PRELIMINARY SCHEDULE OF ACQUISITIONS
(Segment start/end times in seconds since ascending node)
 
ST PASS_NUMBER ''  ASC_NODE_TIME        SEGMENT_START SEGMENT_END
-- ----------- --- -------------------- ------------- -----------

-- Sample Records:

AF        5550     12-MAY-96 18:59:40            1748        2210
AF        5551     12-MAY-96 20:40:15            1777        2122

==============================================================================*/
VALUE_DEFS ESA_valuedefs_shaqp[] =  
{
	/* 
	-- HEADER FIELDS:  
		< There are no header processing instructions in the SHAQP file >
		< But we do have a special read_variable_SHAQP_header action below>
	*/


	/* 
	-- DEFAULT FIELDS   :
	*/

	/* 
	-- the next 2 instructions use the global fa_filename variable
	*/
	/*  0 shaqp */
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_get_shaqp_sat,	NULL,		DTK_SAT} ,
	{FA_DEFAULT,	REPORT_HEADER,
		-1,	-1,	ESAc_get_shaqp_creation_date,		
									NULL,		(int) fa_creation_date} ,

    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_sensor,NULL,		DTK_SENSOR},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_actid,	NULL,		DTK_ACTID},
	/*  4 shaqp */
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_transid,NULL,		DTK_TRANSID},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_darid,	NULL,		DTK_DARID},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_shaqp_stat, 
									NULL,		DTK_DTKSTAT},
	/*  7 shaqp */
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	ESAc_set_READ_TO_EOF,	
									NULL,		(int) &fa_number_of_records},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	ESAc_default_shaqp_type,	
									NULL,		(int) fa_activity_type},
	/*
	-- gen_set_trigger populates triggers with a TRUE value
	*/
	/*  9 shaqp */
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_set_trigger,	NULL,
							(int) &fa_trigger_SHAQP_dtk_consolidation},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_set_trigger,	NULL,
							(int) &fa_trigger_read_variable_SHAQP_header},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_set_trigger,	NULL,
							(int) &fa_trigger_dtk_create_FA_downlinks},
	/* 
	-- fa_subrecord_size is informational, not used by any file utilities
					< a VALUE_DEFS entry has been ommitted here >
	*/

	/* 
	-- put the station part of the DTK_FA_SCHEDULE_LINK field.
	-- (There may be several observation records with same DTK_FA_SCHEDULE_LINK 
	-- value but that's OK because they will all be linked by the same downlink)
	*/
	/* 12 shaqp */
    {FILE_RECORD,	REPORT_RECORD,
		0,	2,	gen_string2str, 	NULL, 		DTK_FA_SCHEDULE_LINK}, 
    {FILE_RECORD,	REPORT_RECORD,
		0,	2,	table_lookupFA2APS, (void *)ESA_facility_id,
												DTK_STATION_ID}, 
	/* the following is needed to get permission in multi-user environment */
	/* 14 shaqp */
    {FILE_RECORD,	REPORT_CONTROL,
		0,	2,	table_lookupFA2APS, (void *)ESA_mu_facility_id,
												(int) fa_station_id}, 
    {FILE_RECORD,	REPORT_RECORD,
		9,	5,	gen_string2int,		NULL,		DTK_REV}, 

	/* 
	-- SHAQP populates the DTK_FADTKID field, just as the SHAQ does.
	-- It does not have the 'schedule_originator' information which would
	-- be needed to set up a DTK_FADTKID which would be similar to the 
	-- old SHAQ value.  
	-- This value is needed to match up SHAQP datatake proposals
	-- to datatakes already in the database.
	*/
	/* 16 shaqp */
	{FILE_RECORD,	REPORT_RECORD,
		9,	5,	gen_string2zero_filled_str,		
									NULL,		DTK_FADTKID}, 
	{FILE_RECORD,	REPORT_RECORD,
		0,	2,	gen_string2strcat,	NULL,		DTK_FADTKID},

	/* 
	-- complete the DTK_FA_SCHEDULE_LINK field by going from 
	-- "AF" to "12345_AF"  
	*/
	/* 18 shaqp */
    {FILE_RECORD,	REPORT_RECORD,
		9,	5,	gen_string2_insert_zero_filled_str,	
									NULL,		DTK_FA_SCHEDULE_LINK}, 

	/*
	-- NOTE: we no longer need AOS_TIME/LOS_TIME. Any antenna-specific
	-- procedures are now handled during conflict analysis of each 
	-- individual datatake proposal, and during creation of the WOS file
	-- which contains the instructions for the antenna.
	--
				< 2 VALUE_DEFS entry has been ommitted here >
	*/
	/* 19 shaqp */
    {FILE_RECORD,	REPORT_RECORD,
		19,	46,	ESAc_shaqp_time_duration2strttime,
									NULL,	DTK_STRTTIME},
    {FILE_RECORD,	REPORT_RECORD,
		19,	46,	ESAc_shaqp_time_duration2stoptime,
									NULL,	DTK_STOPTIME},
	{0, 0, -1, -1, NULL, NULL, NULL} 
} ;                                                           

FA_FILEDEF ESA_filedef_shaqp =
{
	3,	/* test header string (3 chars) */ 
	71,	/* data record:  70 chars + newline    */
	ESA_valuedefs_shaqp
} ;




/*==============================================================================
Function:      array of VALUE_DEFS ESA_valuedefs_shaq 

Description:    This array is used to describe the field values in the ESA SHAQ
				file that can be processed.  The array describes each field's

					source 		(the type of record which produced it)
					use			(value placed in a report or in a DB record)
					offset		(from the beginning of the record)
					length		(number of bytes in field)
					conversion_function		(if needed)
					equivalence_table		(if needed by conversion_function)
					destination (either an indexed location in DB record or 
								 the address of a variable)

Creator:        Miguel Siu

Creation Date:  Wed Jul 12 16:20:58 PDT 1995

Notes:	See file fa_defs.h for more information regarding available field source
		and field use values.


-- Sample Header:

SHAQ_930302ECAF8825.E113:20:53SA1H

-- Sample Records:

00212AFU31-JUL-1991 20:54:00.00031-JUL-1991 21:10:00.00031-JUL-1991 20:54:28.28331-JUL-1991 21:01:28.283                                                                                                                                                
08524AFU03-MAR-1993 05:43:51.57703-MAR-1993 05:49:28.26403-MAR-1993 05:44:13.58903-MAR-1993 05:49:08.264                                                                                                                                                
==============================================================================*/
VALUE_DEFS ESA_valuedefs_shaq[] =  
{
#ifdef TEMPLATE
	{FILE_HEADER,	REPORT_CONTROL,	 0, 0, function, equiv_table, dest},
	{FILE_HEADER,	REPORT_HEADER,	 0, 0, function, equiv_table, dest},
    {FILE_HEADER,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},

    {FILE_RECORD,	REPORT_HEADER,	 0, 0, function, equiv_table, dest},
    {FILE_RECORD,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
    {FA_DEFAULT,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
#endif

	/* 
	-- HEADER FIELDS:  
	*/
	/*  0 shaq */
	{FILE_HEADER,	REPORT_HEADER,
		0,	22,	gen_string2malloc_str, NULL,		(int) &fa_filename} ,
    {FILE_HEADER,	REPORT_RECORD,
		0,	5,	table_lookupFA2APS, (void *)ESA_dtk_status,	
												DTK_DTKSTAT},
    {FILE_HEADER,	REPORT_CONTROL,
		0,	5,	table_lookupFA2APS, (void *)ESA_activity_type,	
												(int) fa_activity_type},
	/*  3 shaq */
	{FILE_HEADER,	REPORT_HEADER,
		5,	25,	ESAc_yymmdd_pad_hh_mm_ss2asftime,		
									NULL,		(int) fa_creation_date} ,
	{FILE_HEADER,	REPORT_CONTROL,
		15,	4,	gen_string2int,		NULL,		(int) &fa_cyclic_counter} ,
    {FILE_HEADER,	REPORT_RECORD,
		20,	2,	gen_string2str,		NULL,		DTK_SAT} ,
	/*  6 shaq */
    {FILE_HEADER,	REPORT_RECORD,
		13,	2,	table_lookupFA2APS, (void *)ESA_facility_id,
												DTK_STATION_ID}, 
    {FILE_HEADER,	REPORT_CONTROL,
		13,	2,	table_lookupFA2APS, (void *)ESA_mu_facility_id,
												(int) fa_station_id}, 
	/* 
	-- put the station part of the DTK_FA_SCHEDULE_LINK field.
	-- (There may be several observation records with same DTK_FA_SCHEDULE_LINK 
	-- value but that's OK because they will all be linked by the same downlink)
	*/
	/*  8 shaq */
    {FILE_HEADER,	REPORT_RECORD,
		13,	2,	gen_string2str, 	NULL, 		DTK_FA_SCHEDULE_LINK}, 

	/* 
	-- DEFAULT FIELDS   :
	*/

	/*  9 shaq */
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_shaq_sensor,NULL,		DTK_SENSOR},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_shaq_actid,NULL,		DTK_ACTID},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_transid,	NULL,		DTK_TRANSID},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_darid,		NULL,		DTK_DARID},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	ESAc_set_READ_TO_EOF,	
										NULL,	(int) &fa_number_of_records},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_set_trigger,		NULL,
								(int) &fa_trigger_shaq_observation_generation},
	/* 
	-- DEFAULT FIELD
	-- fa_subrecord_size is informational, not used by any file utilities
	*/
	/* 15 shaq */
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	ESAc_default_subrec_size,	
									NULL,		(int) &fa_subrecord_size},

    {FILE_RECORD,	REPORT_RECORD,
		0,	5,	gen_string2int,		NULL,		DTK_REV}, 

	/* 
	-- 8-char value = rrrrrAF    where rrrrr = rev number, AF = "AF", 
	--    (format rrrrrAFo which uses o = schedule originator 
	--     is no longer used)
	*/
	/* 17 shaq */
    {FILE_RECORD,	REPORT_RECORD,
		0,	7,	gen_string2str,	    NULL,		DTK_FADTKID},

	/* 
	-- complete the DTK_FA_SCHEDULE_LINK field by going from 
	-- "AF" to "12345_AF"  
	*/
	/* 18 shaq */
    {FILE_RECORD,	REPORT_RECORD,
		0,	5,	gen_string2_insert_zero_filled_str,	
									NULL,		DTK_FA_SCHEDULE_LINK}, 

	/*
	-- NOTE: we no longer need AOS_TIME/LOS_TIME. Any antenna-specific
	-- procedures are now handled during conflict analysis of each 
	-- individual datatake proposal, and during creation of the WOS file
	-- which contains the instructions for the antenna.
	--
				< 2 VALUE_DEFS entry has been ommitted here >
	*/
	/* 19 shaq */
    {FILE_RECORD,	REPORT_RECORD,
		56,	24,	ESAc_dd_mmm_yy_hh_mm_ss_ccc2asftime,
									NULL,	DTK_STRTTIME},
    {FILE_RECORD,	REPORT_RECORD,
		80,	24,	ESAc_dd_mmm_yy_hh_mm_ss_ccc2asftime,
									NULL,	DTK_STOPTIME},
	{0, 0, -1, -1, NULL, NULL, NULL} 
} ;                                                           

FA_FILEDEF ESA_filedef_shaq =
{
	36,		/* 
			-- Fixed portion:  30 + newline, 
			-- Variable Portion 4 + newline 
			-- Total:  36 characters.  
			*/
	249,	/* data record:  248 + newline    */
	ESA_valuedefs_shaq
} ;



/*==============================================================================
Function:      array of VALUE_DEFS ESA_valuedefs_mpsg 

Description:    This array is used to describe the field values in the ESA GAP 
				file (MPSG) that can be processed.  
				The array describes each field's

					source 		(the type of record which produced it)
					use			(value placed in a report or in a DB record)
					offset		(from the beginning of the record)
					length		(number of bytes in field)
					conversion_function		(if needed)
					equivalence_table		(if needed by conversion_function)
					destination (either an indexed location in DB record or 
								 the address of a variable)

Creator:        Miguel Siu

Creation Date:  Wed Jul 12 16:20:58 PDT 1995

Notes:	See file fa_defs.h for more information regarding available field source
		and field use values.
		There are some especialized field values that are used to control the
		processing of whole records or groups of records.  These are:

		fa_processing_flag			(indicates whether this data record, and its
								 subrecords are to be processed or not)

-- Sample Header:

MPSG_950615ECAF0626.E119:22:11G1995032109014935019960114222929230020204971924804288005010715949596001165000009854210090000002701334                                        000000000000000000000000000000000000000000000000000000000000000000                                        000000000000000000000000000000000000000000000000000000000000000000                                        000000000000000000000000000000000000000000000000000000000000000000                                        000000000000000000000000000000000000000000000000000000000000000000                                        000000000000000000000000000000000000000000000000000000000000000000     2027321132    

-- Sample Records:

20273SSARNG 19950601000329054000700717079.07055.42
20274DSARNG 19950601012816898000255758042.92053.23
==============================================================================*/
VALUE_DEFS ESA_valuedefs_mpsg[] =  
{
	/* 
	-- HEADER FIELDS:  
	*/

	/*  0 mpsg */
	{FILE_HEADER,	REPORT_HEADER,
		0,	22,	gen_string2malloc_str, 	NULL,	(int) &fa_filename} ,
	{FILE_HEADER,	REPORT_HEADER,
		5,	25,	ESAc_yymmdd_pad_hh_mm_ss2asftime,		
										NULL,	(int) fa_creation_date} ,
	{FILE_HEADER,	REPORT_CONTROL,
		15,	4,	gen_string2int,			NULL,	(int) &fa_counter} ,
    {FILE_HEADER,	REPORT_RECORD,
		20,	2,	gen_string2str,			NULL,	DTK_SAT} ,

	/* the file name indicates status:  MPSG -> PLN   */
	/*  4 mpsg */
    {FILE_HEADER,	REPORT_RECORD,
		0,	5,	table_lookupFA2APS, 	(void *)ESA_dtk_status,	
												DTK_DTKSTAT},
    {FILE_HEADER,	REPORT_CONTROL,
		0,	5,	table_lookupFA2APS, 	(void *)ESA_activity_type,	
												(int) fa_activity_type},
	/*  6 mpsg */
    {FILE_HEADER,	REPORT_RECORD,
		13,	2,	table_lookupFA2APS, 	(void *)ESA_facility_id,
												DTK_STATION_ID}, 
	/* 
	-- put the station part of the DTK_FA_SCHEDULE_LINK field.
	-- (There may be several observation records with same DTK_FA_SCHEDULE_LINK 
	-- value but that's OK because they will all be linked by the same downlink)
	*/
    {FILE_HEADER,	REPORT_RECORD,
		13,	2,	table_lookupFA2APS, 	(void *)ESA_facility_id,
												DTK_FA_SCHEDULE_LINK}, 

	/* 
	-- DEFAULT FIELDS   
	*/

	/*  8 mpsg */
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_sensor,	NULL,	DTK_SENSOR},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_actid,		NULL,	DTK_ACTID},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_transid,	NULL,	DTK_TRANSID},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	ESAc_set_READ_TO_EOF,	
										NULL,	(int)&fa_number_of_records},
	/* the following is used to get permissions in multi-user environment */
	/* 12 mpsg */
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_default_ALL_stations,
										NULL,	(int)fa_station_id},
    {FA_DEFAULT,	REPORT_RECORD,
		-1,	-1,	ESAc_default_darid,	NULL,		DTK_DARID},
	/*
	-- gen_set_trigger populates triggers with a TRUE value
	*/
	/* 14 mpsg */
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_set_trigger,	NULL,
									(int) &fa_trigger_MPSG_station_mask_check},
    {FA_DEFAULT,	REPORT_CONTROL,
		-1,	-1,	gen_set_trigger,	NULL,
									(int) &fa_trigger_dtk_create_FA_downlinks},

	/* 
	-- DATA RECORD FIELDS:  
	*/

	/* 
	-- complete the DTK_FA_SCHEDULE_LINK field by going from 
	-- "AF" to "12345_AF"  
	*/
	/* 15 mpsg */
    {FILE_RECORD,	REPORT_RECORD,
		0,	5,	gen_string2_insert_zero_filled_str,	
									NULL,		DTK_FA_SCHEDULE_LINK}, 

	/* if the value of this field is not 'S', then SKIP the record.  */
	/* 16 mpsg */
    {FILE_RECORD,	REPORT_CONTROL,
		5,	 1,	table_lookupFA2APS,			(void *)ESA_plan_flags,	
													(int) fa_processing_flag},
    {FILE_RECORD,	REPORT_RECORD,
		0,	 5,	gen_string2int,				NULL,	DTK_REV },
    {FILE_RECORD,	REPORT_RECORD,
		12,	17,	ESAc_yyyymmddhhmmssccc2asftime,	
											NULL,	DTK_STRTTIME},
	/* 19 mpsg */
    {FILE_RECORD,	REPORT_RECORD,
		12,	26,	ESAc_time_duration2asftime,	NULL,	DTK_STOPTIME},
	{0, 0, -1, -1, NULL, NULL, NULL} 
} ;                                                           

FA_FILEDEF ESA_filedef_mpsg = 
{
	682,  			/* 
					-- 30 + newline for Fixed Portion, 1x(650 + newline) 
					-- for variable portion.
					-- Total of 30 +1 + 1x(650+1) = 682 bytes.  
					-- as of now, Thu Jan  4 15:51:34 PST 1996,
					-- there is only one phase record instead of 
					-- 6 phase records in the GAP file for both 
					-- E1 and E2.  
					*/
	51,                   /* data record composed of 50 bytes + newline   */
	ESA_valuedefs_mpsg    /* pointer to the descriptions				  */
} ;


FA_FILENAME ESA_files[] =
{
	{"ESA", "SHAQ",	&ESA_filedef_shaq,	NULL,	NULL},
	{"ESA", "SHQP",&ESA_filedef_shaqp,	NULL,	NULL},
	{"ESA", "MPSG",	&ESA_filedef_mpsg,	NULL,	NULL},
	{NULL, NULL, NULL}
} ;


/*==============================================================================
Function:      ESAc_dd_mmm_yy_hh_mm_ss_ccc2asftime 

Description:    translate ESA time string into asf time string

Parameters:     
Type		Name        		Definition
char 		*source_string 		"dd-mmm-yyyy hh:mm:ss:ccc"	ESA time string
char 		*result_string		"yyyy:ddd:hh:mm:ss.ccc"		asf time string

Returns:        
Type		  Name        		Definition
int								TRUE = success.
int								FALSE = error.

Creator:        Miguel Siu

Creation Date:  Thu Jun 22 13:28:29 PDT 1995

Notes:  This call allows us to access routine tc_esadaytime_asf() while 
		maintaining the function prototype for ESAc_* conversion routines.
		Time conversion returns success = 1   errors = 0
==============================================================================*/
int ESAc_dd_mmm_yy_hh_mm_ss_ccc2asftime(
	void		*unused_pointer, 
	char 		*source_string, 
	char 		*result_string)
{
    return ( tc_esadaytime2asf(source_string,result_string) ) ;
}


/*==============================================================================
Function:      ESAc_yyyymmddhhmmssccc2asftime 

Description:   Translate ESA time string into asf time string 

Parameters:     
Type		Name        		Definition
char 		*source_string 		"yyyymmhhmmddssccc" 	ESA x-time string
char 		*result_string		"yyyy:ddd:hh:mm:ss.ccc"	asf time string

Returns:        
Type		  Name        		Definition
int								TRUE = success.
int								FALSE = error.

Creator:        Miguel Siu

Creation Date:  Tue Jul 11 08:34:13 PDT 1995

Notes:		
==============================================================================*/
int ESAc_yyyymmddhhmmssccc2asftime(
	void		*unused_pointer, 
	char 		*source_string, 
	char 		*result_string)
{
	char	esa_string[] =	"yyyymmddhhmmss" ;
	char	esa_msec[] =	"ccc" ;

	strncpy (esa_string,source_string,		strlen(esa_string) ) ;
	strncpy (esa_msec, source_string+15,	strlen(esa_msec) ) ;

	if ( !tc_esa2asf(esa_string, result_string) )
		return (FALSE) ;

	strncpy (result_string+18, esa_msec, strlen(esa_msec) ) ;		
#ifdef PRINT_DIAG
	printf("in ESAc_yyyymmddhhmmssccc2asftime:%s\n",result_string) ;
#endif

	/* check validity of resulting asftime */
	return ( tc_validate_asf_datetime(result_string) != TRUE ? FALSE : TRUE ) ;
}


/*==============================================================================
Function:      ESAc_time_duration2asftime 

Description:   convert embedded starttime + duration_time into asf time 

Parameters:     
Type		Name        		Definition
char 		*source_string 		"yyyymmddhhmmssccchhmmssccc" ESA time string
															     and duration
char 		*result_string		"yyyy:ddd:hh:mm:ss.ccc"		 asf time string

Returns:        
Type		  Name        		Definition
int								TRUE = success.
int								FALSE = error.

Creator:        Miguel Siu

Creation Date:  Tue Jul 11 10:09:27 PDT 1995

Notes:		
==============================================================================*/
int ESAc_time_duration2asftime(
	void		*unused_pointer, 
	char 		*source_string, 
	char 		*result_string)
{
	char		starttime[] = 	"yyyy:ddd:hh:mm:ss.ccc"  ;
	char		two_bytes[] =	"xx"  ;
	char		*ptr  ;			/* pointer to hhmmssccc */ 
	double		ndays  ;
	float		hours  ;
	float		minutes  ;
	float		seconds  ;
	float		mseconds  ;

	if(!ESAc_yyyymmddhhmmssccc2asftime(unused_pointer, source_string, starttime) )
		return (FALSE) ;

	ptr = source_string+17 ;
#ifdef PRINT_DIAG
	printf("in ESA_time_duration:%s %s\n",starttime, ptr) ;
#endif

	/*
	-- convert hours
	*/
	strncpy(two_bytes, ptr, strlen(two_bytes) ) ;
	if ( !gen_field_is_numeric( two_bytes, 2)  )
		return FALSE ;
	hours = atof(two_bytes) ;
	ptr += 2 ;

	/*
	-- convert minutes 
	*/
	strncpy(two_bytes, ptr, strlen(two_bytes) ) ;
	if ( !gen_field_is_numeric( two_bytes, 2)  )
		return FALSE ;
	minutes = atof(two_bytes) ;
	ptr += 2 ;
		
	/*
	-- convert seconds
	*/
	strncpy(two_bytes, ptr, strlen(two_bytes) ) ;
	if ( !gen_field_is_numeric( two_bytes, 2)  )
		return FALSE ;
	seconds = atof(two_bytes) ;
	ptr += 2 ;

	/*
	-- convert milliseconds
	*/
	if ( !gen_field_is_numeric( ptr, strlen(ptr))  )
		return FALSE ;
	mseconds = atof(ptr) ;

#ifdef PRINT_DIAG
	printf("(cont.) hh:%6.2f mm:%6.2f ss:%6.2f cc:%6.2f\n",
		hours, minutes, seconds, mseconds) ;
#endif

	if (seconds >= 60.0) return FALSE ;
	seconds += mseconds /1000.0 ;

	if (minutes >= 60.0) return FALSE ;
	minutes += seconds  /60.0 ;

	if (hours >= 24.0) return FALSE ;
	hours	+= minutes  /60.0 ;

	ndays = hours /24.0  ;

#ifdef PRINT_DIAG
	tc_asf_add_ndays(starttime, ndays, result_string) ;
	printf("(cont.) result:%s\n",result_string) ;
#endif

	return ( tc_asf_add_ndays(starttime, ndays, result_string) ) ;

}

/*==============================================================================
Function:      ESAc_default_sensor 

Description:    returns the ESA default sensor.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default sensor returned.

Creator:        Miguel Siu

Creation Date:  Thu Jun 22 13:35:20 PDT 1995

Notes:		
==============================================================================*/
int ESAc_default_sensor(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*sensor)
{
	strcpy(sensor, "SAR") ;
	return(TRUE) ;
}


/*==============================================================================
Function:      ESAc_default_shaq_sensor 

Description:    returns the ESA default sensor.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default sensor returned.

Creator:        Miguel Siu

Creation Date:  Thu Mar 27 08:34:08 PST 1997

Notes:		
	The SHAQ contains downlink information ONLY, so we use the default sensor
	value DTKM_SENSOR_REALTIME_DOWNLINK_CODE.
==============================================================================*/
int ESAc_default_shaq_sensor(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*sensor)
{
	strcpy(sensor, DTKM_SENSOR_REALTIME_DOWNLINK_CODE) ;
	return(TRUE) ;
}


/*==============================================================================
Function:      ESAc_default_darid 

Description:    returns the ESA default darid.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default darid returned.

Creator:        Miguel Siu

Creation Date:  Tue Jun 20 16:14:52 PDT 1995

Notes:		
==============================================================================*/
int ESAc_default_darid(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	int 		*darid)
{
	*darid = 0  ;
	return(TRUE)  ;
}


/*==============================================================================
Function:       ESAc_default_transid

Description:    returns the ESA default transid.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default transid returned.

Creator:        Miguel Siu

Creation Date:  Thu Jun 22 13:36:31 PDT 1995

Notes:		Always returns the transid "00" which corresponds to REALTIME
			transmission.
==============================================================================*/
int ESAc_default_transid(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*transid)
{
	strcpy(transid, "00")  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:       ESAc_default_actid

Description:    returns the ESA default activity id.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default activity id returned.

Creator:        Miguel Siu

Creation Date:  Thu Jun 22 13:36:31 PDT 1995

Notes:		Always returns the actid which corresponds to REALTIME
			observation; used to create an observation or 'sensing activity'
			record.  Corresponding downlinks are created elsewhere during
			file processing (see dtk_create_esa_downlinks).
==============================================================================*/
int ESAc_default_actid(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*actid)
{
	strcpy(actid, DTKM_ACTID_REALTIME_OBSERVATION_CODE)  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:       ESAc_default_shaq_actid

Description:    returns the ESA default activity id.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int							TRUE = success. ESA default activity id returned.

Creator:        Miguel Siu

Creation Date:  Thu Mar 27 08:38:07 PST 1997

NOTES:
	The SHAQ contains downlink information ONLY, so we use the default actid
	value DTKM_SENSOR_REALTIME_DOWNLINK_CODE.
==============================================================================*/
int ESAc_default_shaq_actid(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*actid)
{
	strcpy(actid, DTKM_ACTID_REALTIME_OBSERVATION_CODE)  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:       ESAc_default_shaqp_stat

Description:    returns the ESA default status for SHAQP dtk proposals.

Parameters:     NO input required.

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default status returned.

Creator:        Miguel Siu

Creation Date:  Thu Sep  5 15:43:28 PDT 1996

Notes:		Always returns the status "SCH" which corresponds to SCHEDULED
			activity.
==============================================================================*/
int ESAc_default_shaqp_stat(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*status)
{
	strcpy(status, "SCH")  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:       ESAc_default_shaqp_type

Description:    Returns the ESA default activity type. Used to get
				permissions in multi-user environment.

Parameters:     NO input required.

Returns:       	Always TRUE. 

Creator:        Miguel Siu

Creation Date:  Tue Dec 17 08:34:04 PST 1996

Notes:
==============================================================================*/
int ESAc_default_shaqp_type(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*type)
{
	strcpy(type, MU_SHAQP )  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:       ESAc_set_READ_TO_EOF

Description:   returns the value READ_TO_EOF to the  flag indicated by user.

Parameters:     NO input required

Returns:        
Type		  Name        		Definition
int								TRUE = success. READ_TO_EOF value returned. 

Creator:        Miguel Siu

Creation Date:  Mon Jul 31 13:36:14 PDT 1995

Notes:		
==============================================================================*/
int ESAc_set_READ_TO_EOF(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	int 		*read_flag)
{
	*read_flag = READ_TO_EOF  ;
	return(TRUE)  ;
}

/*==============================================================================
Function:       ESAc_default_subrecords(

Description:    returns ESA default subrecords (always equals zero)

Parameters:     NO input required

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA default subrecords returned.

Creator:        Miguel Siu

Creation Date:  Wed Jul 12 11:32:48 PDT 1995

Notes:		
==============================================================================*/
int ESAc_default_subrecords(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	int 		*subrecords)
{
	*subrecords = 0  ;
	return(TRUE)  ;
}


/*==============================================================================
Function:       ESAc_default_subrecords(

Description:    returns ESA default subrecord size (always equals zero)

Parameters:     NO input required

Returns:        
Type		  Name        		Definition
int								TRUE = success. ESA subrecord size returned.

Creator:        Miguel Siu

Creation Date:  Wed Jul 12 11:32:48 PDT 1995

Notes:		
==============================================================================*/
int ESAc_default_subrec_size(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	int 		*subrecsize)
{
	*subrecsize = 0  ;
	return(TRUE)  ;
}


/*==============================================================================
Function:      ESAc_get_shaqp_sat 

Description:    Scans the global variable fa_filename for an instance of
				"E1" or "e1" or "E2" or "e2" in order to determine satellite.

Parameters:     

Returns:        TRUE/FALSE 

Creator:        Miguel Siu

Creation Date:  Thu Sep  5 15:49:42 PDT 1996

Notes:	Uses the global variable fa_filename	
==============================================================================*/
int ESAc_get_shaqp_sat(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*sat)
{
	if (strfind(fa_filename, ".E1") != -1 )
		strcpy(sat, "E1")  ;
	else if (strfind(fa_filename, ".e1") != -1 )
		strcpy(sat, "E1")  ;
	else if (strfind(fa_filename, ".E2") != -1 )
		strcpy(sat, "E2")  ;
	else if (strfind(fa_filename, ".e2") != -1 )
		strcpy(sat, "E2")  ;
	else
		return (FALSE) ;

	return (TRUE) ;
}

	char datestr[8] ;
/*==============================================================================
Function:       ESAc_get_shaqp_creation_date

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Sep  5 15:50:34 PDT 1996

Notes:	Uses the global variable fa_filename	
		Assumes (!!) that the creation date is embedded in the filename 
		as follows: shaqpYYMMDDxxxxnnnn.e1
==============================================================================*/
int ESAc_get_shaqp_creation_date(
	void		*unused_pointer, 
	char 		*unused_pointr2,
	char 		*creation_date)
{
	char datestr[8] = "" ;
	/*
	-- Make sure that the year 2000 or beyond can be processed.
	-- Any two-digit year in the 90's produces a "19xx" year.
	-- All others create "20xx" year.
	*/
	if (fa_filename[5] != '9')
		strcpy (datestr, "20") ;
	else
		strcpy (datestr, "19") ;
	strncat (datestr, fa_filename+5, 6) ;

	return (tc_yyyymmdd2asf(datestr, creation_date) ) ;
}

/*==============================================================================
Function:      ESAc_shaqp_time_duration2strttime 

Description:    convert DD-MMM-YY hh:mm:ss + duration in seconds into asftime

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Sep  5 17:23:06 PDT 1996

Notes:		
==============================================================================*/
int ESAc_shaqp_time_duration2strttime(
	void		*unused_pointer, 
	char 		*time_duration_string,
	char 		*strttime)
{
	char esa_time[24] = "" ;
	char asftime[] = "yyyy:ddd:hh:mm:ss.ccc" ;
	char ascii_seconds[] = "nnnnn" ;
	float days ;

	/* 
	-- put together 'esa_time' from the time and duration string.
	-- Take the year 2000 into account, by embedding all the years in the
	-- last decade within "19xx"  All other years get "20xx"
	-- esa_time string will look like "DD-MMM-YYYY hh:mm:ss.ccc"
	*/
	strncpy (esa_time, time_duration_string, 7) ;
	if (time_duration_string[7] != '9')
		strcat (esa_time, "20") ;
	else
		strcat (esa_time, "19") ;
	strncpy (esa_time+9, time_duration_string+7, 11) ;
	strcat (esa_time, ".000") ;

	if (!tc_esadaytime2asf(esa_time, asftime) ) 
		return (FALSE) ;
		
	strncpy (ascii_seconds, time_duration_string+29, strlen(ascii_seconds) );
	if ( !gen_field_is_numeric(ascii_seconds, strlen(ascii_seconds)) )
		return (FALSE) ;
	else
		days = atof(ascii_seconds) / 60.0 / 60.0 / 24.0 ;

	return (tc_asf_add_ndays(asftime, days, strttime) ) ;
}

/*==============================================================================
Function:      ESAc_shaqp_time_duration2stoptime 

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Sep  5 17:23:43 PDT 1996

Notes:		
==============================================================================*/
int ESAc_shaqp_time_duration2stoptime(
	void		*unused_pointer, 
	char 		*time_duration_string,
	char 		*stoptime)
{
	char esa_time[24] = "" ;
	char asftime[] = "yyyy:ddd:hh:mm:ss.ccc" ;
	char ascii_seconds[] = "nnnnn" ;
	float days ;

	/* 
	-- put together 'esa_time' from the time and duration string.
	-- Take the year 2000 into account, by embedding all the years in the
	-- last decade within "19xx"  All other years get "20xx"
	-- esa_time string will look like "DD-MMM-YYYY hh:mm:ss.ccc"
	*/
	strncpy (esa_time, time_duration_string, 7) ;
	if (time_duration_string[7] != '9')
		strcat (esa_time, "20") ;
	else
		strcat (esa_time, "19") ;
	strncpy (esa_time+9, time_duration_string+7, 11) ;
	strcat (esa_time, ".000") ;

	if (!tc_esadaytime2asf(esa_time, asftime) ) 
		return (FALSE) ;
		
	strncpy(ascii_seconds, time_duration_string+41, strlen(ascii_seconds) );
	if ( !gen_field_is_numeric(ascii_seconds,strlen(ascii_seconds)) )
		return (FALSE) ;
	else
		days = atof(ascii_seconds) / 60.0 / 60.0 / 24.0 ;

	return (tc_asf_add_ndays(asftime, days, stoptime) ) ;
}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	file_utilities.h

Description:	
    Function Declarations for accessing file ingestion/creation 
	utility routines.
    Also return code #defines.  

	
Notes:

==============================================================================*/
#pragma ident "@(#)file_utilities.h	5.1 98/01/08 APS/ASF"
#pragma ident "@(#) /home/aps/r2.1.2/include/local/SCCS/s.file_utilities.h"

#ifndef _FILE_UTILITIES_H_
#define _FILE_UTILITIES_H_


typedef struct 
		{int id ;
		 int seconds_between ;
		 int number_o_retry ;
		} PERMISSION_ ;


/* MACROS  */


/* INCLUDES  */
#include <fa_defs.h>
#include <dapps_list.h>   /* for llist   */
#include <stdio.h>        /* for FILE *  */
#include <APSpmfutilities.h>	/* for PMF_FILENAME */
#include <mu_utilities.h>		/* for PERMISSION_ */

/* CONSTANTS   */

	/*
	-- these might alternatively be in the 
	-- db or a resource file 
	*/
#define 	PRINT_HEADER_ONLY			0




/* STATUS CODES  */

#define  FA_ASCII_REC_PROCESS_OK	         0
#define  FA_ASCII_REC_PROCESS_OK_CREATE_META 1
#define  FA_ASCII_REC_PROC_DONE_CREATE_META  2
#define  FA_ASCII_REC_PROC_PERMISSION_DENIED 3
#define  FA_ASCII_REC_PROCESS_ERROR			 4
#define  FA_ASCII_HEADER_INGESTION_OK        0
#define  FA_ASCII_RECORD_INGESTION_OK        0

#define  FA_ODL_PROCESS_OK_BUT_PLANNER_MUST_READ     1
#define  FA_ODL_PROCESS_OK                           0
#define  FA_ODL_PROCESS_ERROR                       -1
#define  FA_ODL_PROCESS_PERMDENIED                  -2
#define  FA_ODL_PROCESS_PERMFAILED                  -3

#define  FA_ODL_INGESTION_OK                         0
 

/*
--  START
--	ERROR CODES AND MESSAGES
--
--  These error codes and corresponding messages are 
--  implemented here and in file_ingestion.c
--  The code v. message correspondances must be the 
--  same here as there.  
--
-- NOTE:  these error codes must be < 0.  this is because the 
-- calling routines check for the return code < 0 to do 
-- error handling.  
--
*/

/* a macro to decode the error code into a string:  */
extern char *fa_ascii_rec_error_message[] ;
extern char *fa_odl_error_message[] ;

#define FA_ASCII_REC_ERROR_MESSAGE( code ) \
		fa_ascii_rec_error_message[ -(code) ]
#define FA_ODL_ERROR_MESSAGE( code ) \
		fa_odl_error_message[ -(code) ]


#define FA_ASCII_FILE_UNABLE_TO_OPEN        -1
#define FA_ASCII_REC_HEADER_NOT_OK          -2
#define FA_ASCII_REC_DBRECORD_COPY_FAILED   -3
#define FA_ASCII_REC_AGGREGATE_NOT_OK       -4
#define FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND -5
#define FA_DEFAULT_NOT_FOUND                -6
#define FA_DESCRIPTOR_NOT_FOUND             -7
#define FA_ASCII_REC_SUBRECORD_NOT_OK       -8

#define FA_ODL_UNABLE_TO_PARSE_FILE			-1
#define FA_ODL_HEADER_KEYWORD_NOT_OK        -2
#define FA_ODL_SEGMENT_COUNT_MISSING        -3
#define FA_ODL_SEGMENT_MISSING              -4
#define FA_ODL_ACTIVITY_MISSING				-5
#define FA_ODL_DBRECORD_COPY_FAILED			-6
#define FA_ODL_AGGREGATE_KEYWORD_NOT_OK		-7
#define FA_ODL_DTK_DEFAULTS_NOT_FOUND		-8
#define FA_ODL_DEFAULT_NOT_FOUND            -9
#define FA_ODL_PERMISSIONS_DENIED           -10
#define FA_ODL_PERMISSIONS_FAILED           -11
#define FA_ODL_INDETERMINATE_FILE_TIMES     -12

/*
--	END
--	ERROR CODES AND MESSAGES
--
--  These error codes and corresponding messages are 
--  implemented here and in ODL_file_ingestion.c
--  The code v. message correspondances must be the 
--  same here as there.  
*/



/* FUNCTION PROTOTYPES */

/* PROTOTYPES for $APS_HOME/src/lib_fileutils/ODL_file_ingestion.c */
/*
GLOBAL Function:	identify_ODL_file (currently used by aps_proc_ODL_File only)
GLOBAL Function:	retrieve_dtks_for_WOS
STATIC Function:	RES_rej_downtime 
STATIC Function:	RES_fillin_response 
STATIC Function:	ODL_nano2msecs
STATIC Function:	ODL_find_and_convert
GLOBAL Function:	fa_odl_ingestion 
GLOBAL Function:	fa_odl_processor  (currently used by aps_proc_ODL_File only)
*/
int identify_ODL_file(	
					char            *file_type,
					ODL_FILENAME     *fa_files,
					ODL_FILENAME     **file_descriptor) ;

int retrieve_dtks_for_WOS(
        char                *file_type,
        char                *strttime,
        char                *stoptime,
        llist               *WOS_dtks_list) ;

int fa_odl_ingestion(
    FILE        *file_name,
    DBPROCESS   *APS_dbproc,        /* open Sybase database process */
    int         *permission_id,
    ODL_FILEDEF *filedefs,
    llist       *input_dtk_list,
    FILE        *report_log) ;

int fa_odl_processor(
    DBPROCESS       *APS_dbproc,   /* open Sybase database process          */
    int             *permission_id,
    ODL_FILENAME    *filedef,
    char            *file_name,    /* input CSA Reception Request file name */
    FILE            *report_log) ; /* report file pointer                   */


/* PROTOTYPES for $APS_HOME/src/lib_fileutils/file_ingestion.c */
/*
STATIC  Function:       db_record_rev_comparison
STATIC  Function:       db_record_llist2ord_llist 
STATIC  Function:       db_record_ord_llist2llist 
STATIC  Function:       get_ascii_and_convert
STATIC  Function:       populate_subrecord_control
STATIC  Function:       fa_ascii_header_ingestion
STATIC  Function:		fa_ascii_record_ingestion 
STATIC  Function:       dtk_status_by_blanking
STATIC  Function:       dtk_fillin_FA_response
STATIC  Function:       dtk_create_esa_observation_dtks
STATIC  Function:       dtk_delete_range
GLOBAL	Function:       fa_ascii_processor
GLOBAL	Function:       identify_file
GLOBAL	Function:		identify_PMF_file
*/

int fa_ascii_processor(
    DBPROCESS   *APS_dbproc,        /* open Sybase database process         */
    PERMISSION_ permission,         /* permission id structure              */
    FA_FILENAME *file_descriptor,
    char        *file_name,
    FILE        *ascii_file,        /* input ASCII file name                */
    FILE        *report_log,        /* report file name                     */
    FILE        *response_file) ;   /* response file name                   */

int identify_file(	char            *file_type,
					FA_FILENAME     *fa_files,
					FA_FILENAME     **file_descriptor) ;

int identify_PMF_file(
    char            *file_type,
    PMF_FILENAME    *fa_files,
    PMF_FILENAME    **file_descriptor) ;
 



/* PROTOTYPES for $APS_HOME/src/lib_fileutils/ascii_file_creator.c */

int ascii_file_creator( 	/* file creator prototype */
				int			print_header_flag,
				llist		*dtk_list1 ,
				llist		*dtk_list2 ,
				FA_FILEDEF	*FA_file,
				FILE		*fp ) ;

/* PROTOTYPES for $APS_HOME/src/lib_fileutils/fa_move_file.c */

int fa_move_file(
    char        *fullpath_file, /* the full path name of the file to move  */
    int         id ) ;   /* integer indicating code for destination path.  */
                         /* used as:    aps_path_table[id].full_path       */
                         /* see:  static APS_PATH_TABLE aps_path_table[]   */
                         /* in src/lib_APS/apspath.c for paths.            */
                         /* see include/local/apspath.h for id value defs  */

/* PROTOTYPES for $APS_HOME/src/lib_fileutils/fa_copy_file.c */

int fa_copy_file(
    char        *fullpath_file, /* the full path name of the file to copy  */
    int         id ) ;   /* integer indicating code for destination path.  */
                         /* used as:    aps_path_table[id].full_path       */
                         /* see:  static APS_PATH_TABLE aps_path_table[]   */
                         /* in src/lib_APS/apspath.c for paths.            */
                         /* see include/local/apspath.h for id value defs  */

/* GLOBAL VARIABLES */

extern	char *file_util_progname ;	/* for syslogging				*/
extern	char file_util_msg[] ; 		/* for syslogging				*/

#endif /*  _FILE_UTILITIES_H_  */

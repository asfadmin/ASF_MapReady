#define DEBUG
#undef DEBUG

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   fa_csaf_processor.c

Description:    

External Functions Defined:
int fa_csaf_processor   Manages ingestion of CSA Reception Request/Schedule file
int fa_csaf_ingestion   processes, scans a file. Places its info in linked list.
    
File Scope Functions:
int find_ag_count       gets aggregate counts.
int find_and_convert    locate keyword strings, convert them.
    
External Variables Defined:
    
File Scope Variables:
CSA_VALUEDEF filedefs[] 
CSA_VALUEDEF areq_file[] 
                        arrays describing CSA Reception Request/Schedule and
                        CSA response to AREQ (request file)
                        keywords and conversion requirements, as well as 
                        database destinations for this information.
    
Notes:

==============================================================================*/
#pragma ident   "@(#)CSA_dtkf_processor.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/CSA_dtkf_p/SCCS/s.CSA_dtkf_processor.c"

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <stdlib.h>         /* for getopt                           */

#include <sys/types.h>      /* for read             */
#include <sys/uio.h>        /* for read             */
#include <sys/unistd.h>     /* for read             */
#include <mu_utilities.h>   /* for permissions      */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */
#include "aps_log_msg.h"    /* for syslogging                       */
#include "aps_defs.h"       /* for syslogging, exit codes           */

/* FOR DATABASE TABLES        */
#include "db_dtk.h"         /* for dtk table             */
#include "db_antenna_down_times.h"         /* for dtk table             */

/* FOR DTK_UTILITIES DEFINITIONS MULTI_ANTENNA */
#include "dtkm_utilities.h"

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

/* for asftime2rev()  */
#include "phase_utilities.h"

/* FOR CSA PARSE */
#include "CSAparse.h"

/* FOR FA_CSAF_PROCESSOR */
#include "GENconversions.h"
#include "CSAconversions.h"
#include "CSA_dtkf_processor.h"

#include <unistd.h>         /* for unlink() */
#include <timeconv.h>      

#define  FA_CSAF_PROCESS_OK                           0
#define  FA_CSAF_PROCESS_ERROR                       -1
#define  FA_CSAF_PROCESS_PERMDENIED                  -2
#define  FA_CSAF_PROCESS_PERMFAILED                  -3
#define  FA_CSAF_PROCESS_OK_BUT_PLANNER_MUST_READ     1

#define  FA_CSAF_INGESTION_OK                         0

char        *csa_proc_progname ;
char        csa_proc_msg[MSG_LEN] ;

char        *file_util_progname ; /* required by libfileutils.a */
char        file_util_msg[MSG_LEN]; /* required by libfileutils.a */

/*==============================================================================
Function:       CSA_dl2recording

Description:    Given a R1 recording dtk proposal, query the database for any 
				R1 recordings which overlap it, and return them in a list.
				We will retain all dtk database values for most fields, but the
				following will be updated with proposal information:
				DTK_ASCDSC                  set to NULL
				DTK_STRTTIME                modify with file times, if needed
				DTK_STOPTIME                modify with file times, if needed
				DTK_STRTLAT                	set to NULL 
				DTK_STOPLAT                	set to NULL 
				DTK_NRLAT1                  set to NULL
				DTK_NRLON1                  set to NULL
				DTK_FARLAT1                 set to NULL
				DTK_FARLON1                 set to NULL
				DTK_NRLAT2                  set to NULL
				DTK_NRLON2                  set to NULL
				DTK_FARLAT2                 set to NULL
				DTK_FARLON2                 set to NULL
				DTK_LOOKANGL                set to NULL
				DTK_DTKSTAT                 modify from file
				DTK_PROPOSED_DTKSTAT        set to NULL                
				DTK_FA_SCHEDULE_LINK		modify from file


Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Jul 22 13:19:27 PDT 1997

Notes:			
==============================================================================*/
int CSA_dl2recording(DB_RECORD	**recording_dtk_rec, 
					 llist		*new_rec_dtks)
{
    DB_RECORD   **dtk_unlinked_rec ;
    DB_RECORD   **dtk_rec = NULL ;
    cursor      dtk_list_ptr ;
    llist       *fetched_dtks = NULL ;

	/*
	-- quick error checking.
	-- The proposal must not be NULL and the linked list must not be NULL.
	-- The proposal must be for R1; proposal must be a recording observation.
	*/
	if (recording_dtk_rec == NULL
	||  new_rec_dtks == NULL)
		return FALSE ;
	if (strcmp(CAST_DTK_SAT recording_dtk_rec[DTK_SAT], "R1") != 0)
		return FALSE ;
	if (strncmp(CAST_DTK_ACTID recording_dtk_rec[DTK_ACTID], 
				DTKM_ACTID_RECORDING_OBSERVATION_CODE, 3) != 0)
		return FALSE ;

    /*
    -- get all overlaps for the incoming recording dtk proposal.
    -- We don't match on rev because the incoming recording proposal 
	-- is allowed to span revs.
    -- A recording dtk could conceivably be in a different rev and still
    -- overlap the input recording dtk proposal
    -- So we will look at the current rev +- 10, to quicken the search.
	--
	-- We will only fetch overlapping recording dtks, not realtime observations
	-- or downlinks of any type (realtime/tapedump)
	-- DELeted records in the database are not considered
    */
    sprintf(where_clause,
"where %s = '%s' and \
%s >= %d and %s <= %d and \
%s != '%s' and %s < '%s' and %s > '%s' and %s like '%s%%' order by %s",
        APS_COL(DTK, DTK_SAT),  CAST_DTK_SAT recording_dtk_rec[DTK_SAT],
        APS_COL(DTK, DTK_REV),  ( CAST_DTK_REV recording_dtk_rec[DTK_REV] ) -10,
        APS_COL(DTK, DTK_REV),  ( CAST_DTK_REV recording_dtk_rec[DTK_REV] ) +10,
        APS_COL(DTK, DTK_DTKSTAT),  "DEL",
        APS_COL(DTK, DTK_STRTTIME), 
						CAST_DTK_STOPTIME recording_dtk_rec[DTK_STOPTIME],
        APS_COL(DTK, DTK_STOPTIME), 
						CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME],
        APS_COL(DTK, DTK_ACTID),    DTKM_ACTID_RECORDING_OBSERVATION_CODE,
		APS_COL(DTK, DTK_STRTTIME) ) ;

    fetched_dtks = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS ) ;
    if ( NUMELTS(fetched_dtks) == 0 )
        return TRUE ; 
		/* 
		-- This return=TRUE is OK. 0 elements is not a fatal event. But we 
		-- don't want to continue processing; it would be pointless.
		*/

#ifdef DEBUG
dtkm_print_list(stdout, fetched_dtks) ;
#endif

    for ( dtk_rec = (DB_RECORD **) FIRST(fetched_dtks, dtk_list_ptr);
          dtk_rec != NULL ;
          dtk_rec = (DB_RECORD **) NEXT(fetched_dtks, dtk_list_ptr)   )
    {
		/*
		-- If dtk from database has an earlier starttime than the proposal,
		-- modify the starttimes to match.
		*/
		if (strcmp(CAST_DTK_STRTTIME           dtk_rec[DTK_STRTTIME],
				   CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME]) < 0)
			strcpy( CAST_DTK_STRTTIME          dtk_rec[DTK_STRTTIME], 
					CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME]) ;

		/*
		-- If dtk from database has a later stoptime than the proposal,
		-- modify the stoptimes to match.
		*/
		if (strcmp(CAST_DTK_STOPTIME recording_dtk_rec[DTK_STOPTIME],
				   CAST_DTK_STOPTIME           dtk_rec[DTK_STOPTIME]) < 0)
			strcpy( CAST_DTK_STOPTIME          dtk_rec[DTK_STOPTIME], 
					CAST_DTK_STOPTIME recording_dtk_rec[DTK_STOPTIME]) ;

		CAST_DTK_ASCDSC dtk_rec[DTK_ASCDSC] = ' ' ;
		CAST_DTK_STRTLAT dtk_rec[DTK_STRTLAT] = 0.0 ;
		CAST_DTK_STOPLAT dtk_rec[DTK_STOPLAT] = 0.0 ;
		CAST_DTK_NRLAT1 dtk_rec[DTK_NRLAT1] = 0.0 ;
		CAST_DTK_NRLON1 dtk_rec[DTK_NRLON1] = 0.0 ;
		CAST_DTK_FARLAT1 dtk_rec[DTK_FARLAT1] = 0.0 ;
		CAST_DTK_FARLON1 dtk_rec[DTK_FARLON1] = 0.0 ;
		CAST_DTK_NRLAT2 dtk_rec[DTK_NRLAT2] = 0.0 ;
		CAST_DTK_NRLON2 dtk_rec[DTK_NRLON2] = 0.0 ;
		CAST_DTK_FARLAT2 dtk_rec[DTK_FARLAT2] = 0.0 ;
		CAST_DTK_FARLON2 dtk_rec[DTK_FARLON2] = 0.0 ;
		CAST_DTK_LOOKANGL dtk_rec[DTK_LOOKANGL] = 0.0 ;
		strcpy( CAST_DTK_PROPOSED_DTKSTAT dtk_rec[DTK_PROPOSED_DTKSTAT], "") ;

		/*
		-- The following fields are updated with values from proposal dtk
		*/
		strcpy( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], 
				CAST_DTK_DTKSTAT recording_dtk_rec[DTK_DTKSTAT] ) ;
		strcpy( CAST_DTK_FA_SCHEDULE_LINK dtk_rec[DTK_FA_SCHEDULE_LINK], 
			CAST_DTK_FA_SCHEDULE_LINK recording_dtk_rec[DTK_FA_SCHEDULE_LINK] );

		dtk_unlinked_rec = (DB_RECORD **)
			UNLINK_AT_CURSOR(fetched_dtks, dtk_list_ptr) ;

#ifdef DEBUG
		printf ("COMPONENT RECORD:\n") ;
        db_print_record( dtk_unlinked_rec, APS_CDEFS(DTK) );
#endif
		APPEND( new_rec_dtks, dtk_unlinked_rec, 
						free_db_record, dtk_unlinked_rec ) ;
	}

#ifdef DEBUG
printf("%s(%d):  INCOMING DB_RECORD to be split......:\n", __FILE__, __LINE__ );
db_print_record( recording_dtk_rec, APS_CDEFS(DTK) );

printf(
"DTK list RECORDINGS FETCHED and UPDATED:   %d\n", NUMELTS( new_rec_dtks ) ) ;
dtkm_print_list(stdout, new_rec_dtks) ;
#endif

	return TRUE ;
}

/*==============================================================================
Function:       check_playback_and_recording_times

Description:    This routine examines the PLAYBACK (tapedump) events in the
				incoming list and compares them to their corresponding 
				ACQUISITION (recording) events.  If the recording time is
				greater than the tapedump time, we discard the recording
				(by placing it in the rejected_dtk_recordings list).
				The tapedump is not discarded.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Jul 15 14:14:12 PDT 1997

Notes:			
==============================================================================*/
int check_playback_and_recording_times(
	llist *input_dtks, 
	llist *rejected_dtks)
{
	int			return_code ;
	char		actid_string[7]="xxxxxx" ;
	char		actid_string2[7]="xxxxxx" ;
	char		actid_string3[7]="xxxxxx" ;

    DB_RECORD   **dtk_rec ;
    cursor      dtk_rec_ptr ;
    DB_RECORD   **dmp_rec ;
    cursor      dmp_rec_ptr ;
    llist       *recording_dtks = NULL ;
    DB_RECORD   **dtk_unlinked_rec ;

	double		et_dtk_rec_diff, et_dmp_rec_diff ;

	/*
	-- Quick error checking
	*/
	if (input_dtks == NULL
	||  rejected_dtks == NULL)
		return (-1) ;

	/*
	-- Move recording proposals into separate list, recording_dtks.
	*/
	strcpy(actid_string,	DTKM_ACTID_RECORDING_OBSERVATION_CODE) ;
	strcat(actid_string,	"ASF") ;
	strcpy(actid_string2,	DTKM_ACTID_RECORDING_OBSERVATION_CODE) ;
	strcat(actid_string2,	"CEF") ;
	strcpy(actid_string3,	DTKM_ACTID_RECORDING_OBSERVATION_CODE) ;
	strcat(actid_string3,	"CSA") ;
    recording_dtks = create_dyn_llist() ;

    if( move_db_record_matches2llist( APS_CDEFS(DTK), DTK_ACTID,
        actid_string, recording_dtks, input_dtks ) == NULL)
		return (-1) ;

    if( move_db_record_matches2llist( APS_CDEFS(DTK), DTK_ACTID,
        actid_string2, recording_dtks, input_dtks ) == NULL)
		return (-1) ;

    if( move_db_record_matches2llist( APS_CDEFS(DTK), DTK_ACTID,
        actid_string3, recording_dtks, input_dtks ) == NULL)
		return (-1) ;

	/*
	-- Check each recording proposal against its playback event.
	-- If the recording is longer than the playback, move it to 
	-- the rejected_dtks list.
	*/
    for (
        dtk_rec = (DB_RECORD **) FIRST(recording_dtks, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **) NEXT(recording_dtks, dtk_rec_ptr)
        )
    {
		for (
			dmp_rec = (DB_RECORD **) FIRST(input_dtks, dmp_rec_ptr) ;
			dmp_rec ;
			dmp_rec = (DB_RECORD **) NEXT(input_dtks, dmp_rec_ptr)
			)
        {
			if (strcmp(CAST_DTK_FA_SCHEDULE_LINK dtk_rec[DTK_FA_SCHEDULE_LINK],
				CAST_DTK_FA_SCHEDULE_LINK dmp_rec[DTK_FA_SCHEDULE_LINK]) != 0)
				continue ;

			/* 
			-- the recording (dtk_rec) has found a matching playback (dmp_rec)
			*/
			return_code = tc_et_ASF_datetime_diff(
				CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
				CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
				&et_dtk_rec_diff) ;
			if (return_code != TRUE)
				return (-1) ;
					
			return_code = tc_et_ASF_datetime_diff(
				CAST_DTK_STRTTIME dmp_rec[DTK_STRTTIME],
				CAST_DTK_STOPTIME dmp_rec[DTK_STOPTIME],
				&et_dmp_rec_diff) ;
			if (return_code != TRUE)
				return (-1) ;

			/* 
			-- If the recording time difference is _GREATER_ than the
			-- playback time difference, discard the recording.
			*/
			if (et_dtk_rec_diff > et_dmp_rec_diff)
			{
				dtk_unlinked_rec = (DB_RECORD **)
					UNLINK_AT_CURSOR(recording_dtks, dtk_rec_ptr) ;
				APPEND( rejected_dtks, dtk_unlinked_rec, free_db_record, 
						dtk_unlinked_rec ) ;
			}
			break ;
		}

	} /* get next recording */

	/*
	-- Any recordings left in list 'recording_dtks' have passed our test; 
	-- they are not _GREATER_ than their corresponding playback.
	-- If there are any recordings in the 'recording_dtks' list, 
	-- place them in the input_dtks list.
	*/
	if ( NUMELTS(recording_dtks)  > 0 )
		db_record_llist_move (input_dtks, recording_dtks) ;

	/*
	-- Clean up.
	*/
    DEL_LIST ( recording_dtks ) ;

	return 0 ;
}

/*==============================================================================
Function:      match_file__get_field_defs 

Description:   This routine matches the incoming file code, retrieves
               field definitions for the incoming file.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Fri Sep 13 09:31:20 PDT 1996

Notes:      
==============================================================================*/
int match_file__get_field_defs(
    char            *file_type,
    CSA_FILENAME    *fa_files,
    CSA_VALUEDEF    **value_defs)
{
    int i ;
 
    for( i = 0 ; fa_files[i].file_type != NULL ; i++ )
    {
#ifdef PRINT_DIAG
        printf ("comparing %s to %s \n", file_type, fa_files[i].file_type) ;
#endif
        if (strncmp(file_type, fa_files[i].file_type,
                        strlen(fa_files[i].file_type) ) == 0)
        {
            *value_defs = fa_files[i].field_def ;
#ifdef PRINT_DIAG
        printf ("FOUND MATCH: \n");
#endif
            return (TRUE) ;
        }
    }
 
    return  (FALSE) ;
}

/*==============================================================================
Function:       find_and_convert

Description:    starting from the node specified, find the string which is 
                identified by the *keyword which is passed to this routine.
                Then, convert the string using the specified (*conversion)

Parameters:     
Type          Name              Definition
NODEPTR       node              node address from which to start search
char          *keyword          string label identifier                   
int           (*conversion())   conversion to be applied to found string  
struct EQUIV_TABLE
              *conversion_table table holding the equivalences for target string
void          *dest             place conversion result at this adddress 

Returns:        
Type  Name                  Definition
*int  conversion_status     status of the conversion used on the string 
                            found by routine  FALSE = error
                                              TRUE  = success

Creator:        Miguel Siu

Creation Date:  Fri May 19 13:53:43 PDT 1995

Notes:  ASSUMPTION: all conversion routines return TRUE/FALSE   
==============================================================================*/
int  find_and_convert(
    NODEPTR     node, 
    char        *keyword, 
    int         (conversion()), 
    EQUIV_TABLE *conversion_table,
    void        *dest,
    char        **keyword_value )
{
    int           conversion_status;

    *keyword_value = (char *) find_keyword_value(keyword, node);
    /*
    -- NOTE:  2 keywords, IMG_DATA_ACQ_START, IMG_DATA_ACQ_STOP, 
    -- can have null values; this is OK.  
    */
    if ( strcmp( keyword, "IMG_DATA_ACQ_START" ) == 0 
    ||   strcmp( keyword, "IMG_DATA_ACQ_STOP" ) == 0 )
    {
        if (*keyword_value == NULL)
        {
            strcpy( dest, "" ) ;
#ifdef DEBUG
            printf("\nFIND_AND_CONVERT: %s\n", keyword); 
            printf("                :found NULL VALUE\n" );
#endif
            return TRUE ;
        }
    }

    if (*keyword_value == NULL)
        return (FALSE);

#ifdef DEBUG
    printf("\nFIND_AND_CONVERT: %s\n", keyword); 
    printf("                :found %s\n",*keyword_value);
#endif

    conversion_status = conversion(conversion_table, *keyword_value, dest);
    return(conversion_status);
}             

/*==============================================================================
Function:       find_ag_count

Description:    find the number of aggregations identified by the ag_string

Parameters:     
Type        Name                Definition
char        *ag_string          string which identifies the aggregation
NODEPTR     node                node address from which to start search

Returns:        
Type        Name                Definition
int                             number of aggregates found:  0  error
                                                            >0  correct number

Creator:        Miguel Siu

Creation Date:  Fri May 19 15:21:48 PDT 1995

Notes:  A count of 0 aggregates is considered an error. 
==============================================================================*/
int find_ag_count(
    char *ag_string, 
    NODEPTR node)
{
    char *keyword_value;
    int  aggregates;    

    keyword_value = (char *) find_keyword_value (ag_string, node);
    if (keyword_value == NULL)
        return(0);

    if (!gen_field_is_numeric(keyword_value, strlen(keyword_value)) )
        return(0);

    aggregates = atoi(keyword_value);

    return(aggregates);
}

/*==============================================================================
Function:      fa_csaf_ingestion 

Description:    
    Creates and scans a tree structure for information 
    which is placed in a DB_RECORD linked list.

Parameters:     

Returns:
Type        Name                Definition
zero value
int         FA_CSAF_INGESTION_OK    Success.
 
Negative value
int         FA_CSAF_UNABLE_TO_PARSE_FILE
            FA_CSAF_DEFAULT_NOT_FOUND
            FA_CSAF_HEADER_KEYWORD_NOT_OK
            FA_CSAF_SEGMENT_COUNT_MISSING
            FA_CSAF_SEGMENT_MISSING
            FA_CSAF_ACTIVITY_MISSING
            FA_CSAF_DBRECORD_COPY_FAILED
            FA_CSAF_AGGREGATE_KEYWORD_NOT_OK
            FA_CSAF_DTK_DEFAULTS_NOT_FOUND
            FA_CSAF_PERMISSIONS_DENIED
            FA_CSAF_PERMISSIONS_FAILED
            FA_CSAF_INDETERMINATE_FILE_TIMES

Creator:        Miguel Siu

Creation Date:  Thu Jun 15 11:51:51 PDT 1995

Notes:      All permissions will be requested in this module.
            However, termination of these permissions occurs when the
            calling program exits.  
==============================================================================*/
int fa_csaf_ingestion(
    char        *file_name,
    DBPROCESS   *APS_dbproc,        /* open Sybase database process */
    int         *permission_id, 
    CSA_VALUEDEF *filedefs, 
    llist       *input_dtk_list, 
    FILE        *report_log)
{
#define ACTVY_RECEPTION_SPECS    "RECEPTION_ACTIVITY_SPECS"
#define SEGMENT_EVENTS           "NB_OF_SEGMENTS"


    /* declarations         */
    int           i,j;
    int           reception_cnt,  segment_cnt;
    int           number_o_segments;
    int           return_code;      /* for permissions */
    int           number_of_errors_in_reception = 0 ; 

    char          *keyword_value ;
	char          dtk_string[512];       /* for dtk printing in error message */


    cursor        dtk_list_ptr ;
    DB_RECORD     **dtk_unlinked_rec ;
    DB_RECORD     **dtk_rec = NULL ;
    DB_RECORD     **recording_dtk_rec = NULL ;
    DB_RECORD     **input_dtk_rec = NULL ;
    DB_RECORD     **template_dtk_rec = NULL ;
    DB_RECORD     *destination = NULL ;
    DB_RECORD     **min_dtk_rec ;
    DB_RECORD     **max_dtk_rec ;

    NODEPTR       top_o_tree;
    NODEPTR       rec_node;

	llist		  *new_rec_dtk_list ;

    fa_number_of_error_records = 0 ;
    top_o_tree = (NODEPTR) create_tree (file_name);

    if(top_o_tree == NULL)
    {
#ifdef DEBUG                                        
    top_o_tree = (NODEPTR) create_tree (file_name);
        printf ("create_tree could not parse file %s\n", file_name);
#endif
        return (FA_CSAF_UNABLE_TO_PARSE_FILE);
    }


    /*
    -- Make an empty    DBRECORD template
    */
    template_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
    /*
    -- put various appropriate blank values into 
    -- the record.  
    */
    dtkm_blank_values(template_dtk_rec, template_dtk_rec ) ;

    /* 
    -- find values for HEADER and DEFAULT keywords
    */

    for( i = 0 ; filedefs[i].source_code ; i++ )
    {
        if (filedefs[i].source_code != FILE_HEADER
        &&  filedefs[i].source_code != FA_DEFAULT) 
            continue ;

        /* 
        -- Handling only FILE_HEADER and FA_DEFAULT values
        -- Assign a  destination, index or pointer as needed.
        */
        if (filedefs[i].destination_code == REPORT_RECORD)
            destination = 
                template_dtk_rec[filedefs[i].destination.index];
        else
            destination = (DB_RECORD *)filedefs[i].destination.pointer; 

        if (filedefs[i].source_code == FA_DEFAULT)
        {
            /*
            -- assign values to the destination pointer
            */
            if (! (filedefs[i].conversion)(NULL, NULL, destination) )
            {
                free_db_record( template_dtk_rec ) ;
                return (FA_CSAF_DEFAULT_NOT_FOUND);
            }
        }
        else 
        {
            /* FILE_HEADER  */
            if (! find_and_convert(top_o_tree, 
                  filedefs[i].keyword, 
                  filedefs[i].conversion,
                  filedefs[i].conversion_table,
                  destination, &keyword_value ))
            {
                if (*keyword_value == NULL)strcpy(keyword_value,"<null value>");
                sprintf(csa_proc_msg, 
                    "fa_csaf_ingest():error parsing header:  %s = %s\n",
                    filedefs[i].keyword, keyword_value ) ;
                aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
                    DO_SYSLOG, DO_PRINT);

                fprintf (report_log, 
                    "fa_csaf_ingest:error parsing header:  \n\t%s = %s\n", 
                    filedefs[i].keyword, keyword_value ) ;

                free_db_record( template_dtk_rec ) ;
                return (FA_CSAF_HEADER_KEYWORD_NOT_OK);
            }
        }
    }

    /*
    -- At this point, we have the fa_activity_type (it is derived from header)
    -- so we can request the single_activity permission.
 
       Here is the chain of events:
       0) Parse the header.
    -> 1) Handle permissions.  
          If activity permission not provided, get it.
          If activity permission is  provided, validate it.
       2) Parse the data records.
       3) Get planning permission.
       4) ACCESS DATABASE. PROCESS PROPOSAL LIST.
       When execution terminates, permissions are automatically released.  
 
    */
 
    /*
    -- Item (1). Get ACTIVITY permission if it is not provided.
    -- NOTE: both (1) and (2) can be satified using mu_get_permission routine.
    */
    return_code = mu_get_permission(
        csa_proc_progname,           /*  name of executable, for syslog()    */
        APS_dbproc,                  /*  Sybase process pointer              */
        *permission_id,              /*  if != 0, verify this permission.    */
        MU_SINGLE_ACTIVITY_TYPE,     /*  Multi-user activity type.           */
        fa_activity_type,            /*  Multi-user activity id.             */
        NULL,                        /*  start time of planning time bracket.*/
        NULL,                        /*  end time of planning time bracket.  */
        NULL,                        /*  station id of planning activity.    */
        0,                           /*  DAR id of dar activity.             */
        0,                           /*  retry logic n_retries               */
        0  ) ;                       /*  retry logic n_seconds_retry         */
    if ( return_code < 0 )
        return FA_CSAF_PERMISSIONS_FAILED ;
    if ( return_code == 0 )
        return FA_CSAF_PERMISSIONS_DENIED ;
    /*
    -- we have gotten our activity permission.
    */
    *permission_id = return_code ;

    /*
    -- Process AGGREGATION variables, append information to link list
    -- NOTE: global variable fa_number_of_records carries the number
    --      of receptions expected for this file.
    */

    for(reception_cnt=0;reception_cnt<fa_number_of_records;reception_cnt++)
    {
        /*
        -- find a reception aggregate !!!
        */

        if (reception_cnt == 0)
            rec_node  = (NODEPTR) find_ag(ACTVY_RECEPTION_SPECS,top_o_tree);
        else
        {
            /*
            -- skip the SEGMENT aggregations before going to next reception
            */
            number_o_segments = find_ag_count(SEGMENT_EVENTS,rec_node);
            if (!number_o_segments)
            {
                sprintf(csa_proc_msg, 
                    "fa_csaf_ingest():error parsing keyword %s\n",
                    SEGMENT_EVENTS);
                aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
                    DO_SYSLOG, DO_PRINT);

                fprintf(report_log, 
                    "fa_csaf_ingest:error parsing keyword %s\n", 
                    SEGMENT_EVENTS);
                free_db_record( template_dtk_rec ) ;
                return (FA_CSAF_SEGMENT_COUNT_MISSING);
            }

            for (segment_cnt=0;segment_cnt<number_o_segments; segment_cnt++)
            {
                rec_node = (NODEPTR) next_ag(rec_node, top_o_tree);
                if (rec_node  == NULL)
                {
                    aps_log_msg(csa_proc_progname, APS_ERROR, 
                        "could not find SEGMT aggregation!\n", 
                        DO_SYSLOG, DO_PRINT);
                    fprintf (report_log, "could not find SEGMT aggregation!\n");
                    free_db_record( template_dtk_rec ) ;
                    return (FA_CSAF_SEGMENT_MISSING);
                }
            }

            rec_node  = (NODEPTR) next_ag (rec_node, top_o_tree);
        }
        if(!rec_node)
        {
            aps_log_msg(csa_proc_progname, APS_ERROR, 
                "could not find ACTIVITY aggregation!!!\n", 
                DO_SYSLOG, DO_PRINT);
            fprintf (report_log, "could not find ACTIVITY aggregation!!!\n");
            free_db_record( template_dtk_rec ) ;
            return (FA_CSAF_ACTIVITY_MISSING);
        }



        /*
        -- Make an empty    DBRECORD template
        */
        input_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;


        /*
        -- Copy a new empty DBRECORD to use when reading each record
        -- from the file.
        */
        if (db_copy_record(APS_CDEFS(DTK),
                input_dtk_rec,template_dtk_rec) != DB_COPY_RECORD_OK)
        {
#ifdef DEBUG                                        
            printf ("Could not copy template of DBRECORD. \n");
#endif
            return (FA_CSAF_DBRECORD_COPY_FAILED);
        }


        /*
        -- Process all variables for this reception.
        */

        number_of_errors_in_reception = 0 ; 
        for ( j = 0 ; filedefs[j].source_code ; j++ )     
        {
            /* 
            -- look at the FILE_RECORD entries to build up 
            -- the dtk record values.  
            */
            if ( filedefs[j].source_code != FILE_RECORD )
                continue ;

            if ( filedefs[j].destination_code == REPORT_RECORD )
            {
                if ( ! find_and_convert(rec_node, 
                         filedefs[j].keyword, 
                         filedefs[j].conversion,
                         filedefs[j].conversion_table,
                         input_dtk_rec[filedefs[j].destination.index],
                         &keyword_value)  )
                {
                    number_of_errors_in_reception ++ ;
                    if (*keyword_value == NULL)
                        strcpy(keyword_value,"<null value>");
                    sprintf(csa_proc_msg, 
            "find_and_convert:error parsing reception:  %s = %s\n",
                        filedefs[j].keyword, keyword_value ) ;
                    aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
                        DO_SYSLOG, DO_PRINT);

                    fprintf(report_log, 
            "find_and_convert:error parsing reception:  \n\t%s = %s\n",
                        filedefs[j].keyword, keyword_value ) ;
                }
                continue ;
            }

            if ( filedefs[j].destination_code == GLOBAL_VARIABLE )
            {
                destination = (DB_RECORD *)filedefs[j].destination.pointer ; 

                if (! find_and_convert(rec_node, 
                    filedefs[j].keyword, 
                    filedefs[j].conversion,
                    filedefs[j].conversion_table,
                    destination, &keyword_value)  )
                {
                    number_of_errors_in_reception ++ ;
                    if (*keyword_value == NULL)
                        strcpy(keyword_value,"<null value>");
                    sprintf(csa_proc_msg, 
                        "find_and_convert():error parsing:  %s = %s\n",
                        filedefs[j].keyword, keyword_value );
                    aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
                        DO_SYSLOG, DO_PRINT);

                    fprintf(report_log,
                        "find_and_convert:error parsing:  \n\t%s = %s\n",
                        filedefs[j].keyword, keyword_value );
                }
            }

        } /* end of for loop:  while(j = 0 ; filedefs[j].source_code ; j++ ) */

        /* this reception was processed.  */
        if ( number_of_errors_in_reception > 0 )
        {
            /* a global variable:  */
            fa_number_of_error_records ++ ;
            aps_log_msg(csa_proc_progname, APS_ERROR, 
                "SKIPPING A RECEPTION DUE TO ERRORS IN FILE\n\n", 
                DO_SYSLOG, DO_PRINT);
            fprintf( report_log, 
                "SKIPPING A RECEPTION DUE TO ERRORS IN FILE\n\n" ) ;
            continue ;
        }

        /*
        -- OK.  number_of_errors_in_reception <= 0  
        -- check the rec values, fill in with default values
        */
        return_code = dtkm_default_values (input_dtk_rec, input_dtk_rec);
        if (return_code < 0 )
        {
            aps_log_msg(csa_proc_progname, APS_ERROR, 
                DTKM_ERROR_MESSAGE(return_code), 
                DO_SYSLOG, DO_PRINT);
            number_of_errors_in_reception++ ;
            continue ;
        }

#ifdef DEBUG
        /* -- print the DBRECORD: */
        printf("%s(%d):  APPENDING DB_RECORD to input_dtk_list:\n", 
            __FILE__, __LINE__ ) ;
        db_print_record( input_dtk_rec, APS_CDEFS(DTK) );
#endif 

        /*
        -- append the rec to the list: 
        */
        APPEND( input_dtk_list, input_dtk_rec, free_db_record, 
            input_dtk_rec) ;

        if ( strcmp( fa_record_write_flag, "RITE" ) == 0 )
        {
            /* 
            -- used to make asftime2rev() argument 
            -- compatible 
            */
            int rev ;   

            /* 
            -- now write a RECORDING record to go with the tape dump.  
            --
            -- make a new DB_RECORD, copy the values from 
            -- input_dtk_rec into it.  
            -- than adjust the appropriate values to make it a 
            -- recording, then copy the recording times.  then 
            -- adjust the rev number to match the times.  
            -- then APPEND it to the list.  
            */
            recording_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
            db_copy_record(APS_CDEFS(DTK), recording_dtk_rec, 
                input_dtk_rec ) ;
            if ( ! CSAc_default_sensor( NULL, NULL, 
                CAST_DTK_SENSOR recording_dtk_rec[DTK_SENSOR] ) )
            {
                free_db_record( template_dtk_rec ) ;
                free_db_record( recording_dtk_rec ) ;
                return FA_CSAF_DEFAULT_NOT_FOUND ;
            }
            strcpy(CAST_DTK_ACTID recording_dtk_rec[DTK_ACTID], 
                       DTKM_ACTID_RECORDING_OBSERVATION_CODE ) ;
            strcpy( CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME], 
                fa_recording_start_time ) ;
            strcpy( CAST_DTK_STOPTIME recording_dtk_rec[DTK_STOPTIME], 
                fa_recording_stop_time ) ;
            return_code = asftime2rev(
                CAST_DTK_SAT recording_dtk_rec[DTK_SAT],
                CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME], 
                &rev ) ;
            if ( return_code < 0)
            {
                aps_log_msg(csa_proc_progname, APS_ERROR, 
                    PHASE_ERROR_MESSAGE(return_code), 
                    DO_SYSLOG, DO_PRINT);
                sprintf(csa_proc_msg, 
                    "sat = %s, strttime = %s\n",
                    CAST_DTK_SAT recording_dtk_rec[DTK_SAT],
                    CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME] );
                aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
                    DO_SYSLOG, DO_PRINT);

                fprintf (report_log, 
                    "%s\n",PHASE_ERROR_MESSAGE(return_code) );
                fprintf( report_log, "sat = %s, strttime = %s\n" ,
                    CAST_DTK_SAT recording_dtk_rec[DTK_SAT],
                    CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME] );
                free_db_record( recording_dtk_rec ) ;
                free_db_record( template_dtk_rec ) ;
                return (FA_CSAF_DTK_DEFAULTS_NOT_FOUND);
            }

            CAST_DTK_REV recording_dtk_rec[DTK_REV] = rev ;
            strcpy( CAST_DTK_TRANSID recording_dtk_rec[DTK_TRANSID], 
                "00" ) ;

			new_rec_dtk_list = create_dyn_llist() ;
			if (fa_trigger_CSA_dl_mapping)
			{
				/* 
				-- We will now try to break a downlink datatake into its
				-- component recordings, which may already be in our db.
				-- This is an operation used only in CSA file processing.
				*/
				return_code = CSA_dl2recording(recording_dtk_rec, 
													new_rec_dtk_list) ;
				if( return_code != TRUE)
				{
					aps_log_msg(csa_proc_progname, APS_ERROR, 
						"ERROR: could not parse recording record",
						DO_SYSLOG, DO_PRINT);

					fprintf (report_log, 
						"ERROR: could not parse recording record\n" );

                    free_db_record( recording_dtk_rec ) ;
					free_db_record( template_dtk_rec ) ;
					return (FA_CSAF_COULD_NOT_PARSE_RECORDING);
				}
				if (NUMELTS (new_rec_dtk_list) == 0)
					APPEND( new_rec_dtk_list, recording_dtk_rec, 
					free_db_record, recording_dtk_rec) ;
			}
			else
            	APPEND( new_rec_dtk_list, recording_dtk_rec, 
				free_db_record, recording_dtk_rec) ;

			/*
			-- process each of the proposals in new_rec_dtk_list
			*/
			for (dtk_rec= (DB_RECORD **) FIRST(new_rec_dtk_list, dtk_list_ptr);
				 dtk_rec!= NULL ;
				 dtk_rec = (DB_RECORD **) NEXT(new_rec_dtk_list, dtk_list_ptr) )
			{
            return_code = dtkm_default_values( dtk_rec, dtk_rec ) ;
            if ( return_code < 0)
            	{
				sprintf(dtk_string, "%3.3s %3.3s",
					CAST_DTK_DTKSTAT recording_dtk_rec[DTK_DTKSTAT],
					CAST_DTK_STATION_ID recording_dtk_rec[DTK_STATION_ID] ) ;

				/* print the antenna_id if > 0   */
				if ( CAST_DTK_ANTENNA_ID recording_dtk_rec[DTK_ANTENNA_ID] > 0 )
					sprintf(dtk_string, "%s%2d", dtk_string, 
					CAST_DTK_ANTENNA_ID recording_dtk_rec[DTK_ANTENNA_ID] ) ;
				else
					strcat(dtk_string, "  " ) ;

				sprintf(dtk_string,
					"%s %2.2s/%3.3s/%05ld.%02d           %2.2s     %1c %1c %s",
					dtk_string,
					CAST_DTK_SAT recording_dtk_rec[DTK_SAT],
					CAST_DTK_SENSOR recording_dtk_rec[DTK_SENSOR],
					CAST_DTK_REV recording_dtk_rec[DTK_REV],
					CAST_DTK_DTKID recording_dtk_rec[DTK_DTKID],
					CAST_DTK_TRANSID recording_dtk_rec[DTK_TRANSID],
					CAST_DTK_SCIENCE_QUICKLOOK recording_dtk_rec[DTK_SCIENCE_QUICKLOOK],
					CAST_DTK_ASCDSC recording_dtk_rec[DTK_ASCDSC],
					CAST_DTK_FADTKID recording_dtk_rec[DTK_FADTKID] ) ;

				/* print the darid if > 0   */
				if ( CAST_DTK_DARID recording_dtk_rec[DTK_DARID] > 0 )
					sprintf(dtk_string, "%s darid=%ld", dtk_string,
					CAST_DTK_DARID recording_dtk_rec[DTK_DARID] ) ;
	 
				sprintf(dtk_string,
					"%s\n                              %21s %21s", dtk_string,
					CAST_DTK_STRTTIME recording_dtk_rec[DTK_STRTTIME],
					CAST_DTK_STOPTIME recording_dtk_rec[DTK_STOPTIME] ) ;

				/*
				-- If the error was caused by a too-large time range, do not
				-- consider it an error; continue after skipping this proposal.
				-- We expect these types of errors to occur whenever a tapedump
				-- or 'PLAYBACK' event is scheduled, and the corresponding
				-- observations are grouped together under single start/stoptime
				-- which may be longer than the actual station mask!
				*/
				if ( return_code == DTKM_ERROR_TIMES_NOT_WITHIN_REV )
					{
					aps_log_msg(csa_proc_progname, APS_INFO, 
"Warning: the following dtk proposal spans more than one rev. \
It may be too long; we discard it.\n", 
						DO_SYSLOG, DO_PRINT);
					aps_log_msg(csa_proc_progname, APS_INFO, dtk_string, 
						DO_SYSLOG, DO_PRINT);

					fprintf (report_log, 
"Warning: the following dtk proposal spans more than one rev. \
 It may be too long; we discard it.\n");
					fprintf( report_log, "%s\n" , dtk_string ) ;

					continue ;
					}
				else
					{
					/* This is a real error. Handle it. */
					aps_log_msg(csa_proc_progname, APS_ERROR, 
						DTKM_ERROR_MESSAGE(return_code), 
						DO_SYSLOG, DO_PRINT);
					aps_log_msg(csa_proc_progname, APS_ERROR, dtk_string, 
						DO_SYSLOG, DO_PRINT);

					fprintf (report_log, 
						"%s\n",DTKM_ERROR_MESSAGE(return_code) );
					fprintf( report_log, "%s\n" , dtk_string ) ;

					free_db_record( recording_dtk_rec ) ;
					free_db_record( template_dtk_rec ) ;
					return (FA_CSAF_DTK_DEFAULTS_NOT_FOUND);
					}

            	}
#ifdef DEBUG
/* -- print the DBRECORD: */
printf( "%s(%d):  APPENDING DB_RECORD - a recording - to input_dtk_list:\n",
__FILE__, __LINE__ ) ;
db_print_record( dtk_rec, APS_CDEFS(DTK) );
#endif 
			dtk_unlinked_rec = (DB_RECORD **)
				UNLINK_AT_CURSOR(new_rec_dtk_list, dtk_list_ptr) ;
            APPEND( input_dtk_list, dtk_unlinked_rec, 
								free_db_record, dtk_unlinked_rec) ;

			} /* get new proposal in new_rec_dtk_list */

    		DEL_LIST ( new_rec_dtk_list  ) ;

        }  /*  END if    fa_record_write_flag = "RITE"   */

    } /* END of for each reception  */

    /*
    -- Setup some global variables needed by permission logic, which follows.
    */
    min_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
    max_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

    if (db_get_min_record(input_dtk_list, APS_CDEFS(DTK), DTK_STRTTIME,
            &min_dtk_rec) != DB_MIN_RECORD_OK
    ||  db_get_max_record(input_dtk_list, APS_CDEFS(DTK), DTK_STOPTIME,
            &max_dtk_rec) != DB_MAX_RECORD_OK)
    {
        aps_log_msg(file_util_progname, APS_ERROR,
            "COULD NOT FIND START TIME OR STOP TIME FOR FILE\n",
            DO_SYSLOG, DO_PRINT);
		free_db_record (min_dtk_rec) ;
		free_db_record (max_dtk_rec) ;
        return (FA_CSAF_INDETERMINATE_FILE_TIMES) ;
    }

    strcpy(fa_file_start_time, CAST_DTK_STRTTIME min_dtk_rec[DTK_STRTTIME]);
    strcpy(fa_file_stop_time,  CAST_DTK_STOPTIME max_dtk_rec[DTK_STOPTIME]);

    free_db_record (min_dtk_rec) ;
    free_db_record (max_dtk_rec) ;
    free_db_record (template_dtk_rec) ;

    /*
    -- NOW, get more permissions before proceeding to the database changes.
    -- Until now, we have read the database for information, while processing
    -- our file into a list of datatake proposals.
    -- But before we actually CHANGE the database, we must get permissions.
    --
       Here is the chain of events:
       0) Parse the header.
       1) Handle permissions.  
          If activity permission not provided, get it.
          If activity permission is  provided, validate it.
       2) Parse the data records.
    -> 3) Get planning permission.
       4) ACCESS DATABASE. PROCESS PROPOSAL LIST.
       When execution terminates, permissions are automatically released.  
 
    */
 
    /*
    -- Item (3). Get PLANNING permission. 
    */
    return_code = mu_get_permission(
        csa_proc_progname,           /*  name of executable, for syslog()    */
        APS_dbproc,                  /*  Sybase process pointer              */
        *permission_id,              /*  if != 0, verify this permission.    */
        MU_PLANNING_ACTIVITY_TYPE,   /*  Multi-user activity type.           */
        fa_activity_type,            /*  Multi-user activity id.             */
        fa_file_start_time,          /*  start time of planning time bracket.*/
        fa_file_stop_time,           /*  end time of planning time bracket.  */
        fa_station_id,               /*  station id of planning activity.    */
        0,                           /*  DAR id of dar activity.             */
        0,                           /*  retry logic n_retries               */
        0  ) ;                       /*  retry logic n_seconds_retry         */
    if ( return_code < 0 )
        return FA_CSAF_PERMISSIONS_FAILED ;
    if ( return_code == 0 )
        return FA_CSAF_PERMISSIONS_DENIED ;

    return (FA_CSAF_INGESTION_OK);

}/* end of fa_csaf_ingestion() */


/*==============================================================================
Function:       fa_csaf_processor

Description:    Accepts a CSA Reception Request/Schedule File, 
                calls routines to convert it to a tree structure 
                and to scan it for information which is placed on
                a DBRECORD linked list.  
                This list is then processed by appropriate dtk routines and 
                its information incorporated into the DTK database table.

Parameters:    
Type     Name        Definition
*char    filename    Name of incoming CSA Reception Request file to be read.

Returns:
        FA_CSAF_PROCESS_OK                          success
        FA_CSAF_PROCESS_OK_BUT_PLANNER_MUST_READ    warning
        FA_CSAF_PROCESS_PERMDENIED                  error
        FA_CSAF_PROCESS_PERMFAILED                  error
        FA_CSAF_PROCESS_ERROR                       error

Creator:        Miguel Siu

Creation Date:  Fri May 19 09:46:50 PDT 1995

Notes:      FA_CSAF_PROCESS_OK follow convention  0 = no errors.
                                                  !0 = error.
==============================================================================*/
int fa_csaf_processor(
    DBPROCESS   *APS_dbproc,   /* open Sybase database process          */
    int         *permission_id,/* used to get permissions               */
    CSA_VALUEDEF *file_defs,
    char        *file_name,    /* input CSA Reception Request file name */
    FILE        *report_log)   /* report file pointer                   */
{
    int         return_code;
    int         fa_csaf_ingest_return_code;
    int         dtkm_process_list_return_code ;
    int         planner_read_status = 0 ;
	int			number_of_new_dtks  = 0 ;

    llist       *input_dtk_list = NULL ;
    llist       *accepted_dtks  = NULL ;
    llist       *rejected_dtks  = NULL ;
    llist       *error_dtks     = NULL ;
    llist       *omission_dtks  = NULL ;
    llist       *CON_dtks       = NULL ;
    llist       *deleted_dtks   = NULL ;
    llist       *other_sat_dtks = NULL ;
    llist       *same_sat_dtks  = NULL ;
    llist       *dtk_updates    = NULL ;

    llist       *rdl_dtks = NULL ;
    llist       *llist_check = NULL ;
    llist		*rejected_dtk_recordings ;

    NODEPTR       top_o_tree;

    input_dtk_list = create_dyn_llist() ;
    if(input_dtk_list == NULL)
    {
        sprintf(csa_proc_msg,
            "%s(%d):  ERROR creating a linked list.\n\n",
            __FILE__, __LINE__ ) ;
        aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg,
            DO_SYSLOG, DO_PRINT);
        return(FA_CSAF_PROCESS_ERROR);
    }


    /* Ingest the file */

    fa_csaf_ingest_return_code = 
    fa_csaf_ingestion(file_name, APS_dbproc, permission_id,
        file_defs, input_dtk_list, report_log );
    if ( fa_number_of_error_records > 0 )
    {
        sprintf(csa_proc_msg, 
            "NUMBER OF RECEPTIONS SKIPPED DUE TO ERRORS:  %d\n\n",
            fa_number_of_error_records ) ;
        aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);

        fprintf(report_log, 
            "\nNUMBER OF RECEPTIONS SKIPPED DUE TO ERRORS:  %d\n\n",
            fa_number_of_error_records ) ;
    }

    if ( fa_csaf_ingest_return_code != FA_CSAF_INGESTION_OK)
    {
        aps_log_msg(csa_proc_progname, APS_ERROR, 
            FA_CSAF_ERROR_MESSAGE(fa_csaf_ingest_return_code), 
            DO_SYSLOG, DO_PRINT);
        /*
        -- Special case, if permissions were denied,
        -- or if permissions process failed, notify calling program,
        -- by returning FA_CSAF_PROCESS_PERMDENIED/ FA_CSAF_PERMISSIONS_FAILED
        */
        if (fa_csaf_ingest_return_code == FA_CSAF_PERMISSIONS_DENIED)
            return (FA_CSAF_PROCESS_PERMDENIED) ;
        else if (fa_csaf_ingest_return_code == FA_CSAF_PERMISSIONS_FAILED)
            return (FA_CSAF_PROCESS_PERMFAILED) ;
        else
            return(FA_CSAF_PROCESS_ERROR);
    }

    /* 
    -- file ingested OK.  Now create and add 
    -- real-time downlinks to the list.  
    -- the ingestion only put in real-time 
    -- observations.  
    */
    rdl_dtks = create_dyn_llist() ;
    return_code = dtkm_add_rdl2dtks( input_dtk_list, rdl_dtks ) ;
    if( return_code < 0 )
        return return_code ;

#ifdef DEBUG
    printf(
"DTK list before adding realtime downlinks and consolidating tapedumps:   %d\n",
        NUMELTS( input_dtk_list ) ) ;
    dtkm_print_list(stdout, input_dtk_list) ;
    printf("Realtime data-takes created for realtime observations:  %d\n", 
        NUMELTS( rdl_dtks ) ) ;
    dtkm_print_list(stdout, rdl_dtks) ;
#endif

    /* 
    -- move the realtime downlinks to the 
    -- input list for later processing.  
    */
	number_of_new_dtks  = NUMELTS (rdl_dtks) ;

    llist_check = db_record_llist_move( input_dtk_list, rdl_dtks ) ;
    if( llist_check != input_dtk_list )
    {
        sprintf(csa_proc_msg,
            "%s(%d):  ERROR in moving data-takes to linked list.\n\n",
            __FILE__, __LINE__ ) ;
        aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg,
            DO_SYSLOG, DO_PRINT);
        return(FA_CSAF_PROCESS_ERROR);
    }
    DEL_LIST( rdl_dtks ) ;

    /* 
    -- file ingested OK.  Now consolidate all 
    -- tape dump downlinks into one tape 
    -- dump downlink.  
    */
#ifdef DEBUG
    printf("DTK list before consolidation of tape dumps:   %d\n", 
        NUMELTS( input_dtk_list ) ) ;
    dtkm_print_list(stdout, input_dtk_list) ;
#endif

    return_code = dtkm_consolidate_tapedumps( input_dtk_list ) ;
    if( return_code < 0 )
        return return_code ;

#ifdef DEBUG
    printf("DTK list after consolidation of tape dumps:   %d\n", 
        NUMELTS( input_dtk_list ) ) ;
    dtkm_print_list(stdout, input_dtk_list) ;
#endif

#ifdef DEBUG
    printf("DTK list completed:   %d\n", NUMELTS( input_dtk_list ) ) ;
    dtkm_print_list(stdout, input_dtk_list) ;
#endif

	/*
	-- FINAL step before dtkm processing. Verify that all PLAYBACK
	-- events correspond to ACQUISITION (recording) activities which
	-- have a time range smaller or equal to the PLAYBACK.
	*/
    rejected_dtk_recordings = create_dyn_llist() ;
    if(rejected_dtk_recordings == NULL)
    {
        sprintf(csa_proc_msg,
            "%s(%d):  ERROR creating a linked list.\n\n",
            __FILE__, __LINE__ ) ;
        aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg,
            DO_SYSLOG, DO_PRINT);
        return(FA_CSAF_PROCESS_ERROR);
    }

#ifdef DEBUG
    printf("DTK list before check_playback_and_recording_times\n") ;
    dtkm_print_list(stdout, input_dtk_list) ;
#endif

	return_code = check_playback_and_recording_times(
						input_dtk_list, rejected_dtk_recordings) ;
	if (return_code < 0)
    {
        sprintf(csa_proc_msg,
            "%s(%d):  ERROR when trying to discard too-long recordings.\n\n",
            __FILE__, __LINE__ ) ;
        aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg,
            DO_SYSLOG, DO_PRINT);
        return(FA_CSAF_PROCESS_ERROR);
    }

	if (NUMELTS (rejected_dtk_recordings) != 0)
	{
		fprintf(report_log, 
		"\nTHE FOLLOWING OBSERVATIONS ARE TOO LONG; THEY ARE DISCARDED:\n") ;
		dtkm_print_list(report_log, rejected_dtk_recordings) ;

        aps_log_msg(csa_proc_progname, APS_INFO, 
		"THE FOLLOWING OBSERVATIONS ARE TOO LONG; THEY ARE DISCARDED:\n",
            DO_SYSLOG, DO_PRINT);
		dtkm_print_list(stdout, rejected_dtk_recordings) ;
		DEL_LIST (rejected_dtk_recordings) ;
	}

    /* Create a report header */

    if ( !aps_report_header (report_log, 
                            "CSA", 
                            fa_file_type,
                            file_name, 
                            fa_creation_date))
    {
#ifdef DEBUG                                        
        printf("Could not create dtk report header\n");
#endif
        aps_log_msg(csa_proc_progname, APS_ERROR,
                    "Could not create dtk report header.",
                    DO_SYSLOG, DO_PRINT);
        return(FA_CSAF_PROCESS_ERROR);
    }

/*============================================================================
      ##     ####    ####   ######   ####    ####           #####   #####
     #  #   #    #  #    #  #       #       #               #    #  #    #
    #    #  #       #       #####    ####    ####           #    #  #####
    ######  #       #       #            #       #          #    #  #    #
    #    #  #    #  #    #  #       #    #  #    #          #    #  #    #
    #    #   ####    ####   ######   ####    ####           #####   #####
 =============================================================================*/
    /* NOW, process the link list into database records */
    accepted_dtks  = create_dyn_llist() ;
    rejected_dtks  = create_dyn_llist() ;
    CON_dtks       = create_dyn_llist() ;
    deleted_dtks   = create_dyn_llist() ;
    error_dtks     = create_dyn_llist() ;
    omission_dtks  = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks  = create_dyn_llist() ;
    dtk_updates    = create_dyn_llist() ;

#ifdef DEBUG                                        
    printf("\n\n\n\n\n about to call dtk_process_list()\n\n\n\n\n");
#endif

    dtkm_process_list_return_code = 
    dtkm_process_dtk_proposal_list (APS_dbproc, input_dtk_list,
        accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks, error_dtks, 
        omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates, 
        report_log);

    aps_log_msg(csa_proc_progname, APS_INFO, 
        "DTK STATISTICS\n", 
        DO_SYSLOG, DO_PRINT);
    sprintf(csa_proc_msg, 
        "DL/DTKS processed          : %d", NUMELTS (input_dtk_list)) ;
    aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
        DO_SYSLOG, DO_PRINT);

	if (number_of_new_dtks > 0)
	{
		sprintf(csa_proc_msg, 
			"DL/DTK read from CSA file  : %d", 
			NUMELTS(input_dtk_list) - number_of_new_dtks) ;
		aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
			DO_SYSLOG, DO_PRINT);
		sprintf(csa_proc_msg, 
			"DL/DTK created for CSA file: %d\n", number_of_new_dtks) ;
		aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
			DO_SYSLOG, DO_PRINT);
	}
    sprintf(csa_proc_msg, 
		"ACCEPTED                   : %d", NUMELTS (accepted_dtks)) ;
    aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
        DO_SYSLOG, DO_PRINT);

    sprintf(csa_proc_msg, 
        "REJECTED                   : %d", NUMELTS (rejected_dtks)) ;
    if ( NUMELTS (rejected_dtks) > 0 )
    aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
        DO_SYSLOG, DO_PRINT);

    sprintf(csa_proc_msg, 
        "BLOCKED BY CONFLICTS       : %d", NUMELTS (CON_dtks)) ;
    if ( NUMELTS (CON_dtks) > 0 )
    aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
        DO_SYSLOG, DO_PRINT);

    if ( NUMELTS (error_dtks) > 0 )
    {
        sprintf(csa_proc_msg, 
        	"ERROR                      : %d", NUMELTS (error_dtks)) ;
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(csa_proc_progname, APS_INFO, 
            "        PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n", 
            DO_SYSLOG, DO_PRINT);

        fprintf (report_log, 
            "\nPLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n" ) ; 
    }

    sprintf(csa_proc_msg, 
		"REJECTED BY OMISSION       : %d", NUMELTS (omission_dtks)) ;
    if ( NUMELTS (omission_dtks) > 0 )
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);

    sprintf(csa_proc_msg, 
        "TOTAL UPDATES              : %d", NUMELTS (dtk_updates)) ;
    aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
        DO_SYSLOG, DO_PRINT);
 
    if ( fa_number_of_error_records > 0 )
    {
        sprintf(csa_proc_msg, 
        	"ERROR RECS IN FA FILE      : %d", fa_number_of_error_records ) ;
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);

        aps_log_msg(csa_proc_progname, APS_INFO, 
            "        PLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n", 
            DO_SYSLOG, DO_PRINT);
 
        fprintf (report_log,
            "\nERROR RECS IN FA FILE      : %d", fa_number_of_error_records ) ;
        fprintf (report_log,
            "\nPLANNER MUST REVIEW LOG FILE ERROR MESSAGES.\n" ) ;
    }
 
    if (fa_number_of_error_records  > 0
    ||  NUMELTS( error_dtks )    > 0 
    ||  NUMELTS( rejected_dtks ) > 0 
    ||  NUMELTS( CON_dtks )      > 0 
    ||  NUMELTS( omission_dtks ) > 0  )
    {
        /* 
        -- we want the planner to read the 
        -- report.  
        */
        planner_read_status = 1 ;
    }

    /* 
    -- clean up: delete storage structures
    */
    DEL_LIST ( accepted_dtks  ) ;
    DEL_LIST ( rejected_dtks  ) ;
    DEL_LIST ( CON_dtks     ) ;
    DEL_LIST ( deleted_dtks ) ;
    DEL_LIST ( error_dtks   ) ;
    DEL_LIST ( omission_dtks) ;
    DEL_LIST ( other_sat_dtks ) ;
    DEL_LIST ( same_sat_dtks  ) ;
    DEL_LIST ( dtk_updates ) ;
    DEL_LIST ( input_dtk_list ) ;

    /*
    -- COMPLETED processing.
    -- Terminate the planning permission.
    -- Actually, execution is ending, no need.  
    */
    if (dtkm_process_list_return_code < 0)
    {
        aps_log_msg(csa_proc_progname, APS_INFO, 
            DTKM_ERROR_MESSAGE(dtkm_process_list_return_code), 
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, 
        "%s\n",DTKM_ERROR_MESSAGE(dtkm_process_list_return_code) );
        fprintf(report_log, "CSA ingestion completion: FAILED\n" ) ;
        return(FA_CSAF_PROCESS_ERROR);
    }
    else if ( planner_read_status )
    {
        aps_log_msg(csa_proc_progname, APS_INFO, 
            "There were errors and/or rejections.  \n", 
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, "\nThere were errors and/or rejections.  \n" ) ;
        fprintf(report_log, 
            "CSAF PROCESS OK BUT PLANNER MUST READ THE REPORT\n" ) ;
        
        if(fa_number_of_error_records > 0)
            return FA_CSAF_PROCESS_ERROR ;
        else
            return FA_CSAF_PROCESS_OK_BUT_PLANNER_MUST_READ ;
    }

    /* normal return.  */
    aps_log_msg(csa_proc_progname, APS_INFO, 
        "There were no errors or rejections.  \n", 
        DO_SYSLOG, DO_PRINT);
    fprintf(report_log, "There were no errors or rejections.  \n" ) ;
    return (FA_CSAF_PROCESS_OK) ;

}  /* end of fa_csaf_processor  */




/*==============================================================================
Function:       main                

Description:    Gets the command line arguments (ie: database name, password,
                name of CSA Reception Request File) open up the database, and
                pass this information to the routine fa_csaf_processor() 

Parameters:     

Returns:        APS_EXIT_OK = success.
                APS_EXIT_ERROR = error.

Creator:        Miguel Siu

Creation Date:  Fri May 19 09:22:16 PDT 1995

Notes:  ASSUMPTION:fa_csaf_processor routine returns APS_REPORT_* codes
==============================================================================*/
void
main(int argc, char *argv[])
{     
    /* declarations     */
    void        csa_usage_exit(char *progname) ;
    DBPROCESS   *dbproc;

    char    *dbname = NULL ;
    char    *sybase_userid = NULL ;
    char    *password = NULL ;
    char    *fullpath_file = NULL ;
    char    *input_file;
    char    *report_dir;
    char    *fa_error_dir;
    char    *full_reportfile;

    char    flag_list[20] = "p:U:P:t:k"; /* list of flags for getopts  */
    char    *env_dbname;        /* dbname from environment      */
    char    *env_sybase_userid; /* userid from environment      */
    char    *file_type = NULL;

    int     j, c;      /* used as return character from getopt()       */
    int     rcode;  /* return code for DB open                      */
    int     status; /* return code for fa_csaf_processor()          */
    int     kflag = 0 ;/* (k for keep) disables the error file move. */
    int     return_code;        /* for getting permission */
    int     permission_id = 0;  /* for getting permission */

    CSA_VALUEDEF     *CSA_file_defs ;

    static char report_ext[] = ".rpt";
    static char slash[] = "/";

    extern char *optarg;
    extern int  optind;

    FILE    *filedesc ;
    FILE    *report_log ;

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    /* initialize the vector library; exit with a message if an error   */
    return_code = init_vec_lib();
    if(return_code)
    {
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
        init_vec_lib_exit(APS_EXIT_ERROR);
    }

    /*
    -- store the program name for system/error logging
    -- and start up the Syslog process
    */
    csa_proc_progname = strrchr( argv[0], '/' ) ;
    if (csa_proc_progname == NULL)
        csa_proc_progname = argv[0] ;
    else
        csa_proc_progname++ ;

    file_util_progname = csa_proc_progname ;
    aps_open_syslog();

    sprintf(csa_proc_msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(csa_proc_msg, " " ) ;
        strcat(csa_proc_msg, argv[j] ) ;
    }
    aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg,
        DO_SYSLOG, DO_PRINT);


    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
        case 'P':
            if(password != NULL)
                csa_usage_exit(csa_proc_progname);
            password = optarg ;
            break;
        case 'U':
            if(sybase_userid != NULL)
                csa_usage_exit(csa_proc_progname);
            sybase_userid = optarg ;
            break;
        case 'p':
            return_code = sscanf( optarg, "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                csa_usage_exit(csa_proc_progname);
            }
            break ;
        case 't':
            if(file_type != NULL)
                csa_usage_exit(csa_proc_progname);
            file_type = optarg;
            break;
        case 'k':
            if(kflag != 0)
                csa_usage_exit(csa_proc_progname);
            kflag ++ ;
            break;
        case '?':
            csa_usage_exit(csa_proc_progname);
            break;
        default:
            /* do  nothing  */
            break;
        }

    /* check that all arguments are attached to flags, except one */
    if(optind != argc - 1)
        csa_usage_exit(csa_proc_progname);

    /* manditory flag:  */
    if(password == NULL
    || file_type == NULL)
        csa_usage_exit(csa_proc_progname);
 
    /* optional flag    */
    if(sybase_userid == NULL)
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            aps_log_msg(csa_proc_progname, APS_INFO, 
"ERROR:  sybase_userid not found in environment variable APS_SYBASE_USERID\n",
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(csa_proc_progname, APS_INFO, 
                "Use -U sybase_userid or setenv APS_SYBASE_USERID.\n\n", 
                DO_SYSLOG, DO_PRINT);
            csa_usage_exit(csa_proc_progname);
        }
        else
            /* use the environment sybase_userid.   */
            sybase_userid = env_sybase_userid ;
    }

    /* get the database name from the environment   */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
        aps_log_msg(csa_proc_progname, APS_INFO, 
            "ERROR:  dbname not found in environment variable APSDB\n", 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(csa_proc_progname, APS_INFO, 
            "Use setenv APSDB <dbname>. \n\n", 
            DO_SYSLOG, DO_PRINT);
        csa_usage_exit(csa_proc_progname);
    }
    dbname = env_dbname ;

    /* now open the database.  */
    /* db_open will handle the errors.      */
    dbproc = db_open(dbname,csa_proc_progname,sybase_userid,password,NULL,
        error_handler_exit,&rcode);
    if(rcode != DB_OPEN_OK)
    {
        sprintf(csa_proc_msg, "Could not open database '%s'\n", dbname) ;
        aps_log_msg(csa_proc_progname, APS_ERROR, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);

        db_open_errs(rcode, dbname, sybase_userid);
    }

    report_dir = aps_fullpath (APS_REPORTS, NULL);

    fullpath_file = argv[argc -1] ;
    input_file = aps_pathname2filename (fullpath_file) ;

    full_reportfile = malloc(
        strlen(report_dir) + 1 + strlen(input_file) + 
        strlen(file_type)  + 1 + strlen(report_ext) + 1) ;
    sprintf (full_reportfile, 
        "%s%s%s.%s%s", report_dir, slash, input_file, file_type, report_ext) ;

    /*
    -- check the input_file for existence.  
    */
    filedesc = fopen( fullpath_file, "r" ) ;
    if ( filedesc == NULL )
    {
        sprintf(csa_proc_msg, 
"file %s could not be opened for read access check for existence and permission\n", input_file) ;
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(csa_proc_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }
    else
    {
        fclose( filedesc ) ;
    }

    /* 
    -- open the report file
    */
    report_log = fopen(full_reportfile,"w");
    if (report_log == NULL)
    {
        aps_log_msg(csa_proc_progname, APS_ERROR, 
            "could not open report error log file\n", 
            DO_SYSLOG, DO_PRINT);

        aps_log_msg(csa_proc_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }

    /*
    -- Search through CSA file descriptors.
    */
    if (!match_file__get_field_defs(file_type, CSA_files,  &CSA_file_defs ))
    {
        sprintf (csa_proc_progname,
            "COULD NOT MATCH FILE %s TO A FLIGHT AGENCY.\n",file_type);
        aps_log_msg(csa_proc_progname, APS_ERROR, file_util_msg,
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(csa_proc_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);

        /*
        -- delete the report file; it is empty
        */
        (void) unlink(full_reportfile) ;

        exit (APS_EXIT_ERROR) ;
    }



    aps_log_msg(csa_proc_progname, APS_INFO, 
        "CSA ingestion starting... \n", 
        DO_SYSLOG, DO_PRINT);
    status =  fa_csaf_processor(dbproc, &permission_id, CSA_file_defs,
                fullpath_file, report_log);

    aps_log_msg(csa_proc_progname, APS_INFO, 
        "CSA ingestion completion: ", 
        DO_SYSLOG, DO_PRINT);
    if ( status == FA_CSAF_PROCESS_OK )
    {
        sprintf(csa_proc_msg, 
            "SUCCESS.      (see report %s for status of DTK processing)\n",
            full_reportfile);
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(csa_proc_progname, APS_INFO, 
            "Program completed successfully.", 
            DO_SYSLOG, DO_PRINT);

        /* 
        -- SUCCESS:must move the error input file to FA_input_files directory.  
        --  -k (for keep file ) sets the kflag, to disable the file move.  
        --
        -- This routine also prints an informational message to the Syslog.
        */
        if ( kflag == 0 )
        {
            /* fa_error_dir is not really an error dir...  */
            fa_error_dir = aps_fullpath (APS_FA_INPUT_FILES, NULL);

            /*
            -- fa_copy_file() will print its own messages to syslog
            */
            if ( !fa_copy_file( fullpath_file, APS_FA_INPUT_FILES ) )
            {
                sprintf(file_util_msg, 
                    "Non-fatal ERROR. Could not copy %s to %s area.\n",
                    fullpath_file, fa_error_dir);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            else
                fprintf(report_log, 
    "File %s has no errors; it was copied to %s area for your convenience.\n",
                fullpath_file, fa_error_dir);

            free (fa_error_dir);
        }

        status = APS_EXIT_OK ;
    }
    else if ( status == FA_CSAF_PROCESS_OK_BUT_PLANNER_MUST_READ)
    {
        sprintf(csa_proc_msg, 
            "PROCESSED.  (review report %s for status of DTK processing)\n",
            full_reportfile);
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);

        /* 
        -- ERROR:  must move the error input file to FA_error_files directory.  
        --  -k (for keep file ) sets the kflag, to disable the file move.  
        --
        -- This routine also prints an informational message to the Syslog.
        */
        if ( kflag == 0 )
        {
            fa_error_dir = aps_fullpath (APS_FA_ERROR_FILES, NULL);

            /*
            -- fa_move_file() will print its own messages to syslog
            */
            if ( !fa_move_file( fullpath_file, APS_FA_ERROR_FILES ) )
            {
                sprintf(file_util_msg, 
                    "Non-fatal ERROR. Could not move %s to %s area.\n",
                    fullpath_file, fa_error_dir);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            else
                fprintf(report_log, 
    "File %s has no errors,but we move it to %s area for your convenience.\n",
                fullpath_file, fa_error_dir);

            free (fa_error_dir);
        }


        aps_log_msg(csa_proc_progname, APS_INFO, 
            "Program terminated; review results.", 
            DO_SYSLOG, DO_PRINT);

        status = APS_EXIT_ERROR ;
    }
    else if ( status == FA_CSAF_PROCESS_ERROR)
    {
        sprintf(csa_proc_msg, 
            "FAILED.   (review report %s for status of DTK processing)\n",
            full_reportfile);
        aps_log_msg(csa_proc_progname, APS_INFO, csa_proc_msg, 
            DO_SYSLOG, DO_PRINT);

        /* 
        -- ERROR:  must move the error input file to FA_error_files directory.  
        --  -k (for keep file ) sets the kflag, to disable the file move.  
        --
        -- This routine also prints an informational message to the Syslog.
        */
        if ( kflag == 0 )
        {
            fa_error_dir = aps_fullpath (APS_FA_ERROR_FILES, NULL);

            /*
            -- fa_move_file() will print its own messages to syslog
            */
            if ( !fa_move_file( fullpath_file, APS_FA_ERROR_FILES ) )
            {
                sprintf(file_util_msg, 
                    "Non-fatal ERROR. Could not move %s to %s area.\n",
                    fullpath_file, fa_error_dir);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            else
                fprintf(report_log, 
                "File %s has an error.  It has been moved to %s area.\n",
                fullpath_file, fa_error_dir);

            free (fa_error_dir);
        }


        aps_log_msg(csa_proc_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);

        status = APS_EXIT_ERROR ;
    }
    else if ( status == FA_CSAF_PROCESS_PERMDENIED)
    {
        aps_log_msg(csa_proc_progname, APS_INFO,
            "PROCESSING STOPPED.  PERMISSIONS WERE DENIED.\n",
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(csa_proc_progname, APS_INFO,
            "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
 
        /*
        -- delete the report file, it is empty
        */
        (void) unlink(full_reportfile) ;
 
        status = APS_EXIT_ERROR ;
    }
    else if ( status == FA_CSAF_PROCESS_PERMFAILED)
    {
        aps_log_msg(csa_proc_progname, APS_INFO,
            "PROCESSING STOPPED.  PERMISSION-ACQUISITION PROCESS FAILED.\n",
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(csa_proc_progname, APS_INFO,
            "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
 
        /*
        -- delete the report file, it is empty
        */
        (void) unlink(full_reportfile) ;
 
        status = APS_EXIT_ERROR ;
    }
    else
    /* indeterminate error condition */
    {
        aps_log_msg(csa_proc_progname, APS_INFO,
            "UNKNOW ERROR CONDITION ENCOUNTERED.\n",
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(csa_proc_progname, APS_INFO,
            "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
 
        status = APS_EXIT_ERROR ;
    }


    free (report_dir);
    free (input_file);
    free (full_reportfile);
    fclose( report_log );
    exit(status); /* status = APS_EXIT_OK or APS_EXIT_ERROR */

} /* end of main() */



void csa_usage_exit(char *progname)
{
    fprintf(stderr,     /* nicer format, keep it */
"\nusage:  %s  -P <password>  -t <filetype> [-U <user name>]\n\t\t\t[-p <permission>] [-k]   filename\n", progname);
    fprintf(stderr,     /* nicer format, keep it */
"\n\tNOTES:  \n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t%s will ingest CSA Reception Request/Schedule files\n", progname);
    fprintf(stderr,     /* nicer format, keep it */
"\tIt scans the file for appropriate keywords and converts these   \n");
    fprintf(stderr,     /* nicer format, keep it */
"\tvalues before inserting them into the DTK relation.              \n");
    fprintf(stderr,     /* nicer format, keep it */
"\tIf there is an error in the input file, it is moved to the \n");
    fprintf(stderr,     /* nicer format, keep it */
"\tAPS_DATA/FA_error_files directory.  If not, it is moved to the \n");
    fprintf(stderr,     /* nicer format, keep it */
"\tAPS_DATA/FA_input_files tdirectory.  \n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-U <user name>   Sybase userid for Sybase account") ;
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-P <password>    Sybase password for Sybase account") ;
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-p <permission>  single-activity permission (from the calling program)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-k   keeps the input file where it is; prevents the move or copy.\n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-t <filetype> %s will ingest the following ODL format files:\n", progname );
    fprintf(stderr,     /* nicer format, keep it */
"\t   CRRA  (CSA request file  for ASF groundstation)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\t   CRRM  (CSA request file  for McMurdo groundstation)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\t   CRSA  (CSA schedule file for ASF groundstation)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\t   CRSM  (CSA schedule file for McMurdo groundstation)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\t---------------------------------------------------\n");

    aps_log_msg(csa_proc_progname, APS_INFO,
        "ODL ingestion Program terminated abnormally.\n\n\n",
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
}


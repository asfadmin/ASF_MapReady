#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		WOS_compare.c

Description:	This is a tool used to compare a given WOS file to its
				equivalent database contents, for the time range of the 
				WOS file.

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
==============================================================================*/
#pragma ident	"@(#)WOS_compare.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/WOS_tools/SCCS/s.WOS_compare.c"

#include <stdlib.h>         /* for getopt                           */
#include <stdio.h>
#include <string.h>
#include <unistd.h>			/* for unlink command					*/

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */
#include "db_dtk.h"         /* for dtk table definitions */
#include "aps_log_msg.h"    /* for APS logging                      */
#include "aps_defs.h"       /* exit codes for APS logging           */

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"

/* FOR ODL PARSE
*/
#include <odldef.h>
 
#include "ODLconversions.h"
#include "GENconversions.h"
#include "file_utilities.h"
#include "APSpmfutilities.h"
#include "dtkm_utilities.h"  /* for dtkm_print prototype, etc. */
#include "timeconv.h"  /* for dtkm_print prototype, etc. */


char        *file_util_progname ; /* required by libfileutils.a */
char        file_util_msg[MSG_LEN]; /* required by libfileutils.a */

/* STRICTLY FOR WOS COMPARE */
#define		MWOSCOMP_MATCH					0
#define		MWOSCOMP_NO_MATCH				1
#define		MWOSCOMP_DTKSTAT_MODIFIED		2
#define		MWOSCOMP_BAD_DB_RECORD_COPY    -1
#define		MWOSCOMP_DB_CORRUPTION         -2


static void 
usage_exit(char *progname)
{
    fprintf(stderr,     /* nicer format, keep it */
    "\nusage: %s  [-U sybase_userid] -P password  filename\n",
        progname);
    fprintf(stderr,     /* nicer format, keep it */
        "\n\tNOTES:  \n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\tThis programs compares WOS files to the contents of db. \n");
	fprintf(stderr, 	/* nicer format, keep it */
"\n\t%s will compare the following WOS files:\n", progname );
	fprintf(stderr, 	/* nicer format, keep it */
"\n\t   AWOS (ASF WOS schedule file, sent to Host Controller)\n");
	fprintf(stderr, 	/* nicer format, keep it */
"\t   MWOS (WFF WOS schedule file, sent to Wallops)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t%s version:  %s %s   ---------\n", progname, __DATE__, __TIME__ );
 
    aps_log_msg(file_util_progname, APS_INFO,
        "MWOS comparison: terminated abnormally.\n\n\n",
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
}

/*==============================================================================
Function:       dtk_proposal_matches_db

Description:   	Compare a datatake proposal with its database counterpart.
				Report any differences.
				This routine will only work for WOS datatake proposals,
				since it assumes a status value of 'SCH' or 'PLN'
Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Oct  8 12:43:18 PDT 1996

Notes:		
	This routine does NOT compare all of the fields in the db record.
	It concentrates only on those fields that may have changed during planning
	activities.

	The incoming dtk proposal is assumed to have a status of 'SCH' or 'PLN'
	We are therefore forgiving, and will accept either status, if found in  
	the database, as a match.
==============================================================================*/
static int 
dtk_proposal_matches_db(
	DBPROCESS		*APS_dbproc,
	DB_RECORD       **proposal_dtk,
	DB_RECORD       **db_dtk)
{
    DB_RECORD   **lookup_rec ;
	cursor      lookup_rec_ptr ;
    llist       *lookup_dtks_list  ;
	int			return_code ;

	sprintf(where_clause,
		"where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
		APS_COL(DTK,DTK_SAT), CAST_DTK_SAT proposal_dtk[DTK_SAT],
		APS_COL(DTK,DTK_SENSOR), CAST_DTK_SENSOR proposal_dtk[DTK_SENSOR],
		APS_COL(DTK,DTK_REV), CAST_DTK_REV proposal_dtk[DTK_REV],
		APS_COL(DTK,DTK_DTKID), CAST_DTK_DTKID proposal_dtk[DTK_DTKID] ) ;

	/*
	-- get the corresponding db record
	*/
	lookup_dtks_list = 
		db_get_records(APS_dbproc, APS_TABLE(DTK),
		where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
	lookup_rec = (DB_RECORD **) FIRST(lookup_dtks_list, lookup_rec_ptr) ;

	if (lookup_rec == NULL)
		return (MWOSCOMP_DB_CORRUPTION) ;

	return_code = db_copy_record( APS_CDEFS(DTK), db_dtk, lookup_rec ) ;
	if ( return_code < 0 )
		{
		return (MWOSCOMP_BAD_DB_RECORD_COPY) ;
		}
	/*
	-- COMPARISON: check key fields that may change during
	-- planning.
	-- NOTE: We only compare the activity in DTK_ACTID, not the agency.
	*/
	if (strcmp(CAST_DTK_STRTTIME proposal_dtk[DTK_STRTTIME],
			   CAST_DTK_STRTTIME       db_dtk[DTK_STRTTIME] ) == 0
	&&  strcmp(CAST_DTK_STOPTIME proposal_dtk[DTK_STOPTIME],
			   CAST_DTK_STOPTIME       db_dtk[DTK_STOPTIME] ) == 0
	&&  strcmp(CAST_DTK_STATION_ID proposal_dtk[DTK_STATION_ID],
			   CAST_DTK_STATION_ID       db_dtk[DTK_STATION_ID] ) == 0
	&& CAST_DTK_ANTENNA_ID proposal_dtk[DTK_ANTENNA_ID] ==
			   CAST_DTK_ANTENNA_ID       db_dtk[DTK_ANTENNA_ID]
	&&  strncmp(CAST_DTK_ACTID proposal_dtk[DTK_ACTID],
			   CAST_DTK_ACTID       db_dtk[DTK_ACTID] , 3) == 0
	&&  strcmp(CAST_DTK_TRANSID proposal_dtk[DTK_TRANSID],
			   CAST_DTK_TRANSID       db_dtk[DTK_TRANSID] ) == 0
		)
	{
		if ( strcmp(CAST_DTK_DTKSTAT  db_dtk[DTK_DTKSTAT], "SCH") == 0
		||   strcmp(CAST_DTK_DTKSTAT  db_dtk[DTK_DTKSTAT], "PLN") == 0 )
			return (MWOSCOMP_MATCH) ;
		else
			return (MWOSCOMP_DTKSTAT_MODIFIED) ;
	}


	return (MWOSCOMP_NO_MATCH) ;
}

void
main(int argc, char *argv[])
{
	DBPROCESS   *APS_dbproc;
	char		dtk_string[512];		/* for dtk printing in error message */

	int		j, c ;
	int		status ;
	int		permission_id = 0 ;
    int     rcode;  /* return code for DB open                      */
    int     return_code;  /* return code for init_vec_lib           */
	int		WOS_EMPTY = 0 ;
	int		WOS_PMF_DATA_FOUND = 0 ;
	int		WOS_DTK_CHANGES = 0 ;
	int		WOS_STAT_CHANGES = 0 ;
	int		WOS_NEW_DTKS_IN_DB = 0 ;
	int		WOS_NEW_DTKS_IN_WOS = 0 ;

    char    *dbname = NULL ;
    char    *env_dbname;        /* dbname from environment      */
	char    flag_list[20] = "U:P:t:"; /* list of flags for getopts  */
    char    *env_sybase_userid; /* userid from environment      */
    char    *sybase_userid = NULL ;
    char    *password = NULL ;
    char    *fullpath_file;
    char    *input_file;
    char    *report_dir;
	char    *full_reportfile;
	char    *full_pmffile;
	char	pmf_keyword[] =  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
	char	pmf_value[]   =  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ;
	char	wos_strttime[] = "yyyy:ddd:hh:mm:ss.ccc" ; 
	char	wos_stoptime[] = "yyyy:ddd:hh:mm:ss.ccc" ; 

    llist       *input_dtk_list = NULL ;
    llist       *WOS_dtks_list = NULL ;
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **corresponding_db_dtk ;
    cursor      dtk_rec_ptr ;

    static char report_ext[] = ".rpt";
    static char slash[] = "/";

	extern int  optind;
    extern char *optarg;

    FILE    *filedesc ;
    FILE    *file_pmf ;
    FILE    *report_log ;

#ifdef NOT_NEEDED_UNTIL_MORE_FILE_DESCRIPTORS_ARE_ADDED
	ODL_FILENAME *ODL_file_descriptors ;
#endif

    /* initialize the vector library; exit with a message if an error   */
    return_code = init_vec_lib();
    if(return_code)
    {
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
        init_vec_lib_exit(APS_EXIT_ERROR);
    }

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;
 
    /*
    -- store the program name for system/error logging
    -- and start up the Syslog process
    */
    file_util_progname = argv[0] ;
    aps_open_syslog();
 
    sprintf(file_util_msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(file_util_msg, " " ) ;
        strcat(file_util_msg, argv[j] ) ;
    }
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg,
        DO_SYSLOG, DO_PRINT);

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
        case 'P':
            if(password != NULL)
                usage_exit(argv[0]);
            password = optarg ;
            break;
        case 'U':
            if(sybase_userid != NULL)
                usage_exit(argv[0]);
            sybase_userid = optarg ;
            break;
        case '?':
            usage_exit(argv[0]);
            break;
        default:
            /* do  nothing  */
            break;
        }

    /* check for extraneous words not attached to a flag.   */
    if(optind != argc - 1)
        usage_exit(argv[0]);
 
    /* manditory flag:  */
    if(password == NULL)
        usage_exit(argv[0]);
 
    /* optional flag    */
    if(sybase_userid == NULL)
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            aps_log_msg(file_util_progname, APS_ERROR,
"sybase_userid not found in environment variable APS_SYBASE_USERID\n",
            DO_SYSLOG, DO_PRINT);
 
            aps_log_msg(file_util_progname, APS_ERROR,
"Use -U sybase_userid or setenv APS_SYBASE_USERID.\n",
            DO_SYSLOG, DO_PRINT);
            usage_exit(argv[0]);
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
        aps_log_msg(file_util_progname, APS_ERROR,
            "dbname not found in environment variable APSDB\n",
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR,
            "Use setenv APSDB <dbname>. \n",
            DO_SYSLOG, DO_PRINT);
    
        usage_exit(argv[0]);
    }
    dbname = env_dbname ;

    /* now open the database.  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open(dbname,argv[0],sybase_userid,password,NULL,
        error_handler_exit,&rcode);
    if(rcode != DB_OPEN_OK)
    {
        db_open_errs(rcode, dbname, sybase_userid);
        rcode = fflush(stdout) ;
 
        aps_log_msg(file_util_progname, APS_INFO,
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }


    report_dir = aps_fullpath (APS_REPORTS, NULL);

    fullpath_file = argv[argc -1];
	input_file = aps_pathname2filename (fullpath_file) ;
	fa_filename = input_file ;

    full_reportfile = malloc( 
		strlen(report_dir) + 1 + strlen(input_file) +
		strlen(report_ext) + 1	) ;
    sprintf (full_reportfile,
        "%s%s%s%s", report_dir, slash, input_file, report_ext) ;

    full_pmffile = malloc( strlen(fullpath_file) + 3 ) ;
    sprintf (full_pmffile, "%s.M", fullpath_file) ;



    /*
    -- check the input_file, and its PMF file for existence.
    */
    filedesc = fopen( fullpath_file, "r" ) ;
    file_pmf = fopen( full_pmffile, "r" ) ;
    if ( filedesc == NULL )
    {
        sprintf(file_util_msg,
"file %s could not be opened for read access; check file existence/permission", input_file) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit (APS_EXIT_ERROR) ;
    }

	if ( file_pmf == NULL )
    {
        sprintf(file_util_msg,
"file %s.M could not be opened for read access; check its existence/permission",
		input_file) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit (APS_EXIT_ERROR) ;
    }
 

    /*
    -- open the report file
    */
    report_log = fopen(full_reportfile,"w");
    if (report_log == NULL)
    {
        aps_log_msg(file_util_progname, APS_ERROR,
            "could not open report error log file\n",
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit (APS_EXIT_ERROR) ;
    }
 
 
	/*
	-- Create the empty list of datatake proposals
	*/
    input_dtk_list = create_dyn_llist() ;
    if(input_dtk_list == NULL)
    {
#ifdef DEBUG
        printf ("could not create dyn llist!!!\n");
#endif
		unlink( full_reportfile ) ;
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit(APS_EXIT_ERROR);
    }


	/*
	-- Before ingesting the file,
	-- determine if this file contains any records.
	*/
	while ( fscanf(filedesc,"%s = %s", pmf_keyword, pmf_value) != EOF)
	{
		if (strcmp(pmf_keyword, "NUMBER_OF_RECORDS") == 0)
		{
			if (strncmp(pmf_value, "0", 1) == 0)
				WOS_EMPTY = 1 ;
			break ;
		}
	}

	if( WOS_EMPTY )
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"THIS FILE CONTAINS ZERO (0) DOWNLINKS. NO COMPARISON NEEDED.\n",
			DO_SYSLOG, DO_PRINT);

		aps_log_msg(file_util_progname, APS_INFO,
            "WOS comparison with the APS database completed.\n\n\n",
			DO_SYSLOG, DO_PRINT);

		unlink( full_reportfile ) ;
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
		exit (APS_EXIT_OK) ;
	}

	/*
	-- Now that we have scanned the file for NUMBER_OF_RECORDS keyword,
	-- let's reset the file.
	*/
	fclose ( filedesc ) ;
    filedesc = fopen( fullpath_file, "r" ) ;

	/*
	-- Before ingesting the file, we need to get start time and stop time
	-- for our WOS comparison run.
	-- We get these values from the fields 
	--              VALID_START_TIME and VALID_STOP_TIME
	-- which reside in the PMF file which corresponds to the 
	-- data file we are about to ingest.
	*/
	while ( fscanf(file_pmf,"%s = %s", pmf_keyword, pmf_value) != EOF)
	{
		if (strcmp(pmf_keyword, "VALID_START_TIME") == 0)
		{
			strncpy (wos_strttime,		pmf_value,  strlen(wos_strttime) ) ;
			if ( tc_odl2asf(wos_strttime, wos_strttime) )
				WOS_PMF_DATA_FOUND++ ;
		}
		if (strcmp(pmf_keyword, "VALID_END_TIME") == 0)
		{
			strncpy (wos_stoptime,		pmf_value, strlen(wos_stoptime) ) ;
			if ( tc_odl2asf(wos_stoptime, wos_stoptime) )
				WOS_PMF_DATA_FOUND++ ;
		}
	}

	if( WOS_PMF_DATA_FOUND != 2)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"COULD NOT FIND START TIME OR STOP TIME FOR COMPARISON.\n",
			DO_SYSLOG, DO_PRINT);

		aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
			DO_SYSLOG, DO_PRINT);

		unlink( full_reportfile ) ;
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
		exit (APS_EXIT_ERROR) ;
	}


#ifdef NOT_NEEDED_UNTIL_MORE_FILE_DESCRIPTORS_ARE_ADDED
    /*
    -- Search through ODL file descriptors.
    */
    if (!identify_ODL_file(file_type, ODL_files,  &ODL_file_descriptors ))
    {
        sprintf (file_util_msg,
            "COULD NOT MATCH FILE %s TO A FLIGHT AGENCY.\n",file_type);
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		unlink( full_reportfile ) ;
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit (APS_EXIT_ERROR) ;
    }

	status = fa_odl_ingestion(filedesc, DB_SYBINT_USE_APS_READER_DBPROC, 
						&permission_id, ODL_file_descriptors->file_definition, 
						input_dtk_list, report_log );

#endif /*NOT_NEEDED_UNTIL_MORE_FILE_DESCRIPTORS_ARE_ADDED*/



	/*
	-- We use the generic WOS descriptor; it can be used for AWOS and MWOS
	*/
	status = fa_odl_ingestion(filedesc, APS_dbproc, &permission_id, 
								&wos_file, input_dtk_list, report_log );




#ifdef DEBUG
	dtkm_print_list (stdout, input_dtk_list ) ;
#endif

    if ( status != FA_ODL_INGESTION_OK)
    {
        aps_log_msg(file_util_progname, APS_ERROR,
            FA_ODL_ERROR_MESSAGE(status),
            DO_SYSLOG, DO_PRINT);
        fclose (file_pmf) ;
        fclose (filedesc) ;
		unlink( full_reportfile ) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit(APS_EXIT_ERROR) ;
    }
 
 
    /* 
	-- Create a report header, followed by the time range being examined
	*/
 
    if ( !aps_report_header (report_log,
                            fa_flight_agency,	
                            fa_file_type,
                            fa_filename,
                            fa_creation_date))
    {
#ifdef DEBUG
        printf("Could not create dtk report header\n");
#endif
        aps_log_msg(file_util_progname, APS_INFO,
            "Could not create dtk report header.  \n",
            DO_SYSLOG, DO_PRINT);
        fprintf(report_log, "\nCould not create dtk report header.  \n" ) ;
 
        fclose (file_pmf) ;
        fclose (filedesc) ;
		unlink( full_reportfile ) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
		exit(APS_EXIT_ERROR) ;
    }
	fprintf (report_log, 
	"\nTIME RANGE FOR WOS COMPARISON:\n\tSTART_TIME:%s\n\t STOP_TIME:%s\n\n",
		wos_strttime, wos_stoptime) ;

	/*
	-- NOW, go through the list and compare one datatake proposal at at time
	*/
	corresponding_db_dtk =  new_table_record(APS_CDEFS(DTK)) ;

    for (
        dtk_rec = (DB_RECORD **)FIRST(input_dtk_list, dtk_rec_ptr);
        dtk_rec;
        dtk_rec = (DB_RECORD **) NEXT(input_dtk_list, dtk_rec_ptr) 
        )
    {
		switch (dtk_proposal_matches_db(DB_SYBINT_USE_APS_READER_DBPROC, 
						dtk_rec,corresponding_db_dtk) )
		{
		case MWOSCOMP_MATCH:
			/*
			-- Take no action.
			*/
			break ;

		case MWOSCOMP_NO_MATCH:
			fprintf (report_log, "\nTHESE DOWNLINKS DO NOT MATCH:\n") ;
			fprintf (stdout    , "\nTHESE DOWNLINKS DO NOT MATCH:\n") ;
#ifdef DEBUG
			db_print_record(dtk_rec, APS_CDEFS(DTK) ) ;
			db_print_record(corresponding_db_dtk, APS_CDEFS(DTK) ) ;
#endif
			fprintf (report_log, "wos : ") ;
			dtkm_print (report_log, dtk_rec) ;
			fprintf (report_log, "db  : ") ;
			dtkm_print (report_log, corresponding_db_dtk) ;

			fprintf (stdout    , "wos : ") ;
			dtkm_print (stdout    , dtk_rec) ;
			fprintf (stdout    , "db  : ") ;
			dtkm_print (stdout    , corresponding_db_dtk) ;

			WOS_DTK_CHANGES++ ;
			break ;

		case MWOSCOMP_DTKSTAT_MODIFIED:
			fprintf (report_log, "\nTHESE DOWNLINKS HAVE A CHANGED STATUS:\n") ;
			fprintf (stdout    , "\nTHESE DOWNLINKS HAVE A CHANGED STATUS:\n") ;
#ifdef DEBUG
			db_print_record(dtk_rec, APS_CDEFS(DTK) ) ;
			db_print_record(corresponding_db_dtk, APS_CDEFS(DTK) ) ;
#endif
			fprintf (report_log, "wos : ") ;
			dtkm_print (report_log, dtk_rec) ;
			fprintf (report_log, "db  : ") ;
			dtkm_print (report_log, corresponding_db_dtk) ;

			fprintf (stdout    , "wos : ") ;
			dtkm_print (stdout    , dtk_rec) ;
			fprintf (stdout    , "db  : ") ;
			dtkm_print (stdout    , corresponding_db_dtk) ;

			WOS_STAT_CHANGES++ ;
			break ;

		case MWOSCOMP_BAD_DB_RECORD_COPY:
			aps_log_msg(file_util_progname, APS_ERROR,
				"Could not get a blank record.\n",
				DO_SYSLOG, DO_PRINT);
	 
			aps_log_msg(file_util_progname, APS_INFO,
				"WOS compare Program terminated abnormally.\n\n\n",
				DO_SYSLOG, DO_PRINT);
			fclose( report_log ) ;
			fclose (file_pmf) ;
			fclose( filedesc ) ;
			free_db_record (corresponding_db_dtk) ;
			DEL_LIST (input_dtk_list) ;
			free (report_dir) ;
			free (full_reportfile) ;
			free (full_pmffile) ;
			exit (APS_EXIT_ERROR) ;
			/* This is not a fall-thru, but LINT may mis-interpret it */

		case MWOSCOMP_DB_CORRUPTION:
			fprintf (report_log, 
"\nDB INCONSISTENCY: DOWNLINK IN WOS BUT NOT IN DATABASE. Please investigate.") ;
			fprintf (stdout, 
"\nDB INCONSISTENCY: DOWNLINK IN WOS BUT NOT IN DATABASE. Please investigate.") ;
	 
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
				"%s %2.2s/%3.3s/%05ld.%02d           %2.2s     %1c %1c %s",
				dtk_string,
				CAST_DTK_SAT dtk_rec[DTK_SAT],
				CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
				CAST_DTK_REV dtk_rec[DTK_REV],
				CAST_DTK_DTKID dtk_rec[DTK_DTKID],
				CAST_DTK_TRANSID dtk_rec[DTK_TRANSID],
				CAST_DTK_SCIENCE_QUICKLOOK dtk_rec[DTK_SCIENCE_QUICKLOOK],
				CAST_DTK_ASCDSC dtk_rec[DTK_ASCDSC],
				CAST_DTK_FADTKID dtk_rec[DTK_FADTKID] ) ;
		 
			/* print the darid if > 0   */
			if ( CAST_DTK_DARID dtk_rec[DTK_DARID] > 0 )
				sprintf(dtk_string, "%s darid=%ld", dtk_string,
					CAST_DTK_DARID dtk_rec[DTK_DARID] ) ;
		 
			sprintf(dtk_string, "%s\n                              %21s %21s",
				dtk_string,
				CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
				CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
 
			fprintf(report_log, "\n%s\n", dtk_string) ;
			fprintf(stdout    , "\n%s\n", dtk_string) ;

			WOS_NEW_DTKS_IN_WOS++ ;
			break ;
		}
	}


	/*
	-- THEN, go through the list and find out if there are new datatakes
	-- in the database.
	-- To do this, first get all the appropriate datatakes in the database for
	-- the time range in our list.  Then, compare both lists.
	*/

	WOS_dtks_list = create_dyn_llist() ;
	status = retrieve_dtks_for_WOS( fa_file_type,
					wos_strttime, wos_stoptime, WOS_dtks_list) ;
	if ( status < 0 )
	{
        aps_log_msg(file_util_progname, APS_ERROR,
            "Could not retrieve WOS records from db.\n",
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		fclose( report_log ) ;
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		free_db_record (corresponding_db_dtk) ;
		DEL_LIST (WOS_dtks_list) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
		exit (APS_EXIT_ERROR) ;
	}

#ifdef DEBUG
	printf ("These are the database dtks for the comparison\n\n") ;
	dtkm_print_list (stdout, WOS_dtks_list ) ;

	printf ("These are the   MWOS   dtks for the comparison\n\n") ;
	dtkm_print_list (stdout, input_dtk_list ) ;
#endif

	if (!dtkm_remove_dtks_from_list(input_dtk_list,WOS_dtks_list) )
	{
        aps_log_msg(file_util_progname, APS_ERROR,
            "Could not delete from dtk list.\n",
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(file_util_progname, APS_INFO,
            "WOS compare Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		fclose( report_log ) ;
		fclose (file_pmf) ;
		fclose( filedesc ) ;
		free_db_record (corresponding_db_dtk) ;
		DEL_LIST (WOS_dtks_list) ;
		DEL_LIST (input_dtk_list) ;
		free (report_dir) ;
		free (full_reportfile) ;
		free (full_pmffile) ;
        exit (APS_EXIT_ERROR) ;
	}

    for (
        dtk_rec = (DB_RECORD **)FIRST(WOS_dtks_list, dtk_rec_ptr) ;
        dtk_rec ;
        dtk_rec = (DB_RECORD **) NEXT(WOS_dtks_list, dtk_rec_ptr) 
        )
    {
		/*
		-- This datatake is NEW in the database, and was not
		-- reported in the WOS.
		*/
		fprintf(report_log,"\nTHIS DOWNLINK IS IN DATABASE, BUT NOT IN WOS:\n") ;
		fprintf(stdout    ,"\nTHIS DOWNLINK IS IN DATABASE, BUT NOT IN WOS:\n") ;
#ifdef DEBUG
		db_print_record(dtk_rec, APS_CDEFS(DTK) ) ;
#endif
		dtkm_print (report_log, dtk_rec) ;
		dtkm_print (stdout    , dtk_rec) ;

		WOS_NEW_DTKS_IN_DB++ ;
	}

	strcpy (dtk_string, "WOS COMPARISON SUMMARY:\n") ;

	if (WOS_DTK_CHANGES)
	{
		sprintf( file_util_msg, 
			"DOWNLINKS WITH CHANGES IN DB       : %5d\n", WOS_DTK_CHANGES) ;
		strcat (dtk_string, file_util_msg) ;
	}

	if (WOS_STAT_CHANGES)
	{
		sprintf(file_util_msg, 
			"DOWNLINKS WITH CHANGED STATUS IN DB: %5d\n", WOS_STAT_CHANGES) ;
		strcat (dtk_string, file_util_msg) ;
	}

	if (WOS_NEW_DTKS_IN_DB)
	{
		sprintf(file_util_msg, 
			"DOWNLINKS FOUND IN DB, NOT IN WOS  : %5d\n", WOS_NEW_DTKS_IN_DB) ;
		strcat (dtk_string, file_util_msg) ;
	}

	if (WOS_NEW_DTKS_IN_WOS)
	{
		sprintf(file_util_msg, 
			"DOWNLINKS FOUND IN WOS, NOT IN DB  : %5d", WOS_NEW_DTKS_IN_WOS) ;
		strcat (dtk_string, file_util_msg) ;
	}

	if (!WOS_DTK_CHANGES)
		strcat (dtk_string,
	"THERE ARE NO DOWNLINKS WITH CHANGES IN DB FOR COMPARISON TIME RANGE.\n") ;

	if (!WOS_STAT_CHANGES)
		strcat (dtk_string,
"THERE ARE NO DOWNLINKS WITH CHANGED STATUS IN DB FOR COMPARISON TIME RANGE.\n") ;

	if (!WOS_NEW_DTKS_IN_DB)
		strcat (dtk_string,
"THERE ARE NO NEW DOWNLINKS IN DB FOR COMPARISON TIME RANGE.\n") ;

	if (!WOS_NEW_DTKS_IN_WOS)
		strcat (dtk_string,
"THERE ARE NO NEW DOWNLINKS IN WOS FOR COMPARISON TIME RANGE.") ;


	fprintf(report_log, "\n\n%s", dtk_string) ;
	aps_log_msg(file_util_progname, APS_INFO, dtk_string, 
		DO_SYSLOG, DO_PRINT);


	sprintf(file_util_msg, 
"WOS comparison with APS database completed. \
Please review SUMMARY displayed above.\n\
(The summary is also available in the report file named %s)", full_reportfile);
	aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
		DO_SYSLOG, DO_PRINT);


	free (report_dir) ;
	free (full_reportfile) ;
	free (full_pmffile) ;
	fclose (file_pmf) ;
	fclose( filedesc ) ;
	fclose( report_log ) ;
	free_db_record (corresponding_db_dtk) ;
	DEL_LIST (WOS_dtks_list) ;
	DEL_LIST (input_dtk_list) ;

	exit (APS_EXIT_OK) ;
}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	ODL_dtkf_processor.c

Description:	

External Functions Defined:
int fa_odl_processor	Manages ingestion of ODL format files
	
File Scope Functions:
int find_and_convert	locate keyword strings, convert them.
	
External Variables Defined:
	
File Scope Variables:
ODL_FILEDEF res_file[] 
						array describing WFF response to AVAILABILITY_RESPONSE 
						(ARES request file). It points to values for objects
						which describe the aggregates in the file,
						as well as a set keywords and conversion requirements,
						and a set database destinations for this information.
	
Notes:

==============================================================================*/
#pragma ident   "@(#)ODL_dtkf_processor.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/ODL_dtkf_p/SCCS/s.ODL_dtkf_processor.c"

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absloute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <stdlib.h>			/* for getopt							*/

#include <sys/types.h>      /* for read             */
#include <sys/uio.h>        /* for read             */
#include <sys/unistd.h>     /* for read             */

/* FOR FILE UTILITIES DEFINITIONS 
	AND ERROR DEFINITIONS */
#include "file_utilities.h"

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

#include "dapps_defs.h"     /* for APS basic definitions  			*/
#include "apsfiledef.h"		/* for APS error definitions			*/
#include "aps_defs.h"		/* for syslogging, exit codes			*/
#include "aps_log_msg.h"	/* for syslogging						*/

#include "GENconversions.h"
#include "ODLconversions.h"


char		*file_util_progname ; /* required by libfileutils.a */
char		file_util_msg[MSG_LEN]; /* required by libfileutils.a */

/*==============================================================================
Function:       main                

Description:    Gets the command line arguments (ie: database name, password,
				name of ODL File) open up the database, and pass this 
				information to the routine fa_odl_processor() 

Parameters:     

Returns:        APS_REPORT_OK = success.
				other:
					APS_REPORT_FILE_ERROR
					APS_REPORT_ERROR

Creator:        Miguel Siu

Creation Date:  Fri May 19 09:22:16 PDT 1995

Notes:	ASSUMPTION:fa_odl_processor routine returns APS_REPORT_* codes
==============================================================================*/
void
main(int argc, char *argv[])
{     
    /* declarations		*/
    void        odl_usage_exit(char *progname) ;
    DBPROCESS	*dbproc;

	char    *dbname = NULL ;
	char    *sybase_userid = NULL ;
	char    *password = NULL ;
	char	*fullpath_file = NULL ;
	char    *input_file;
	char    *report_dir;
	char    *fa_error_dir;
	char    *full_reportfile;

	char    flag_list[20] = "p:U:P:t:k"; /* list of flags for getopts  */
	char    *env_dbname;        /* dbname from environment      */
	char    *env_sybase_userid; /* userid from environment      */
	char	*file_type = NULL;

	int     j, c;		/* used as return character from getopt()       */
	int		rcode;		/* return code for DB open     					*/
    int    	status;		/* return code for fa_odl_processor() 			*/
	int		kflag = 0 ;	/* (k for keep) disables the error file move.	*/
    int     return_code;        /* for getting permission */
    int     permission_id = 0;  /* for getting permission */

    ODL_FILENAME     *ODL_file_defs ;

	static char report_ext[] = ".rpt";
	static char slash[] = "/";

	extern char *optarg;
	extern int	optind;

	FILE	*filedesc ;
	FILE	*report_log ;

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
    file_util_progname = strrchr( argv[0], '/' ) ;
    if (file_util_progname == NULL)
        file_util_progname = argv[0] ;
    else
        file_util_progname++ ;
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
        case 'p':
            return_code = sscanf( optarg, "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                odl_usage_exit(file_util_progname);
            }
            break ;
        case 'P':
            if(password != NULL)
				odl_usage_exit(file_util_progname);
			password = optarg ;
            break;
        case 'U':
            if(sybase_userid != NULL)
				odl_usage_exit(file_util_progname);
			sybase_userid = optarg ;
            break;
        case 'k':
            if(kflag != 0)
				odl_usage_exit(file_util_progname);
			kflag ++ ;
            break;
		case 't':
			if(file_type != NULL)
				odl_usage_exit(file_util_progname);
			file_type = optarg;
			break;
        case '?':
			odl_usage_exit(file_util_progname);
            break;
        default:
            /* do  nothing  */
            break;
		}

	/* check that all arguments are attached to a flag, except for one */
	if(optind != argc - 1)
		odl_usage_exit(file_util_progname);

	/* check that all manditory flags are entered */
	if(password == NULL
	|| file_type == NULL)
		odl_usage_exit(file_util_progname);
 
	/* optional flag	*/
    if(sybase_userid == NULL)
    {
        /* sybase_userid not supplied in command line.      */
    	/* obtain from the environment:                     */
    	env_sybase_userid = getenv("APS_SYBASE_USERID");
    	if(env_sybase_userid == NULL)
		{
        	/* userid not supplied at all   */
			aps_log_msg(file_util_progname, APS_INFO, 
"ERROR:  sybase_userid not found in environment variable APS_SYBASE_USERID\n",
				DO_SYSLOG, DO_PRINT);
			aps_log_msg(file_util_progname, APS_INFO, 
				"Use -U sybase_userid or setenv APS_SYBASE_USERID.\n\n", 
				DO_SYSLOG, DO_PRINT);
			odl_usage_exit(file_util_progname);
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
		aps_log_msg(file_util_progname, APS_INFO, 
			"ERROR:  dbname not found in environment variable APSDB\n", 
			DO_SYSLOG, DO_PRINT);
		aps_log_msg(file_util_progname, APS_INFO, 
			"Use setenv APSDB <dbname>. \n\n", 
			DO_SYSLOG, DO_PRINT);
		odl_usage_exit(file_util_progname);
	}
	dbname = env_dbname ;

	/* now open the database.  */
	/* db_open will handle the errors.  	*/
	dbproc = db_open(dbname,file_util_progname,sybase_userid,password,NULL,
		error_handler_exit,&rcode);
	if(rcode != DB_OPEN_OK)
	{
		sprintf(file_util_msg, "Could not open database '%s'\n", dbname) ;
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
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
	-- open the report file
	*/
	report_log = fopen(full_reportfile,"w");
	if (report_log == NULL)
	{
		aps_log_msg(file_util_progname, APS_ERROR, 
			"could not open report error log file\n", 
			DO_SYSLOG, DO_PRINT);

        aps_log_msg(file_util_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
	}

	/*
	-- check the input_file for existence.  
	*/
	filedesc = fopen( fullpath_file, "r" ) ;
	if ( filedesc == NULL )
	{
		sprintf(file_util_msg, 
"file %s could not be opened for read access check for existence and permission\n", input_file) ;
		aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);

		(void) unlink(full_reportfile) ;
		exit (APS_EXIT_ERROR) ;
	}
	else
	{
		/*
		-- file exists, initialize the global variable fa_filename
		*/
		fa_filename = input_file ;

		fclose( filedesc ) ;
	}




	/* 
	-- NOTE: we now create the report header within fa_odl_processor
	*/

    /*
    -- ASSIGN the ARES file file descriptors.
    -- ASSIGN the ARES file file descriptors.
    -- ASSIGN the ARES file file descriptors.
    -- ASSIGN the ARES file file descriptors.
	-- NOTE: when other ODL files are identified for ingestion, you
	-- must search among file descriptions for the correct one.
    */
	if (strcmp (file_type, "ARES") == 0 )
		ODL_file_defs = &ARES_file ;
	else
    {
        sprintf (file_util_msg,
            "COULD NOT MATCH FILE %s TO A FLIGHT AGENCY.\n",file_type);
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(file_util_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
		(void) unlink(full_reportfile) ;
        exit (APS_EXIT_ERROR) ;
    }



	aps_log_msg(file_util_progname, APS_INFO, 
		"ODL ingestion starting... \n", 
		DO_SYSLOG, DO_PRINT);




	status =  fa_odl_processor(dbproc, &permission_id, ODL_file_defs,
				fullpath_file, report_log);

	aps_log_msg(file_util_progname, APS_INFO, 
		"ODL ingestion completion: ", 
		DO_SYSLOG, DO_PRINT);
    if ( status == FA_ODL_PROCESS_OK )
	{
		sprintf(file_util_msg, 
			"SUCCESS.      (see report %s for status of DTK processing)\n",
			full_reportfile);
		aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		aps_log_msg(file_util_progname, APS_INFO, 
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
	else if ( status == FA_ODL_PROCESS_OK_BUT_PLANNER_MUST_READ)
	{
		sprintf(file_util_msg, 
			"PROCESSED.  (review report %s for status of DTK processing)\n",
			full_reportfile);
		aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
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


		aps_log_msg(file_util_progname, APS_INFO, 
			"Program terminated; review results.", 
			DO_SYSLOG, DO_PRINT);

		status = APS_EXIT_ERROR ;
	}
    else if (status == FA_ODL_PROCESS_ERROR)
	{
		sprintf(file_util_msg, 
			"FAILED.   (review report %s for status of DTK processing)\n",
			full_reportfile);
		aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
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


        aps_log_msg(file_util_progname, APS_INFO,
            "ODL ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);

		status = APS_EXIT_ERROR ;
	}
	else if ( status == FA_ODL_PROCESS_PERMDENIED)
	{
		aps_log_msg(file_util_progname, APS_INFO, 
			"PROCESSING STOPPED.  PERMISSIONS WERE DENIED.\n", 
			DO_SYSLOG, DO_PRINT);

		aps_log_msg(file_util_progname, APS_INFO, 
			"Program terminated; review results.", 
			DO_SYSLOG, DO_PRINT);

		(void) unlink(full_reportfile) ;
		status = APS_EXIT_ERROR ;
	}
	else if ( status == FA_ODL_PROCESS_PERMFAILED)
	{
		aps_log_msg(file_util_progname, APS_INFO, 
			"PROCESSING STOPPED.  PERMISSION-ACQUISITION PROCESS FAILED.\n", 
			DO_SYSLOG, DO_PRINT);

		aps_log_msg(file_util_progname, APS_INFO, 
			"Program terminated abnormally.", 
			DO_SYSLOG, DO_PRINT);

		(void) unlink(full_reportfile) ;
		status = APS_EXIT_ERROR ;
	}
	else
	/*
	-- unknown error code 
	*/
	{
		aps_log_msg(file_util_progname, APS_INFO, 
			"UNKNOWN ERROR CODE ENCOUNTERED.)\n", 
			DO_SYSLOG, DO_PRINT);

		aps_log_msg(file_util_progname, APS_INFO, 
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



void odl_usage_exit(char *progname)
{
	fprintf(stderr,		/* nicer format, keep it */
"\nusage:  %s  -P <password>  -t <filetype> [-U <user name>]\n\t\t\t[-p <permission>] [-k]   filename\n", progname);
	fprintf(stderr,		/* nicer format, keep it */
"\n\tNOTES:  \n");
	fprintf(stderr,		/* nicer format, keep it */
"\n\t%s will ingest ODL format files\n", progname);
	fprintf(stderr,		/* nicer format, keep it */
"\tIt scans the file for appropriate keywords and converts these   \n");
	fprintf(stderr,		/* nicer format, keep it */
"\tvalues before inserting them into the DTK relation.              \n");
	fprintf(stderr,		/* nicer format, keep it */
"\tIf there is an error in the input file, it is moved to the \n");
    fprintf(stderr,     /* nicer format, keep it */
"\tAPS_DATA/FA_error_files directory.  If not, it is moved to the \n");
    fprintf(stderr,     /* nicer format, keep it */
"\tAPS_DATA/FA_input_files tdirectory.  \n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-p <permission>  single-activity permission (from the calling program)") ;
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-U <user name>   Sybase userid for Sybase account") ;
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-P <password>    Sybase password for Sybase account") ;
	fprintf(stderr, 	/* nicer format, keep it */
"\n\t-k   keeps the input file where it is; prevents the move or copy.");
	fprintf(stderr, 	/* nicer format, keep it */
"\n\t-t <filetype>  %s will ingest the following ODL format files:", progname);
	fprintf(stderr, 	/* nicer format, keep it */
"\n\t   ARES (WFF AVAILABILITY RESPONSE file, from Wallops)\n");
	fprintf(stderr,		/* nicer format, keep it */
"\t---------------------------------------------------\n");

	aps_log_msg(file_util_progname, APS_INFO,
		"ODL ingestion Program terminated abnormally.\n\n\n",
		DO_SYSLOG, DO_PRINT);
	exit (APS_EXIT_ERROR);
}


#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   FA_dtkf_processor.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
VALUE_DEFS  [] array describing ASCII File and its
                        fields and conversion requirements, as well as 
                        database destinations for this information.
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)FA_dtkf_processor.c	3.11 97/04/14 APS/ASF"
#pragma ident   "@(#) /ua/aps/aps/r2.1/src/FA_dtkf_p/SCCS/s.FA_dtkf_processor.c"

#include <stdlib.h>         /* for getopt                           */
#include <string.h>         /* for strrchr()                        */
#include <stdio.h>          /* for fclose()                         */
#include <errno.h>

#include <unistd.h>    /* for access(), F_OK   */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */
#include "aps_log_msg.h"    /* for APS logging                      */
#include "aps_defs.h"       /* exit codes for APS logging           */
#include "db_dtk.h"			/* for DTK definitions */


/* FOR FILE UTILITIES DEFINITIONS and PROTOTYPES */
#include "file_utilities.h"

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"
 
/* FOR FA_ASCII_REC_PROCESSOR */
#include "GENconversions.h"
#include "ESAconversions.h"
#include "NASDAconversions.h"
#include "APSpmfutilities.h"

/* FOR DIAGNOSTIC PRINTING */
#define PRINT_DIAG
#undef  PRINT_DIAG

#define MAX_DTKID_VALUE 99  /* this might be equal to 250 in other databases */

/* FUNCTION PROTOTYPES FROM lb_APSpmfutils:  */
int APS_create_pmf (
	char            *metafilename, 
	APSPMF_metadata *PMF_struct) ;

/* must be GLOBAL variables:  */
DBPROCESS   *APS_dbproc;
DBPROCESS   *IMS_dbproc;
char        *file_util_progname ; /* required by libfileutils.a */
char        file_util_msg[MSG_LEN]; /* required by libfileutils.a */


static void esa_usage_exit(char *progname)
{
    (void)fprintf(stderr,     /* nicer format, keep it */
"\nusage: %s [-U <username>] -p <n> -t <d>\n", progname);
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t\t\t-s<sat> -m<sensor> -P <password> filename\n") ;
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\tNOTES:  \n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\t%s moves frames from IMS table granules_112 and \n", progname);
    (void)fprintf(stderr,     /* nicer format, keep it */
"\tplaces them in dtk relation in the APS database selected.\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\tThe mapper can then be used to plot the frames.\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-t <d>  -- d is the number of datatakes you expect to add\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-p <n>  -- n is the start rev for the dtks to be added\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-s <sat>  -- satellite for the dtks being added\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-m <sensor>  -- sensor mode for the dtks being added\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-U <user name>  -- Sybase login user ID\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-P <password>   -- Sybase login password\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t%s version:  %s %s   ---------\n", progname, __DATE__, __TIME__ );

    aps_log_msg(file_util_progname, APS_INFO, 
        "ASCII ingestion Program terminated abnormally.\n\n\n",
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
}


/*==============================================================================
Function:       main                

Description:    Gets the command line arguments (ie: database name, password,
                name of ESA Reception Request File) open up the database, and
                pass this information to the routine fa_ascii_processor() 

Parameters:     

Returns:        APS_EXIT_OK = success.
                APS_EXIT_ERROR = otherwise.

Creator:        Miguel Siu

Creation Date:  Fri May 19 09:22:16 PDT 1995

Notes:  ASSUMPTION:fa_ascii_processor routine returns APS_REPORT_* codes
==============================================================================*/
void 
main(int argc, char *argv[])
{     
    /* declarations     */

    char    *dbname = NULL ;
    char    *sybase_userid = NULL ;
    char    *password = NULL ;
    char    *sat_arg = NULL ;
    char    *sensor_arg = NULL ;
    char    *fullpath_file;
    char    *input_file;
    char    *outgoing_dir;
    char    *fa_error_dir;
    char    *full_reportfile;
    char    *full_responsefile;
    char    *short_responsefile;
    char    *metafilename ;
    char    *response_ext;

    char    flag_list[20] = "U:P:t:p:s:m:"; /* list of flags for getopts  */
	char	string1[20] = "xxxxxxxxxxxxxxxxxxx" ;
    int     pflag = 0 ;         /* used to determine existence of pflag */
    char    *env_dbname;        /* dbname from environment      */
    char    *env_sybase_userid; /* userid from environment      */

	int		int1 ;
    int     j;
    int     c;      /* used as return character from getopt()       */
    int     rcode;  /* return code for DB open                      */
    int     status; /* return code for fa_ascii_processor()			*/
	int		return_code;  					/* for getting permission */
	int		TOTAL_DTKS ;  /* datatakes will be added in bunches */
	int		rev_start ;

	float   float1, float2, float3, float4, float5 ;
	float   float6, float7, float8 ;
    FILE            *ascii_file_ptr ;
    FILE            *report_log ;
    FILE            *response_file ;

	llist       	*dtk_updates = NULL ;
    DB_RECORD       **insertion_dtk = NULL ; /* existing data-take    */

    extern char *optarg;
    extern int  optind;

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
	/* find pointer to last occurrence of '/', if any, in the string:  */
	file_util_progname = strrchr( argv[0], '/' ) ;
	if( file_util_progname == NULL )
		file_util_progname = argv[0] ;
	else
		file_util_progname ++ ;

    aps_open_syslog();

    (void)sprintf(file_util_msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        (void)strcat(file_util_msg, " " ) ;
        (void)strcat(file_util_msg, argv[j] ) ;
    }
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg,
        DO_SYSLOG, DO_PRINT);


    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
        case 'p':
            return_code = sscanf( optarg, "%d", &rev_start ) ;
            if( return_code != 1 )
            {
                (void)fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                esa_usage_exit(file_util_progname);
            }
            break ;
        case 't':
            return_code = sscanf( optarg, "%d", &TOTAL_DTKS ) ;
            if( return_code != 1 )
            {
                (void)fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                esa_usage_exit(file_util_progname);
            }
            break ;
        case 's':
            if(sat_arg != NULL)	/* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            sat_arg = optarg ;
            break;
        case 'm':
            if(sensor_arg != NULL)	/* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            sensor_arg = optarg ;
            break;
        case 'U':			
            if(sybase_userid != NULL)/* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            sybase_userid = optarg ;
            break;
        case 'P':
            if(password != NULL)	/* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            password = optarg ;
            break;
        case '?':
            esa_usage_exit(file_util_progname);
            break;
        default:
            /* do  nothing  */
            break;
        }



    /* check for extraneous words not attached to a flag.   */
    if(optind != argc-1)
        esa_usage_exit(file_util_progname);

    fullpath_file = argv[argc -1];

    /* manditory flags  */
    if (password == NULL)
        esa_usage_exit(file_util_progname);
 
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
            esa_usage_exit(file_util_progname);
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
        esa_usage_exit(file_util_progname);
    }
    dbname = env_dbname ;

    /* now open the database.  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open(dbname,file_util_progname,sybase_userid,password,NULL,
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

    /*
    -- Open the ASCII file
    -- ascii_file_ptr = fopen("ims_granule.txt", "r") ;
    */
    ascii_file_ptr = fopen(fullpath_file, "r") ;
    if (!ascii_file_ptr)
    {
        (void)sprintf(file_util_msg,
            "COULD NOT OPEN INPUT ASCII FILE %s (errno: %d)\n",
            fullpath_file, errno ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
            DO_SYSLOG, DO_PRINT);
 
        aps_log_msg(file_util_progname, APS_INFO,
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }

	dtk_updates  = create_dyn_llist() ;
	insertion_dtk =  new_table_record(APS_CDEFS(DTK)) ;

	strcpy (CAST_DTK_SAT insertion_dtk[DTK_SAT], sat_arg) ;
	strcpy (CAST_DTK_SENSOR insertion_dtk[DTK_SENSOR], sensor_arg) ;
	strcpy (CAST_DTK_ACTID insertion_dtk[DTK_ACTID], "ROBCEF") ;
	CAST_DTK_ASCDSC insertion_dtk[DTK_ASCDSC] = '-' ;
	CAST_DTK_PLANNER_QUICKLOOK insertion_dtk[DTK_PLANNER_QUICKLOOK] = 'Y' ;
	CAST_DTK_SCIENCE_QUICKLOOK insertion_dtk[DTK_SCIENCE_QUICKLOOK] = 'Y' ;
	strcpy (CAST_DTK_STRTTIME insertion_dtk[DTK_STRTTIME], 
				"1996:063:03:12:30.000") ;
	strcpy (CAST_DTK_STOPTIME insertion_dtk[DTK_STOPTIME], 
				"1996:063:03:13:01.000") ;
	strcpy (CAST_DTK_SUBMIT_TIME insertion_dtk[DTK_SUBMIT_TIME], 
				"1996:354:03:13:01.000") ;
	strcpy (CAST_DTK_DTKSTAT insertion_dtk[DTK_DTKSTAT], "SCH") ;
	strcpy (CAST_DTK_PROPOSED_DTKSTAT insertion_dtk[DTK_PROPOSED_DTKSTAT], 
				"SCH") ;
	strcpy (CAST_DTK_TRANSID insertion_dtk[DTK_TRANSID], "00") ;
	strcpy (CAST_DTK_STATION_ID insertion_dtk[DTK_STATION_ID], "ASF") ;

	for (j=0; j<TOTAL_DTKS; j++)
	{
		/*
		FROM VANCES FILE:
		fscanf (ascii_file_ptr, "%f %f %f %f %f %f %f %f",
			&float1, &float2,
			&float3, &float4, &float5,
			&float6, &float7, &float8) ;

		FROM ISQL SCRIPT FROM IMS:
		fscanf (ascii_file_ptr, "%d %s %f %f\n%f %f %f\n%f %f %f",
			&int1, &string1, &float1, &float2,
			&float3, &float4, &float5,
			&float6, &float7, &float8) ;
		*/

		fscanf (ascii_file_ptr, "%f %f %f %f %f %f %f %f",
			&float1, &float2,
			&float3, &float4, &float5,
			&float6, &float7, &float8) ;

		CAST_DTK_REV   insertion_dtk[DTK_REV] = 
							(int) (j/MAX_DTKID_VALUE)+rev_start ;
		CAST_DTK_DTKID insertion_dtk[DTK_DTKID] = 0 ;
		CAST_DTK_NRLAT1  insertion_dtk[DTK_NRLAT1] = float1 ;
		CAST_DTK_NRLON1  insertion_dtk[DTK_NRLON1] = float2 ;
		CAST_DTK_NRLAT2  insertion_dtk[DTK_NRLAT2] = float3 ;
		CAST_DTK_NRLON2  insertion_dtk[DTK_NRLON2] = float4 ;
		CAST_DTK_FARLAT1 insertion_dtk[DTK_FARLAT1] = float5 ;
		CAST_DTK_FARLON1 insertion_dtk[DTK_FARLON1] = float6 ;
		CAST_DTK_FARLAT2 insertion_dtk[DTK_FARLAT2] = float7 ;
		CAST_DTK_FARLON2 insertion_dtk[DTK_FARLON2] = float8 ;

		return_code = dtkm_insert_dtk_record(APS_dbproc,
					insertion_dtk, insertion_dtk, dtk_updates ) ;
		printf("inserting DTK #%d : status %d\n", j+1,return_code) ;
	}

	exit(0) ;
} /* end of main() */

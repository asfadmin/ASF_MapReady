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
#pragma ident   "@(#)FA_dtkf_processor.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/FA_dtkf_p/SCCS/s.FA_dtkf_processor.c"

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

/* FUNCTION PROTOTYPES FROM lb_APSpmfutils:  */
int APS_create_pmf (
    char            *metafilename, 
    APSPMF_metadata *PMF_struct) ;

/* must be GLOBAL variables:  */
DBPROCESS   *APS_dbproc;
char        *file_util_progname ; /* required by libfileutils.a */
char        file_util_msg[MSG_LEN]; /* required by libfileutils.a */


static void esa_usage_exit(char *progname)
{
    (void)fprintf(stderr,     /* nicer format, keep it */
"\nusage: %s [-k] [-U <username>] [-p permission]\n", progname);
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t\t\t[-R <retries>] [-S <seconds>]\n") ;
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t\t\t-P <password>  -t <filetype>  filename\n") ;
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\tNOTES:  \n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\t%s ingests Flight Agency files.  If there is an \n", progname);
    (void)fprintf(stderr,     /* nicer format, keep it */
"\terror in the input file, it is moved to the APS_DATA/FA_error_files\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\tdirectory.  If not, it is moved to the APS_DATA/FA_input_files\n");
    (void)fprintf(stderr,     
"\tdirectory.  \n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\t-k   keeps the input file where it is; prevents the move or copy.\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-p   single-activity permission (obtained by the calling program)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-R <retries> number of times to retry if planning permission is DENIED\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-S <seconds> number of seconds to wait in between retry attempts.\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-U <user name>  -- Sybase login user ID\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t-P <password>   -- Sybase login password\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\t-t <filetype>  %s will ingest the following ASCII files:\n",progname);
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\t   MPSG  (this is the ESA Request  file also known as GAP)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   SHQP  (this is the ESA Preliminary Schedule file)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   SHAQ  (this is the ESA Schedule file)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   REQM  (this is the NASDA Request  file)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   REQA  (this is the NASDA Reply to the REQQ Request file)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   OPLN  (this is the NASDA Schedule file)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   REQR  (this is the NASDA Request  file for ADEOS)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\t   OPL1  (this is the NASDA Schedule file for ADEOS)\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\n\tIt scans the file for appropriate strings and converts these\n");
    (void)fprintf(stderr,     /* nicer format, keep it */
"\tvalues before inserting them into the DTK relation. \n");
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
    char    *fullpath_file;
    char    *input_file;
    char    *report_dir;
    char    *outgoing_dir;
    char    *fa_error_dir;
    char    *full_reportfile;
    char    *full_responsefile;
    char    *short_responsefile;
    char    *metafilename ;
    char    *response_ext;
    char    *file_type = NULL;

    char    flag_list[20] = "kU:P:t:p:S:R:"; /* list of flags for getopts  */
    int     kflag = 0 ;         /* (k for keep) disables the error file move. */
    int     pflag = 0 ;         /* used to determine existence of pflag */
    int     Rflag = 0 ;         /* used to determine existence of Rflag */
    int     Sflag = 0 ;         /* used to determine existence of Sflag */
    char    *env_dbname;        /* dbname from environment      */
    char    *env_sybase_userid; /* userid from environment      */

    int     j;
    int     c;      /* used as return character from getopt()       */
    int     rcode;  /* return code for DB open                      */
    int     status; /* return code for fa_ascii_processor()         */
    int     return_code;                    /* for getting permission */
    PERMISSION_ permission = { 0, 0, 0} ;   /* for getting permission */

    APSPMF_metadata *response_PMF_struct ;
    PMF_FILENAME    *pmf_descriptors ;
    FA_FILENAME     *file_descriptors ;
    FILE            *ascii_file_ptr ;
    FILE            *report_fp ;
    FILE            *response_file ;

    static char report_ext[] = ".rpt";
    static char slash[] = "/";

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
        case 'k':
            if(kflag != 0)          /* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            kflag ++ ;
            break;
        case 'U':           
            if(sybase_userid != NULL)/* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            sybase_userid = optarg ;
            break;
        case 'p':
            if(pflag != 0)          /* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            pflag ++ ;
            return_code = sscanf( optarg, "%d", &permission.id ) ;
            if( return_code != 1 )
            {
                (void)fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                esa_usage_exit(file_util_progname);
            }
            break ;
        case 'R':
            if(Rflag != 0)          /* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            Rflag ++ ;
            return_code = sscanf( optarg, "%d", &permission.number_o_retry ) ;
            if( return_code != 1 )
            {
                (void)fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int retries\n",
                    __FILE__, __LINE__, optarg) ;
                esa_usage_exit(file_util_progname);
            }
            break ;
        case 'S':
            if(Sflag != 0)          /* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            Sflag ++ ;
            return_code = sscanf( optarg, "%d", &permission.seconds_between ) ;
            if( return_code != 1 )
            {
                (void)fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int seconds between\n",
                    __FILE__, __LINE__, optarg) ;
                esa_usage_exit(file_util_progname);
            }
            break ;
        case 'P':
            if(password != NULL)    /* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            password = optarg ;
            break;
        case 't':
            if(file_type != NULL)   /* checking for duplicate flags */
                esa_usage_exit(file_util_progname);
            file_type = optarg ;
            break;
        case '?':
            esa_usage_exit(file_util_progname);
            break;
        default:
            /* do  nothing  */
            break;
        }



    /* check for extraneous words not attached to a flag.   */
    if(optind != argc - 1)
        esa_usage_exit(file_util_progname);

    /* manditory flags  */
    if (password == NULL
    ||  file_type == NULL )
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


    report_dir = aps_fullpath (APS_REPORTS, NULL);

    fullpath_file = argv[argc -1];
    input_file = aps_pathname2filename (fullpath_file);
    fa_filename = aps_pathname2filename (fullpath_file);

    full_reportfile = malloc(
        strlen(report_dir) + 1 + strlen(input_file) + strlen(report_ext) + 1) ;
    (void)sprintf (full_reportfile, 
        "%s%s%s%s", report_dir, slash, input_file, report_ext) ;

#ifdef PRINT_DIAG                                        
/*DEBUG*/printf(
        "IN MAIN: initial filename:%s\nnewname:%s\nreportname:%s\n\n",
        fullpath_file,input_file,full_reportfile);
#endif 



    /*
    -- Open the ASCII file
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



#ifdef TEST_ONLY
    /* 
    -- (when using the debugger)
    -- it is easiest if the report or log file goes to stdout.  
    -- You invoke this feature defining TEST_ONLY as a CFLAG
    -- in our makefile.  [ DEFS =      -D$(MACH) -DTEST_ONLY ]
    */
    report_fp = stdout ;
#else
    /* 
    -- Open up the report log 
    -- (for operations)
    */
    report_fp = fopen(full_reportfile,"w") ;
    if (report_fp == NULL)
    {
        (void)sprintf (file_util_msg, 
            "COULD NOT OPEN REPORT ERROR LOG FILE %s (errno: %d)\n", 
            full_reportfile, errno ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        aps_log_msg(file_util_progname, APS_INFO, 
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }
    /* 
    -- Unbuffer the report log file 
    -- for easier monitoring of the run:
    */
    setbuf( report_fp, (char *) NULL ) ;

#endif    /* not TEST_ONLY  */

    /* start the report file by noting the file being processed. */
    fprintf( report_fp, "%s\n\n", full_reportfile ) ;

    /*
    -- Search through ESA and NASDA file descriptors.
    -- There are currently no CSA files using ASCII descriptors.
    */
    if      (identify_file(file_type, NASDA_files,  &file_descriptors ))
        outgoing_dir = aps_fullpath (APS_NASDA_FILES, NULL);
    else if (identify_file(file_type, ESA_files,    &file_descriptors ))
        outgoing_dir = aps_fullpath (APS_ESA_FILES, NULL);
    else
    {
        (void)sprintf (file_util_msg, 
            "COULD NOT MATCH FILE %s TO A FLIGHT AGENCY.\n",file_type);
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        aps_log_msg(file_util_progname, APS_INFO, 
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }

#ifdef PRINT_DIAG                                        
/*DEBUG*/printf("IN MAIN: file_descriptors:%x\n",&file_descriptors) ;
#endif 


    /*
    -- Open up the response file (which may not be needed in all cases)
    */

    response_ext = file_descriptors->response_extension ;
    if (!response_ext)
    {
        /* 
        -- no response file is required
        */
        status = fa_ascii_processor(APS_dbproc, permission, file_descriptors,
                    fullpath_file, ascii_file_ptr, report_fp, NULL) ; 
    }
    else
    {
        full_responsefile = malloc(
            strlen(outgoing_dir) + 1 + strlen(input_file) + 1);
        (void)sprintf (full_responsefile, "%s%s%s%s", 
            outgoing_dir, slash, response_ext, input_file+strlen(response_ext));

        short_responsefile = malloc( strlen(input_file) + 1);
        (void)sprintf (short_responsefile, "%s%s",
            response_ext, input_file+strlen(response_ext)) ;

        response_file = fopen(full_responsefile,"w") ;
        if (response_file == NULL)
        {
            (void)sprintf (file_util_msg, 
                "COULD NOT OPEN RESPONSE FILE %s (errno: %d)\n", 
                full_responsefile, errno ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);

            aps_log_msg(file_util_progname, APS_INFO, 
                "ASCII ingestion Program terminated abnormally.\n\n\n",
                DO_SYSLOG, DO_PRINT);
            exit (APS_EXIT_ERROR) ;
        }
#ifdef PRINT_DIAG                                        
/*DEBUG*/printf("IN MAIN: response filename:%s\n\n", response_file) ; 
#endif 



        /* process file expecting a possible response.  */
        status = fa_ascii_processor(APS_dbproc, permission, file_descriptors,
                    fullpath_file, ascii_file_ptr, report_fp, response_file) ; 



        (void)fclose(response_file) ;

        if ( status == FA_ASCII_REC_PROCESS_ERROR )
        {
            /*
            -- delete response_file, if the processing resulted in an error.
            */
            (void)unlink(full_responsefile) ;
        }
        else if ( status == FA_ASCII_REC_PROCESS_OK )
        {
            /* 
            -- No action. If any empty response files were created,
            -- they are left around for the operator to examine.
            */;
        }
        else if ( status == FA_ASCII_REC_PROCESS_OK_CREATE_META 
             ||   status == FA_ASCII_REC_PROC_DONE_CREATE_META  )
        {
            /*
            -- find the PMF file descriptor
            */
            if(!identify_PMF_file(file_type, PMF_files, &pmf_descriptors ))
            {
                (void)sprintf (file_util_msg, 
                    "COULD NOT MATCH PMF %s TO A FLIGHT AGENCY.\n",file_type);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);

                aps_log_msg(file_util_progname, APS_INFO, 
                    "ASCII ingestion Program terminated abnormally.\n\n\n",
                    DO_SYSLOG, DO_PRINT);

                exit (APS_EXIT_ERROR) ;
            }


            /*
            -- START generation PMF for this response file 
            */
            response_PMF_struct = 
                pmf_descriptors->file_descriptor ;

            /*
            -- Populate global variables which will be used by the PMF creator
            */
            (void)strncpy(response_PMF_struct[RESPONSE_TO].field, 
                input_file, strlen(input_file) );
            (void)strncpy(response_PMF_struct[FILE_NAME].field, 
                short_responsefile, strlen(short_responsefile) );

            metafilename = malloc (
                strlen(outgoing_dir) + 1 + strlen(short_responsefile) + 2 + 1) ;
            (void)sprintf (metafilename, "%s%s%s.M", 
                outgoing_dir, slash, short_responsefile) ;

            if (! APS_create_pmf(metafilename, response_PMF_struct) )
            {
                (void)sprintf (file_util_msg, 
                    "Error creating IMS PMF file for File Type: %s '%s'\n",
                    file_type, fullpath_file ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            free(metafilename) ;


            (void)sprintf (file_util_msg,
                "     (response file %s was generated.)\n", 
                full_responsefile);
            aps_log_msg(file_util_progname, APS_INFO, file_util_msg,
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_INFO, 
                "* * * THIS FILE MUST  BE TRANSFERRED NOW  BY OPERATOR * * *\n",
                DO_SYSLOG, DO_PRINT);


        } /* end of if (status == *)  */

        free  (full_responsefile) ;
        free  (short_responsefile) ;

    } /* end of if (response_file) */



    aps_log_msg(file_util_progname, APS_INFO, 
        "ASCII ingestion completion: ",
        DO_SYSLOG, DO_PRINT);
    if ( status == FA_ASCII_REC_PROCESS_OK 
    ||   status == FA_ASCII_REC_PROCESS_OK_CREATE_META )
    {
        (void)sprintf (file_util_msg, 
            "SUCCESS.  (see report %s for status of DTK processing)\n",
            full_reportfile);
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        status = APS_EXIT_OK ;

        aps_log_msg(file_util_progname, APS_INFO, 
            "ASCII ingestion Program completed.\n",
            DO_SYSLOG, DO_PRINT);

        /* 
        -- SUCCESS:must copy the successful input file to 
        --         FA_input_files directory.  
        --  -k (for keep file ) sets the kflag, to disable the file move.  
        --
        -- An informational message is generated by fa_copy_file()
        -- and we also report the copy in the file ingestion log.
        */
        if ( kflag == 0 )
        {
            fa_error_dir = aps_fullpath (APS_FA_INPUT_FILES, NULL);

            /*
            -- fa_copy_file() will print its own messages to syslog
            */
            if ( !fa_copy_file( fullpath_file, APS_FA_INPUT_FILES ) )
            {
                (void)sprintf(file_util_msg, 
                    "Non-fatal ERROR. Could not copy %s to %s area.\n",
                    fullpath_file, fa_error_dir);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            else
                (void)fprintf(report_fp, 
    "File %s has no errors, and is copied to %s area for your convenience.\n",
                fullpath_file, fa_error_dir);

            free (fa_error_dir);
        }

    }
    else if ( status == FA_ASCII_REC_PROC_DONE_CREATE_META ) 
    {
        /* 
        -- FA_ASCII_REC_PROC_DONE_CREATE_META means that there were no errors
        -- in the incoming file, but there may have been rejected/deleted/conflict
        -- datatakes.
        */
        (void)sprintf (file_util_msg, 
            "PROCESSED.  (see report %s for status of DTK processing)\n",
            full_reportfile);

        aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        /* 
        -- NO ERROR:  input file has no errors, but we move it to 
        -- FA_error_files directory; we may wish to reprocess it.
        --
        --  -k (for keep file ) sets the kflag, to disable the file move.  
        --
        -- An informational message is generated by fa_move_file()
        -- and we also report the move in the file ingestion log.
        */
        if ( kflag == 0 )
        {
            fa_error_dir = aps_fullpath (APS_FA_ERROR_FILES, NULL);

            if ( !fa_move_file( fullpath_file, APS_FA_ERROR_FILES ) )
            {
                (void)sprintf(file_util_msg, 
                    "Non-fatal ERROR. Could not move %s to %s area.\n",
                    fullpath_file, fa_error_dir);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            else
                (void)fprintf(report_fp, 
    "File %s has no errors,but we move it to %s area for your convenience.\n",
                fullpath_file, fa_error_dir);

            free (fa_error_dir);
        }


        aps_log_msg(file_util_progname, APS_INFO, 
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        status = APS_EXIT_ERROR ;
    }
    else if ( status == FA_ASCII_REC_PROC_PERMISSION_DENIED ) 
    {
        (void)sprintf (file_util_msg, 
            "PROCESSED. PERMISSIONS WERE DENIED.\n",
            full_reportfile);

        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);


        aps_log_msg(file_util_progname, APS_INFO, 
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        status = APS_EXIT_ERROR ;
    }
    else
    {
        (void)sprintf (file_util_msg, 
            "FAILED      (see report %s for status of DTK processing)\n",
            full_reportfile);

        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        /* 
        -- ERROR:  must move the error input file to 
        --         FA_error_files directory.  
        --  -k (for keep file ) sets the kflag, to disable the file move.  
        --
        -- An informational message is generated by fa_move_file()
        -- and we also report the move in the file ingestion log.
        */
        if ( kflag == 0 )
        {
            fa_error_dir = aps_fullpath (APS_FA_ERROR_FILES, NULL);

            /*
            -- fa_move_file() will print its own messages to syslog
            */
            if ( !fa_move_file( fullpath_file, APS_FA_ERROR_FILES ) )
            {
                (void)sprintf(file_util_msg, 
                    "Non-fatal ERROR. Could not move %s to %s area.\n",
                    fullpath_file, fa_error_dir);
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                    DO_SYSLOG, DO_PRINT);
            }
            else
                (void)fprintf(report_fp, 
                "File %s has an error.  It has been moved to %s area.\n",
                fullpath_file, fa_error_dir);

            free (fa_error_dir);
        }


        aps_log_msg(file_util_progname, APS_INFO, 
            "ASCII ingestion Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        status = APS_EXIT_ERROR ;
    }


    (void)fclose(ascii_file_ptr) ;
    (void)fclose(report_fp) ;

    free (report_dir) ;
    free (input_file) ;
    free (outgoing_dir) ;
    free (full_reportfile);
    free (fa_subrecord_control);  /* This is a global array which is 
                                  -- dynamically allocated during subrecord
                                  -- processing for the OPL1 file
                                  */
    exit(status); /* Will exit with APS_EXIT_OK or APS_EXIT_ERROR, 
                  -- depending on status returned by fa_ascii_processor.
                  */

} /* end of main() */

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

#define DEBUG
#undef  DEBUG

/*==============================================================================
Filename:       DTK_deletion.c

Description:    This is a tool used to select all DTKS for a given platform
                and time range; the selected DTKs are then marked 'DEL'
                (with all attendant effects) and also submitted to the 
                IMS for REJection.

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
==============================================================================*/
#pragma ident   "@(#)upd_DTK_stat.c	5.2 98/02/24 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/DTK_tools/SCCS/s.upd_DTK_stat.c"

#include <stdlib.h>         /* for getopt                           */
#include <stdio.h>
#include <string.h>
#include <unistd.h>         /* for unlink command                   */
#include <mu_utilities.h>   /* for obtaining permissions        */

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



static void 
usage_exit(char *progname)
{
    fprintf(stderr,     /* nicer format, keep it */
"\nusage:  %s -s <platform>  -b <strttime> -e <stoptime> \n",
        progname);
    fprintf(stderr,     /* nicer format, keep it */
"\t\t-P <password> [-U <user name>]\n\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-s <platform>  satellite being affected\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-U <user name> Sybase userid for Sybase account\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-P <password>  Sybase password for Sybase account\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-b <strttime>  start time in asf format:  yyyy:ddd:hh:mm:ss.sss\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-e <stoptime>  stop time  in asf format:  yyyy:ddd:hh:mm:ss.sss\n") ;

    fprintf(stderr,     /* nicer format, keep it */
        "\n\tNOTES:  \n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tThe data-takes are read from the database according to the\n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\ttime bracket in the command line.\n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tWe require the use of a password to implement the multi-user\n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tpermissions and to write to the database.\n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tThe selected DTKs are marked 'DEL' (with all attendant effects) \n") ;
    fprintf(stderr,     /* nicer format, keep it */
    "\tand also submitted to the IMS for REJection.\n") ;

    fprintf(stderr,     /* nicer format, keep it */
    "\n\t%s version %s %s\n", progname, __DATE__, __TIME__ ) ;

    aps_log_msg(file_util_progname, APS_INFO,
        "Program terminated abnormally.\n\n\n",
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
}

void
main(int argc, char *argv[])
{
    DBPROCESS   *APS_dbproc;

    int     j, c ;
    int     rcode;  /* return code for DB open                      */
    int     return_code;  /* return code for init_vec_lib           */
    int     permission_id = 0 ;         /* optional permission_id passed in */
    int     dtkm_process_list_return_code ;

    char    *dbname = NULL ;
    char    *env_dbname;        /* dbname from environment      */
    char    flag_list[20] = "s:U:P:b:e:" ; /* list of flags for getopts  */
    char    fullpath_file[20] = "upd_DTK_stat" ;
    char    dtkdate[ASF_TIME_STR_LENGTH+1] ;
    char    *env_sybase_userid; /* userid from env */
    char    *sybase_userid = NULL ;
    char    *password = NULL ;
    char    *input_file;
    char    *report_dir;
    char    *full_reportfile;
    char    *strttime = NULL ;
    char    *stoptime = NULL ;
    char    *platform = NULL ;

    llist       *input_dtk_list = NULL ;
    llist       *accepted_dtks = NULL ;
    llist       *rejected_dtks = NULL ;
    llist       *CON_dtks = NULL ;
    llist       *deleted_dtks = NULL ;
    llist       *error_dtks    = NULL ;
    llist       *omission_dtks = NULL ;
    llist       *other_sat_dtks = NULL ;
    llist       *same_sat_dtks = NULL ;
    llist       *dtk_updates = NULL ;
    DB_RECORD   **dtk_rec ;
    cursor      dtk_rec_ptr ;

    static char report_ext[] = ".rpt";
    static char slash[] = "/";

    extern int  optind;
    extern char *optarg;

    FILE    *report_log ;


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
        case 'P':
            if(password != NULL)
                usage_exit(file_util_progname);
            password = optarg ;
            break;
        case 'U':
            if(sybase_userid != NULL)
                usage_exit(file_util_progname);
            sybase_userid = optarg ;
            break;
        case 's':
            if(platform != NULL)            /* Check for duplicate flags */
                usage_exit(file_util_progname);
            platform = optarg ;
            break;
        case 'b':
            if(strttime != NULL)            /* Check for duplicate flags */
                usage_exit(file_util_progname);
            strttime = optarg ;
            if (tc_validate_asf_datetime(strttime) != TRUE)
            {
                sprintf(file_util_msg,
                    "Error in strttime, %s\n", strttime) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
                    DO_SYSLOG, DO_PRINT);
                usage_exit(file_util_progname);
            }
            break;
        case 'e':
            if(stoptime != NULL)            /* Check for duplicate flags */
                usage_exit(file_util_progname);
            stoptime = optarg ;
            if (tc_validate_asf_datetime(stoptime) != TRUE)
            {
                sprintf(file_util_msg,
                    "Error in stoptime, %s\n", stoptime ) ;
                aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
                    DO_SYSLOG, DO_PRINT);
                usage_exit(file_util_progname);
            }
            break;
        case '?':
            usage_exit(file_util_progname);
            break;
        default:
            /* do  nothing  */
            break;
        }

    /* check that all arguments are attached to flags.   */
    if(optind != argc)
        usage_exit(file_util_progname);
 
    /* manditory flag:  */
    if(password == NULL
    || strttime == NULL
    || stoptime == NULL
    || platform == NULL)
        usage_exit(file_util_progname);
 
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
            usage_exit(file_util_progname);
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
    
        usage_exit(file_util_progname);
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
            "Error opening db. Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }

    /*
    --
    -- Before accessing the database, get activity and planning permissions.
    --
       Here is the chain of events:
       1) Get planning permission.
       2) ACCESS DATABASE. RETRIEVE DTKS. UPDATE DTKS TO "DEL".
       3) Terminate planning permission.
 
    */
    return_code= mu_get_permission(file_util_progname, APS_dbproc,
                permission_id,              /*  is now populated            */
                MU_PLANNING_ACTIVITY_TYPE,  /*  single, planning, dar...    */
                MU_CREATE_EDIT_OR_DELETE_DTK,/* activity type               */
                strttime,                   /*  starttime range             */
                stoptime,                   /*  stoptime range              */
                MU_ALL_STATIONID,           /*  station_id ASF and MCM      */
                NULL,                       /*  darid                       */
                0,                          /*  number of retries           */
                0 ) ;                       /*  number of seconds between   */
    if (return_code < 0)
    {
#ifdef DEBUG
        printf("ERROR getting permissions.") ;
#endif
        aps_log_msg(file_util_progname, APS_ERROR,
            "could not get permissions for given time range, satellite.\n",
            DO_SYSLOG, DO_PRINT);
        exit (APS_EXIT_ERROR) ;
    }


    /*
    -- get today's date:  yyyy:ddd:hh:mm:ss.sss
    */
    return_code = tc_systime2asf(dtkdate);

    report_dir = aps_fullpath (APS_REPORTS, NULL);

    input_file = aps_pathname2filename (fullpath_file) ;
    fa_filename = input_file ;

    full_reportfile = malloc( 
        strlen(report_dir) + 1 + strlen(input_file) +
        strlen(report_ext) + 1 + ASF_TIME_STR_LENGTH ) ;
    sprintf (full_reportfile,
        "%s%s%s%4.4s_%3.3s_%2.2s_%2.2s_%2.2s%s", report_dir, slash, input_file, 
        &dtkdate[0], &dtkdate[5], &dtkdate[9], &dtkdate[12], &dtkdate[15], 
        report_ext) ;



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
            "Program terminated abnormally.\n\n\n",
            DO_SYSLOG, DO_PRINT);
        free (report_dir) ;
        free (full_reportfile) ;
        exit (APS_EXIT_ERROR) ;
    }
 
 
    /*
    -- Create the empty list of datatake proposals
    */
    input_dtk_list = create_dyn_llist() ;
    if(input_dtk_list == NULL)
    {
        unlink( full_reportfile ) ;
        free (report_dir) ;
        free (full_reportfile) ;
        exit(APS_EXIT_ERROR);
    }

    sprintf(where_clause,
"where %s = '%s' and %s>='%s' and %s<='%s' and(%s like '%s%%' or %s like '%s%%')",
        APS_COL(DTK,DTK_SAT), platform,
        APS_COL(DTK,DTK_STRTTIME), strttime,
        APS_COL(DTK,DTK_STOPTIME), stoptime,
        APS_COL(DTK,DTK_ACTID), DTKM_ACTID_REALTIME_OBSERVATION_CODE,
        APS_COL(DTK,DTK_ACTID), DTKM_ACTID_RECORDING_OBSERVATION_CODE
        ) ;
    input_dtk_list =
        db_get_records(APS_dbproc, APS_TABLE(DTK),
        where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;

#ifdef DEBUG
    dtkm_print_list (stdout, input_dtk_list ) ;
#endif

    /* 
    -- Create a report header, followed by the time range being examined
    */
    fprintf (report_log, 
    "date: %s \n\t\t%s SUMMARY for SAT:%s \n\tTIME RANGE: %s to %s\n\n",
            dtkdate, file_util_progname, platform, strttime, stoptime) ;        
 

    /*
    -- NOW, go through the list and set 'DEL' one proposal at at time
    */

    for (
        dtk_rec = (DB_RECORD **)FIRST(input_dtk_list, dtk_rec_ptr);
        dtk_rec;
        dtk_rec = (DB_RECORD **) NEXT(input_dtk_list, dtk_rec_ptr) 
        )
    {
        strcpy (CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "DEL") ;
    }

#ifdef DEBUG
    dtkm_print_list (stdout, input_dtk_list ) ;
#endif

    /*
    -- FINALLY, process the DTK list of proposals.
    */
    accepted_dtks = create_dyn_llist();
    rejected_dtks = create_dyn_llist();
    CON_dtks = create_dyn_llist() ;
    deleted_dtks = create_dyn_llist() ;
    error_dtks    = create_dyn_llist();
    omission_dtks = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks = create_dyn_llist() ;
    dtk_updates = create_dyn_llist() ;

 
 
 
    dtkm_process_list_return_code =
        dtkm_process_dtk_proposal_list (APS_dbproc, input_dtk_list,
        accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks,
        error_dtks, omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates,
        report_log);




    sprintf(file_util_msg, 
        "A summary is available in the report file named %s)", full_reportfile);
    aps_log_msg(file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);
    aps_log_msg(file_util_progname, APS_INFO, 
        "SUCCESS. Program completed normally.",
        DO_SYSLOG, DO_PRINT);


    free (report_dir) ;
    free (full_reportfile) ;
    fclose( report_log ) ;
    DEL_LIST (input_dtk_list) ;

    exit (APS_EXIT_OK) ;
}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       FA_dtkf_creator.c

Description:    source file for routines that create an REQQ/REQW 
                file from a dtk llist of DB_RECORDS.  

External Functions Defined:
    
File Scope Functions:

External Variables Defined:
    
File Scope Variables:
                VALUE_DEFS  NASDA_value_defs[] 
                            tables describing value format requirements, as well
                            as well as database sources for this informatiob.

Notes:

==============================================================================*/
#pragma ident   "@(#)FA_dtkf_creator.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.FA_dtkf_creator.c"

#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h>         /* for getopt                           */
#include <mu_utilities.h>   /* for permissions                      */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions        */
#include "apsfiledef.h"     /* for APS error definitions        */

/* FOR DATABASE TABLES        */
#include "db_dtk.h"         /* for dtk table             */
#include "db_schstat.h"     /* for schstat table         */

/* FOR DTK UTILITIES DEFINITIONS */
#include "dtkm_utilities.h"

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

#include "phase_utilities.h"
#include "file_utilities.h"     /* for PRINT_HEADER_ONLY definition */
#include "GENconversions.h"     /* for fa_creation_date and other globals */
#include "NASDAconversions.h"   /* for NASDA_files[] definitions */
#include "APSpmfutilities.h"    /* for PMF definitions */
#include "timeconv.h"           /* for tc_validate_asf_datetime(), 
                                       tc_systime2asf()             */
#include "aps_log_msg.h"        /* for APS logging                      */
#include "aps_defs.h"           /* exit codes for APS logging           */
#include "crt_grs_reqq.h"       /* for crt_grs_reqq()                   */

/* for debugging purposes */
#define   PRINT_DIAG 1 
#undef    PRINT_DIAG  

char        *file_util_progname ;   /* required by libfileutils.a   */
char        file_util_msg[MSG_LEN]; /* required by libfileutils.a,libNASDAc.a */

static void error_exit(char *progname)
{
        aps_log_msg(progname, APS_INFO, 
            "Program terminated abnormally.\n", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_EXIT_ERROR) ;
}

static void usage_exit(char *progname)
{

    fprintf(stderr,     /* nicer format, keep it */
"\nusage:  %s  -P <password> {-t REQW | -t REQQ -r <REQQ_phase>}",progname); 
    fprintf(stderr,     /* nicer format, keep it */
"\n\t\t-b <strttime> -e <stoptime> -o <filename>"); 
    fprintf(stderr,     /* nicer format, keep it */
"\n\t\t[-U <user name>] [-d] [-D] [-p <permission>]\n"); 
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-U <user name>  Sybase userid for Sybase account.\n" ) ;
    fprintf(stderr,     /* nicer format, keep it */
"\t-P <password>   Sybase password for Sybase account.\n" ) ;

    fprintf(stderr,     /* nicer format, keep it */
"\n\t-r <REQQ_phase>  REQQ phase number, used only with -t REQQ\n") ;

    fprintf(stderr,     /* nicer format, keep it */
"\t-b <strttime>  start time in asf format: yyyy:ddd:hh:mm:ss.sss\n") ;

    fprintf(stderr,     /* nicer format, keep it */
"\t-e <strttime>  stop time in asf format: yyyy:ddd:hh:mm:ss.sss\n") ;

    fprintf(stderr,     /* nicer format, keep it */
"\t-o <filename>  nomenclature follows filetype (eg:REQQ, REQW). \n");
    fprintf(stderr,     /* nicer format, keep it */
"\n\t-p <permission> single-activity permission (from the calling program)\n");
    fprintf(stderr,     /* nicer format, keep it */
"\t-d    if given, the program also prints out the data-take \n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t      list to standard output.\n") ;

    fprintf(stderr,     /* nicer format, keep it */
"\t-D    if given, the program also prints out the data-take \n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t      list to a file named with the characters '_DTK' appended\n");
    fprintf(stderr,     /* nicer format, keep it */
"\t      to the argument <filename>, in the APS_REPORTS area.\n") ;

    fprintf(stderr,     /* nicer format, keep it */
"\tNOTES:  \n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\tThe program also creates a metadata file whose name is the\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\targument <filename> with '.M' appended to it.  \n\n") ;
    fprintf(stderr,     /* nicer format, keep it */
"\t%s version   %s  %s\n", progname, __DATE__, __TIME__ ) ;

    aps_log_msg(file_util_progname, APS_INFO, 
        "Program terminated abnormally.\n", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR);
}

/*==============================================================================
Function:       Main routine to create an REQQ/REQW file from data-takes.  

Description:    

Parameters:     

Returns:        

Creator:        Ted Pavlovitch  (modify larry stevens' ODL_dtkf_creator)

Creation Date:  Mon oct 9 16:37:34 PDT 1995

Notes:      
==============================================================================*/
void
main(int argc, char *argv[])
{
    /* for getopt()   */
    extern char     *optarg ;
    extern int      optind ;

    char        *dbname = NULL ;
    char        *sybase_userid = NULL ;
    char        *password = NULL ;
    char        *env_dbname;        /* dbname from environment      */
    char        *env_sybase_userid; /* userid from environment      */

    RETCODE     return_code;
     
    FILE    *fp_output_file ; 
    FILE    *log_file_ptr ; 
    FILE    *req_file_ptr = NULL;  /* file pointer to REQQ or REQW file  */

    int     print_dtk_list_stdout = FALSE ;/* flag to print dtk list to stdout*/
    int     print_dtk_list_file = FALSE ;  /* flag to print dtk list to file. */
    int     free_outgoing_file = FALSE ;
    int     pflag = 0 ;                 /* signals terminate permission     */
    int     rflag = 0 ;                 /* check mandatory REQQ phase #     */
    int     permission_id = 0 ;         /* optional permission_id passed in */
    int     phase = 0 ;                 /* mandatory REQQ phase # passed in */
    int     return_status ;
    char    *reqfilename =   NULL;      /* REQQ or REQW filename            */
    char    *metafilename = NULL; 
    char    *outgoing_file = NULL ;
    char    *short_filename = NULL ;
    char    *temp_filename = NULL ;
    char    filename_DTK[128] ;
    char    *filetype =      NULL;     /* REQQ or REQW */
    char    *strttime =      NULL;     /* req strttime */
    char    *stoptime =      NULL;     /* req stoptime */
    char    *log_dir = NULL ;
    char    full_log_file_name[200] ;
    char    now_asftime[ASF_TIME_STR_LENGTH+1] ;
    llist   *dtk_list =      NULL;
    llist   *dl_dtk_list = NULL ;
    llist   *dtk_empty_list = NULL;

    int     j, c;      /* used as return character from getopt()       */
    int     int4rev ;

    char    qorw;   /* filetype select in where clause              */
     
    char    flag_list[20] = "p:U:P:t:b:e:o:r:dD"; /* flags for getopt  */
     
    int     slash = '/';


    PMF_FILENAME    *pmf_descriptors ;
    FA_FILENAME     *file_descriptors ;

    APSPMF_metadata *req_PMF_struct ;
    FA_FILEDEF      *req_filedef_ptr = NULL ;

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
            if (pflag != 0)                 /* Checking for duplicate flags */
                error_exit(file_util_progname) ;
            pflag++ ;
            return_code = sscanf( optarg, "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
                    "%s(%d): %s  can't be scanned into int permission_id\n",
                    __FILE__, __LINE__, optarg) ;
                usage_exit(file_util_progname) ;
            }
            break ;
        case 'r':
            if (rflag != 0)                 /* Checking for duplicate flags */
                error_exit(file_util_progname) ;
            rflag++ ;
            phase = atoi(optarg);
            break ;
        case 'U':
                if( sybase_userid != NULL ) /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                sybase_userid = optarg ;
                break;
        case 'P':
                if( password != NULL )      /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                password = optarg ;
                break;
        case 't':
                if(filetype != NULL)        /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                filetype = optarg ;
                if(!identify_file(filetype, NASDA_files, &file_descriptors ))
                {
                    sprintf (file_util_msg, 
                    "COULD NOT MATCH FILE %s TO A FLIGHT AGENCY.\n",filetype);
                    aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                        DO_SYSLOG, DO_PRINT);

                    error_exit(file_util_progname) ;
                }
                else
                    req_filedef_ptr = file_descriptors->file_definition ;

                /* for now, this file only works for REQQ, REQW:  */
                if(strcmp(filetype,"REQQ") == 0 || strcmp(filetype,"REQW") == 0)
                break;
                else
                    usage_exit(file_util_progname);
                break;
        case 'b':
                if(strttime != NULL)        /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                strttime = optarg ;
                if (tc_validate_asf_datetime(strttime) != TRUE)
                {
                    aps_log_msg(file_util_progname, APS_ERROR, 
                        "error in strttime\n", 
                        DO_SYSLOG, DO_PRINT);

                    error_exit(file_util_progname) ;
                }
                break;
        case 'e':
                if(stoptime != NULL)        /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                stoptime = optarg ;
                if (tc_validate_asf_datetime(stoptime) != TRUE)
                {
                    aps_log_msg(file_util_progname, APS_ERROR, 
                        "error in stoptime\n", 
                        DO_SYSLOG, DO_PRINT);

                    error_exit(file_util_progname) ;
                }
                break;
        case 'o':
                if( reqfilename != NULL)    /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                reqfilename = optarg;

                /* 
                -- Check the reqfilename.  If it is a fullpathname, keet it.
                -- If not a fullpathname, find destination directory for file
                -- and use it to precede the filename.
                --
                -- Also, create the metafilename at this time.  Use it to store 
                -- the PMF information which accompanies the main file.
                */
                if ( strrchr(reqfilename, slash) == NULL)
                {
                    /* 
                    -- This is a filename, not a fullpathname. 
                    -- We need to determine outgoing_file, which is where 
                    -- any files that we create will be deposited.
                    */
                    outgoing_file = aps_fullpath (APS_NASDA_FILES, reqfilename);
                    free_outgoing_file = TRUE ;
                }
                else
                    /*
                    -- this is fullpath file_name, take no action to shorten it.
                    -- Send PMF file to the same fullpath destination specified.
                    */
                    outgoing_file = reqfilename ;
                short_filename = aps_pathname2filename (outgoing_file);
                metafilename =   malloc(strlen(outgoing_file) +2 + 1) ;
                sprintf(metafilename,"%s.M", outgoing_file) ;

                break;
        case 'd':
                if( print_dtk_list_stdout ) /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                print_dtk_list_stdout = TRUE ;
                break;
        case 'D':
                if( print_dtk_list_file )   /* Checking for duplicate flags */
                    usage_exit(file_util_progname);
                print_dtk_list_file = TRUE ;
                break;
        case '?':
                usage_exit(file_util_progname);
                break;
        default:
                /* do  nothing  */
                break;
    }

    /* Check that all arguments are attached to flags */
    if(optind != argc )
        usage_exit(file_util_progname);

    /* Check that all mandatory arguments are entered  */
    if ( password == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "password not given.\n", 
            DO_SYSLOG, DO_PRINT);
    if ( strttime  == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "strttime not given.\n", 
            DO_SYSLOG, DO_PRINT);
    if ( stoptime  == NULL )
        aps_log_msg(file_util_progname, APS_ERROR, 
            "stoptime not given.\n", 
            DO_SYSLOG, DO_PRINT);
    if ( reqfilename == NULL)
        aps_log_msg(file_util_progname, APS_ERROR, 
            "request file name not given.\n",
            DO_SYSLOG, DO_PRINT);
    if ( filetype == NULL )
    {
        aps_log_msg(file_util_progname, APS_ERROR, "filetype not given.\n", 
        DO_SYSLOG, DO_PRINT);
    }
    else
    {
        if (strcmp(filetype,"REQQ") == 0
        && phase == 0)
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "phase number for REQQ not given.\n", DO_SYSLOG, DO_PRINT);
            usage_exit(file_util_progname); 
        }
    }
    if ( password == NULL 
      || filetype == NULL 
      || strttime  == NULL 
      || stoptime  == NULL 
      || reqfilename == NULL)
    {
        usage_exit(file_util_progname); 
    }


    if ( print_dtk_list_file )
    {
        /* 
        -- open and check file name for writeability.  
        */
        temp_filename = aps_fullpath (APS_REPORTS, short_filename);
        strcpy(filename_DTK, temp_filename) ;
        strcat(filename_DTK, "_DTK") ;

        if ( (fp_output_file = fopen(filename_DTK, "w")) == NULL )
        {
            sprintf (file_util_msg, 
                "could not open file filename_DTK = %s for writing.\n", 
                filename_DTK ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);

            error_exit(file_util_progname) ;
        }
        free (temp_filename) ;
    }

    /* 
    -- check the start and stop times now.  
    -- use a test from later on, when processing 
    -- the PMF.  
    -- NOTE:  WE ASSUME J1, since only J1 is accepted at this point.  
    */
    if ( asftime2rev( "J1", strttime, &int4rev) != PHASE_ASFTIME2REV_OK )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "error in strttime; not within phase.  \n",
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        error_exit(file_util_progname) ;
    }

    if ( asftime2rev( "J1", stoptime, &int4rev) != PHASE_ASFTIME2REV_OK )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "error in stoptime; not within phase.  \n",
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        error_exit(file_util_progname) ;
    }

    /*
    -- check for being able to write to the 
    -- database:
    */

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
                    "sybase_userid not given\n",
                    DO_SYSLOG, DO_PRINT);

                if ( print_dtk_list_file )
                    (void) unlink(filename_DTK) ;
                error_exit(file_util_progname) ;
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
                "Use setenv APSDB <dbname>. \n\n",
                DO_SYSLOG, DO_PRINT);

            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            usage_exit(file_util_progname);
        }
        dbname = env_dbname ;
     
        /* now open the database.            */
        /* db_open will handle the errors.   */
        FA_dtkf_dbproc = 
            db_open( dbname, file_util_progname, sybase_userid, 
            password, NULL, error_handler_exit, &return_code);
        if(return_code != DB_OPEN_OK)
        {
            db_open_errs(return_code, dbname, sybase_userid);

            aps_log_msg(file_util_progname, APS_INFO, 
                "DB could not be opened. Program terminated abnormally.\n", 
                DO_SYSLOG, DO_PRINT);
            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            exit (APS_EXIT_ERROR) ;
        }

    
    /*
    -- Set up file-dependent variables.
    */ 
    if( strcmp(filetype,"REQQ") == 0)
    {
        qorw = 'q';
        strcpy (fa_activity_type, MU_REQQ) ;
    }
    else
    {
        qorw = 'w';
        strcpy (fa_activity_type, MU_REQW) ;
    }


    /* open the REQQ/REQW file */

    if ( ( req_file_ptr = fopen(outgoing_file,"w") ) == NULL )
    {
        sprintf (file_util_msg, 
            "could not open file outgoing_file = %s for writing.\n", 
            outgoing_file);
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        error_exit(file_util_progname) ;
    }

    sprintf( file_util_msg, "Writing REQQ file to %s\n", outgoing_file ) ;
    aps_log_msg(file_util_progname, APS_INFO, 
        file_util_msg, DO_SYSLOG, DO_PRINT);

    /*
    --
    -- Before accessing the database, get activity and planning permissions.
    --
       Here is the chain of events:
    -> 1) If activity permission not provided, request it.
          If activity permission is  provided, validate it.
       2) Get planning permission.
       3) ACCESS DATABASE. RETRIEVE DTKS. CREATE REPORT. UPDATE DTKS.
       4) Terminate planning permission.
       5) If we requested activity permission ourselves, terminate it.
    --
    */
    return_code= mu_get_permission(file_util_progname, FA_dtkf_dbproc,
                permission_id,              /*  may be populated, passed in */
                MU_SINGLE_ACTIVITY_TYPE,    /*  single, planning, dar....   */
                fa_activity_type,           /*  activity type               */
                NULL,                       /*  strttime                    */
                NULL,                       /*  stoptime                    */
                NULL,                       /*  station_id                  */
                NULL,                       /*  darid                       */
                0,                          /*  number of retries           */
                0 ) ;                       /*  number of seconds between   */
    if (return_code < 0)
    {
        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        (void) unlink(outgoing_file) ;
        error_exit(file_util_progname) ;
    }
    else
        permission_id = return_code ;

    /*
    -- Item (2). Get planning permission. 
    -- PLEASE NOTE: J1 is only supported at ASF groundstation.
    */
    return_code= mu_get_permission(file_util_progname, FA_dtkf_dbproc,
                permission_id,              /*  is now populated            */
                MU_PLANNING_ACTIVITY_TYPE,  /*  single, planning, dar...    */
                fa_activity_type,           /*  activity type               */
                strttime,                   /*  starttime for report        */
                stoptime,                   /*  stoptime for report         */
                MU_ASF_STATIONID,           /*  station_id for J1           */
                NULL,                       /*  darid                       */
                0,                          /*  number of retries           */
                0 ) ;                       /*  number of seconds between   */
    if (return_code < 0)
    {
        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        (void) unlink(outgoing_file) ;
        error_exit(file_util_progname) ;
    }

    /*
    -- Now retrieve the data-takes according to the 
    -- file type:  only REQQ and REQW are allowed right now:
    */
    sprintf(where_clause, "where %s = 'J1'\
                    \nand %s != '%s'\
                    \nand %s != '%s'\
                    \nand %s < '%s'\nand %s > '%s'\
                    \nand %s = status and req%c = 'Y'",
                    APS_COL(DTK, DTK_SAT),
                    APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,
                    APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
                    APS_COL(DTK, DTK_STRTTIME), stoptime,
                    APS_COL(DTK, DTK_STOPTIME), strttime,
                    APS_COL(DTK, DTK_DTKSTAT),qorw);



    sprintf(tables, "%s, schstat",APS_TABLE(DTK));

    sprintf(orderby_cols, "%s", APS_COL(DTK, DTK_STRTTIME)) ; 

    dtk_list = db_get_records(FA_dtkf_dbproc, 
                    tables, where_clause, orderby_cols,
        APS_CDEFS(DTK), ALL_COLS) ;

    /* 
    -- the data-takes were retrieved ; check them
    */
    if (dtk_list == NULL)
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "Error in retrieving Sybase data.\n",
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        (void) unlink(outgoing_file) ;
        error_exit(file_util_progname) ;
    }

    /* 
    -- NOTE:  if the list had 0 members, make a file 
    -- anyway.  
    */

    /*
    -- Now create the REQQ/REQW file.  
    */

    /* first set the creation time  */

    tc_systime2asf(fa_creation_date) ;


    if(!identify_PMF_file(filetype, PMF_files,     &pmf_descriptors ))
    {
        sprintf (file_util_msg, 
        "COULD NOT MATCH PMF %s TO A FLIGHT AGENCY.\n",filetype) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        (void) unlink(outgoing_file) ;
        error_exit(file_util_progname) ;
    }
    else
        req_PMF_struct = pmf_descriptors->file_descriptor ;


    dtk_empty_list = create_dyn_llist() ;

    /* 
    -- no matter how many records are retrieved, 
    -- create the whole file.  
    -- Header         :  always
    -- data records   :  probably, none of 0 records retrieved  
    -- trailer records:  there is no trailer in REQQ/REQW.  
    */
    return_code = ascii_file_creator( !(PRINT_HEADER_ONLY),
                      dtk_list,dtk_empty_list,req_filedef_ptr,req_file_ptr) ;
    if ( return_code < 0 )
    {
        /* 
        -- error in creating the file; 
        -- terminate now.  
        */
        sprintf (file_util_msg, 
        "%s(%d):  ERROR in creating the file.  \n", __FILE__, __LINE__ ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR, 
            "TERMINATING THE RUN\n", 
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        (void) unlink(outgoing_file) ;
        error_exit(file_util_progname) ;
    }

    /*
    -- Create the report file describing the REQQ/REQW creation.
    */
    log_dir = aps_fullpath( APS_REPORTS, NULL ) ;
    tc_systime2asf( now_asftime );
    (void) sprintf( full_log_file_name,
        "%s/%4.4s_%4.4s%3.3s_%2.2s%2.2s%2.2s.rpt",
        log_dir, filetype, now_asftime, now_asftime+5,
        now_asftime+9, now_asftime+12, now_asftime+15);

#ifdef TEST_ONLY
    log_file_ptr = stdout ;   /* for testing, write log to stdout, see it.   */
#else 
    /* not testing:  write log file to file.  */
    if ( ( log_file_ptr = fopen(full_log_file_name,"w") ) == NULL )
    {
        sprintf (file_util_msg, 
            "could not open file full_log_file_name = %s for writing.\n", 
            full_log_file_name);
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        error_exit(file_util_progname) ;
    }

    sprintf( file_util_msg, "Writing log file to %s\n", full_log_file_name ) ;
    aps_log_msg(file_util_progname, APS_INFO, 
        file_util_msg, DO_SYSLOG, DO_PRINT);
#endif

    /* CLOSE THE OUTPUT FILE  (outgoing_file, req_file_ptr )   */
    fclose(req_file_ptr) ;
    if (fa_trigger_REQQ_add_grs_records)
    {
        /*
        -- Add the GRS information to the end of the REQQ file.
        -- Re-pen the REQQ file with "r+" (update), so that 
        -- the header may be updated with the correct number of 
        -- records by the grs routines, and so that records can 
        -- be added, too.  
        */
        if ( ( req_file_ptr = fopen(outgoing_file,"r+") ) == NULL )
        {
            sprintf (file_util_msg, 
                "could not open file outgoing_file = %s for REQQ updating.\n", 
                outgoing_file);
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            error_exit(file_util_progname) ;
        }

        return_status = crt_grs_reqq( req_file_ptr, log_file_ptr, 
                                      strttime, stoptime, phase) ;
        fclose(req_file_ptr) ;   /* close the output file     */
        if (return_status != 0)
        {
            /* 
            -- error in appending grs records to the file; 
            -- terminate now.  
            */
            sprintf (file_util_msg, 
            "%s(%d):  ERROR in appending out-of-mask info to the file.  \n", 
                __FILE__, __LINE__ ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_ERROR, 
                "TERMINATING THE RUN\n", 
                DO_SYSLOG, DO_PRINT);

            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            error_exit(file_util_progname) ;
        }
    }

    /* 
    -- find the downlink data-takes for the observations.  
    -- update the observation data-takes to SUB status from QUE; 
    -- also update their downlink data-takes to SUB.  
    -- use dtk_empty_list as the output list.  
    */
    dl_dtk_list = create_dyn_llist() ;
    return_code = dtkm_obs2dl_list( dtk_list, dl_dtk_list ) ;
    if ( return_code < 0 )
    {
        sprintf (file_util_msg, 
            "%s(%d):  ERROR in finding downlink data-takes.\n%s\n",
            __FILE__, __LINE__, DTKM_ERROR_MESSAGE( return_code ) ) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(file_util_progname, APS_ERROR, "TERMINATING THE RUN\n", 
            DO_SYSLOG, DO_PRINT);
        if ( print_dtk_list_file )
            (void) unlink(filename_DTK) ;
        (void) unlink(outgoing_file) ;
        error_exit(file_util_progname) ;
    }

    /* 
    -- update the data-takes to 
    -- DTK_PROPOSED_DTKSTAT = "SUB"  
    -- the updated version will be in dtk_empty_list 
    */ 
    if( NUMELTS(dtk_list) > 0 )
    {
        return_code = dtkm_update_dtks_field( FA_dtkf_dbproc, dtk_list, 
            DTK_PROPOSED_DTKSTAT, "SUB", dtk_empty_list ) ;
        if ( return_code < 0 )
        {
            sprintf (file_util_msg, 
           "%s(%d):  ERROR in updating data-take proposed status to SUB.\n%s\n",
                __FILE__, __LINE__, DTKM_ERROR_MESSAGE( return_code ) ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_ERROR, "TERMINATING THE RUN\n", 
                DO_SYSLOG, DO_PRINT);
            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            (void) unlink(outgoing_file) ;
            error_exit(file_util_progname) ;
        }
    }
    /* 
    -- update the downlink data-takes to 
    -- DTK_PROPOSED_DTKSTAT = "SUB"  
    -- the updated data-takes will be added to dtk_empty_list 
    */ 
    if( NUMELTS(dl_dtk_list) > 0 )
    {
        return_code = dtkm_update_dtks_field( FA_dtkf_dbproc, dl_dtk_list, 
            DTK_PROPOSED_DTKSTAT, "SUB", dtk_empty_list ) ;
        if ( return_code < 0 )
        {
            sprintf (file_util_msg, 
          "%s(%d):  ERROR in updating data-take proposed status to SUB.\n%s\n",
                __FILE__, __LINE__, DTKM_ERROR_MESSAGE( return_code ) ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_ERROR, 
                "TERMINATING THE RUN\n", 
                DO_SYSLOG, DO_PRINT);
            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            (void) unlink(outgoing_file) ;
            error_exit(file_util_progname) ;
        }
    }

    /* 
    -- now further update all the data-takes to 
    -- DTK_DTKSTAT = "SUB"  
    -- must provide the RECENTLY UPDATED 
    -- versions of all the DB_RECORDS when calling
    -- dtkm_update_dtks_field(), which writes all of 
    -- the values from the list.  
    -- If we used the original list, dtk_list, we 
    -- would lose the update of the DTK_PROPOSED_DTKSTAT
    -- value.  
    -- Notes that dtk_empty_list is not empty at this 
    -- point, it contains all the dtks that were updated, 
    -- which should be all of the dtks from the 2 lists.  
    */
    if( NUMELTS( dtk_empty_list ) )
    {
        /* if there were any dtks updated, update them again. */
        return_code = dtkm_update_dtks_field(FA_dtkf_dbproc, dtk_empty_list, 
            DTK_DTKSTAT, "SUB", dtk_list ) ;
        if ( return_code < 0 )
        {
            sprintf (file_util_msg, 
                "%s(%d):  ERROR in updating data-takes to SUB status.\n%s\n",
                __FILE__, __LINE__, DTKM_ERROR_MESSAGE( return_code ) ) ;
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(file_util_progname, APS_ERROR, 
                "TERMINATING THE RUN\n", 
                DO_SYSLOG, DO_PRINT);

            if ( print_dtk_list_file )
                (void) unlink(filename_DTK) ;
            (void) unlink(outgoing_file) ;
            error_exit(file_util_progname) ;
        }
    }

    /*
    -- populate some of the global variables that will be needed
    -- fa_filename = short_filename ;
    */
    req_PMF_struct[FILE_NAME].field =  short_filename;
    strncpy(fa_sat_id, "J1", 2) ;
 
    if( !(asftime2rev( fa_sat_id, strttime, &int4rev) == PHASE_ASFTIME2REV_OK 
               &&      sprintf(fa_file_start_rev, "%d", int4rev) > 0 ) 
 
    ||  !(asftime2rev( fa_sat_id, stoptime, &int4rev) == PHASE_ASFTIME2REV_OK
               &&  sprintf(fa_file_stop_rev, "%d", int4rev) > 0 )
 
    ||  !  tc_asf2odl(strttime,    fa_file_start_time)
    ||  !  tc_asf2odl(stoptime,    fa_file_stop_time)
    ||  !  APS_create_pmf(metafilename, req_PMF_struct)   )
    {
        sprintf (file_util_msg, 
            "Error creating IMS PMF file for File Type: %s '%s'\n",
            filetype, outgoing_file) ;
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        /*
        -- there were errors creating the PMF file, but leave the
        -- data file and the dtk_file around for tracking purposes.
        -- (ie: there is no 'unlink' action)
        */
        error_exit(file_util_progname) ;
    }


    /* 
    -- Now optionally print out the dtk list:
    */
    if ( print_dtk_list_stdout )
    {
        fprintf(stdout, "\n" ) ; /* nicer format, keep it */

        fprintf(stdout,         /* nicer format, keep it */
            "LIST OF DATATAKES IN %s FILE\n", filetype ) ;
        fprintf(stdout,         /* nicer format, keep it */
            "-----------------------------\n" ) ;
        if (outgoing_file!= NULL)
            fprintf(stdout,         /* nicer format, keep it */
                "FILENAME     :  %s\n", outgoing_file ) ;

        fprintf(stdout,         /* nicer format, keep it */
            "CREATION TIME:  %s\n", fa_creation_date ) ;
        dtkm_print_list(stdout, dtk_list ) ;
    }

    if ( print_dtk_list_file )
    {
        fflush(fp_output_file) ;
        fprintf(fp_output_file, "LIST OF DATATAKES IN %s FILE\n", filetype ) ;
        fprintf(fp_output_file, "-----------------------------\n" ) ;
        if (outgoing_file!= NULL)
            fprintf(fp_output_file, "FILENAME     :  %s\n", outgoing_file ) ;

        fprintf(fp_output_file, "CREATION TIME:  %s\n", fa_creation_date ) ;
        dtkm_print_list(fp_output_file, dtk_list ) ;
        fclose(fp_output_file) ;
    }

    free(short_filename) ;
    if (free_outgoing_file) 
        free(outgoing_file ) ;
    free(metafilename ) ;

    /*
    -- Item (4) Terminate single activity permission.
    */
    if (!pflag)
    {
        /*
        -- We acquired the single activity permission id ourselves.
        -- Now, we must terminate the single activity permission.
        -- If mu_permission_terminate() fails, it is non-fatal.
        */
        return_code = mu_permission_terminate( FA_dtkf_dbproc,
            permission_id,
            fa_activity_type,               /*  mu_activity_id  */
            MU_SINGLE_ACTIVITY_TYPE) ;      /*  activity_types  */
     
        if( return_code < 0 )
        {
            fprintf(stderr,"%s(%d):  permission terminate error code %d\n",
                __FILE__, __LINE__, return_code ) ;
            fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
        }
    }

    /*
    -- Item (5). Terminate the planning permission.
    -- If mu_permission_terminate() fails, it is non-fatal.
    */
    return_code = mu_permission_terminate( FA_dtkf_dbproc,
        permission_id,
        fa_activity_type,               /*  mu_activity_id  */
        MU_PLANNING_ACTIVITY_TYPE) ;    /*  activity_types  */
 
    if( return_code < 0 )
    {
        fprintf(stderr,"%s(%d):  permission terminate error code %d\n",
            __FILE__, __LINE__, return_code ) ;
        fprintf(stderr,"%s\n", MU_ERROR_MESSAGE(return_code) ) ;
    }

    aps_log_msg(file_util_progname, APS_INFO, 
        "Program completed successfully.\n", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_OK) ;

} /* end of main()  */

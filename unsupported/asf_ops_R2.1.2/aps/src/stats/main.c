#undef RUN_FROM_GUI
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    Main driver for APS statistics; intended to be run 
                as a cron job daily just after 00 hours system time.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)main.c	1.2 98/03/23 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.main.c"

#include <unistd.h>         /* for access() determine file accessibility*/
#include <stdio.h>
#include <stdlib.h>         /* for getenv(), getopt(), malloc()         */
#include <string.h>         /* for strrchr()                            */

#include <db_sybint.h>      /* for APS sybase interface routines.       */
#include <aps_db_table.h>   /* for DTK - accessing dtk table.           */
#include <dapps_defs.h>     /* for ASF_TIME_STR_LENGTH                  */
#include <db_dtk.h>         /* for APS db relation dtk.                 */
#include <aps_encrypt.h>    /* for aps_encrypt()                        */
#include <aps_defs.h>       /* for APS_EXIT_ERROR etc.                  */
#include <apspath.h>        /* for aps_fullpath, needed when using -E   */
#include <open_log_file.h>  /* for LOG_FILE_NAME_MIN_SIZE, open_log_file()  */
#include <mu_utilities.h>   /* for getting permissions                  */
#include <timeconv.h>       /* for tc_systime2asf()                     */

/* 
--  this line was required to eliminate a warning:  
--  implicitly declaring function to return int
--  even though we include the unix .h files above.  
*/
extern int      getopt(int, char *const *, const char *);

#include <aps_log_msg.h>    /* for aps_log_msg() stuff and MSG_LEN      */
#include "aps_Statistics.h" /* for application function prototypes      */

/* GLOBAL variable for use in messages:  */
char        msg[MSG_LEN];

static void normal_exit(char *progname, FILE *logfp, int error_count)
{
    if( error_count == 0 )
    {
        (void) sprintf(msg, "Program completed successfully." ) ;
        (void) fprintf(logfp, "\n%s:  APS_INFO:  %s\n", progname, msg ) ;
        aps_log_msg(progname, APS_INFO,  msg, DO_SYSLOG, DO_PRINT);
        exit(APS_EXIT_OK) ;
    }
    if( error_count == 1 )
    {
        (void) sprintf(msg,
"Program completed normally, but there was 1 call with an error." ) ;
        (void) fprintf(logfp, "\n%s:  APS_INFO:  %s\n", progname, msg ) ;
        aps_log_msg( progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT );
        exit(APS_EXIT_OK) ;
    }
    if( error_count > 1 )
    {
        (void) sprintf(msg,
"Program completed normally, but there were %d calls with errors.",
            error_count ) ;
        (void) fprintf(logfp, "\n%s:  APS_INFO:  %s\n", progname, msg ) ;
        aps_log_msg( progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT );
        exit(APS_EXIT_OK) ;
    }
}

static void error_exit(char *progname, FILE *logfp )
{
    if( (int) logfp != NULL )
        (void) fprintf(logfp, 
            "\n%s:  APS_INFO:  Program terminated abnormally.\n",
            progname ) ;
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_ERROR) ;
}

static void usage_exit(char *progname)
{
    (void) fprintf(stderr, "\nusage: %s { -E | [-U Sybase_userid] -P Sybase_password }\n",
            progname);
    (void) fprintf(stderr, 
    "       [ -p <permission id> | \n");
    (void) fprintf(stderr, 
    "       { -R <number of retries> -S <seconds between retries> } ]\n");
    (void) fprintf(stderr, 
    "       [ -d dtkdate ] [ -l n ]  [-k]\n");
    (void) fprintf(stderr,
    "\n    This program reports APS statistics for the previous 10 days.\n") ;
    (void) fprintf(stderr,
    "    It reports on SCH, cancelled, and reduced downlinks with stoptime \n");
    (void) fprintf(stderr,
    "    in the previous day.  It also reports on PLN SAR dtks planned on \n");
    (void) fprintf(stderr,
    "    the previous day.\n") ;

    (void) fprintf(stderr,
    "    This program is to be run in a daily Cron job just after 00:00 \n");
    (void) fprintf(stderr,
    "    using the -E feature.  When testing, the -d feature is useful.\n") ;
    (void) fprintf(stderr,
    "    -E     obtain Sybase_userid and Sybase_password via Encryption\n");
    (void) fprintf(stderr,
    "    <permission id>   An integer indicating an existing permission.\n" ) ; 
    (void) fprintf(stderr,
    "                      If not provided, -R can be used.\n" ) ;
    (void) fprintf(stderr,
"    <number of retries>     An integer indicating the number of extra\n" ) ; 
    (void) fprintf(stderr,
    "                            attempts to get permission if the first\n" ) ;
    (void) fprintf(stderr,
    "                            attempt fails.  -S is required with this\n" ) ;
    (void) fprintf(stderr,
    "                            parameter.\n" ) ;
    (void) fprintf(stderr,
"    <seconds between retries>   An integer indicating the amount of time\n" ) ;
    (void) fprintf(stderr,
"                                in seconds between permission attempts.\n" ) ;
    (void) fprintf(stderr,
    "    -d dtkdate   yyyy:ddd The date which is to be taken for today.\n");
    (void) fprintf(stderr,
    "    -l n         Look back n days (instead of 10) when reporting data.\n");
    (void) fprintf(stderr,
    "    -k           Keep old stats_calls records, do not delete any.\n" ) ;

    (void) fprintf(stderr, "\n    %s  Version compiled:  %s %s\n",
        progname, __DATE__, __TIME__ ) ;
    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    error_exit(progname, NULL) ;
}
static int 
env_is_set( 
    char *progname, 
    char *env_name )
{
    char *check_env_value ;

    check_env_value = getenv(env_name) ;
    if ( check_env_value == NULL)
    {
        (void) sprintf(msg, "environment variable:  %s is not set.", env_name);
#ifdef  RUN_FROM_GUI
        (void) fprintf( stderr, "%s:\n\n%s\n", progname, msg );
#endif  /*   RUN_FROM_GUI    */
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return FALSE ;
    }
    else
    {
        return TRUE ;
    }
}


/*==============================================================================
Function:       main.c

Description:    main driver for APS statistics.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 18 17:28:58 PST 1997

==============================================================================*/
void main (int argc, char *argv[])
{
    extern int init_vec_lib();

    APS_STATS_IMS_INFO *aps_stats_ims_info ;
    FILE        *logfp ;      /* log file pointer.  */
    char        now_asftime[ASF_TIME_STR_LENGTH+1] ;
    char        log_filename[LOG_FILE_NAME_MIN_SIZE] ;

    DBPROCESS   *APS_dbproc ;
    char        *dbname = NULL ;

    char        *user_pw_filename = NULL ;
    char        *APS_rootpath = NULL ;
    int         bad_env_count  = 0 ;
    char        *sybase_userid = NULL ;
    char        *password = NULL ;
    char        dtkdate[9] = "yyyy:ddd" ;
    char        today_asftime[ASF_TIME_STR_LENGTH+1] = "yyyy:ddd:12:00:00.000" ;
    char        *progname ;
    char        *slash_ptr ;
    char        *comma_ptr ;
    char        *user_pw_string ;
 
    int         j ;
 
    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;
 
    RETCODE     return_code;
    int         retcode;

    char        *env_value;         /* value from environment           */
    char        *env_dbname;        /* dbname from environment          */
    char        *env_sybase_userid; /* userid from environment          */
    char        *pmf_directory ;     /* directory for output PMF files.  */

    int         c;      /* used as return character from getopt()       */
 
                /* flag list for getopt */
    char        flag_list[20] = "U:P:L:N:ER:S:p:d:kl:"; 
 
    int     Eflag = 0 ;
    int     Lflag = 0 ; /* log file to stdout flag    */
    int     Nflag = 0 ;
    int     dflag = 0;  /* used to check for optional dtkdate                */
    int     kflag = 0;  /* used to check for optional -k                     */
    int     lflag = 0;  /* used to check for optional look back no. days     */
    int     n_lookback_days = 10 ; /* look back no. days defaults to 10.     */
    int     keep_flag = FALSE ; /* if true, keep all old stats_calls recs */

    /* for MU permission */
    int     pflag = 0;  /* used to check for optional permission_id          */
    int     Sflag = 0;  /* used to check for optional n_seconds_retry        */
    int     Rflag = 0;  /* used to check for optional n_retries              */
    int     permission_id   = 0 ;  /* optional permission_id passed     */
    int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
    int     n_retries       = 0 ;  /* optional number of re-tries       */

    int     IMS_error_count = 0 ;
    int     IMS_fatal_count = 0 ;
 
    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    /* 
    -- get progname as file name without 
    -- the directory path.  
    */
    progname = argv[0] ;
    slash_ptr = strrchr( progname, '/' ) ;
    if ( slash_ptr )
    {
        /* 
        -- if there is a directory path in progname,
        -- remove it.  reset the progname pointer to 
        -- the next character after the last slash:  
        */
        progname = slash_ptr + 1 ;
    }
 
    /*
    -- start up syslog messages.
    */
    aps_open_syslog();
    (void) sprintf(msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        (void) strcat(msg, " " ) ;
        (void) strcat(msg, argv[j] ) ;
    }
    aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

    if ( argc <= 1 )
        usage_exit(progname);

    /*
    -- The log file will now contain anything non-brief.
    -- the reporting methods now are:
    -- log file:  all messages, including start, end, errors, extra info.
    -- sysout and syserr:  goes to e-mail; this job is usually a cronjob.
    --                     a few brief messages only.
    -- syslog:  brief messages only, including start, end, and errors.
    */

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'E':
                if( Eflag != 0 )
                    usage_exit(progname);
                if( password != NULL )
                    usage_exit(progname);
                if( sybase_userid != NULL )
                    usage_exit(progname);
                Eflag++ ;
                break;
            case 'L':
                if( Lflag != 0 )
                    usage_exit(progname);
                Lflag++ ;
                if( strcmp( optarg, "stdout" ) != 0 )
                    usage_exit(progname);
                (void) printf(
                    "Log file printed to standard out:  -L stdout\n" ) ;
                break;
            case 'N':
                if( Nflag != 0 )
                    usage_exit(progname);
                Nflag++ ;
                if( strcmp( optarg, "IMS" ) != 0 )
                    usage_exit(progname);
                (void) printf( "** IMS calls surpressed:  -N IMS\n" ) ;
                break;
            case 'P':
                if( Eflag != 0 )
                    usage_exit(progname);
                if( password != NULL )
                    usage_exit(progname);
                password = optarg ;
                break;
            case 'U':
                if( Eflag != 0 )
                    usage_exit(progname);
                if( sybase_userid != NULL )
                    usage_exit(progname);
                sybase_userid = optarg ;
                break;
            case 'd':
                if( dflag != 0 )
                    usage_exit(progname);
                dflag++ ;
                (void) strcpy(dtkdate, optarg) ;
                break;
            case 'k':
                if( kflag != 0 )
                    usage_exit(progname);
                kflag++ ;
                keep_flag = TRUE ;
                break;
            case 'l':
                if( lflag != 0 )
                    usage_exit(progname);
                lflag++ ;
                retcode = sscanf( optarg, "%d", &n_lookback_days ) ;
                if( retcode != 1 )
                {
                    (void) fprintf(stderr,
                    "%s(%d): error: %s :  n_lookback_days must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                break;
            case 'p':
                /* -p is not allowed with either -R or -S   */
                if(Rflag != 0)
                    usage_exit(progname);
                if(Sflag != 0)
                    usage_exit(progname);
                if(pflag != 0)
                    usage_exit(progname);
                pflag++;
                retcode = sscanf( optarg, "%d", &permission_id ) ;
                if( retcode != 1 )
                {
                    (void) fprintf(stderr,
                    "%s(%d): error: %s :  permission id must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                break;
            case 'S':
                /* -S is not allowed with -p   */
                if(pflag != 0)
                    usage_exit(progname);
                if(Sflag != 0)
                    usage_exit(progname);
                Sflag++;
                retcode = sscanf( optarg, "%d", &n_seconds_retry ) ;
                if( retcode != 1 )
                {
                    (void) fprintf(stderr,
           "%s(%d): error: %s :  seconds between re-tries must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                if ( n_seconds_retry <= 0 )
                {
                    (void) fprintf(stderr,
                  "%s(%d): error: %d :  seconds between re-tries must be > 0\n",
                        __FILE__, __LINE__, n_seconds_retry ) ;
                    usage_exit(progname);
                }
                break;
            case 'R':
                /* -R is not allowed with -p   */
                if(pflag != 0)
                    usage_exit(progname);
                if(Rflag != 0)
                    usage_exit(progname);
                Rflag++;
                retcode = sscanf( optarg, "%d", &n_retries ) ;
                if( retcode != 1 )
                {
                    (void) fprintf(stderr,
                 "%s(%d): error: %s :  number of re-tries must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                if( n_retries <= 0 )
                {
                    (void) fprintf(stderr,
                     "%s(%d): error: %d :  number of re-tries must be an > 0\n",
                        __FILE__, __LINE__, n_retries) ;
                    usage_exit(progname);
                }
                break;
            case '?':
                usage_exit(progname);
                break ;
            default:
                /* do nothing.  */
                break ;
        }

    /*
    -- The retry flag (-R) and the "delay in seconds" flag (-S) must be both 
    -- set or unset, otherwise there is an error in the retry logic.
    */
    if (Sflag != Rflag )
        usage_exit(progname);
 
    /*
    -- check for extraneous words not attached to
    -- any flag; no other argument is allowed.
    */
    if ( optind != argc )
        usage_exit(progname);

    if( Lflag )
    {
        /* 
        -- diagnostic print.  send log 
        -- file output to stdout for ease of 
        -- checking.  set filename to indicate this, too:  
        */
        (void) strcpy( log_filename, 
            "Log file is directed to standard output in this run." ) ;
        (void) tc_systime2asf( now_asftime ) ;
        logfp = stdout ;
        /* print to stdout for e-mail during cron job:  */
        (void) printf(
            "\nTHE LOG FILE IS SENT TO STANDARD OUTPUT for this run.\n\n") ;
    }
    else
    {
        /* -- start up the log file: */
        logfp = open_log_file( progname, log_filename, sizeof(log_filename),
            now_asftime, sizeof(now_asftime) ) ;
        /* print to stdout for e-mail during cron job:  */
        (void) printf("\nSEE THE LOG FILE for this run for details:\n%s\n\n",
            log_filename ) ;
        /*
        -- if there was an error opening the log file, the
        -- message was already printed by open_log_file().
        -- no need for more error messages.
        -- check for error:
        */
        if( (int)logfp == -1 )
        {
            (void) sprintf(msg, "%s:  log file could not be opened.\n", 
                progname ) ;
            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            error_exit(progname, NULL ) ;
        }
    }
    /* print the list-of-argument msg above into the log file:  */
    (void) fprintf( logfp, "%s\n\n", msg ) ;
    if( Nflag > 0 )
        (void) fprintf(logfp, "** IMS calls surpressed:  -N IMS\n" ) ;

    if ( Eflag )
    {
        (void) fprintf( logfp, 
            "Obtaining Sybase userid and password from encrypted file\n") ;
        /* 
        -- get the Sybase_password and Sybase_userid 
        -- from encrypted file.  
        -- expected string:  "userid,password"
        -- the comma (,) is the separater.  
        */
        /*
        -- path of password file:
        */
        APS_rootpath = aps_fullpath(APS_UPW, NULL) ;
        if ( APS_rootpath == NULL )
        {
            (void) sprintf( msg, 
                "directory path for APS_UPW not obtained by aps_fullpath()" ) ;
#ifdef RUN_FROM_GUI
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */

            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
        if (access(APS_rootpath, R_OK) != 0)
        {
            (void) sprintf( msg, 
                "program does not have R_OK access to directory %s",
                APS_rootpath ) ;
#ifdef RUN_FROM_GUI
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }

        user_pw_filename = (char *)malloc( sizeof(char)*(  
            strlen(APS_rootpath)+1 + 
            strlen(APS_IMS_DB_UPW_FILE)+1)   ) ;

        (void) strcpy(user_pw_filename, APS_rootpath ) ;
        (void) strcat(user_pw_filename, "/") ;
        (void) strcat(user_pw_filename, APS_IMS_DB_UPW_FILE ) ;
        (void) fprintf( logfp, "Encrypted file:  %s\n", user_pw_filename ) ;

        user_pw_string = get_APS_upass( user_pw_filename ) ;
        if ( user_pw_string == NULL )
        {
            (void) sprintf( msg, "info not obtainable from expected encrypted file %s",
                user_pw_filename ) ;
#ifdef RUN_FROM_GUI
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
        comma_ptr = strrchr( user_pw_string, ',' ) ;
        if ( comma_ptr == NULL )
        {
            (void) sprintf( msg, 
                "expected comma (,) not found in decrypted string from %s",
                user_pw_filename ) ;
#ifdef RUN_FROM_GUI
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }

        /*
        -- set the password pointer to just after 
        -- the comma.  
        -- then set the userid pointer to the start
        -- and terminate with a null character.  
        */
        password = comma_ptr + 1 ;
        sybase_userid = user_pw_string ;
        *comma_ptr = '\0' ;
        /* done with encryption.  */
        free( user_pw_filename ) ;
    }

    /* mandatory flags:  */
    if ( password == NULL )
    {
        (void) strcpy( msg, "Sybase_password not given." ) ;
#ifdef RUN_FROM_GUI
        (void) fprintf(stderr, "%s:\n\n%s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname, logfp ) ;
    }

    /*
    -- all of the values have been obtained from the
    -- command line.
    */
 
    /* optional flag    */
    if(sybase_userid == NULL)
    {
        /* 
        -- sybase_userid not supplied in command line 
        -- or encryption.   obtain from the environment: 
        */
        env_sybase_userid = getenv(APS_USERID_ENVVAR);
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            (void) sprintf( msg, 
            "ERROR:  sybase_userid not given; environment variable %s not set",
                APS_USERID_ENVVAR ) ;
#ifdef RUN_FROM_GUI
            (void) fprintf( stderr, "%s:\n\n%s\n", progname, msg );
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit(progname, logfp ) ;
        }
        else
            /* use the environment sybase_userid.   */
            sybase_userid = env_sybase_userid ;
    }
 
    if ( init_vec_lib() )
    {
        aps_log_msg(progname, APS_CRITICAL, "error initializing the Stoic file",
            DO_SYSLOG, DO_PRINT);
#ifdef RUN_FROM_GUI
        (void) fprintf(stderr, "%s:\n\nERROR initializing the Stoic file.\n", progname);
#endif /*   RUN_FROM_GUI    */
        (void) fprintf(stderr, "     check for existance, permissions, and \n" ) ;
        (void) fprintf(stderr, "     value of environment variable APS_DATA\n" ) ;
        error_exit(progname, logfp ) ;
    }

    /*
    -- check the other needed environment variables 
    -- for this job.  
    -- APSDB_ENVVAR, IMS_DB_ENVVAR,  and IMS_SERVER_ENVVAR.  
    */
    bad_env_count  = 0 ;
    if ( !env_is_set(progname, APSDB_ENVVAR ) )
        bad_env_count ++ ;
    if ( !env_is_set(progname, IMS_DB_ENVVAR ) )
        bad_env_count ++ ;
    if ( !env_is_set(progname, IMS_SERVER_ENVVAR ) )
        bad_env_count ++ ;
    if ( !env_is_set(progname, IMS_ACCOUNT_ENVVAR ) )
        bad_env_count ++ ;
    if ( bad_env_count )
    {
        (void) sprintf(msg, 
        "one or more environment variables not set:\n %s, %s, %s, %s\n",
            APSDB_ENVVAR, IMS_DB_ENVVAR, IMS_SERVER_ENVVAR, IMS_ACCOUNT_ENVVAR);

#ifdef RUN_FROM_GUI
        (void) fprintf( stderr, "%s:\n\n%s\n", progname, msg );
#endif /*   RUN_FROM_GUI    */
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, logfp ) ;
    }

    /* get the database name from the environment   */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
 
        aps_log_msg(progname, APS_CRITICAL,
        "ERROR:  environment variable APSDB not set; cannot open a database",
            DO_SYSLOG, DO_PRINT);
        error_exit (progname, logfp ) ;
    }
    dbname = env_dbname ;
 
    /* now open the database - we need to do this to get permissions  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open( dbname, progname, sybase_userid, password, NULL,
        error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        (void) sprintf( msg, "ERROR opening database '%s'", dbname ) ;
        db_open_errs(return_code, dbname, sybase_userid);
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname, logfp ) ;
    }
 
    /*
    -- Get permissions
    */
    
    retcode = mu_get_permission(
        progname, 
        APS_dbproc, 
        permission_id,
        MU_SINGLE_ACTIVITY_TYPE,
        MU_APS_STATISTICS,
        NULL, NULL, NULL, 0,   /* parameters not used */
        n_retries,
        n_seconds_retry ) ;
    if ( retcode < 0 )
        error_exit (progname, logfp ) ;
    
    /* permission was obtained; now proceed.  */
    if ( dflag == 0 )
    {
        /*
        -- get today's time.
        */
        tc_systime2asf( today_asftime );
        (void) strncpy( dtkdate, today_asftime, 8 ) ;
    }
    else
    {
        /* dtkdate was provided from -d on command line.  check it.  */
        (void) strncpy( today_asftime, dtkdate, 8 ) ;
        return_code = tc_validate_asf_datetime(today_asftime) ;
        if ( return_code != TRUE )
        {
            /*   
            -- aps_stats_ims_info->logfile_ptr, is not yet set.  
            -- use logfp.  
            */
            (void) fprintf(logfp, 
                "%s:\n\nerror in input dtkdate, %s\n", progname,
                dtkdate ) ;
            aps_log_msg(progname, APS_ERROR, "error in input -d dtkdate",
                DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
    }

    /*****************************************************************
    *                                                                *
    *  Put info about IMS in structure.         9 values.            *
    *  info on APS_STATS_IMS_INFO structure is in aps_Statistics.h   *
    *  in current directory.                                         *
    *                                                                *
    *****************************************************************/

    aps_stats_ims_info = malloc(sizeof(APS_STATS_IMS_INFO)) ;

    (void) strcpy( aps_stats_ims_info->username, sybase_userid ) ;

    (void) strcpy( aps_stats_ims_info->password, password ) ;

    env_value = getenv(IMS_ACCOUNT_ENVVAR);
    (void) strcpy( aps_stats_ims_info->accountId, env_value ) ;

    pmf_directory = aps_fullpath( APS_TEMP, NULL ) ;
    (void) strcpy( aps_stats_ims_info->sourceDir, pmf_directory ) ;
    free(pmf_directory) ;

    (void) strcpy( aps_stats_ims_info->programName, progname ) ;

    env_value = getenv(IMS_SERVER_ENVVAR);
    (void) strcpy( aps_stats_ims_info->catSrvName, env_value ) ;

    env_value = getenv(IMS_DB_ENVVAR);
    (void) strcpy( aps_stats_ims_info->catDbName, env_value ) ;

    /* 
    -- set ims_call_flag to TRUE when Nflag = 0, 
    -- FALSE when Nflag = 1.  Using -N IMS will prevent 
    -- any actual calls to the IMS.  Used during installation 
    -- to prevent old activities from being reported and still 
    -- allowing the operational daily run.  When the flag is used, 
    -- activities will be stored in the stats_calls relation to 
    -- prevent calls during later, normal, operations.  
    */
    aps_stats_ims_info->ims_call_flag = 1 - Nflag ;

    aps_stats_ims_info->logfile_ptr = logfp ;
    /*****************************************************************
    *                                                                *
    *  completed loading data into APS_STATS_IMS_INFO structure      *
    *                                                                *
    *****************************************************************/

    return_code = aps_Statistics( APS_dbproc, aps_stats_ims_info, 
        keep_flag, dtkdate, n_lookback_days, &IMS_error_count, 
        &IMS_fatal_count ) ;
    free( aps_stats_ims_info ) ;
    if( IMS_error_count > 0 )
    {
        (void)sprintf( msg,
            "Count of IMS errors:  %d; must check log file and re-run", 
            IMS_error_count ) ;
        (void)fprintf(logfp,
            "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
    }

    if( IMS_fatal_count > 0 )
    {
        (void)sprintf( msg,
            "Count of Fatal IMS errors:  %d; must check log file and re-run", 
            IMS_fatal_count ) ;
        (void)fprintf(logfp,
            "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
    }

    if ( return_code < 0 )
    {
        /* 
        -- an error occurred in which 
        -- the routine could not finish.  
        */
        (void)sprintf(msg,"%s", STATS_ERROR_MESSAGE(return_code) ) ;
        (void)fprintf(logfp,
            "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);

        error_exit(progname, logfp  ) ;
    }

    normal_exit(progname, logfp, return_code ) ;

} /* main */

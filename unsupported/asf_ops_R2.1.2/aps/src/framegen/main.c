#define LOGFILE_2_STDOUT

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    Main driver for APS frame generation; intended to be run 
                as a cron job daily just after 00 hours system time.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.main.c"

#include <stdio.h>
#include <mu_utilities.h>
#include "aps_encrypt.h"    /* for aps_encrypt()                        */
#include "aps_framegen.h"   /* for aps_framegen()         */
#include "apspath.h"        /* for aps_fullpath - APS_UPW etc.          */


#include <open_log_file.h>  /* for LOG_FILE_NAME_MIN_SIZE, open_log_file()  */

/* static GLOBAL variable for use in messages:  */
static char        msg[MSG_LEN];

static void normal_exit(char *progname, FILE *logfp, int error_count)
{
    if( error_count == 0 )
    {
        (void)sprintf(msg, "Program completed successfully." ) ;
        (void)fprintf(logfp, "\n%s:  APS_INFO:  %s\n", progname, msg ) ;
        aps_log_msg(progname, APS_INFO,  msg, DO_SYSLOG, DO_PRINT);
        exit(APS_EXIT_OK) ;
    }
    if( error_count == 1 )
    {
        (void)sprintf(msg, 
"Program completed normally, but there was 1 call with an error." ) ;
        (void)fprintf(logfp, "\n%s:  APS_INFO:  %s\n", progname, msg ) ;
        aps_log_msg( progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT );
        exit(APS_EXIT_OK) ;
    }
    if( error_count > 1 )
    {
        (void)sprintf(msg, 
"Program completed normally, but there were %d calls with errors.",
            error_count ) ;
        (void)fprintf(logfp, "\n%s:  APS_INFO:  %s\n", progname, msg ) ;
        aps_log_msg( progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT );
        exit(APS_EXIT_OK) ;
    }
}

static void error_exit(char *progname, FILE *logfp ) 
{
    if( (int) logfp != NULL )
        (void)fprintf(logfp, "\n%s:  APS_INFO:  Program terminated abnormally.\n", 
            progname ) ;
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_ERROR) ;
}
static void usage_exit(char *progname)
{
    (void)fprintf(stderr, 
    "\nusage: %s { -E | [-U Sybase_userid] -P Sybase_password } [-d dtkdate]\n", progname );
    (void)fprintf(stderr, 
    "       [ -p <permission id> | \n");
    (void)fprintf(stderr, 
    "       { -R <number of retries> -S <seconds between retries> } ]\n");
    (void)fprintf(stderr, 
    "\n    This program retrieves data-takes with yesterday's date stamp.\n" ) ;
    (void)fprintf(stderr,
    "    and reports information to the IMS via an application \n" ) ;
    (void)fprintf(stderr,
    "    programming interface. \n" ) ;
    (void)fprintf(stderr,
    "    It reports sat/sensor/rev, time bracket, and darid for each PLN,\n" ) ;
    (void)fprintf(stderr,
    "    SCH, DEL, and REJ data-take, with previously called times as \n" ) ;
    (void)fprintf(stderr,
    "    needed.  For each SCH data-take combined from PLN data-takes,\n" ) ;
    (void)fprintf(stderr,
    "    it reports the previous PLN time brackets along with the SCH \n" ) ;
    (void)fprintf(stderr,
    "    data-take.\n" ) ;

    (void)fprintf(stderr,
    "\n    This program is intended to be run in a Cron job just after\n") ; 
    (void)fprintf(stderr,
    "    00:00 hours daily, which will use encryption to obtain the userid\n") ;
    (void)fprintf(stderr,
    "    and password.  \n") ;
    (void)fprintf(stderr,
    "\n    -E                Use ecrypted, previously saved files to \n" ) ;
    (void)fprintf(stderr,
    "                      obtain Sybase_userid and Sybase_password.\n" ) ;
    (void)fprintf(stderr,
    "    Sybase_userid     Sybase account userid for the APS database\n" ) ;
    (void)fprintf(stderr,
    "    Sybase_password   Sybase account password for the APS database\n" ) ;
    (void)fprintf(stderr,
    "    dtkdate           date in yyyy:ddd format.  The program will \n"  ) ;
    (void)fprintf(stderr,
    "                      retrieve data-takes with a date stamp one day \n") ;
    (void)fprintf(stderr,
    "                      before this date.  Normally, you do not provide\n") ;
    (void)fprintf(stderr,
    "                      this date; today's date is obtained and used\n" ) ;
    (void)fprintf(stderr,
    "                      as the default.\n"  ) ; 
    (void)fprintf(stderr,
    "    <permission id>   An integer indicating an existing permission.\n" ) ; 
    (void)fprintf(stderr,
    "                      If not provided, -R can be used.\n" ) ;
    (void)fprintf(stderr,
    "\n    <number of retries>     An integer indicating the number of extra\n" ) ; 
    (void)fprintf(stderr,
    "                            attempts to get permission if the first\n" ) ;
    (void)fprintf(stderr,
    "                            attempt fails.  -S is required with this\n" ) ;
    (void)fprintf(stderr,
    "                            parameter.\n" ) ;
    (void)fprintf(stderr,
    "\n    <seconds between retries>     An integer indicating the amount of time\n" ) ;
    (void)fprintf(stderr,
    "                                  in seconds between permission attempts.\n" ) ;

    (void)fprintf(stderr, "\n\n    %s  Version compiled:  %s %s\n\n",
        progname, __DATE__, __TIME__ ) ;
    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    error_exit(progname, (FILE *)NULL) ;
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
        (void)sprintf(msg, "environment variable:  %s is not set.", env_name);
        (void)fprintf( stderr, "%s:\n\n%s\n", progname, msg );
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

Description:    main driver for APS Frame Generation.  

Creator:        Lawrence Stevens

Creation Date:  Tue Feb 27 11:55:02 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
void main (int argc, char *argv[])
{
 
    DBPROCESS   *APS_dbproc ;
    char        *dbname = NULL ;
    char        *sybase_userid = NULL ;
    char        *password = NULL ;
    int         Eflag = 0 ;
    char        *APS_rootpath = NULL ;
    char        *user_pw_filename = NULL ;
    char        *comma_ptr ;
    char        *user_pw_string ;

    char        log_filename[LOG_FILE_NAME_MIN_SIZE] ;
    char        now_asftime[ASF_TIME_STR_LENGTH+1] ;

    FILE        *logfp ;      /* log file pointer.  */

    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;
 
    char        *env_dbname;        /* dbname from environment      */
    char        *env_sybase_userid; /* userid from environment      */
 
    int         c;      /* used as return character from getopt()       */
 
    char        flag_list[20] = "U:P:d:ER:S:p:"; /* list of flags for getopt  */
 
    /* for MU permission */
    int     pflag = 0;  /* used to check for optional permission_id          */
    int     Sflag = 0;  /* used to check for optional n_seconds_retry        */
    int     Rflag = 0;  /* used to check for optional n_retries              */
    int     permission_id   = 0 ;  /* optional permission_id passed     */
    int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
    int     n_retries       = 0 ;  /* optional number of re-tries       */

    int         bad_env_count  = 0 ;
    char        *progname ;
    char        *slash_ptr ;

    char        today_asftime[] = "yyyy:ddd:12:00:00.000" ;
    char        today_dtkdate[] = "yyyy:ddd" ;
    char        *dtkdate = NULL ;
 
    int         j ;
    
    int         retcode;
    RETCODE     return_code;

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
    (void)sprintf(msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        (void)strcat(msg, " " ) ;
        (void)strcat(msg, argv[j] ) ;
    }
    aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

    if ( argc < 2 )
        usage_exit(progname);

    /* 
    -- start up the log file:  
    */
    logfp = open_log_file( progname, 
        log_filename, sizeof(log_filename), 
        now_asftime, sizeof(now_asftime) ) ;

    /* 
    -- if there was an error opening the log file, the 
    -- message was already printed by open_log_file(). 
    -- no need for more error messages.
    -- check for error:  
    */
    if( (int)logfp == -1 )
    {
        (void)sprintf(msg, "%s:  log file could not be opened.\n", progname ) ;
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, NULL) ;
    }

#ifdef LOGFILE_2_STDOUT
	logfp = stdout ;
#endif    /*  LOGFILE_2_STDOUT    */

    /* print to stdout for e-mail during cron job:  */
    (void)printf("\nSEE THE LOG FILE for this run for details:\n%s\n\n", 
        log_filename ) ;

    /* print the msg above into the log file:  */
    (void)fprintf( logfp, "%s\n\n", msg ) ;

    /*
    -- The log file will now contain anything non-brief.  
    -- the reporting methods now are:
    -- log file:  all messages, including start, end, errors, extra info.  
    -- sysout and syserr:  goes to e-mail, as this job is usually a cronjob.
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
            case 'P':
                if( password != NULL )
                    usage_exit(progname);
                password = optarg ;
                break;
            case 'U':
                if( sybase_userid != NULL )
                    usage_exit(progname);
                sybase_userid = optarg ;
                break;
            case 'd':
                if(dtkdate != NULL)
                    usage_exit(progname);
                dtkdate = optarg ;
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
                    (void)fprintf(stderr,
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
                    (void)fprintf(stderr,
                            "%s(%d): error: %s :  seconds between re-tries must be an integer\n",
                            __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                if ( n_seconds_retry <= 0 )
                {
                    (void)fprintf(stderr,
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
                    (void)fprintf(stderr,
                            "%s(%d): error: %s :  number of re-tries must be an integer\n",
                            __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                if( n_retries <= 0 )
                {
                    (void)fprintf(stderr,
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

    if ( Eflag )
    {
        (void)fprintf(logfp,"Obtaining Sybase userid and password from encrypted file.\n");
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
            (void)sprintf( msg,
                "directory path for APS_UPW not obtained by aps_fullpath()" ) ;
            (void)fprintf(logfp, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
        if (access(APS_rootpath, R_OK) != 0)
        {
            (void)sprintf( msg,
                "program does not have R_OK access to directory %s",
                APS_rootpath ) ;
            (void)fprintf(logfp, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
 
        user_pw_filename = (char *)malloc( sizeof(char)*(
            strlen(APS_rootpath)+1 +
            strlen(APS_IMS_DB_UPW_FILE)+1)   ) ;
 
        (void)strcpy(user_pw_filename, APS_rootpath ) ;
        (void)strcat(user_pw_filename, "/") ;
        (void)strcat(user_pw_filename, APS_IMS_DB_UPW_FILE ) ;
 
        user_pw_string = get_APS_upass( user_pw_filename ) ;
        if ( user_pw_string == NULL )
        {
            (void)sprintf( msg, "info not obtainable from expected encrypted file %s",
                user_pw_filename ) ;
            (void)fprintf(logfp, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
        comma_ptr = strrchr( user_pw_string, ',' ) ;
        if ( comma_ptr == NULL )
        {
            (void)sprintf( msg,
                "expected comma (,) not found in decrypted string from %s",
                user_pw_filename ) ;
            (void)fprintf(logfp, "%s:\n\nERROR:  %s\n", progname, msg ) ;
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
        (void)fprintf(logfp, "%s:\n\nSybase_password not given.  Run terminating.\n", 
            progname ) ;
        aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
        usage_exit(progname) ;
    }

    if ( dtkdate == NULL ) 
    {
        /*
        -- get today's time.  
        */
        tc_systime2asf( today_asftime );
    }
    else
    {
        /* dtkdate was provided from -d on command line.  check it.  */
        (void)strncpy( today_asftime, dtkdate, 8 ) ;
        return_code = tc_validate_asf_datetime(today_asftime) ;
        if ( return_code != TRUE )
        {
            (void)fprintf(logfp,"%s:\n\nerror in input dtkdate, %s\n", progname, 
                dtkdate ) ;
            aps_log_msg(progname, APS_ERROR, "error in input -d dtkdate",
                DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
        }
    }

    /* use the date that we figured on, above:  */
    (void)strncpy( today_dtkdate, today_asftime, 8 ) ;
 
    /*
    -- all of the values have been obtained from the
    -- command line.
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
            (void)fprintf(logfp,
"%s:\n\nERROR:  sybase_userid not given; \n        environment variable APS_SYBASE_USERID not set\n",
                progname);
            aps_log_msg(progname, APS_CRITICAL,
"ERROR:  sybase_userid not given; environment variable APS_SYBASE_USERID not set", DO_SYSLOG, DO_PRINT);
            error_exit (progname, logfp ) ;
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
        (void)fprintf(logfp,
"%s:\n\nERROR:  environment variable APSDB not set; cannot open a database\n",
            progname ) ;
        (void)fprintf(logfp, "    Use setenv APSDB <dbname>. \n");
 
        aps_log_msg(progname, APS_CRITICAL,
        "ERROR:  environment variable APSDB not set; cannot open a database",
            DO_SYSLOG, DO_PRINT);
        error_exit (progname, logfp ) ;
    }
    dbname = env_dbname ;
 
    /* now open the database.  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open( dbname, progname, sybase_userid, password, NULL,
        error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        (void)sprintf( msg, "ERROR opening database '%s'", dbname ) ;
        (void)fprintf( logfp, "%s:\n\n%s\n", progname, msg ) ;
        db_open_errs(return_code, dbname, sybase_userid);
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname, logfp ) ;
    }

    /*
    -- Get permissions
    */

    retcode = mu_get_permission(progname, 
                                APS_dbproc, 
                                permission_id,
                                MU_SINGLE_ACTIVITY_TYPE,
                                MU_FRAME_GENERATION,
                                NULL, NULL, NULL, 0,     /* parameters not used        */
                                n_retries,
                                n_seconds_retry ) ;
    if ( retcode < 0 )
        error_exit (progname, logfp ) ;

    permission_id = retcode;

    /* permission was obtained; now proceed.  */

    /*
    -- check the needed environment variables 
    -- for this job.  
    -- APSDB_ENVVAR, IMS_DB_ENVVAR,  and IMS_SERVER_ENVVAR.  
    */
    bad_env_count  = 0 ;
    if ( !env_is_set(progname, APSDB_ENVVAR ) )
    {
        (void)sprintf(msg, "%s environment variable is not set.", APSDB_ENVVAR );
        (void)fprintf( logfp, "%s:\n\nERROR:  %s\n", progname, msg );
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        bad_env_count ++ ;
    }
    if ( !env_is_set(progname, IMS_DB_ENVVAR ) )
    {
        (void)sprintf(msg, "%s environment variable is not set.", IMS_DB_ENVVAR );
        (void)fprintf( logfp, "%s:\n\nERROR:  %s\n", progname, msg );
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        bad_env_count ++ ;
    }
    if ( !env_is_set(progname, IMS_SERVER_ENVVAR ) )
    {
        (void)sprintf(msg, "%s environment variable is not set.", IMS_SERVER_ENVVAR );
        (void)fprintf( logfp, "%s:\n\nERROR:  %s\n", progname, msg );
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        bad_env_count ++ ;
    }
    if ( bad_env_count )
    {
        (void)sprintf(msg, "one or more environment variables not set." );
        (void)fprintf( logfp, "%s:\n\n%s\n", progname, msg );
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname, logfp ) ;
    }

    return_code = aps_framegen( progname, logfp, APS_dbproc, today_dtkdate ) ;

    if ( return_code < 0 )
    {
        /* 
        -- an error occurred in which 
        -- the routine could not finish.  
        */
        (void)sprintf(msg,"%s", APS_FRAMEGEN_ERROR_MESSAGE(return_code) ) ;
        (void)fprintf(logfp,
            "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname, logfp) ;
    }

    normal_exit(progname, logfp, return_code) ;

} /* main */

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    Main driver for aps_coverage_window; maintains the planning 
                window for create_nominal_coverage runs.  
                runs from a C-shell script, which sets up env's, in a 
                cron job daily at low CPU usage times.  

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_coverage_window/SCCS/s.main.c"

int 
init_vec_lib() ;

#include <stdio.h>
#include "db_sybint.h"      /* for APS sybase interface routines.       */
#include "aps_db_table.h"   /* for CVRGMDAT, CVRG, etc. - accessing dtk table.*/
#include "aps_defs.h"       /* for APSDB_ENVVAR and APS_USERID_ENVVAR   */
#include <string.h>         /* for strrchr()                            */
#include <stdlib.h>         /* for getenv()                             */
#include <unistd.h>         /* for access()                             */
#include <aps_encrypt.h>    /* for aps_encrypt()                        */

#include "apspath.h"        /* for aps_fullpath         */

#include "aps_coverage_window.h"          /* for aps_coverage_window         */

/* GLOBAL variable for use in messages:  */
static char        msg[MSG_LEN+1];

static 
void normal_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_OK) ;
}

static void 
error_exit(char *progname) 
{
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_ERROR) ;
}
static 
void usage_exit(char *progname)
{
    (void) fprintf(stderr, 
        "\nusage: %s {-E | [-U Sybase_userid] -P Sybase_password }\n",
        progname);

    (void) fprintf(stderr, 
        "     [ -R number_of_Re-tries -S Seconds_between_re-tries ]\n");

    (void) fprintf(stderr, 
"     [-a adays] [-b bdays] [-d dminutes] [-m mdays] [-w wdays] [-g gdays]\n");

    (void) fprintf(stderr, 
"This program reads from the cvrgmdat relation to help maintain the cvrg and\n");
    (void) fprintf(stderr, 
"maskinout relations in the APS planning window:\n");
    (void) fprintf(stderr, 
"1.  It deletes old records from the cvrg and cvrgmdat relations which are \n");
    (void) fprintf(stderr, 
"    Aged more than adays AND which pertain to a time bdays Back from the \n");
    (void) fprintf(stderr, 
"    present time.\n");

    (void) fprintf(stderr, 
"2.  It deletes records from the Maskinout relation pertaining to times\n");
    (void) fprintf(stderr, 
"    mdays before the present.\n");
    (void) fprintf(stderr, 

"3.  It writes create nominal coverage commands to a C-shell script.  These\n");
    (void) fprintf(stderr, 
"    commands, for each sat/sensor, will fill out the needed planning \n");
    (void) fprintf(stderr, 
"    window until the next window run is expected.  The time bracket is:\n");
    (void) fprintf(stderr, 
"    start time = max (previous cvrgmdat stop times for sensor) - 2 minutes\n");
    (void) fprintf(stderr, 
"    stop time  = the present time + wdays + gdays\n");
    (void) fprintf(stderr, 
"    This C-shell script is submitted at the end of this run using: \n" ) ;
    (void) fprintf(stderr, 
"    'at now + <dminutes> min'.  See below for dminutes description.\n\n" ) ;

    (void) fprintf(stderr, 
"Seconds_between_re-tries  Wait time between Multi-user permission requests\n");
    (void) fprintf(stderr, 
"number_of_Re-tries        Number of permission requests to re-try\n\n");

    (void) fprintf(stderr, 
"-E        Obtain Sybase_userid and Sybase_password via Encryption \n" ) ;
    (void) fprintf(stderr, "          (see APS_encrypt)\n");
    (void) fprintf(stderr, 
"adays     Age of cvrg data to delete.  The age of cvrg data is from the\n");
    (void) fprintf(stderr, 
"          time of the run to the present time.  Default = 14\n");
    (void) fprintf(stderr, 
"bdays     number of days Before the present such that cvrg data pertaining to\n");
    (void) fprintf(stderr, 
"          it is to be deleted.  Default = 14\n");
    (void) fprintf(stderr, 
"dminutes  Delay in minutes on submitting the C-shell script in Step 3.  \n");
    (void) fprintf(stderr, 
"          Default = 2\n");
    (void) fprintf(stderr, 
"mdays     Maskinout data pertaining to before the present time - mdays is to\n");
    (void) fprintf(stderr, 
"          be deleted.  If mdays > 10000, no maskinout data is deleted.\n");
    (void) fprintf(stderr, 
"          Default = 365\n");
    (void) fprintf(stderr, 
"wdays     number of days in the planning Window, starting at the present.\n");
    (void) fprintf(stderr, 
"          Default = 180\n");
    (void) fprintf(stderr, 
"gdays     the number of days that the window is to be Good for.  Default = 1\n");
    (void) fprintf(stderr, 
"          gdays must also be the frequency that aps_coverage_window is run.\n");

    (void) fprintf(stderr, "\n    %s  Version compiled:  %s %s\n",
        progname, __DATE__, __TIME__ ) ;
    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    error_exit(progname) ;
}
static int 
env_is_set( 
    char *env_name,
    char **env_value )  /* char pointer, you must free() the storage.  */
{

    /*  returning the pointer value to the calling routine.  */
    *env_value = getenv(env_name) ;
    if ( env_value == NULL)
        return FALSE ;
    else
        return TRUE ;
}


/*==============================================================================
Function:       main.c

Description:    main driver for aps_coverage_window.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun  5 09:15:20 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
void main (int argc, char *argv[])
{
 
    DBPROCESS   *APS_dbproc ;
    char        *user_pw_filename = NULL ;
    char        *APS_rootpath = NULL ;
    char        *sybase_userid = NULL ;
    char        *sybase_password = NULL ;
    int         Eflag = 0 ;
    char        *progname ;
    char        *slash_ptr ;
    char        *comma_ptr ;
    char        *user_pw_string ;

    /* to help build the log file name and open it:  */
    FILE        *logfp ;
    char        *aps_fullpath_log_filename ; 
    char        log_filename[100] ; 
    char        now_asftime[ASF_TIME_STR_LENGTH+1] ;
    int         year, yy, doy, hour, min, sec, msec ;

    int         j ;
 
    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;
 
    RETCODE     return_code;
 
    char        *env_dbname = NULL ;  /* dbname from environment      */
    char        *env_sybase_userid ;  /* userid from environment      */
 
    int         c;      /* used as return character from getopt()       */
 
    /*   list of getopt() flags        */
    char        flag_list[] = "S:R:U:P:Ea:b:d:m:w:g:"; 

    int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
    int     n_retries       = 0 ;  /* optional number of re-tries       */

    int         adays = 0 ;
    int         adays_default = 14 ;
    int         bdays = 0 ;
    int         bdays_default = 14 ;
    int         dminutes = 0 ;
    int         dminutes_default = 2 ;
    int         mdays = 0 ;
    int         mdays_default = 365 ;
    int         wdays = 0 ;
    int         wdays_default = 180 ;
    int         gdays = 0 ;
    int         gdays_default = 1 ;
 
    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    /* 
    -- get progname as file name without 
    -- the path.  
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

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'S':
                if(n_seconds_retry != 0)
                    usage_exit(progname);
                return_code = sscanf( optarg, "%d", &n_seconds_retry ) ;
                if( return_code != 1 )
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
                if(n_retries != 0)
                    usage_exit(progname);
                return_code = sscanf( optarg, "%d", &n_retries ) ;
                if( return_code != 1 )
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
            case 'E':
                if( Eflag != 0 )
                    usage_exit(progname);
                if( sybase_password != NULL )
                    usage_exit(progname);
                if( sybase_userid != NULL )
                    usage_exit(progname);
                Eflag++ ;
                break;
            case 'P':
                if( Eflag != 0 )
                    usage_exit(progname);
                if( sybase_password != NULL )
                    usage_exit(progname);
                sybase_password = optarg ;
                break;
            case 'U':
                if( Eflag != 0 )
                    usage_exit(progname);
                if( sybase_userid != NULL )
                    usage_exit(progname);
                sybase_userid = optarg ;
                break;
            case 'a':
                if( adays != 0 )
                    usage_exit(progname);
                return_code = sscanf(optarg, "%d", &adays ) ;
                if( return_code != 1 || adays <= 0 )
                {
                    (void) fprintf(stderr, 
                        "%s:  error:  adays value %s was not an integer > 0\n",
                        progname, optarg ) ;
                    usage_exit(progname);
                }
                break;
            case 'b':
                if( bdays != 0 )
                    usage_exit(progname);
                return_code = sscanf(optarg, "%d", &bdays ) ;
                if( return_code != 1 || bdays <= 0 )
                {
                    (void) fprintf(stderr, 
                        "%s:  error:  bdays value %s was not an integer > 0\n",
                        progname, optarg ) ;
                    usage_exit(progname);
                }
                break;
            case 'd':
                if( dminutes != 0 )
                    usage_exit(progname);
                return_code = sscanf(optarg, "%d", &dminutes ) ;
                if( return_code != 1 || dminutes <= 0 )
                {
                    (void) fprintf(stderr, 
                    "%s:  error:  dminutes value %s was not an integer > 0\n",
                        progname, optarg ) ;
                    usage_exit(progname);
                }
                break;
            case 'm':
                if( mdays != 0 )
                    usage_exit(progname);
                return_code = sscanf(optarg, "%d", &mdays ) ;
                if( return_code != 1 || mdays <= 0 )
                {
                    (void) fprintf(stderr, 
                        "%s:  error:  mdays value %s was not an integer > 0\n",
                        progname, optarg ) ;
                    usage_exit(progname);
                }
                break;
            case 'w':
                if( wdays != 0 )
                    usage_exit(progname);
                return_code = sscanf(optarg, "%d", &wdays ) ;
                if( return_code != 1 || wdays <= 0 )
                {
                    (void) fprintf(stderr, 
                        "%s:  error:  wdays value %s was not an integer > 0\n",
                        progname, optarg ) ;
                    usage_exit(progname);
                }
                break;
            case 'g':
                if( gdays != 0 )
                    usage_exit(progname);
                return_code = sscanf(optarg, "%d", &gdays ) ;
                if( return_code != 1 || gdays <= 0 )
                {
                    (void) fprintf(stderr, 
                        "%s:  error:  gdays value %s was not an integer > 0\n",
                        progname, optarg ) ;
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
    -- check for extraneous words not attached to
    -- any flag; no other argument is allowed.
    */
    if ( optind != argc )
        usage_exit(progname);

    /*
    -- check for n_retries without n_seconds_retry 
    -- and vice-versa.  
    */
    if ( n_retries >  0 && n_seconds_retry <= 0
    ||   n_retries <= 0 && n_seconds_retry >  0  )
    {
        (void) strcpy( msg, 
        "number_of_Re-tries and Seconds_between_re-tries must both be given" ) ;
        (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname) ;
    }

    if ( Eflag )
    {
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
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }
        if (access(APS_rootpath, R_OK) != 0)
        {
            (void) sprintf( msg, 
                "program does not have R_OK access to directory %s",
                APS_rootpath ) ;
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }

        user_pw_filename = (char *)malloc( sizeof(char)*(  
            strlen(APS_rootpath)+1 + 
            strlen(APS_IMS_DB_UPW_FILE)+1)   ) ;

        (void) strcpy(user_pw_filename, APS_rootpath ) ;
        (void) strcat(user_pw_filename, "/") ;
        (void) strcat(user_pw_filename, APS_IMS_DB_UPW_FILE ) ;

        user_pw_string = get_APS_upass( user_pw_filename ) ;
        if ( user_pw_string == NULL )
        {
            (void) sprintf( msg, 
                "info not obtainable from expected encrypted file %s",
                user_pw_filename ) ;
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }
        comma_ptr = strrchr( user_pw_string, ',' ) ;
        if ( comma_ptr == NULL )
        {
            (void) sprintf( msg, 
                "expected comma (,) not found in decrypted string from %s",
                user_pw_filename ) ;
            (void) fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }

        /*
        -- set the password pointer to just after 
        -- the comma.  
        -- then set the userid pointer to the start
        -- and terminate with a null character.  
        */
        sybase_password = comma_ptr + 1 ;
        sybase_userid = user_pw_string ;
        *comma_ptr = '\0' ;
        /* done with encryption.  */
        free( user_pw_filename ) ;
    }

    /* mandatory flags:  */
    if ( sybase_password == NULL )
    {
        (void) strcpy( msg, "Sybase_password not given." ) ;
        (void) fprintf(stderr, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname) ;
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
            (void) fprintf( stderr, "%s:\n\n%s\n", progname, msg );
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit(progname) ;
        }
        else
            /* use the environment sybase_userid.   */
            sybase_userid = env_sybase_userid ;
    }
 
    if ( init_vec_lib() )
    {
        (void) fprintf(stderr, 
            "%s:\n\nERROR initializing the Stoic file.\n", progname);
        (void) fprintf(stderr, 
            "     check for existance, permissions, and \n" ) ;
        (void) fprintf(stderr, 
            "     value of environment variable APS_DATA\n" ) ;
        aps_log_msg(progname, APS_CRITICAL, "error initializing the Stoic file",
            DO_SYSLOG, DO_PRINT);
        error_exit(progname) ;
    }

    /*
    -- check the other needed environment variables 
    -- for this job.  
    -- APSDB_ENVVAR 
    */
    if ( !env_is_set( APSDB_ENVVAR, &env_dbname ) )
    {
        (void) sprintf(msg, "ERROR:  environment variable %s not set.", 
            APSDB_ENVVAR ) ;
        (void) fprintf( stderr, "%s:\n\n%s\n", progname, msg );
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname ) ;
    }

    /* 
    -- open the database:  
    -- db_open() will handle the errors.      
    */
    APS_dbproc = db_open( env_dbname, progname, sybase_userid, 
        sybase_password,NULL, error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        db_open_errs( return_code, env_dbname, sybase_userid);
        (void) sprintf( msg, "ERROR:  dbname %s could not be opened", 
            env_dbname ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        aps_log_msg( progname, APS_INFO,  "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
        error_exit(progname) ;
    }

    /*
    -- open the log file for this 
    -- run.  
    */
    /* get system time in asf format */
    tc_systime2asf(now_asftime) ;
 
    /* check asf time */
    if (!tc_parse_asftime(now_asftime,
            &year, &yy, &doy, &hour, &min, &sec, &msec))
    {
        (void) sprintf(msg, 
            "ERROR: Unable to create log filename from ASF time %s",
            now_asftime ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        aps_log_msg( progname, APS_INFO,  "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
        error_exit(progname) ;
        return ;
    }
 
    /* make file name (aps_coverage_window.yyyyddd_hhmmss.LOG) */
 
    (void)sprintf(log_filename,
        "%s.%04d%03d_%02d%02d%02d.LOG",
        progname, year, doy, hour, min, sec) ;
 
    aps_fullpath_log_filename = aps_fullpath(APS_LOGS, log_filename) ;
 
    /*  Open the file  */
    if (  (logfp = fopen(aps_fullpath_log_filename, "a")) == NULL  )
    {
        (void)sprintf(msg, 
        "Unable to open log file %s; check for permissions or full disk.",
            aps_fullpath_log_filename ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        aps_log_msg( progname, APS_INFO,  "Program terminated abnormally.",
            DO_SYSLOG, DO_PRINT);
        error_exit(progname) ;
    }

    /*
    -- start the log file with some info:
    */
    (void) fprintf(logfp, "LOG FILE %s\n", aps_fullpath_log_filename ) ;
    (void) fprintf(logfp, "created %s\n\n", now_asftime ) ;

    /* print the command and arguments:  */
    (void) fprintf(logfp, "%s\n\n", msg ) ;
    if ( Eflag )
        (void) fprintf( logfp, 
            "Obtained Sybase userid and password from encrypted file.\n") ;

    /* print info to standard output for the cronjob email: */
    (void) printf("\nNOTE:\n" ) ;
    (void) printf("See the COVERAGE WINDOW LOG FILE:\n%s\n", 
        aps_fullpath_log_filename ) ;
    (void) printf("for determination of Create Nominal Coverage parameters\n");
    (void) printf("and deletion of old data.\n\n" ) ;

    free(aps_fullpath_log_filename) ;


    /* if not assigned, then use default values:  */

    if ( adays == 0 )
        adays = adays_default ;
    if ( bdays == 0 )
        bdays = bdays_default ;
    if ( dminutes == 0 )
        dminutes = dminutes_default ;
    if ( gdays == 0 )
        gdays = gdays_default ;
    if ( mdays == 0 )
        mdays = mdays_default ;
    if ( wdays == 0 )
        wdays = wdays_default ;

    /* 
    -- arguments obtained from the command line, encryption, 
    -- env, and defaults.  
    -- possible error here, if the arguments are in the wrong 
    -- order.  Note that the '_days' are in alphabetical order.  
    */
    /* 
    -- opportunity, in debugger, to switch the log file to stdout
    -- to make it easy to follow the program.  
    -- It can be hard to obtain the log file without ending 
    -- the program.  Also, I tried to assign logfp the stdout 
    -- value, but debugger did not allow this.  
    -- When you are in debugger and get here, 
    -- you 'assign return_code = 1234567' to switch the 
    -- log file output to your debugger output window.  
    */
    if ( return_code == 1234567 )
        logfp = stdout ;
    return_code = aps_coverage_window( progname, APS_dbproc, 
        sybase_userid, sybase_password,
        n_retries, n_seconds_retry,
        now_asftime, adays, bdays, dminutes, gdays, mdays, wdays, logfp ) ;

    if ( return_code != APS_EXIT_OK )
    {
        /* 
        -- an error occurred in which 
        -- the routine could not finish.  
        */
        (void) fprintf(logfp, "\nERROR EXIT\n" ) ;
        error_exit(progname ) ;
    }

    (void) fprintf(logfp, "\nNORMAL END\n" ) ;
    normal_exit(progname ) ;

} /* main */

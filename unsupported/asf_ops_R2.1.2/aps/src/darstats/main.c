#undef RUN_FROM_GUI
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    Main driver for Dar statistics; intended to be run 
                as a cron job daily just after 00 hours system time.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          This file was written with a 4-character tab setting.
                If you don't use 4-character tabs, it will look funny.
                Use set tabstop=4 in vi to browse.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/darstats/SCCS/s.main.c"

#include <stdio.h>
#include "db_sybint.h"      /* for APS sybase interface routines.       */
#include "aps_db_table.h"   /* for DTK - accessing dtk table.           */
#include "aps_defs.h"       /* for APSDB_ENVVAR and APS_USERID_ENVVAR   */
#include "dapps_defs.h"     /* for ASF_TIME_STR_LENGTH                  */
#include "db_dtk.h"         /* for APS db relation dtk.                 */
#include <string.h>         /* for strrchr()                            */
#include <stdlib.h>         /* for getenv()                             */
#include <unistd.h>         /* for access()                             */
#include <aps_encrypt.h>    /* for aps_encrypt()                        */
#include <mu_utilities.h>   /* for getting permissions                  */

#include "aps_log_msg.h"    /* for aps_log_msg() stuff and MSG_LEN      */
#include "apspath.h"        /* for aps_fullpath         */
#include "aps_darStatistics.h" /* for aps_darStatistics()         */

/* GLOBAL variable for use in messages:  */
char        msg[MSG_LEN];

static void normal_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_OK) ;
}

void error_exit(char *progname) 
{
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_ERROR) ;
}
static void usage_exit(char *progname)
{
    fprintf(stderr, "\nusage: %s { -E | [-U Sybase_userid] -P Sybase_password }\n",
            progname);
    fprintf(stderr, 
    "       [ -p <permission id> | \n");
    fprintf(stderr, 
    "       { -R <number of retries> -S <seconds between retries> } ]\n");
    fprintf(stderr, 
    "       [ -d dtkdate ]\n");
    fprintf(stderr,
    "\n    This program retrieves data-takes with yesterday's date stamp.\n") ;
    fprintf(stderr,
    "    For data-takes with darid != 0, it then accumulates seconds per\n" ) ;
    fprintf(stderr,
    "    darid for SUB and PLN data-takes, then conveys the totals to the\n") ;
    fprintf(stderr,
    "    IMS via an application programming interface.  \n" ) ;
    fprintf(stderr,
    "\n    This program is intended to be run in a Cron job just after\n") ; 
    fprintf(stderr,
    "    00:00 hours daily, which will use encryption to obtain the userid\n") ;
    fprintf(stderr,
    "    and password.  \n") ;
    fprintf(stderr,
    "\n    Use -E to obtain Sybase_userid and Sybase_password via \n");
    fprintf(stderr,
    "    Encryption\n\n");
        fprintf(stderr,
    "    <permission id>   An integer indicating an existing permission.\n" ) ; 
        fprintf(stderr,
    "                      If not provided, -R can be used.\n" ) ;
    fprintf(stderr,
    "\n    <number of retries>     An integer indicating the number of extra\n" ) ; 
    fprintf(stderr,
    "                            attempts to get permission if the first\n" ) ;
        fprintf(stderr,
    "                            attempt fails.  -S is required with this\n" ) ;
        fprintf(stderr,
    "                            parameter.\n" ) ;
    fprintf(stderr,
    "\n    <seconds between retries>     An integer indicating the amount of time\n" ) ;
    fprintf(stderr,
    "                                  in seconds between permission attempts.\n" ) ;
    fprintf(stderr,
    "\n    dtkdate                       yyyy:ddd   If used, the program takes\n" ) ;
    fprintf(stderr,
    "                                  this date as today. \n" ) ;

    fprintf(stderr, "\n\n    %s  Version compiled:  %s %s\n\n",
        progname, __DATE__, __TIME__ ) ;
    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    error_exit(progname) ;
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
        sprintf(msg, "environment variable:  %s is not set.", env_name);
#ifdef  RUN_FROM_GUI
        fprintf( stderr, "%s:\n\n%s\n", progname, msg );
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

Description:    main driver for DAR statistics.  

Creator:        Lawrence Stevens

Creation Date:  Wed Feb 21 17:44:33 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
void main (int argc, char *argv[])
{
 
    DBPROCESS   *APS_dbproc ;
    char        *dbname = NULL ;

    char        *user_pw_filename = NULL ;
    char        *APS_rootpath = NULL ;
    int         bad_env_count  = 0 ;
    char        *sybase_userid = NULL ;
    char        *password = NULL ;
    char        dtkdate[9] = "yyyy:ddd" ;
    char        today_asftime[ASF_TIME_STR_LENGTH+1] = "yyyy:ddd:12:00:00.000" ;
    int         Eflag = 0 ;
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

    char        *env_dbname;        /* dbname from environment      */
    char        *env_sybase_userid; /* userid from environment      */
 
    int         c;      /* used as return character from getopt()       */
 
    char        flag_list[20] = "U:P:ER:S:p:d:"; /* list of flags for getopt  */
 
    /* for MU permission */
    int     dflag = 0;  /* used to check for optional dtkdate                */
    int     pflag = 0;  /* used to check for optional permission_id          */
    int     Sflag = 0;  /* used to check for optional n_seconds_retry        */
    int     Rflag = 0;  /* used to check for optional n_retries              */
    int     permission_id   = 0 ;  /* optional permission_id passed     */
    int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
    int     n_retries       = 0 ;  /* optional number of re-tries       */
 
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
    sprintf(msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(msg, " " ) ;
        strcat(msg, argv[j] ) ;
    }
    aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

    if ( argc <= 1 )
        usage_exit(progname);

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
				strcpy(dtkdate, optarg) ;
				dflag++ ;
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
                    fprintf(stderr,
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
                    fprintf(stderr,
                            "%s(%d): error: %s :  seconds between re-tries must be an integer\n",
                            __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                if ( n_seconds_retry <= 0 )
                {
                    fprintf(stderr,
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
                    fprintf(stderr,
                            "%s(%d): error: %s :  number of re-tries must be an integer\n",
                            __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                if( n_retries <= 0 )
                {
                    fprintf(stderr,
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
        printf( "Obtaining Sybase userid and password from encrypted file.\n") ;
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
            sprintf( msg, 
                "directory path for APS_UPW not obtained by aps_fullpath()" ) ;
#ifdef RUN_FROM_GUI
            fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */

            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }
        if (access(APS_rootpath, R_OK) != 0)
        {
            sprintf( msg, 
                "program does not have R_OK access to directory %s",
                APS_rootpath ) ;
#ifdef RUN_FROM_GUI
            fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }

        user_pw_filename = (char *)malloc( sizeof(char)*(  
            strlen(APS_rootpath)+1 + 
            strlen(APS_IMS_DB_UPW_FILE)+1)   ) ;

        strcpy(user_pw_filename, APS_rootpath ) ;
        strcat(user_pw_filename, "/") ;
        strcat(user_pw_filename, APS_IMS_DB_UPW_FILE ) ;

        user_pw_string = get_APS_upass( user_pw_filename ) ;
        if ( user_pw_string == NULL )
        {
            sprintf( msg, "info not obtainable from expected encrypted file %s",
                user_pw_filename ) ;
#ifdef RUN_FROM_GUI
            fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }
        comma_ptr = strrchr( user_pw_string, ',' ) ;
        if ( comma_ptr == NULL )
        {
            sprintf( msg, 
                "expected comma (,) not found in decrypted string from %s",
                user_pw_filename ) ;
#ifdef RUN_FROM_GUI
            fprintf(stderr, "%s:\n\nERROR:  %s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }

        /*
        -- set the pasword pointer to just after 
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
        strcpy( msg, "Sybase_password not given." ) ;
#ifdef RUN_FROM_GUI
        fprintf(stderr, "%s:\n\n%s\n", progname, msg ) ;
#endif /*   RUN_FROM_GUI    */
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
            sprintf( msg, 
            "ERROR:  sybase_userid not given; environment variable %s not set",
                APS_USERID_ENVVAR ) ;
#ifdef RUN_FROM_GUI
            fprintf( stderr, "%s:\n\n%s\n", progname, msg );
#endif /*   RUN_FROM_GUI    */
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            error_exit(progname) ;
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
        fprintf(stderr, "%s:\n\nERROR initializing the Stoic file.\n", progname);
#endif /*   RUN_FROM_GUI    */
        fprintf(stderr, "     check for existance, permissions, and \n" ) ;
        fprintf(stderr, "     value of environment variable APS_DATA\n" ) ;
        error_exit(progname) ;
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
    if ( bad_env_count )
    {
        sprintf(msg, "one or more environment variables not set." );
#ifdef RUN_FROM_GUI
        fprintf( stderr, "%s:\n\n%s\n", progname, msg );
#endif /*   RUN_FROM_GUI    */
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progname) ;
    }

    /* get the database name from the environment   */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
 
        aps_log_msg(progname, APS_CRITICAL,
        "ERROR:  environment variable APSDB not set; cannot open a database",
            DO_SYSLOG, DO_PRINT);
        error_exit (progname) ;
    }
    dbname = env_dbname ;
 
    /* now open the database - we need to do this to get permissions  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open( dbname, progname, sybase_userid, password, NULL,
        error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        sprintf( msg, "ERROR opening database '%s'", dbname ) ;
        db_open_errs(return_code, dbname, sybase_userid);
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit (progname) ;
    }
 
    /*
    -- Get permissions
    */
    
    retcode = mu_get_permission(progname, 
                                APS_dbproc, 
                                permission_id,
                                MU_SINGLE_ACTIVITY_TYPE,
                                MU_DAR_STATISTICS,
                                NULL, NULL, NULL, 0,     /* parameters not used */
                                n_retries,
                                n_seconds_retry ) ;
    if ( retcode < 0 )
        error_exit (progname) ;
    
    /* permission was obtained; now proceed.  */
    if ( dflag == 0 )
    {
        /*
        -- get today's time.
        */
        tc_systime2asf( today_asftime );
        strncpy( dtkdate, today_asftime, 8 ) ;
    }
    else
    {
        /* dtkdate was provided from -d on command line.  check it.  */
        strncpy( today_asftime, dtkdate, 8 ) ;
        return_code = tc_validate_asf_datetime(today_asftime) ;
        if ( return_code != TRUE )
        {
            printf("%s:\n\nerror in input dtkdate, %s\n", progname,
                dtkdate ) ;
            aps_log_msg(progname, APS_ERROR, "error in input -d dtkdate",
                DO_SYSLOG, DO_PRINT);
            error_exit (progname) ;
        }
    }

    return_code = aps_darStatistics( progname, sybase_userid, password, 
        dtkdate ) ;

    if ( return_code != APS_EXIT_OK )
    {
        /* 
        -- an error occurred in which 
        -- the routine could not finish.  
        */
        error_exit(progname ) ;
    }

    normal_exit(progname) ;

} /* main */

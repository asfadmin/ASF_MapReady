#undef RUN_FROM_GUI
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    Main driver for APS data migration; intended to be run 
                at ASF to migrate data values in selected dtk data records.  
                The changing of the dtk structure is expected to already 
                have been done.  
==============================================================================*/
#pragma ident   "@(#)main.c	1.1 98/03/05 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/migrate/SCCS/s.main.c"

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

/* routine that does the migration:   */
extern int stats_migrate(
    DBPROCESS   *APS_dbproc, 
    FILE        *logfp,
    char        *sat, 
    int         first_rev, 
    int         last_rev, 
    int         *error_count ) ;


static void normal_exit(char *progname, FILE *logfp, int error_count)
{
    if( error_count == 0 )
    {
        (void) fprintf(logfp, "%s:  Program completed successfully.\n",
            progname ) ;
        exit(APS_EXIT_OK) ;
    }
    if( error_count == 1 )
    {
        (void) fprintf(logfp,
            "%s:  completed normally, but there was 1 error.\n", 
            progname ) ;
        exit(APS_EXIT_OK) ;
    }
    if( error_count > 1 )
    {
        (void) fprintf(logfp,
            "%s:  completed normally, but there were %d errors.\n", 
            progname, error_count ) ;
        exit(APS_EXIT_OK) ;
    }
}

static void error_exit(char *progname, FILE *logfp )
{
    if( (int) logfp != NULL )
        (void) fprintf(logfp, 
            "\n%s:  APS_INFO:  Program terminated abnormally.\n",
            progname ) ;
    exit(APS_EXIT_ERROR) ;
}

static void usage_exit(char *progname)
{
    (void) fprintf(stderr, "\nusage: %s { -E | [-U Sybase_userid] -P Sybase_password }\n",
            progname);
    (void) fprintf(stderr, 
    "       -s satellite  -f first_rev -l last_rev \n");
    (void) fprintf(stderr,
    "\n    This program selects the dtk records according to the input\n") ;
    (void) fprintf(stderr,
    "    parameters.  For each record, a migration of data is then \n");
    (void) fprintf(stderr,
    "    performed as follows:  \n") ;
    (void) fprintf(stderr,
    "    The strttime and stoptime values are copied to fa_strttime and \n") ;
    (void) fprintf(stderr,
    "    fa_stoptime.   The value of fa_duration_min is computed and set.  \n");
    (void) fprintf(stderr,
    "    Finally, the value of asf_reduction_min is set to 0.0.  \n");

    (void) fprintf(stderr,
    "    This program is to be run as desired during installation at ASF.\n");

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
        (void) fprintf(stderr, 
            "%s:  environment variable:  %s is not set.", progname, env_name ) ;
        return FALSE ;
    }
    else
        return TRUE ;
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

    FILE        *logfp ;      /* log file pointer.  */

    char        msg[256] ;

    DBPROCESS   *APS_dbproc ;
    char        *dbname = NULL ;

    char        *user_pw_filename = NULL ;
    char        *APS_rootpath = NULL ;
    int         bad_env_count  = 0 ;
    char        *sybase_userid = NULL ;
    char        *password = NULL ;
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

    char        *env_dbname;        /* dbname from environment          */
    char        *env_sybase_userid; /* userid from environment          */

    int         c;      /* used as return character from getopt()       */
 
    char        flag_list[20] = "U:P:Es:f:l:"; /* flag list for getopt */
 
    int     sflag = 0;  /* used to check for mandatory sat value             */
    char    *sat = NULL ;
    int     fflag = 0;  /* used to check for mandatory first_rev value.      */
    int     first_rev ;
    int     lflag = 0;  /* used to check for mandatory last_rev value.       */
    int     last_rev ;
    int     error_count ;

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
 
    (void) sprintf(msg, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        (void) strcat(msg, " " ) ;
        (void) strcat(msg, argv[j] ) ;
    }
    (void) printf( "%s\n\n", msg ) ;


    if ( argc <= 1 )
        usage_exit(progname);

    /* 
    -- diagnostic print.  send log 
    -- file output to stdout for ease of 
    -- checking.  set filename to indicate this, too:  
    */
    logfp = stdout ;

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
            case 'f':
                if( fflag != 0 )
                    usage_exit(progname);
                fflag++ ;
                retcode = sscanf( optarg, "%d", &first_rev ) ;
                if( retcode != 1 )
                {
                    (void) fprintf(stderr,
                    "%s(%d): error: %s :  first_rev must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                break;
            case 'l':
                if( lflag != 0 )
                    usage_exit(progname);
                lflag++ ;
                retcode = sscanf( optarg, "%d", &last_rev ) ;
                if( retcode != 1 )
                {
                    (void) fprintf(stderr,
                    "%s(%d): error: %s :  last_rev must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                break;
            case 's':
                if( sflag != 0 )
                    usage_exit(progname);
                sflag++ ;
                sat = optarg ;
                break;
            case '?':
                usage_exit(progname);
                break ;
            default:
                /* do nothing.  */
                break ;
        }
    /* the s, f, and l parameters are required.  */
    if( fflag == 0 || lflag == 0 || sflag == 0 )
    {
        (void)fprintf(stderr, "%s:  s, l, and f parameters are all required.\n",
            progname ) ;
        exit(1) ;
    }

    /* check satellite value:  */
    if( strcmp( sat, "E2" ) != 0  
    &&  strcmp( sat, "E1" ) != 0  
    &&  strcmp( sat, "J1" ) != 0  
    &&  strcmp( sat, "R1" ) != 0 )
    {
        (void)fprintf(stderr, "%s:  satellite must be E1, E2, J1, or R1.\n",
            progname ) ;
        exit(1) ;
    }

    /* check first and last revs:  */
    if( last_rev < first_rev )
    {
        (void)fprintf(stderr, "%s:  last_rev < first_rev.\n", progname ) ;
        exit(1) ;
    }
    if( (last_rev - first_rev) > 1000 )
    {
        (void)fprintf(stderr, "%s:  %d %d only 1000 revs at a time allowed.\n", 
            progname, first_rev, last_rev ) ;
        exit(1) ;
    }

    (void)fprintf( logfp, "%s starting with:  sat = %s, revs = [%d-%d]\n",
        progname, sat, first_rev, last_rev ) ;

    /*
    -- check for extraneous words not attached to
    -- any flag; no other argument is allowed.
    */
    if ( optind != argc )
        usage_exit(progname);
 
    if ( Eflag )
    {
        (void) fprintf( logfp, 
            "Obtaining Sybase userid and password from encrypted file.\n") ;
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
            (void) fprintf( stderr, 
            "%s:  directory path for APS_UPW not obtained by aps_fullpath()\n",
                progname );
            error_exit (progname, logfp ) ;
        }
        if (access(APS_rootpath, R_OK) != 0)
        {
            (void) fprintf( stderr, 
                "%s:  does not have R_OK access to directory %s",
                progname, APS_rootpath ) ;
            error_exit (progname, logfp ) ;
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
            (void) printf( msg, 
                "info not obtainable from expected encrypted file %s",
                user_pw_filename ) ;
            error_exit (progname, logfp ) ;
        }
        comma_ptr = strrchr( user_pw_string, ',' ) ;
        if ( comma_ptr == NULL )
        {
            (void) printf( msg, 
                "expected comma (,) not found in decrypted string from %s",
                user_pw_filename ) ;
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
        (void) printf( "Sybase_password not given.\n" ) ;
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
            (void) fprintf( stderr, 
            "ERROR:  sybase_userid not given; environment variable %s not set",
                APS_USERID_ENVVAR ) ;
            error_exit(progname, logfp ) ;
        }
        else
            /* use the environment sybase_userid.   */
            sybase_userid = env_sybase_userid ;
    }
 
    if ( init_vec_lib() )
    {
        (void) fprintf(stderr, "%s:  error initializing the Stoic file\n",
            progname ) ;
        (void) fprintf(stderr, "     check for existance, permissions, and \n");
        (void) fprintf(stderr, "     value of environment variable APS_DATA\n");
        error_exit(progname, stderr ) ;
    }

    /*
    -- check the other needed environment variables 
    -- for this job.  
    -- APSDB_ENVVAR, IMS_DB_ENVVAR,  and IMS_SERVER_ENVVAR.  
    */
    bad_env_count  = 0 ;
    if ( !env_is_set(progname, APSDB_ENVVAR ) )
        bad_env_count ++ ;
    if ( bad_env_count )
    {
        (void)fprintf( stderr,
            "%s:  environment variable not set:  %s \n", 
            progname, APSDB_ENVVAR);
        error_exit(progname, stderr ) ;
    }

    /* get the database name from the environment   */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
        (void)fprintf(stderr, 
            "%s:  ERROR:  env APSDB not set; cannot open database",
            progname ) ;
        error_exit (progname, stderr ) ;
    }
    dbname = env_dbname ;
 
    /* now open the database - we need to do this to get permissions  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open( dbname, progname, sybase_userid, password, NULL,
        error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        (void) fprintf( stderr, "%s:  ERROR opening database '%s'", 
            progname, dbname ) ;
        db_open_errs(return_code, dbname, sybase_userid);
        error_exit (progname, stderr ) ;
    }

    return_code = stats_migrate( APS_dbproc, logfp, sat, first_rev, last_rev, 
        &error_count ) ;
    if ( return_code < 0 )
    {
        /* 
        -- an error occurred in which 
        -- the routine could not finish.  
        */
        (void)fprintf(stderr,"%s:  Error occurred.", progname ) ;
        (void)fprintf(stderr,"     Error count = %d\n", error_count ) ;
        error_exit(progname, stderr  ) ;
    }

    (void)fprintf( logfp, "%s Completed:  sat = %s, revs = [%d-%d]\n",
        progname, sat, first_rev, last_rev ) ;

    (void)fprintf(logfp,"%s:  Error count  = %d\n", progname, error_count ) ;
    (void)fprintf(logfp,"%s:  Update count = %d\n", progname, return_code ) ;

    normal_exit(progname, logfp, error_count ) ;

} /* main */

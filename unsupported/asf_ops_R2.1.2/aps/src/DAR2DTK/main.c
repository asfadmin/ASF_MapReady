/*  #define TEST_ONLY    */
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   main.c

Description:    main source file for the create DAR dtk for R1 AMM.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/DAR2DTK/SCCS/s.main.c"

/* 
-- to decode the error code into a string:  
*/
extern char *crt_dar_dtk_error_message[] ;    /* see this table.  */

#include <stdio.h>
#include <stdlib.h>         /* for getopt           */
#include <string.h>         /* for strcat() etc.    */
#include "crt_dar_dtk.h"
#include "timeconv.h"       /* for time validation  */
#include <aps_log_msg.h>    /* for syslog stuff.    */
#include <aps_defs.h>       /* for APS_EXIT_OK, APS_EXIT_ERROR  */
#include <dapps_defs.h>     /* for ASF_TIME_STR_LENGTH          */
#include <apspath.h>        /* for APS_REPORTS                  */
#include <mu_utilities.h>   /* for obtaining permissions        */

extern int init_vec_lib();


static void usage_exit(char *progname)
{
    char    *env_dbname ;
    env_dbname = getenv("APSDB");

    (void)fprintf(stderr,"\nusage: %s [-U Sybase_userid] -P Sybase_password\n",
        progname ) ;
    (void)fprintf(stderr, "        { -d darid | -f darid1 -l darid2 } \n", 
        progname);
    (void)fprintf(stderr,
    "                [ -s status ]   [ -q { Y | N } ]   [ -m max_minutes ]\n" );
    (void)fprintf(stderr,
    "                [ -A { Y | N } ] [ -M { Y | N } ] \n" );
    (void)fprintf(stderr,
    "                [ -R <number of retries> -S <seconds between retries> ]\n" );
    (void)fprintf(stderr, 
    "\n    This program uses info from a Radarsat DAR to create a dtk \n") ;
    (void)fprintf(stderr, 
    "    proposal which is then submitted for processing.  A log file is \n") ;
    (void)fprintf(stderr, 
    "    created named $APS_DATA/reports/DAR_<darid>.dtk.yyyyddd_hhmmss.rpt\n");
    (void)fprintf(stderr, 
    "    The database used is $APSDB" ) ;

    if ( env_dbname )
        (void)fprintf(stderr, ", which = '%s' right now.\n\n", env_dbname ) ;
    else
        (void)fprintf(stderr, ".\n\n" ) ;

    (void)fprintf(stderr, 
    "    Sybase_userid       Sybase account userid\n" ) ;
    (void)fprintf(stderr, 
    "    Sybase_password     Sybase account password\n" ) ;
    (void)fprintf(stderr, 
    "\n    darid               integer DAR id for desired individual DAR.\n" ) ;
    (void)fprintf(stderr, 
"    darid1              First DAR id if a darid range (-f and -l) is used.\n");
    (void)fprintf(stderr, 
"                        This darid is used in the report file name.\n" ) ;
    (void)fprintf(stderr, 
"    darid2              Last DAR id ( >= First DARid ) if a darid range\n");
    (void)fprintf(stderr, 
    "                        is used.  All integers from darid1 to darid2 \n");
    (void)fprintf(stderr, 
    "                        are processed.                               \n");
    (void)fprintf(stderr, 
"\n    status              desired status: { SUB | PLN }   Default status is PLN.\n") ;
    (void)fprintf(stderr, 
    "    -q { Y | N }        You indicate the desired planner quicklook \n") ;
    (void)fprintf(stderr, 
    "                        by using -q with Y or N.  The default is N. \n") ;
    (void)fprintf(stderr, 
    "    max_minutes         Integer minutes, indicating the maximum time \n");
    (void)fprintf(stderr, 
    "                        length of the DAR:  A dar with start and end \n");
    (void)fprintf(stderr, 
    "                        time that makes a time duration longer than  \n");
    (void)fprintf(stderr, 
    "                        max_minutes is rejected.  The default value is 50.\n");
    (void)fprintf(stderr, 
    "    [ -A { Y | N } ]    This flag indicates whether or not to use \n");
    (void)fprintf(stderr, 
"                        ASF nominal Mask entry and exit times to create\n");
    (void)fprintf(stderr, 
"                        data-takes.  \n");
    (void)fprintf(stderr, 
"                        If -A N is used, no data-takes at ASF will be \n");
    (void)fprintf(stderr, 
"                        realtime.  If -A Y is used, data-takes \n");
    (void)fprintf(stderr, 
"                        within an ASF pass (according to nominal Mask \n");
    (void)fprintf(stderr, 
"                        entry and exit times) will be created as realtime\n");
    (void)fprintf(stderr, 
"                        observations, and a downlink is created, too.\n");
    (void)fprintf(stderr, 
    "                        The default is Y.  \n");
    (void)fprintf(stderr, 
    "    [ -M { Y | N } ]    Same as the -A flag, except that -M refers\n");
    (void)fprintf(stderr, 
    "                        to the McMurdo mask instead of ASF.       \n");
    (void)fprintf(stderr, 
    "                        The default is N for McMurdo, as recordings\n");
    (void)fprintf(stderr, 
    "                        are preferred over realtimes.\n");
    (void)fprintf(stderr,
    "\n    Multi-user permission parameters:  \n" ) ; 
    (void)fprintf(stderr,
    "    <number of retries>     An integer indicating the number of extra\n"); 
    (void)fprintf(stderr,
    "                            attempts to get permission if the first\n" ) ;
    (void)fprintf(stderr,
    "                            attempt fails.  -S is required with this\n" ) ;
    (void)fprintf(stderr,
    "                            parameter.\n" ) ;
    (void)fprintf(stderr,
    "    <seconds between retries>     An integer indicating the amount of time\n" ) ;
    (void)fprintf(stderr,
    "                                  in seconds between permission attempts.\n" ) ;
    (void)fprintf(stderr, "\n\n    %s version:  %s %s   ---------\n\n",
        progname, __DATE__, __TIME__ ) ;

    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR) ;
}

/*==============================================================================
Function:       main()

Description:    main routine for the create DAR dtk tool, in  
                which, for a given DAR, data is extracted and used 
                to create a single dtk recording proposal.  
                This is for an AMM DAR, where satellite is R1, during 
                the Antarctic Mapping Mode phase, and where the 
                info is already in the DAR, such as 
                sat/sensor/rev/strttime/stoptime.  
                The data-take proposal is submitted with a PLN status.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun 18 16:53:15 PDT 1997

==============================================================================*/
int main(int argc, char *argv[])
{

    DBPROCESS   *APS_dbproc ;
    char        *dbname = NULL ;
    char        *sybase_userid = NULL ;
    char        *password = NULL ;

    char        *progname ;
    char        *slash_ptr ;

    char        buf[256] ;
    int         j ;

    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;
     
    RETCODE     return_code;
    int         retcode;
 
    char        *env_dbname;        /* dbname from environment      */
    char        *env_sybase_userid; /* userid from environment      */

    int         n_errors = 0 ;
    int         darid ;
    int         darid1 ;
    int         darid2 ;
    int         dflag = 0;  /* used to check for darid                    */
    int         fflag = 0;  /* used to check for darid1                   */
    int         lflag = 0;  /* used to check for darid2                   */
    int         mflag = 0;  /* used to check for max_minutes              */
    int         Aflag = 0;  /* used to check for -m N or -m Y             */
    char        ASFmaskinout_flag = 'Y';   /* default  */
    int         Mflag = 0;  /* used to check for -m N or -m Y             */
    char        MCMmaskinout_flag = 'N';   /* default  */
    int         qflag = 0;  /* used to check for planner_quicklook        */
    char        planner_quicklook ;   /* 'Y' or 'N'  */
    int         max_minutes = 50 ;  /* default is 50.                     */
    int         sflag = 0;  /* used to check for manditory dtkstat        */
    char        *dtkstat ;
    int         Sflag = 0;  /* used to check for optional n_seconds_retry */
    int         Rflag = 0;  /* used to check for optional n_retries       */
    int         Tflag = 0;  /* used to check for TEST MODE...             */
                            /* in TEST MODE, -s SCH is allowed.           */

    int         n_seconds_retry = 0 ;  /* optional seconds between re-tries */
    int         n_retries       = 0 ;  /* optional number of re-tries       */

    int         c;      /* used as return character from getopt()          */
     
                /* list for getopt */
    char        flag_list[50] = "A:M:P:R:S:TU:d:f:l:m:q:s:";

    FILE        *rptfp ;
    char        msg[200] ;
    char        *report_dir = NULL ;
    char        full_report_file_name[200] ;

    char        now_asftime[ASF_TIME_STR_LENGTH+1] ;

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    /* 
    -- get progname as file name without 
    -- the directory path.  
    */
    progname = argv[0] ;
    slash_ptr = (char *) strrchr( progname, '/' ) ;
    if ( slash_ptr )
    {
        /* 
        -- if there is a directory path in progname,
        -- remove it by resetting the progname pointer to 
        -- the next character after the last slash:  
        */
        progname = slash_ptr + 1 ;
    }

    if( argc <= 1 )
        usage_exit(progname);
        
    aps_open_syslog();

    (void)sprintf(buf, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        (void)strcat(buf, " " ) ;
        (void)strcat(buf, argv[j] ) ;
    }
    aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'A':
                /* ASFmaskinout flag  */
                if(Aflag != 0)
                    usage_exit(progname);
                Aflag++;
                ASFmaskinout_flag = *optarg ;
                if( ASFmaskinout_flag != 'N' 
                &&  ASFmaskinout_flag != 'Y'  )
                {
                    (void)fprintf(stderr, "%s:  error: bad -A value:  %c\n",
                        progname, ASFmaskinout_flag ) ;
                    exit(1) ;
                }
                break;
            case 'M':
                /* MCMmaskinout flag  */
                if(Mflag != 0)
                    usage_exit(progname);
                Mflag++;
                MCMmaskinout_flag = *optarg ;
                if( MCMmaskinout_flag != 'N' 
                &&  MCMmaskinout_flag != 'Y'  )
                {
                    (void)fprintf(stderr, "%s:  error: bad -M value:  %c\n",
                        progname, MCMmaskinout_flag ) ;
                    exit(1) ;
                }
                break;
            case 'P':
                if( password != NULL )
                    usage_exit(progname);
                password = optarg ;
                break;
            case 'T':
                /* Test flag, to allow -s SCH   */
                if(Tflag != 0)
                    usage_exit(progname);
                Tflag++;
                break;
            case 'U':
                if( sybase_userid != NULL )
                    usage_exit(progname);
                sybase_userid = optarg ;
                break;
            case 'd':
                if(fflag != 0)
                    usage_exit(progname);
                if(lflag != 0)
                    usage_exit(progname);
                if(dflag != 0)
                    usage_exit(progname);
                dflag++;
                retcode = sscanf( optarg, "%d", &darid ) ;
                if( retcode != 1 )
                {
                    (void)fprintf(stderr,
                        "%s: error: %s :  darid must be an integer\n",
                            progname, optarg) ;
                    usage_exit(progname);
                }
                if ( darid <= 0 )
                {
                    (void)fprintf(stderr,"%s: error: %d :  darid must be > 0\n",
                        progname, darid ) ;
                    usage_exit(progname);
                }
                break;
            case 'f':
                if(dflag != 0)
                    usage_exit(progname);
                if(fflag != 0)
                    usage_exit(progname);
                fflag++;
                retcode = sscanf( optarg, "%d", &darid1 ) ;
                if( retcode != 1 )
                {
                    (void)fprintf(stderr,
                        "%s: error: %s :  darid1 must be an integer\n",
                            progname, optarg) ;
                    usage_exit(progname);
                }
                if ( darid1 <= 0 )
                {
                    (void)
                    fprintf(stderr,"%s: error: %d :  darid1 must be > 0\n",
                        progname, darid1 ) ;
                    usage_exit(progname);
                }
                break;
            case 'l':
                if(dflag != 0)
                    usage_exit(progname);
                if(lflag != 0)
                    usage_exit(progname);
                lflag++;
                retcode = sscanf( optarg, "%d", &darid2 ) ;
                if( retcode != 1 )
                {
                    (void)fprintf(stderr,
                        "%s: error: %s :  darid2 must be an integer\n",
                            progname, optarg) ;
                    usage_exit(progname);
                }
                if ( darid2 <= 0 )
                {
                    (void)
                    fprintf(stderr,"%s: error: %d :  darid2 must be > 0\n",
                        progname, darid2 ) ;
                    usage_exit(progname);
                }
                break;
            case 'm':
                if(mflag != 0)
                    usage_exit(progname);
                mflag++;
                retcode = sscanf( optarg, "%d", &max_minutes ) ;
                if( retcode != 1 )
                {
                    (void)fprintf(stderr,
                        "%s: error: %s :  max_minutes must be an integer\n",
                            progname, optarg) ;
                    usage_exit(progname);
                }
                if ( max_minutes <= 0 )
                {
                    (void)fprintf(stderr,
                        "%s: error: %d :  max_minutes must be > 0\n",
                        progname, max_minutes ) ;
                    usage_exit(progname);
                }
                break;
            case 'q':
                /* planner quicklook  */
                if(qflag != 0)
                    usage_exit(progname);
                qflag++;
                planner_quicklook = *optarg ;
                if( planner_quicklook != 'N' 
                &&  planner_quicklook != 'Y'  )
                {
                    (void)fprintf(stderr,
                        "%s:  error: bad -q value:  %c\n",
                        progname, planner_quicklook ) ;
                    exit(1) ;
                }
                break;
            case 's':
                /* data-take status.  */
                if(sflag != 0)
                    usage_exit(progname);
                sflag++;
                dtkstat = optarg ;

                if( strcmp(dtkstat, "SUB") != 0 
                &&  strcmp(dtkstat, "PLN") != 0 
                &&  strcmp(dtkstat, "SCH") != 0 )
                {
                    (void)fprintf(stderr,
                        "%s:  error: bad -s status value:  %s\n",
                        progname, dtkstat ) ;
                    exit(1) ;
                }
                break;
            case 'S':
                if(Sflag != 0)
                    usage_exit(progname);
                Sflag++;
                retcode = sscanf( optarg, "%d", &n_seconds_retry ) ;
                if( retcode != 1 )
                {
                    (void)fprintf(stderr,
"%s: error: %s :  seconds between re-tries must be an integer\n",
                        progname, optarg) ;
                    usage_exit(progname);
                }
                if ( n_seconds_retry <= 0 )
                {
                    (void)fprintf(stderr,
"%s: error: %d :  seconds between re-tries must be > 0\n",
                        progname, n_seconds_retry ) ;
                    usage_exit(progname);
                }
                break;
            case 'R':
                if(Rflag != 0)
                    usage_exit(progname);
                Rflag++;
                retcode = sscanf( optarg, "%d", &n_retries ) ;
                if( retcode != 1 )
                {
                    (void)fprintf(stderr,
"%s: error: %s :  number of re-tries must be an integer\n",
                        progname, optarg) ;
                    usage_exit(progname);
                }
                if( n_retries <= 0 )
                {
                    (void)fprintf(stderr,
                        "%s: error: %d :  number of re-tries must be > 0\n",
                        progname, n_retries) ;
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

    if( sflag != 0 )
    {
        /* 
        -- a status value was given 
        -- in the command line.  
        */
        if( strcmp(dtkstat, "SCH") == 0 
        &&  Tflag == 0 )
        {
            /* 
            -- this is not in test mode.  DISALLOW 
            -- -s SCH.  
            */
            (void)fprintf(stderr, "%s:  error: bad -s status value:  %s\n",
                progname, dtkstat ) ;
            exit(1) ;
        }
    }

    /*
    -- check for extraneous words not attached to
    -- any flag; no other argument is allowed.  
    */
    if ( optind != argc )
        usage_exit(progname);

    /* mandatory flags:  */
    if ( password == NULL )
    {
        (void)fprintf(stderr, "%s:\n\nSybase_password not given.\n", progname);
        usage_exit(progname) ;
    }
    if( fflag != lflag )
    {
        (void)fprintf(stderr, 
            "%s:\n\n-f and -l flags must BOTH be used, not just one.\n", 
            progname ) ;
        usage_exit(progname) ;
    }
    if ( dflag == 0 ) 
    {
        if( fflag == 0 || lflag == 0 )
        {
            (void)fprintf(stderr, "%s:\n\ndarid(s) not given.\n", progname ) ;
            usage_exit(progname) ;
        }
        else
        {
            /* 
            -- -f and -l were used.  
            -- set darid, used for the name of the output file.  
            */
            darid = darid1 ;
        }
    }
    else
    {
        /* 
        -- dflag only is used.  
        -- the single darid argument was used OK.  
        */
        darid1 = darid ;
        darid2 = darid ;

    }

    /* default quicklook is N  */
    if ( qflag == 0 )
        planner_quicklook = 'N' ;

    /* default status is "PLN"  */
    if ( sflag == 0 )
        dtkstat = "PLN" ;

    /*
    -- The retry flag (-R) and the "delay in seconds" flag (-S) must be both 
    -- set or unset, otherwise there is an error in the retry logic.
    */
    if (Sflag != Rflag )
        usage_exit(progname);

    /*
    -- all of the values have now been obtained from the
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
            (void)fprintf(stderr, "%s:\n\nERROR:  sybase_userid not given\n", 
                progname);
            aps_log_msg(progname, APS_CRITICAL,
                "ERROR:  sybase_userid not given; environment variable APS_SYBASE_USERID not set.",
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
                DO_SYSLOG, DO_PRINT);
            exit(APS_ERROR) ;
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
        (void)fprintf(stderr,
        "%s:\n\nERROR:  environment variable APSDB not set; cannot upen DB\n",
            progname );
        (void)fprintf(stderr, "Use setenv APSDB <dbname>. \n\n");
        aps_log_msg(progname, APS_CRITICAL,
            "ERROR:  environment variable APSDB not set; cannot upen DB",
            DO_SYSLOG, DO_PRINT);
        usage_exit(progname);
    }
    dbname = env_dbname ;
 
    /* now open the database.  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open( dbname, progname, sybase_userid, password, NULL,
        error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        db_open_errs(return_code, dbname, sybase_userid);
        (void) sprintf( buf, "DB '%s' could not be opened", dbname ) ;
        aps_log_msg(progname, APS_CRITICAL, buf, DO_SYSLOG, DO_PRINT);
        aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit (APS_ERROR) ;
    }

    /* init vector lib */
    if ( init_vec_lib() )
    {
        (void)fprintf(stderr, "%s:\n\nERROR initializing the Stoic file.\n",
            progname ) ;
        (void)fprintf(stderr, "     check for existance, permissions, and \n");
        (void)fprintf(stderr, "     value of environment variable APS_DATA\n");
        aps_log_msg(progname, APS_CRITICAL, "ERROR initializing the Stoic file", 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_ERROR) ;
    }

    /* get report filename and open the report file.  */
    report_dir = aps_fullpath( APS_REPORTS, NULL ) ;
    tc_systime2asf( now_asftime );
    (void) sprintf( full_report_file_name, 
        "%s/DAR_%d.dtk.%4.4s%3.3s_%2.2s%2.2s%2.2s.rpt", 
        report_dir,
        darid,
        now_asftime, 
        now_asftime+5, 
        now_asftime+9, 
        now_asftime+12, 
        now_asftime+15  ) ;

    (void)printf(
"\nNOTE:  see report file \n%s\nfor info on data-takes from DAR %d.\n\n",
        full_report_file_name, darid ) ;

#ifndef TEST_ONLY
    rptfp = fopen(full_report_file_name, "a") ;
    if ( rptfp == NULL  )
    {
        (void)sprintf(msg,
"Unable to open rpt file %s; \ncheck for directory permissions or full disk.",
            full_report_file_name ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        exit (APS_ERROR) ;
    }
    /* 
    -- insert a header rec with filename and time.  
    */
    (void)fprintf( rptfp, "%s\n%s\n\n", full_report_file_name, now_asftime ) ;
#endif /*  TEST_ONLY   */


#ifdef TEST_ONLY
        (void) printf("Using database:  %s\n", dbname ) ; /* for testing...   */
#else
        (void) fprintf(rptfp, 
            "Using database:  %s\n", dbname ) ; /* for operations.*/
#endif

    /* loop thru all integers darid1-darid2  */
    for( j = darid1 ; j <= darid2 ; j ++  )
    {
        return_code = crt_dar_dtk( APS_dbproc, progname, 
            0,   /* permission_id  */
            n_seconds_retry, n_retries,
            j, dtkstat, planner_quicklook, ASFmaskinout_flag, 
            MCMmaskinout_flag, max_minutes, 
#ifdef TEST_ONLY
            stdout ) ;   /* for testing...   */
#else
            rptfp ) ;    /* for operations.  */
#endif

        if ( return_code < 0 )
        {
            n_errors ++ ;
            (void)fprintf(stderr, 
                "%s:\n\nERROR encountered in Create DAR dtk:\n", 
                progname ) ;
            (void)fprintf(stderr, "%s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE(return_code) );
            aps_log_msg(progname, APS_ERROR, 
                CRT_DAR_DTK_ERROR_MESSAGE(return_code),
                DO_SYSLOG, DO_PRINT);
            continue ;
        }
        (void) sprintf( msg, "%d data-take(s) created from DAR %d", 
            return_code, j ) ;
        aps_log_msg(progname, APS_INFO,  msg, DO_SYSLOG, DO_PRINT);
    }
    if( n_errors )
    {
        (void) sprintf(msg, "%d Errors occurred.", n_errors ) ; 
        aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
        aps_log_msg(progname, APS_INFO, "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_ERROR) ;
    }
    aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_OK) ;

    return 1 ;  /* for the lint man.  */

}

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       cdto_getopts.c

Description:    Main routine for create data-take opportunities.  

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)cdto_getopts.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.cdto_getopts.c"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "timeconv.h"
#include <aps_log_msg.h>
#include <aps_defs.h>
#include <db_sybint.h>
#include <mu_utilities.h>

void banner_exit(int exit_code) ;


/*==============================================================================
Function:       error_exit()
 
Description:    prints syslog error message and exits with an error code.
 
Creator:        Lawrence Stevens
 
Creation Date:  Fri Jan 10 16:59:52 PST 1997
 
Notes:
    This file was written with a 4-character tab setting.  If you don't use
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to
    browse.
==============================================================================*/
static error_exit(char *progname, int code )
{
 
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
        DO_SYSLOG, DO_PRINT);
 
    banner_exit(code);
 
}

/*==============================================================================
Function:       usage_exit()
 
Description:    print the usage and then exit.
 
Creator:        Lawrence Stevens
 
Creation Date:  Sat Jan 11 21:53:28 PST 1997
 
Notes:
==============================================================================*/
static usage_exit(char *progname)
{
    printf("usage:  %s [-p <Permission_id>] [-U sybase_userid] -P password {-d darid | -s sitename} \n", 
        progname);
    printf("\t\t{-b begin_time -e end_time | -f first_rev -l last_rev}\n");
    printf("\t\t[-A | -D] sat sensor\n\n");

    printf( "\tThis program accepts a dar or site and then computes all potential \n");
    printf( "\tsensor coverages for that specific site.   The specific site coverages \n");
    printf( "\tare placed into the sscvrg relation, and can eventually become \n");
    printf( "\tdata-takes if the Planner displays them on the Mapper and selects \n");
    printf( "\tthem for input, via a file, to the dtkm_segload function.  For this \n");
    printf( "\treason, these potential coverages are also referred to as data-take \n");
    printf( "\topportunities.  The user provides the parameters to describe the search.\n\n");

    printf( "\tNOTES:\n");
    printf("\t<permission id> is an integer indicating an existing permission.\n");
    printf( "\tUse either times (-b and -e) or revs (-f and -l) to \n");
    printf( "\tindicate the bracket for the run.\n");
    printf("\n\t-A indicates Ascending data-takes only (satellite moves south).\n");
    printf("\t-D indicates Descending data-takes only (satellite moves north).\n");
    printf( "\tDefault is to compute both Ascending and Descending data-takes.\n");
    printf("\n\tThis example creates dar #234 data-take opportunities for E1 SAR:\n");
    printf("\tcreate_dtk_opps  -P password     \\\n");
    printf("\t    -d 234 -b 1993:357:17:39:06.043 -e 1993:360:17:39:06.043 E1 SAR\n");
    printf("\t[%s Version dated %s %s]\n", progname, __DATE__, __TIME__ ) ;

    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);

    exit (APS_EXIT_ERROR);
}

/**********************************************************************
* Name : main()
*
* Purpose: handles the command line arguments for sscvbatch.for,
*           the main routine for create data-take opportunities. 
*
*   usage:  See the routine usage_exit() at the bottom of 
*           this file.  
*
*  Input Parameters:
*  Name             Type    Description
*  argc             int     number of arguments in the command line.  
*  argv             *char[] the values of the arguments.
*
*  Output Parameters:
*  Name             Type        Description
*
*  Variables.  
*  dbname           *char   name of the database
*  sybase_userid    *char   Sybase-approved userid.
*  password         *char   password for userid.  
*  begin_time       *char   start time of run 
*  stoptime         *char   stop time of run 
*  mask             *char   mask id:  currently GBL or ASF.
*  quick_flag       *int    0 for nominal coverage
*                           1 for nominal coverage with no db writes
*  sat              *char   satellite:  E1, J1, etc.  
*  sensor           *char   sensor:  SAR, OPS, etc.  
*
*  Exit Parameter:
*  Type         Description
*  int          0   no errors.
*               2   no value for APS_SYBASE_USERID in environment.
*               3   no value for APSDB in environment.
*
*  NOTE:    if there is an error in the command line; missing argument(s) etc.  
*           then this routine will print the usage info and exit.  
*
******************************************************************************
* 
*  Modification History:
*  Author   Revision    Date
*
****************************************************************************/
main( int   argc, char  *argv[])
{
    DBPROCESS   *APS_dbproc ;
    int         int_APS_dbproc ;
    char        *progname ;

    extern int  init_vec_lib();      /* initializes vector lib with stoicfile */
    extern int  init_vec_lib_exit(int); /* prints msg, exits if stoic error */

    int     j ;
    char    sat[10];
    char    sensor[10];
    int     darid = 0;
    char    sitename[100] = "";
    char    begin_time[40] = "";
    char    end_time[40] = "";
    int     first_rev = 0;
    int     last_rev = 0;
    char    ascdsc[2] = "D";
    char    dbname[100] = "";
    char    sybase_userid[100] = "";
    char    password[100] = "";
    char    buf[200] ;

    int     rcode = 0;

    /* get the sybase_userid (optional) and password (mandatory)    */
    /* int  getopt(int argc, char * const *argv, char *optstring);  */

    extern  char    *optarg;
    extern  int     optind ;
    /*  extern  char*   getenv(char*);  */

    /*  extern chasft_(char*, int*);   */
    extern  int     sscvbatch_(
        char    *progname,
        int     *dbproc,
        char    *sat, 
        char    *sensor, 
        int     *darid,
        char    *sitename,
        char    *begin_time,
        char    *end_time,
        int     *first_rev,
        int     *last_rev,
        char    *ascdsc ) ;

    extern  int     cvrg_allowed(char *sat, char *sensor ) ;

    int     c;      /* used as return character from getopt()       */

    int     Aflag = 0;  /* used to check for optional ascending flag    */
    int     Dflag = 0;  /* used to check for optional descending flag   */
    int     Pflag = 0;  /* used to check for mandatory password     */
    int     Uflag = 0;  /* used to check for optional sybase_userid */
    int     dflag = 0;  /* used to check for dar option.            */
    int     sflag = 0;  /* used to check for sitename option.       */
    int     bflag = 0;  /* used to check for start time.            */
    int     eflag = 0;  /* used to check for stop time.             */
    int     fflag = 0;  /* used to check for first rev.             */
    int     lflag = 0;  /* used to check for last rev.              */
    int     pflag = 0;  /* used to check for optional permission_id */

    char    flag_list[100] = "p:ADP:U:d:s:b:e:f:l:"; /*  flag list for getopt*/
    int     pcount;     /*count of cmd line positional parameters; should be 2*/
    int     return_code ;

    char    *env_dbname;        /* dbname from environment      */
    char    *env_sybase_userid; /* userid from environment      */
    int     permission_id   = 0 ;  /* optional permission_id passed     */

    /* 
    -- create data-take opportunities 
    -- (specific site coverage)
    -- is a DAR activity.  
    */
    char    *mu_activity_type = MU_DAR_ACTIVITY_TYPE ;
    char    *mu_activity_id   = MU_CREATE_SITE_COV_FOR_DAR ;

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    aps_open_syslog();

    sprintf(buf, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(buf, " " ) ;
        strcat(buf, argv[j] ) ;
    }

    /* find last occurrence of '/', if any, in the string:  */
    progname = strrchr( argv[0], '/' ) ;
    if( progname == NULL )
        progname = argv[0] ;
    else
        progname ++ ;

    aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'A':
                if( Aflag | Dflag )
                    usage_exit(progname);
                Aflag++;
                break;
            case 'D':
                if( Aflag | Dflag )
                    usage_exit(progname);
                Dflag++;
                break;
            case 'P':
                if(Pflag != 0)
                    usage_exit(progname);
                Pflag++;
                strcpy(password, optarg);
                break;
            case 'U':
                if(Uflag != 0)
                    usage_exit(progname);
                Uflag++;
                strcpy(sybase_userid, optarg);
                break;
            case 'd':
                if(dflag != 0)
                    usage_exit(progname);
                if(sflag != 0 )
                    usage_exit(progname);
                dflag++;
                darid = atoi(optarg);
                break;
            case 's':
                if(sflag != 0)
                    usage_exit(progname);
                if(dflag != 0 )
                    usage_exit(progname);
                sflag++;
                strcpy(sitename, optarg);
                break;
            case 'b':
                if(bflag != 0)
                    usage_exit(progname);
                if(fflag != 0 || lflag != 0)
                    usage_exit(progname);
                bflag++;
                strcpy(begin_time, optarg);
                break;
            case 'e':
                if(eflag != 0)
                    usage_exit(progname);
                if(fflag != 0 || lflag != 0)
                    usage_exit(progname);
                eflag++;
                strcpy(end_time, optarg);
                break;
            case 'f':
                if(fflag != 0)
                    usage_exit(progname);
                if(bflag != 0 || eflag != 0)
                    usage_exit(progname);
                fflag++;
                first_rev = atoi(optarg);
                break;
            case 'l':
                if(lflag != 0)
                    usage_exit(progname);
                if(bflag != 0 || eflag != 0)
                    usage_exit(progname);
                lflag++;
                last_rev = atoi(optarg);
                break;
            case 'p':
                if(pflag != 0)
                    usage_exit(argv[0]);
                pflag++;
                return_code = sscanf( optarg, "%d", &permission_id ) ;
                if( return_code != 1 )
                {
                    fprintf(stderr,
                    "%s(%d): error: %s :  permission id must be an integer\n",
                        __FILE__, __LINE__, optarg) ;
                    usage_exit(progname);
                }
                break;
            case '?':
                usage_exit(progname);
                break;
            default:
                /* do  nothing; case '?' take care of this...   */
                break;
        }

    if(Aflag > 0)
        strcpy(ascdsc, "A");
    if(Dflag > 0)
        strcpy(ascdsc, "D");
    if(Aflag == 0 && Dflag == 0)
        strcpy(ascdsc, "*");

    if(Pflag <= 0)
        usage_exit(progname);

    if(Uflag == 0)
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            printf(
    "ERROR:  sybase_userid not found in environment variable APS_SYBASE_USERID\n");
            printf("Use -U sybase_userid or setenv APS_SYBASE_USERID.\n\n");
            aps_log_msg(progname, APS_ERROR, 
    "ERROR:  sybase_userid not supplied; value not found in environment variable APS_SYBASE_USERID", 
                DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR);
        }
        else
            /* use the environment sybase_userid.   */
            strcpy(sybase_userid, env_sybase_userid);
    }

    /* both -b, -e must be used, or neither:  */
    if(bflag != eflag )
    {
        aps_log_msg(progname, APS_ERROR,
                "both -e and -e must be used, or neither.", 
                DO_SYSLOG, DO_PRINT);
        usage_exit(progname);
    }

    /* both -f, -l must be used, or neither:  */
    if(fflag != lflag )
    {
        aps_log_msg(progname, APS_ERROR,
                "both -f and -l must be used, or neither.", 
                DO_SYSLOG, DO_PRINT);
        usage_exit(progname);
    }

    /* either -b or -f must be used, not both; also, at least one must be used*/
    if(bflag == fflag)
    {
        aps_log_msg(progname, APS_ERROR, "either -b OR -f MUST be used.", 
                DO_SYSLOG, DO_PRINT);
        usage_exit(progname);
    }

    if(bflag == 1)
    {
        /* check for time order */
        if(strcmp(begin_time, end_time) >= 0)
        {
            printf("ERROR:  end_time must be AFTER begin_time.\n");
            aps_log_msg(progname, APS_ERROR,"end_time must be AFTER begin_time",
                DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR);
        }

        if (strcmp(begin_time, "1990:001:00:00:00.000" ) <= 0
        ||  strcmp(begin_time, "2100:001:00:00:00.000" ) >= 0 )
        {
            printf("ERROR:  begin_time out of range.\n");
            aps_log_msg(progname, APS_ERROR, "begin_time out of range", 
                DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR);
        }

        if (strcmp(end_time, "1990:001:00:00:00.000" ) <= 0
        ||  strcmp(end_time, "2100:001:00:00:00.000" ) >= 0 )
        {
            printf("ERROR:  end_time out of range.\n");
            aps_log_msg(progname, APS_ERROR, "end_time out of range", 
                DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR);
        }

    }

    if(fflag == 1)
    {
        /* check for rev order  */
        if(first_rev > last_rev)
        {
            printf("ERROR:  last_rev cannot be before first_rev.\n");
            printf("\tfirst_rev = %d \n\t last_rev = %d\n",
                first_rev, last_rev);
            aps_log_msg(progname, APS_ERROR, 
                "ERROR:  last_rev cannot be before first_rev.", 
                DO_SYSLOG, DO_PRINT);
            error_exit(progname, APS_EXIT_ERROR);
        }
    }
    /* either -d or -s must be used, not both; also, at least one must be used*/
    if(dflag == sflag)
    {
        aps_log_msg(progname, APS_ERROR, "either -d OR -s MUST be used.", 
                DO_SYSLOG, DO_PRINT);
        usage_exit(progname);
    }

    if(bflag != 0)
    {
        /* check the time values    */
        /*
            chasft_(begin_time, &rcode);
        */
        rcode = tc_validate_asf_datetime(begin_time) ;
        if(rcode < 0)
        {
            printf("Error in begin_time \n");
            aps_log_msg(progname, APS_ERROR, "Error in begin_time", 
                    DO_SYSLOG, DO_PRINT);
            usage_exit(progname);
        }
        /*
            chasft_(end_time, &rcode);
        */
        rcode = tc_validate_asf_datetime(end_time) ;
        if(rcode < 0)
        {
            printf("Error in end_time value\n" );
            aps_log_msg(progname, APS_ERROR, "Error in end_time", 
                    DO_SYSLOG, DO_PRINT);
            usage_exit(progname);
        }
    }


    /* now pick up the positional parameters satellite and sensor  */
    pcount = 0;
    for(; optind < argc; optind++)
    {
        if(pcount == 0)
        {
            strncpy(sat, argv[optind], 2);
            sat[2] = '\0';
        }
        if(pcount == 1)
        {
            strncpy(sensor, argv[optind], 3);
            sensor[3] = '\0';
        }
        pcount++;
    }
    if(pcount != 2)
        usage_exit(progname);

    /* 
    -- check to see if Coverage is allowed 
    -- for this sat/sensor:  
    */
    return_code = cvrg_allowed(sat, sensor ) ;
    if ( return_code != 0 )
    {
        sprintf(buf, 
"sensor coverage not allowed for sat/sensor %s/%s, can't create data-take opps",
        sat, sensor ) ;
        aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }

    /* get the database name from the environment   */
    env_dbname = getenv("APSDB");
    if(env_dbname == NULL)
    {
        /* database name not supplied   */
        printf("Environment variable APSDB not set.\n");
        printf("Use setenv APSDB dbname. \n");
        aps_log_msg(progname, APS_CRITICAL,
                    "Environment variable APSDB not set.", 
                    DO_SYSLOG, DO_PRINT);
        error_exit(progname, APS_EXIT_ERROR) ;
    }

    strcpy(dbname, env_dbname);

    /*
    -- now open the database.
    -- db_open will handle the errors.
    */
    APS_dbproc = db_open(dbname,progname,sybase_userid,password,NULL,
        error_handler_exit,&rcode);
    if(rcode != DB_OPEN_OK)
    {
        db_open_errs( rcode, dbname, sybase_userid);
        sprintf( buf, "ERROR:  dbname %s could not be opened", dbname ) ;
        aps_log_msg( progname, APS_CRITICAL, buf, DO_SYSLOG, DO_PRINT);
        error_exit (progname, APS_EXIT_ERROR);
    }

    /************************************************************************
    *                                                                       *
    *                 GET PERMISSIONS                                       *
    *                                                                       *
    ************************************************************************/
    if( darid > 0 )
    {
        /*
        -- mu_get_permission() takes care of 
        -- the logic and the syslog printing.  for 
        -- example, whether or not permission_id was 
        -- passed in the command line (!=0)
        */
        return_code = mu_get_permission( progname, APS_dbproc, 
            permission_id, mu_activity_type, mu_activity_id, 
            NULL, NULL, NULL, /* parameters not used.  */
            darid,
            0, 0 ) ;  /* parameters not used here (no retries).  */
        if ( return_code < 0 )
        {
            /* permission not granted/validated.  */
            error_exit (progname, APS_EXIT_ERROR);
        }
        /* permission obtained; set id.  */
        permission_id = return_code ;
    }
    else
    {
        /* 
        -- darid == 0:  
        -- no permission is needed to work 
        -- on a site.  
        */
        permission_id = 0 ;
    }
 
    /* 
    -- initialize the vector library; exit with 
    -- a message if an error   
    */
    rcode = init_vec_lib();
    if(rcode)
    {
        aps_log_msg(progname, APS_CRITICAL, "Error finding current Stoic File", 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        init_vec_lib_exit(rcode);
    }

    /* call SSCVBATCH.FOR, the specific site coverage program.  */
    int_APS_dbproc = (int) APS_dbproc ;

    printf("calling sscvbatch.for:  \n");
    printf("\tsat = %s, sensor = %s, darid = %d,\n", sat, sensor, darid);
    printf("\tsitename = %s, \n\tbegin_time = %s, end_time = %s,\n", 
        sitename, begin_time, end_time);
    printf("\tfirst_rev = %d, last_rev = %d, ascdsc = %s\n", 
        first_rev, last_rev, ascdsc);

    rcode = sscvbatch_( progname, &int_APS_dbproc, 
        sat, sensor, &darid, sitename, 
        begin_time, end_time,
        &first_rev, &last_rev, ascdsc );

    if(rcode != 0)
        error_exit(progname, APS_EXIT_ERROR) ;

    /*
    -- Still OK.
    -- if darid > 0, 
    -- Terminate the permission if it was NOT
    -- passed on the command line.
    -- (That is, if it was granted in this routine.)
    */
    if ( pflag == 0 && darid > 0 )
    {
        /*
        -- permission_id was NOT in command line; Terminate the
        -- permission_id; the permission was granted here.
        */
        return_code = mu_permission_terminate( APS_dbproc,
            permission_id,
            mu_activity_id,
            mu_activity_type ) ;
        if( return_code < 0 )
        {
            fprintf( stderr,
                "%s:\n\n%s\n", progname, MU_ERROR_MESSAGE(return_code) ) ;
            sprintf( buf, "ERROR:  %s", MU_ERROR_MESSAGE(return_code) ) ;
            aps_log_msg( progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit (progname, APS_EXIT_ERROR);
        }
        /* OK.  */
    }

    aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);
    
    banner_exit(APS_EXIT_OK);

}



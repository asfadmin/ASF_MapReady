#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		main.c

Description:	main source file for the CON dtks roundup,

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident	"@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/CON_roundup/SCCS/s.main.c"

/* 
-- to decode the error code into a string:  
*/
extern char *CON_roundup_error_message[] ;    /* see this table.  */

#include <stdio.h>
#include <stdlib.h>         /* for getopt                           */
#include <string.h>
#include "CON_roundup.h"
#include "timeconv.h"		/* for time validation  */
#include <aps_log_msg.h>    /* for syslog stuff                     */
#include <aps_defs.h>       /* for APS_EXIT_OK and APS_EXIT_ERROR   */
#include <mu_utilities.h>   /* for obtaining permissions        */

extern int CON_check_station_id();
extern int init_vec_lib();


void normal_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);
    exit(APS_EXIT_OK) ;

}
void error_exit(char *progname)
{
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR) ;

}

void usage_exit(char *progname)
{
    char    *env_dbname ;
    env_dbname = getenv("APSDB");

    fprintf(stderr,
"\nusage: %s [-U Sybase_userid] -P Sybase_password -s station_id \n        -b strttime -e stoptime\n", 
        progname);
    fprintf(stderr, 
    "\n    This program retrieves CON status data-takes that overlap\n" ) ;
    fprintf(stderr, 
    "    the input time bracket for the given station_id and re-submits the\n") ;
    fprintf(stderr, 
    "    data-takes for processing.  This is useful if time on an antenna \n") ;
    fprintf(stderr, 
    "    has been freed up and data-takes which have been conflicted or \n") ;
    fprintf(stderr, 
    "    bumped can be re-instated into the plans or schedules.\n");
    fprintf(stderr, 
    "    The database used is $APSDB" ) ;

    if ( env_dbname )
        fprintf(stderr, ", which = %s right now.\n\n", env_dbname ) ;
    else
        fprintf(stderr, ".\n\n" ) ;

    fprintf(stderr, "    Sybase_userid     Sybase account userid\n" ) ;
    fprintf(stderr, "    Sybase_password   Sybase account password\n" ) ;
    fprintf(stderr, "    station_id        { ASF | MCM }\n" ) ;

    fprintf(stderr, 
    "    strttime          start of time bracket in ASF format:  \n" ) ;
    fprintf(stderr, 
    "                      yyyy:ddd:hh:mm:ss.sss\n" ) ;
    fprintf(stderr, 
    "    stoptime          end of time bracket in ASF format.  \n" ) ;

    fprintf(stderr, "\n\n    %s version:  %s %s   ---------\n\n",
        progname, __DATE__, __TIME__ ) ;
    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    error_exit(progname) ;
}

/*==============================================================================
Function:       main()

Description:    main routine for the DON dtks roundup, in 
                which all CON dtks for a station_id within the time bracket 
                are retrieved and re-submitted to the dtk proposal 
                list processor.  this is done when some antenna time has 
                been freed up for some time bracket and maybe a previously 
                conflicted or bumped data-take can get on the freed antenna.  

Creator:        Lawrence Stevens

Creation Date:  Tue Jan  2 17:48:31 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

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
    int			retcode;
    
    char        *env_dbname;        /* dbname from environment      */
    char        *env_sybase_userid; /* userid from environment      */

    char        *strttime = NULL ;
    char        *stoptime = NULL ;
    char        *station_id = NULL ;

    int     	pflag = 0;  /* used to check for optional permission_id          */
    int     	Sflag = 0;  /* used to check for optional n_seconds_retry        */
    int     	Rflag = 0;  /* used to check for optional n_retries              */

    int	     	permission_id   = 0 ;  /* optional permission_id passed     */
    int     	n_seconds_retry = 0 ;  /* optional seconds between re-tries */
    int     	n_retries       = 0 ;  /* optional number of re-tries       */

    int         c;      /* used as return character from getopt()       */
     
    char        flag_list[20] = "U:P:s:b:e:p:R:S:"; /* list of flags for getopt  */
    
	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;

    aps_open_syslog();
    sprintf(buf, "Program started with arguments: " ) ;
    for( j = 1; j < argc; j++ )
    {
        strcat(buf, " " ) ;
        strcat(buf, argv[j] ) ;
    }
    aps_log_msg(argv[0], APS_INFO, buf, DO_SYSLOG, DO_PRINT);

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'P':
                if( password != NULL )
                    usage_exit(argv[0]);
                password = optarg ;
                break;
            case 'U':
                if( sybase_userid != NULL )
                    usage_exit(argv[0]);
                sybase_userid = optarg ;
                break;
            case 's':
                if(station_id != NULL)
                    usage_exit(argv[0]);
                station_id = optarg ;
                break;
            case 'b':
                if(strttime != NULL)
                    usage_exit(argv[0]);
                strttime = optarg ;
                break;
            case 'e':
                if(stoptime != NULL)
                    usage_exit(argv[0]);
                stoptime = optarg ;
                break;
			case 'p':
				/* -p is not allowed with either -R or -S   */
				if(Rflag != 0)
				   	usage_exit(argv[0]);
				if(Sflag != 0)
				  	usage_exit(argv[0]);
				if(pflag != 0)
				  	usage_exit(argv[0]);
				pflag++;
				retcode = sscanf( optarg, "%d", &permission_id ) ;
				if( retcode != 1 )
				{
				  	fprintf(stderr,
							"%s(%d): error: %s :  permission id must be an integer\n",
							__FILE__, __LINE__, optarg) ;
					usage_exit(argv[0]);
				}
				break;
			case 'S':
				/* -S is not allowed with -p   */
				if(pflag != 0)
				  	usage_exit(argv[0]);
				if(Sflag != 0)
				  	usage_exit(argv[0]);
				Sflag++;
				retcode = sscanf( optarg, "%d", &n_seconds_retry ) ;
				if( retcode != 1 )
            	{
				  	fprintf(stderr,
							"%s(%d): error: %s :  seconds between re-tries must be an integer\n",
							__FILE__, __LINE__, optarg) ;
					usage_exit(argv[0]);
            	}
				if ( n_seconds_retry <= 0 )
            	{
                	fprintf(stderr,
							"%s(%d): error: %d :  seconds between re-tries must be > 0\n",
							__FILE__, __LINE__, n_seconds_retry ) ;
					usage_exit(argv[0]);
            	}
				break;
		    case 'R':
				/* -R is not allowed with -p   */
				if(pflag != 0)
                	usage_exit(argv[0]);
				if(Rflag != 0)
                	usage_exit(argv[0]);
				Rflag++;
				retcode = sscanf( optarg, "%d", &n_retries ) ;
				if( retcode != 1 )
            	{
                	fprintf(stderr,
							"%s(%d): error: %s :  number of re-tries must be an integer\n",
							__FILE__, __LINE__, optarg) ;
					usage_exit(argv[0]);
            	}
				if( n_retries <= 0 )
            	{
                	fprintf(stderr,
							"%s(%d): error: %d :  number of re-tries must be an > 0\n",
							__FILE__, __LINE__, n_retries) ;
					usage_exit(argv[0]);
            	}
				break;
            case '?':
                usage_exit(argv[0]);
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
        usage_exit(argv[0]);

    /* mandatory flags:  */
    if ( password == NULL )
        fprintf(stderr, "%s:\n\nSybase_password not given.\n", argv[0] ) ;
    if ( station_id  == NULL )
        fprintf(stderr, "%s:\n\nstation_id not given.\n", argv[0] ) ;
    if ( strttime  == NULL )
        fprintf(stderr, "%s:\n\nstrttime not given.\n", argv[0] ) ;
    if ( stoptime  == NULL )
        fprintf(stderr, "%s:\n\nstoptime not given.\n", argv[0] ) ;
    if ( password   == NULL 
    ||   station_id == NULL 
    ||   strttime   == NULL 
    ||   stoptime   == NULL )
    {
        aps_log_msg(argv[0], APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
        error_exit (argv[0]) ;
    }

	/*
	-- The retry flag (-R) and the "delay in seconds" flag (-S) must be both 
	-- set or unset, otherwise there is an error in the retry logic.
	*/
	if (Sflag != Rflag )
	  	usage_exit(argv[0]);

    /*
    -- All of the values have been obtained from the
    -- command line.  
    */

    if ( tc_validate_asf_datetime(strttime) < 0 )
    {
        fprintf(stderr,"%s:\n\nerror in strttime, %s\n", argv[0], strttime ) ;
        aps_log_msg(argv[0], APS_ERROR, "error in strttime", 
            DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

    if ( tc_validate_asf_datetime(stoptime) < 0 )
    {
        fprintf(stderr,"%s:\n\nerror in stoptime, %s\n", argv[0], stoptime ) ;
        aps_log_msg(argv[0], APS_ERROR, "error in stoptime", 
            DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

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
            fprintf(stderr, 
                "%s:\n\nERROR:  sybase_userid not given; \n        environment variable APS_SYBASE_USERID not set\n", 
                argv[0]);
            aps_log_msg(argv[0], APS_CRITICAL, 
                "ERROR:  sybase_userid not given; environment variable APS_SYBASE_USERID not set", DO_SYSLOG, DO_PRINT);
            error_exit(argv[0]) ;
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
        fprintf(stderr, 
"%s:\n\nERROR:  environment variable APSDB not set; cannot open a database\n", 
            argv[0] ) ;
        fprintf(stderr, "    Use setenv APSDB <dbname>. \n");

        aps_log_msg(argv[0], APS_CRITICAL, 
        "ERROR:  environment variable APSDB not set; cannot open a database", 
            DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }
    dbname = env_dbname ;
 
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
        -- remove it.  reset the progname pointer to 
        -- the next character after the last slash:  
        */
        progname = slash_ptr + 1 ;
    }

    /* now open the database.  */
    /* db_open will handle the errors.      */
    APS_dbproc = db_open( dbname, progname, sybase_userid, password, NULL,
        error_handler_exit, &return_code);
    if(return_code != DB_OPEN_OK)
    {
        fprintf( stderr, "%s:\n\nERROR opening database '%s'\n", argv[0], 
            dbname ) ;
        db_open_errs(return_code, dbname, sybase_userid);
        sprintf( buf, "ERROR opening database '%s'", dbname ) ;
        aps_log_msg( argv[0], APS_CRITICAL, buf, DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

    /*
    -- now that we know the dbname is OK, we can 
    -- use the db to validate the station_id.  
    */
    if ( ! CON_check_station_id( station_id ) )
    {
        fprintf(stderr, "%s:\n\nERROR:  station_id '%s' not valid\n", argv[0],
            station_id ) ;
        sprintf( buf, "ERROR:  station_id '%s' not valid", station_id ) ;
        aps_log_msg( argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

	/*
	-- Get permissions.
	*/

	retcode = mu_get_permission(progname, 
								APS_dbproc, 
								permission_id,
								MU_PLANNING_ACTIVITY_TYPE,
								MU_CON_ROUNDUP,
								strttime, 
								stoptime, 
								station_id, 
								0,     /* dar_id not used        */
								n_retries,
								n_seconds_retry ) ;
	if ( retcode < 0 )
	  	error_exit(argv[0]) ;

	permission_id = retcode;

	/* permission was obtained; now proceed.  */	

    if ( init_vec_lib() )
    {
        fprintf(stderr, "%s:\n\nerror initializing the Stoic file.\n" ) ;
        fprintf(stderr, "     check for existance, permissions, and \n" ) ;
        fprintf(stderr, "     value of environment variable APS_DATA\n" ) ;
        aps_log_msg(argv[0], APS_CRITICAL, "error initializing the Stoic file", 
            DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

    return_code = CON_roundup( APS_dbproc, station_id, strttime, stoptime, 
        stdout ) ;

    if ( return_code < -3000 )
    {
        fprintf(stderr, 
            "%s:\n\nERROR encountered in CON data-takes roundup.\n", 
            argv[0] ) ;
        fprintf(stderr, "    %s\n", CON_ROUNDUP_ERROR_MESSAGE( return_code ) ) ;
        aps_log_msg(argv[0], APS_ERROR, 
            CON_ROUNDUP_ERROR_MESSAGE( return_code ), DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

    if ( return_code < 0 )
    {
        fprintf(stderr, 
            "%s:\n\nERROR encountered in CON data-takes roundup.\n", argv[0] ) ;
        fprintf(stderr, "    %s\n", DTKM_ERROR_MESSAGE( return_code ) ) ;
        aps_log_msg(argv[0], APS_ERROR, DTKM_ERROR_MESSAGE( return_code ), 
            DO_SYSLOG, DO_PRINT);
        error_exit(argv[0]) ;
    }

    fprintf(stdout, 
        "%s:  CON data-takes roundup completed OK.\n", argv[0] ) ;

    normal_exit(argv[0]) ;

}


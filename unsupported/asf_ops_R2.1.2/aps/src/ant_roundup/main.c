#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   main.c

Description:    main source file for the antenna down times roundup,

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/ant_roundup/SCCS/s.main.c"

/* 
-- to decode the error code into a string:  
*/
extern char *ant_roundup_error_message[] ;    /* see this table.  */

#include <stdio.h>
#include <stdlib.h>         /* for getopt           */
#include <string.h>
#include "ant_roundup.h"
#include "timeconv.h"		/* for time validation  */
#include <aps_log_msg.h>    /* for syslog stuff.    */
#include <aps_defs.h>       /* for APS_EXIT_OK, APS_EXIT_ERROR  */
#include <mu_utilities.h>   /* for obtaining permissions        */

extern int ant_check_station_antenna( 
    char    *station_id, 
    int     antenna_id ) ;

extern int init_vec_lib();


void usage_exit(char *progname)
{
    char    *env_dbname ;
    env_dbname = getenv("APSDB");

    fprintf(stderr,
	"\nusage: %s [-U Sybase_userid] -P Sybase_password -s station_id\n", progname);
    fprintf(stderr,
	"        -b strttime -e stoptime [ -p <permission id> | \n", progname);
    fprintf(stderr,
	"        { -R <number of retries> -S <seconds between retries> } ]\n", progname);
	fprintf(stderr, 
    "\n    This program implements the effect of any recent antenna status\n") ;
    fprintf(stderr, 
    "    change within the input time bracket.  To do this, every CON, QUE,\n");
    fprintf(stderr, 
    "    SUB, PLN, and SCH downlink data-take at the given station is\n");
    fprintf(stderr, 
    "    retrieved within the input time bracket.  These data-takes are \n");
    fprintf(stderr, 
	"    then re-submitted so that the antenna_pref db table will determine\n");
    fprintf(stderr, 
	"    the preferred antenna for each data-take.  All recent antenna \n");
    fprintf(stderr, 
	"    status changes will then take effect.  \n");
    fprintf(stderr, 
    "    The database used is $APSDB" ) ;

    if ( env_dbname )
        fprintf(stderr, ", which = %s right now.\n\n", env_dbname ) ;
    else
        fprintf(stderr, ".\n\n" ) ;

    fprintf(stderr, 
    "    Sybase_userid     Sybase account userid\n" ) ;
    fprintf(stderr, 
    "    Sybase_password   Sybase account password\n" ) ;
    fprintf(stderr, 
    "    station_id        { ASF | MCM }\n" ) ;
    fprintf(stderr, 
    "    strttime          start of time bracket in ASF format:  \n                      yyyy:ddd:hh:mm:ss.sss\n" ) ;
    fprintf(stderr, 
    "    stoptime          end of time bracket in ASF format.  \n" ) ;
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

    fprintf(stderr, "\n\n    %s version:  %s %s   ---------\n\n",
        progname, __DATE__, __TIME__ ) ;

    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR) ;
}

/*==============================================================================
Function:       main()

Description:    main routine for the antenna down times roundup, in 
                which, for a station and time bracket, all CON, QUE, 
                SUB, PLN, and SCH dtks within the time bracket 
                are retrieved and re-submitted to the dtk proposal 
                list processor.  this is done so that the change 
                in the antenna status can have the desired effect 
                on the appropriate data-takes.  

It is kind of like this:  
Think of an antenna schedule as a long telephone wire.  Each yard is 
one pass.  A crow sitting on the wire is a data-take scheduled for that 
antenna.  If a crow can't find room on any wire, it just flys down to the 
ground and sits there.  This is like a CON data-take.  

Suppose there are two long telephone wires side by side.  Each represents 
a different antenna.  Some data-takes prefer one antenna over the other.  
So that some crows prefer one telephone wire, other crows prefer the other 
wire.  However, some crows have a higher priority on one telephone 
wire and a lower priority on the other telephone wire.  Some data-takes 
have a higher priority on one antenna and a lower priority on the other 
antenna.  

Some data-takes cannot get onto an antenna due to conflicts; they have a 
CON status.  Some crows cannot get onto either telephone wire because 
there isn't room enough between the crows.  These crows have to go down 
to the ground and just sit there.  They envy the crows on the wires; the 
crows on the wires know this.  

When an antenna goes down, this is like the telephone wire going down.  
The crows all fly up off the telephone wire and try to get onto the 
other telephone wire.  Those that connot make it to the other wire 
must go down to the ground.  

But when a telephone wire comes back up, we want ALL of the crows to 
wake up and fly around and then find their preferred telephone wire.  

IMPORTANT CASE:  
So if there are a bunch of crows sitting on preferred wire 2, and if 
wire 2 goes down, they all fly over to wire 1, assuming that there is 
room.  If not, some crows may go sit on the ground.  Now if wire 2 comes 
back up, we need the crows on the ground AND the crows on wire 1 to go 
fly up and try to get back on wire 2, if that is their preferred wire.  

THE RULE:
So, for simplicity, whenever we run this command, we always wake up 
every crow, bang the drum, fire a shotgun (with blanks only.  If we shoot a 
crow, this is like a REJ status.) make a lot of noise, enough to wake 
up the crows on the ground and on every telephone wire in the ground 
station.  We wake them all up and then they will fly up and try to get 
onto their preferred telephone wire.  We don't let any crow within the 
time bracket remain asleep.  It can remain dead, though (REJ).  We won't 
wake up the dead crows.  

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
                if( station_id != NULL )
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
    if ( station_id == NULL )
        fprintf(stderr, "%s:\n\nstation_id not given.\n", argv[0] ) ;
    if ( strttime  == NULL )
        fprintf(stderr, "%s:\n\nstrttime not given.\n", argv[0] ) ;
    if ( stoptime  == NULL )
        fprintf(stderr, "%s:\n\nstoptime not given.\n", argv[0] ) ;
    if ( password == NULL
    ||   station_id == NULL  
    ||   strttime  == NULL 
    ||   stoptime  == NULL )
        usage_exit(argv[0]) ;

	/*
	-- The retry flag (-R) and the "delay in seconds" flag (-S) must be both 
	-- set or unset, otherwise there is an error in the retry logic.
	*/
	if (Sflag != Rflag )
	  	usage_exit(argv[0]);

    /*
    -- all of the values have been obtained from the
    -- command line.  
    */

    if ( tc_validate_asf_datetime(strttime) < 0 )
    {
        fprintf(stderr,"%s:\n\nerror in strttime, %s\n", argv[0], strttime ) ;
        sprintf(buf, "error in strttime value, %s", strttime ) ;
        aps_log_msg(argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_EXIT_ERROR) ;
    }

    if ( tc_validate_asf_datetime(stoptime) < 0 )
    {
        fprintf(stderr,"%s:\n\nerror in stoptime, %s\n", argv[0], stoptime ) ;
        sprintf(buf, "error in stoptime value, %s", stoptime ) ;
        aps_log_msg(argv[0], APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_EXIT_ERROR) ;
    }

    /* optional flag    */
    if(sybase_userid == NULL)
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            /* userid not supplied at all   */
            fprintf(stderr, "%s:\n\nERROR:  sybase_userid not given\n", 
                argv[0]);
            aps_log_msg(argv[0], APS_CRITICAL,"ERROR:  sybase_userid not given; environment variable APS_SYBASE_USERID not set.",
                DO_SYSLOG, DO_PRINT);
            aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
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
        fprintf(stderr,
            "%s:\n\nERROR:  environment variable APSDB not set; cannot upen DB\n",
            argv[0] );
        fprintf(stderr, "Use setenv APSDB <dbname>. \n\n");
        aps_log_msg(argv[0], APS_CRITICAL,
            "ERROR:  environment variable APSDB not set; cannot upen DB",
            DO_SYSLOG, DO_PRINT);
        usage_exit(argv[0]);
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
        db_open_errs(return_code, dbname, sybase_userid);
        sprintf( buf, "DB '%s' could not be opened", dbname ) ;
        aps_log_msg(argv[0], APS_CRITICAL, buf, DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit (APS_ERROR) ;
    }

	/*
	-- Get permissions.
	*/

	/* 
	-- We want to just validate the permission id if it's given, not attempt
	-- to get a new one.  If we did that, we would fail to get permission.
	-- In order to validate the permission id, we pass NULL values for strttime,
	-- stoptime, and station_id.
	*/
	if (permission_id > 0)
	  	retcode = mu_get_permission(progname, 
									APS_dbproc, 
									permission_id,
									MU_PLANNING_ACTIVITY_TYPE,
									MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
									NULL, 
									NULL, 
									NULL,
									0,     /* dar_id not used        */
									n_retries,
									n_seconds_retry ) ;
	else
		retcode = mu_get_permission(progname, 
									APS_dbproc, 
									permission_id,
									MU_PLANNING_ACTIVITY_TYPE,
									MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
									strttime, 
									stoptime, 
									station_id, 
									0,     /* dar_id not used        */
									n_retries,
									n_seconds_retry ) ;
	if ( retcode < 0 )
	  	exit(APS_ERROR) ;

	permission_id = retcode;

	/* permission was obtained; now proceed.  */	

    if ( init_vec_lib() )
    {
        fprintf(stderr, "%s:\n\nERROR initializing the Stoic file.\n",
            argv[0] ) ;
        fprintf(stderr, "     check for existance, permissions, and \n" ) ;
        fprintf(stderr, "     value of environment variable APS_DATA\n" ) ;
        aps_log_msg(argv[0], APS_CRITICAL, "ERROR initializing the Stoic file", 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_ERROR) ;
    }

    return_code = ant_roundup( APS_dbproc, station_id, 
        strttime, stoptime, stdout ) ;

    if ( return_code < -2000 )
    {
        fprintf(stderr, 
            "%s:\n\nERROR encountered in antenna down times roundup:\n", 
            argv[0] ) ;
        fprintf(stderr, "%s\n", ANT_ROUNDUP_ERROR_MESSAGE( return_code ) ) ;

        aps_log_msg(argv[0], APS_ERROR, ANT_ROUNDUP_ERROR_MESSAGE(return_code), 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);

        exit(APS_ERROR) ;
    }

    if ( return_code < 0 )
    {
        fprintf(stderr, 
            "%s:\n\nERROR encountered in antenna down times roundup:\n", 
            argv[0] ) ;
        fprintf(stderr, "%s\n", DTKM_ERROR_MESSAGE( return_code ) ) ;
        aps_log_msg(argv[0], APS_ERROR, DTKM_ERROR_MESSAGE(return_code), 
            DO_SYSLOG, DO_PRINT);
        aps_log_msg(argv[0], APS_INFO,  "Program terminated abnormally.", 
            DO_SYSLOG, DO_PRINT);
        exit(APS_ERROR) ;
    }

    fprintf(stdout, 
        "%s:  Antenna down times roundup completed OK.\n", argv[0] ) ;

    aps_log_msg(argv[0], APS_INFO,  "Program completed successfully.", 
        DO_SYSLOG, DO_PRINT);

    exit(APS_EXIT_OK) ;

}


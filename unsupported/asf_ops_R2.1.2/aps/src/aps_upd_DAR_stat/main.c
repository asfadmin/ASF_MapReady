/*  #define TEST_ONLY    */
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   main.c

Description:    main source file for the APS reject DAR executable.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_upd_DAR_stat/SCCS/s.main.c"

/* 
-- to decode the error code into a string:  
*/
extern char *crt_dar_dtk_error_message[] ;    /* see this table.  */

#include <stdio.h>
#include <stdlib.h>         /* for getopt           */
#include <string.h>         /* for strcat() etc.    */
#include "timeconv.h"       /* for time validation  */
#include <aps_log_msg.h>    /* for syslog stuff.    */
#include <aps_defs.h>       /* for APS_EXIT_OK, APS_EXIT_ERROR  */
#include <dapps_defs.h>     /* for ASF_TIME_STR_LENGTH          */
#include <apspath.h>        /* for APS_REPORTS                  */
#include <mu_utilities.h>   /* for obtaining permissions        */
#include "aps_upd_DAR_stat.h"

char	*progname = NULL ;
char    *sybase_userid = NULL ;
char    *password = NULL ;


static void usage_exit(char *progname)
{
    char    *env_dbname ;
    env_dbname = getenv("APSDB");

    (void)fprintf(stderr,
    "\nusage: %s [-U Sybase_userid] -P Sybase_password -p platform\n", 
        progname);
    (void)fprintf(stderr,
    "        -b <begin time> -e <end time> [-o -c comment]\n\n" );
    (void)fprintf(stderr,
    "        This program removes all DARs within the given time\n" );
    (void)fprintf(stderr,
    "        frame and platform from both the APS and IMS relations.\n" );
    (void)fprintf(stderr,
    "        This DAR will be marked as rejected (REJ).\n\n" );
    (void)fprintf(stderr,
    "        -o  This will turn permission checking OFF.\n" );
    (void)fprintf(stderr,
    "        -c  This should be followed by a comment that will be appended \n" );
    (void)fprintf(stderr,
    "            to the Planner Comment field.  The comment should be a text \n" );
    (void)fprintf(stderr,
    "            string contained in quotes (\"\").\n\n" );
    (void)fprintf(stderr,
    "        \n" );

    (void)fprintf(stderr, "\n\n    %s version:  %s %s   ---------\n\n",
        progname, __DATE__, __TIME__ ) ;

    (void)fprintf(stderr, 
    "    The database used is $APSDB" ) ;

    if ( env_dbname )
        (void)fprintf(stderr, ", which = '%s' right now.\n\n", env_dbname ) ;
    else
        (void)fprintf(stderr, ".\n\n" ) ;

    aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
        DO_SYSLOG, DO_PRINT);
    exit (APS_EXIT_ERROR) ;
}


/*==============================================================================
Function:       main()

Description:    main routine for the APS reject DAR routine, where DARs are
		marked as rejected ("REJ") and deleted from both the APS and
		IMS DAR relations.  

Creator:        Philip Yurchuk

Creation Date:  8-13-97

==============================================================================*/
int main(int argc, char *argv[])
{

    DBPROCESS   *APS_dbproc ;
    char        *env_dbname;        /* dbname from environment      */
    char        *dbname = NULL ;
    char        *env_sybase_userid;      /* userid from environment */
    char        *platform = NULL ;
    char        *begintime = NULL ;
    char        *endtime = NULL ;
    char        *comment = NULL ;

    char        *slash_ptr ;

    char        buf[256] ;
    int         j ;

    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;

    RETCODE     return_code;

    int         c;      /* used as return character from getopt()          */

    char        flag_list[50] = "p:U:P:c:ob:e:";        /* list for getopt */

    int pflag = 0 ;   /* used to check for mandatory platform flag */
    int bflag = 0 ;   /* used to check for mandatory start time flag */
    int eflag = 0 ;   /* used to check for mandatory stop time flag */
    int oflag = 0 ;   /* used to check for optional permission checking flag */
    int cflag = 0 ;   /* used to check for optional comment flag */


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
            case 'U':
                if( sybase_userid != NULL )
                    usage_exit(progname);
                sybase_userid = optarg ;
                break;

            case 'P':
                if( password != NULL )
                    usage_exit(progname);
                password = optarg ;
                break;

            case 'p':
                if( platform != NULL )
                    usage_exit(progname);
                platform = optarg ;
				pflag++ ;
                break;

            case 'b':
		if(bflag != 0)
		    usage_exit(progname);
		bflag++;
		begintime = optarg ;
		if ( tc_validate_asf_datetime( begintime ) != TRUE )
		{
		    (void) fprintf(stderr,
			    "%s(%d): error: %s :  begin time not valid\n",
			    __FILE__, __LINE__, begintime) ;
		    usage_exit(progname);
		}
		break;

	    case 'e':
		if(eflag != 0)
		    usage_exit(progname);
		eflag++;
		endtime = optarg ;
		if ( tc_validate_asf_datetime( endtime ) != TRUE )
		{
		    (void) fprintf(stderr,
			    "%s(%d): error: %s :  end time not valid\n",
			    __FILE__, __LINE__, endtime) ;
		    usage_exit(progname);
                }
		break;

	    case 'c':
		if(cflag != 0)
		    usage_exit(progname);
		cflag++;
		comment = optarg ;
		break;

	    case 'o':
		if(oflag != 0)
		    usage_exit(progname);
		oflag++;
		break;
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

    if ( pflag == 0 )
    {
        (void)fprintf(stderr, "%s:\n\nPlatform not given.\n", progname);
        usage_exit(progname) ;
    }

    if ( bflag == 0 )
    {
        (void)fprintf(stderr, "%s:\n\nBegin time not given.\n", progname);
        usage_exit(progname) ;
    }

    if ( eflag == 0 )
    {
        (void)fprintf(stderr, "%s:\n\nEnd time not given.\n", progname);
        usage_exit(progname) ;
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

    return_code = aps_upd_DAR_stat(progname,
				   APS_dbproc,
				   platform,
				   begintime,
				   endtime,
				   comment,
				   oflag) ;
	
	if (return_code < 0)  /* An error occurred */
	{
    	switch (return_code)
    	{
    		case APS_UPD_DAR_STAT_ERROR_DB_QUERY_ERROR:
		  		(void) sprintf( buf, "An error occurred while querying the database." ) ;
				aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
				aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
							DO_SYSLOG, DO_PRINT);
				break ;
				
			case APS_UPD_DAR_STAT_ERROR_PERMISSION_ERROR:
				(void) sprintf( buf, "An error occurred while attempting to get permission." ) ;
				aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);
				aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
							DO_SYSLOG, DO_PRINT);
				break ;

			default:
				aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
							DO_SYSLOG, DO_PRINT);
				break ;
		}
		exit(APS_ERROR) ;
	}	
	else
	  	aps_log_msg(progname, APS_INFO,  "Program completed successfully.", 
					DO_SYSLOG, DO_PRINT);

	exit(APS_EXIT_OK) ;

    return 1 ;  /* for the lint man.  */

}


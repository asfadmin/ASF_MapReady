#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       create_nominal_cov.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)create_nominal_cov.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cnomcov/SCCS/s.create_nominal_cov.c"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <db_sybint.h>
#include <aps_defs.h>
#include <aps_log_msg.h>
#include <mu_utilities.h>

extern int init_vec_lib();

int banner_exit(int) ;

int cvrg_allowed(
    char    *sat,       /* input satellite   */
    char    *sensor ) ; /* input sensor.   */


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
static usage_exit(char *progname, int code )
{
printf(
"usage:  %s  [-U sybase_userid] -P password\n", progname );
printf(
"           [ {  -p <Permission id> \n");
printf(
"              | -R <number of Re-tries> -S <Seconds between re-tries>  } ]\n");
printf(
"        -b begin_time -e end_time [-M] [{-F | -q}]  satellite sensor\n\n");

printf("\tNOTES:\n");
printf("\t<permission id> is an integer indicating an existing permission.\n");
printf("\t                If not provided, -R can be used.\n");
printf("\t<number of Re-tries> is an integer indicating the number of extra\n");
printf("\t                     attempts to get permission if the first try\n");
printf("\t                     fails.  -S is required with this parameter.\n");
printf("\t<seconds between re-tries> is an integer indicating the amount \n");
printf("\t                           of time between permission attempts.\n");
printf("\t-M is used for station Mask (non-global) coverage.\n");
printf("\t-F indicates a \"File Run\" in which case the function\n");
printf("\t   ALSO writes coverage data to an ascii file.\n");
printf("\t-q indicates \"Quick Coverage\" in which case the function\n");
printf("\t   ONLY writes coverage data to an ascii file.\n");
printf("\t-b and -e provide the Begin and End times for the run.\n");
printf("\nThe following example creates nominal coverage for E1 SAR in\n" ) ; 
printf("the ASF and McMurdo masks:\n");
printf("\t%s  -P password     \\\n", progname);
printf("\t-b 1993:357:17:39:06.043 -e 1993:360:17:39:06.043 \\\n");
printf("\t-M   E1 SAR\n");
printf("\n\t[%s Version dated %s %s]\n\n", progname, __DATE__, __TIME__ ) ;

aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);

aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
    DO_SYSLOG, DO_PRINT);

exit (code);

}
/**********************************************************************
* Name : main
*
* Purpose: handles the command line arguments for create_nominal_coverage.for,
*           the main routine for create nominal coverage. 
*           it initiates coverage, a stand-alone executable.
*
*  Exit Parameter:
*  Type         Description
*  int          0   no errors.
*               1   there was an error.  
*
*  NOTE:    if there is an error in the command line; missing argument(s) etc.  
*           then this routine will print the usage info and exit.  
*
******************************************************************************
* 
*  Modification History:
*  Author   Revision    Date
*
*  larry                Wed Apr  5 18:14:37 PDT 1995   change to input args.
*
****************************************************************************/
int main( int   argc, char  *argv[])
{

DBPROCESS   *APS_dbproc ;
int         rcode ;

char    *progname ;
int     int_APS_dbproc ; 

int     j ;
char    dbname[100];
char    sybase_userid[100];
char    password[100];
char    begintime[30];
char    endtime[30];
char    mask[30];
int     quick_flag;  /* if 1, then write ascii cvrg FILE only.  */
int     file_flag;   /* if 1, then output ascii cvrg FILE also.    */
char    sat[30];
char    sensor[30];
int     return_code;

int     step_size;           /* minutes between coverage points           */
char    replication_flag[2]; /* "Y" means to replicate from nominal orbit */

/* coverage program in fortran  */
extern int      coverage_(char *progname, int *dbproc, 
                    char *begintime, char *endtime, 
                    char *mask, 
                    char *sat, char *sensor, 
                    int *step_size, int *quick_flag, int *file_flag, 
                    char *replication_flag ) ;

/* get the sybase_userid (optional) and password (mandatory)    */
/* int  getopt(int argc, char * const *argv, char *optstring);  */
extern  char    *optarg;
extern  int     optind ;
/*  extern  char*   getenv(char*);  */

int     c;      /* used as return character from getopt()       */

int     Fflag = 0;  /* used to check for getting both file AND DB data.  */
int     Pflag = 0;  /* used to check for mandatory password     */
int     Uflag = 0;  /* used to check for optional sybase_userid */
int     bflag = 0;  /* used to check for manditory start time.  */
int     eflag = 0;  /* used to check for manditory stop time.   */
int     qflag = 0;  /* checks for "quick coverage flag" (file data only). */
int     Mflag = 0;  /* used to check for optional mask flag.    */
int     pflag = 0;  /* used to check for optional permission_id          */
int     Sflag = 0;  /* used to check for optional n_seconds_retry        */
int     Rflag = 0;  /* used to check for optional n_retries              */

char    flag_list[100] = "p:S:R:FqMP:U:b:e:";  /* list of flags for getopts    */
int     pcount;     /* count of cmd line positional parameters; should be 2 */

char    *env_dbname;        /* dbname from environment      */
char    *env_sybase_userid; /* userid from environment      */
int     permission_id   = 0 ;  /* optional permission_id passed     */
int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
int     n_retries       = 0 ;  /* optional number of re-tries       */
char    *station_id = "ALL" ;

char    buf[200];

/* create nominal coverage is a planning activity.  */
char    *mu_activity_type = MU_PLANNING_ACTIVITY_TYPE ;
char    *mu_activity_id   = MU_CREATE_NOMINAL_COVERAGE ;

/*
-- unbuffer standard output, to watch
-- while program is executing.
*/
setbuf( stdout, (char *) NULL ) ;

aps_open_syslog();
sprintf(buf, "Program started with arguments: " ) ;
for( j = 1; j < argc; j++ )
{
    strcat(buf, " " ) ;
    strcat(buf, argv[j] ) ;
}

/* find pointer to last occurrence of '/', if any, in the string:  */
progname = strrchr( argv[0], '/' ) ;
if( progname == NULL )
    progname = argv[0] ;
else
    progname ++ ;

aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);

while ((c = getopt(argc, argv, flag_list)) != EOF)
    switch (c)
    {
        case 'F':
            if(Fflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            if(qflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Fflag++;
            break;
        case 'P':
            if(Pflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Pflag++;
            strcpy(password, optarg);
            break;
        case 'U':
            if(Uflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Uflag++;
            strcpy(sybase_userid, optarg);
            break;
        case 'b':
            if(bflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            bflag++;
            strcpy(begintime, optarg);
            if ( tc_validate_asf_datetime( begintime ) != TRUE )
            {
                fprintf(stderr,
                    "%s(%d): error: %s :  begin time not valid\n",
                    __FILE__, __LINE__, begintime) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
            break;
        case 'e':
            if(eflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            eflag++;
            strcpy(endtime, optarg);
            if ( tc_validate_asf_datetime( endtime ) != TRUE )
            {
                fprintf(stderr,
                    "%s(%d): error: %s :  end time not valid\n",
                    __FILE__, __LINE__, endtime) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
            break;
        case 'M':
            if(Mflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Mflag++;
            break;
        case 'q':
            if(qflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            if(Fflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            qflag++;
            break;
        case 'p':
            /* -p is not allowed with either -R or -S   */
            if(Rflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            if(Sflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            if(pflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            pflag++;
            return_code = sscanf( optarg, "%d", &permission_id ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
                    "%s(%d): error: %s :  permission id must be an integer\n",
                    __FILE__, __LINE__, optarg) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
            break;
        case 'S':
            /* -S is not allowed with -p   */
            if(pflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            if(Sflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Sflag++;
            return_code = sscanf( optarg, "%d", &n_seconds_retry ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
"%s(%d): error: %s :  seconds between re-tries must be an integer\n",
                    __FILE__, __LINE__, optarg) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
            if ( n_seconds_retry <= 0 )
            {
                fprintf(stderr,
"%s(%d): error: %d :  seconds between re-tries must be > 0\n",
                    __FILE__, __LINE__, n_seconds_retry ) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
            break;
        case 'R':
            /* -R is not allowed with -p   */
            if(pflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            if(Rflag != 0)
                usage_exit(progname, APS_EXIT_ERROR);
            Rflag++;
            return_code = sscanf( optarg, "%d", &n_retries ) ;
            if( return_code != 1 )
            {
                fprintf(stderr,
"%s(%d): error: %s :  number of re-tries must be an integer\n",
                    __FILE__, __LINE__, optarg) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
            if( n_retries <= 0 )
            {
                fprintf(stderr,
"%s(%d): error: %d :  number of re-tries must be an > 0\n",
                    __FILE__, __LINE__, n_retries) ;
                usage_exit(progname, APS_EXIT_ERROR);
            }
 
            break;

        case '?':
            usage_exit(progname, APS_EXIT_ERROR);
            break;
        default:
            /* do  nothing  */
            break;
    }

/*
--     -S and -R, if given, require each other.
*/
if(Sflag != Rflag )
    usage_exit(progname, APS_EXIT_ERROR);
 
/*
--     -P is manditory
*/
if(Pflag <= 0)
    usage_exit(progname, APS_EXIT_ERROR);

if(Uflag == 0)
{
    /* sybase_userid not supplied in command line.      */
    /* obtain from the environment:                     */
    env_sybase_userid = getenv("APS_SYBASE_USERID");
    if(env_sybase_userid == NULL)
    {
        /* userid not supplied at all   */
        printf("environment variable APS_SYBASE_USERID not set.\n");
        printf("Use -U in command line or setenv. \n"); 
        aps_log_msg(progname, APS_ERROR, 
        "Environment variable APS_SYBASE_USERID not set.  Use -U Sybase_userid",
        DO_SYSLOG, DO_PRINT);
        usage_exit(progname, APS_EXIT_ERROR);
    }
    else
    {
        /* use the environment sybase_userid.   */
        strcpy(sybase_userid, env_sybase_userid);
    }
}

if(bflag != 1)
    usage_exit(progname, APS_EXIT_ERROR);
if(eflag != 1)
    usage_exit(progname, APS_EXIT_ERROR);

/*
-- if ASCII file only is desired, use -q (quick coverage) 
-- if Database AND ASCII file are desired, use -F 
-- if both are desired, use neither flag.  
--
--      -q      ASCII FILE  ( to be backwards compatible )
--      -F      cvrg db data and ASCII FILE ( to make the default db only )
--    no flag  cvrg db data  ( to make the current GUI give db only) 
--
*/
if ( qflag && Fflag )
    usage_exit(progname, APS_EXIT_ERROR);

/* 
-- set quick_flag from argument; if not 
-- given, value = 0.   
-- 1 -> write ascii cvrg file but not DB data.  
*/
quick_flag = qflag;

/* 
-- set file_flag from argument; if 
-- not given, value = 0.   
-- 1 -> write ascii cvrg file.  
*/
file_flag = Fflag;

/* set default mask if none provided.       */
if(Mflag == 0)
    strcpy(mask, "GBL");
else
    strcpy(mask, "MSK") ;

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
    if(pcount > 2)
        usage_exit(progname, APS_EXIT_ERROR);
}

if(pcount != 2)
    usage_exit(progname, APS_EXIT_ERROR);

/* get the database name from the environment   */
env_dbname = getenv("APSDB");
if(env_dbname == NULL)
{
    /* database name not supplied   */
    printf("Environment variable APSDB not set.\n");
    printf("Use setenv APSDB dbname. \n"); 
    aps_log_msg(progname, APS_CRITICAL, "Environment variable APSDB not set",
        DO_SYSLOG, DO_PRINT);
    usage_exit(progname, APS_EXIT_ERROR);
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
printf("Database '%s' is open and in use.\n", dbname ) ;

if ( init_vec_lib() )
{
	fprintf(stderr, "%s:\n\nERROR initializing the Stoic file.\n",
		argv[0] ) ;
	fprintf(stderr, "     check for existance, permissions, and \n" ) ;
	fprintf(stderr, "     value of environment variable APS_DATA\n" ) ;
	aps_log_msg(progname, APS_CRITICAL, "ERROR initializing the Stoic file",
		DO_SYSLOG, DO_PRINT);
	aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
		DO_SYSLOG, DO_PRINT);
	exit(APS_ERROR) ;
}

step_size = 1;
strcpy(replication_flag, "Y");

/* validate the satellite, sensor for making nominal coverage */
return_code = cvrg_allowed(sat, sensor ) ;
if ( return_code != 0 )
{
    sprintf(buf, "sensor coverage not allowed for sat/sensor:  %s/%s",
        sat, sensor ) ;
    aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
    error_exit( progname, APS_EXIT_ERROR ) ;
}

/* call the coverage program */
printf("[%s Version dated %s %s]\n\n", progname, __DATE__, __TIME__ ) ;

/****************************************************************************
*                                                                           *
*                   GET PERMISSIONS                                         *
*                                                                           *
****************************************************************************/
return_code = mu_get_permission( progname, APS_dbproc, 
    permission_id, mu_activity_type, mu_activity_id, 
    begintime, endtime, station_id, 
    0,        /* darid parameter not used.  */
    n_retries, n_seconds_retry ) ;
if ( return_code < 0 )
{
    /* permission not granted/validated.  */
    error_exit (progname, APS_EXIT_ERROR);
}

/* permission obtained; set id.  */
permission_id = return_code ;

int_APS_dbproc = (int) APS_dbproc ;
return_code = coverage_(progname, &int_APS_dbproc,
    begintime, endtime, mask, sat, sensor, 
    &step_size, &quick_flag, &file_flag, replication_flag );

/* return the error code to the shell  */
/* print banner and exit                */
if ( return_code != APS_EXIT_OK )
    error_exit( progname, APS_EXIT_ERROR ) ;

/*
-- Still OK.
-- Terminate the permission if it was NOT
-- passed on the command line.
-- That is, if it was granted in this routine.
*/
if ( pflag == 0 )
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


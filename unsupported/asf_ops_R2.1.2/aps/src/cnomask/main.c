#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    main routine for Create Nominal Mask Entry and Exit

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cnomask/SCCS/s.main.c"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <db_sybint.h>
#include <timeconv.h>        /* for tc_validate_asf_datetime()   */
#include <aps_db_table.h>    /* for PHASE, STATION               */
#include <aps_defs.h>
#include <aps_log_msg.h>
#include <mu_utilities.h>

int banner_exit(int) ;


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
static void error_exit(char *progname, int code )
{
 
    aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.",
        DO_SYSLOG, DO_PRINT);
 
    banner_exit(code);

    return ; /* for lint  */
}

/*==============================================================================
Function:       usage_exit()

Description:    print the usage and then exit.  

Creator:        Lawrence Stevens

Creation Date:  Sat Jan 11 21:53:28 PST 1997

Notes:      
==============================================================================*/
static void usage_exit(char *progname, int code )
{
printf(
"usage:  %s  [-U sybase_userid] -P password\n", progname );
printf(
"           [ {  -p <Permission id> \n");
printf(
"              | -R <number of Re-tries> -S <Seconds between re-tries>  } ]\n");
printf(
"        -b begin_time -e end_time  satellite \n\n");

printf("\tNOTES:\n");
printf("\tThis program creates nominal mask entry and exit records in the\n");
printf("\tmaskinout relation for the input satellite and time bracket.\n");
printf("\t-b and -e provide the Begin and End times for the run.\n\n");
printf("\tMulti-user permissions:  \n");
printf("\t<permission id> is an integer indicating an existing permission.\n");
printf("\t                If not provided, -R can be used.\n");
printf("\t<number of Re-tries> is an integer indicating the number of extra\n");
printf("\t                     attempts to get permission if the first try\n");
printf("\t                     fails.  -S is required with this parameter.\n");
printf("\t<seconds between re-tries> is an integer indicating the amount \n");
printf("\t                           of time between permission attempts.\n");
printf("\n\t[%s Version dated %s %s]\n\n", progname, __DATE__, __TIME__ ) ;

aps_log_msg(progname, APS_ERROR, "Usage error", DO_SYSLOG, DO_PRINT);

aps_log_msg(progname, APS_INFO,  "Program terminated abnormally.", 
    DO_SYSLOG, DO_PRINT);

exit (code);

return ; /* for lint  */

}


/*==============================================================================
Function:       check_sat()

Description:    checks to see if the input satellite is in the phase 
                relation.

Creator:        Lawrence Stevens

Creation Date:  Thu May  1 10:00:08 PDT 1997

==============================================================================*/
#include <db_phase.h>         /* for PHASE_SAT etc.  */
static int check_sat( char *sat ) 
{
    int numrecs ;

    (void) sprintf(where_clause, "where %s = '%s'",
        APS_COL(PHASE, PHASE_SAT),     sat ) ;
 
#ifdef PRINT_DIAG
    printf("check_sat:  where_clause = \n%s\n", where_clause ) ;
#endif
 
    numrecs = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(PHASE), where_clause ) ;

    if( numrecs >= 1 )
        return TRUE ;

    return FALSE ;

}

/*==============================================================================
Function:       check_station()

Description:    checks to see if there is a station record for the input
                satellite.  There can be no maskinout run without any 
                station records for the satellite.  

Creator:        Lawrence Stevens

Creation Date:  Thu May  1 10:00:08 PDT 1997

==============================================================================*/
#include <db_station.h>         /* for STATION_SAT etc.  */
static int check_station( char *sat ) 
{
    int numrecs ;

    (void) sprintf(where_clause, "where %s = '%s'",
        APS_COL(STATION, STATION_SAT),     sat ) ;
 
#ifdef PRINT_DIAG
    printf("check_station:  where_clause = \n%s\n", where_clause ) ;
#endif
 
    numrecs = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(STATION), where_clause ) ;

    if( numrecs >= 1 )
        return TRUE ;

    return FALSE ;

}

/**********************************************************************
* Name : main
*
* Purpose: handles the command line arguments for 
*           the Create Nominal Mask Entry and Exit. 
*           it initiates a stand-alone executable.
*
*  Exit Parameter:
*  Type         Description
*  int          0   no errors.
*               1   there was an error.  
*
*  NOTE:    if there is an error in the command line; missing argument(s) etc.  
*           then this routine will print the usage info and exit.  
*
******************************************************************************/
void main( int   argc, char  *argv[])
{

DBPROCESS   *APS_dbproc ;
int     rcode ;

char    *progname ;
int     int_APS_dbproc ; 

int     j ;
char    dbname[100];
char    sybase_userid[100];
char    password[100];
char    begintime[30];
char    endtime[30];
char    sat[30];
int     return_code;

char    replication_flag[2]; /* "Y" means to replicate from nominal orbit */

/* coverage program in fortran  */
extern int      maskinout_(char *progname, int *dbproc, 
                    char *begintime, char *endtime, 
                    char *sat ) ;

/* get the sybase_userid (optional) and password (mandatory)    */
/* int  getopt(int argc, char * const *argv, char *optstring);  */
extern  char    *optarg;
extern  int     optind ;
/*  extern  char*   getenv(char*);  */

int     c;      /* used as return character from getopt()       */

int     Pflag = 0;  /* used to check for mandatory password     */
int     Uflag = 0;  /* used to check for optional sybase_userid */
int     bflag = 0;  /* used to check for manditory start time.  */
int     eflag = 0;  /* used to check for manditory stop time.   */
int     pflag = 0;  /* used to check for optional permission_id          */
int     Sflag = 0;  /* used to check for optional n_seconds_retry        */
int     Rflag = 0;  /* used to check for optional n_retries              */

char    flag_list[100] = "p:S:R:P:U:b:e:";  /* list of flags for getopts    */
int     pcount;     /* count of cmd line positional parameters; should be 2 */

char    *env_dbname;        /* dbname from environment      */
char    *env_sybase_userid; /* userid from environment      */
int     permission_id   = 0 ;  /* optional permission_id passed     */
int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
int     n_retries       = 0 ;  /* optional number of re-tries       */
char    *station_id = "ALL" ;

char    buf[200];

/* create nominal maskinout is a planning activity.  */
char    *mu_activity_type = MU_PLANNING_ACTIVITY_TYPE ;
char    *mu_activity_id   = MU_CREATE_NOMINAL_MASKINOUT ;

/*
-- unbuffer standard output, to watch
-- while program is executing.
*/
(void) setbuf( stdout, (char *) NULL ) ;

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


/* now pick up the positional parameter satellite */
pcount = 0;
for(; optind < argc; optind++)
{
    if(pcount == 0)
    {
        strncpy(sat, argv[optind], 2);
        sat[2] = '\0';
        /* 
        -- validate the satellite for 
        -- making nominal maskinout 
        */
        return_code = check_sat(sat) ;
        if( return_code != TRUE )
        {
            sprintf(buf, "Nominal maskinout not allowed for sat:  %s", sat ) ;
            aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit( progname, APS_EXIT_ERROR ) ;
        }
        /* 
        -- make sure that there is at 
        -- least 1 station record for the sat.   
        */
        return_code = check_station( sat ) ;
        if( return_code != TRUE )
        {
            sprintf(buf, 
                "Can't run %s; No records in station relation for sat:  %s",
                progname, sat ) ;
            aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            error_exit( progname, APS_EXIT_ERROR ) ;
        }
    }
    pcount++;
    if(pcount > 1)
        usage_exit(progname, APS_EXIT_ERROR);
}

if(pcount != 1)
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

strcpy(replication_flag, "Y");


/* call the maskinout program */
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
return_code = maskinout_(progname, &int_APS_dbproc,
    begintime, endtime, sat );

/* return the error code to the unix shell  */
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

return ;   /* for lint  */

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif


/*==============================================================================
Filename:       main.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/cnomorb/SCCS/s.main.c"

#include <stdlib.h>
#include <unistd.h>    /* for sleep()   */
#include <string.h>
#include <stdio.h>
#include <aps_defs.h>
#include <aps_log_msg.h>
#include <mu_utilities.h>

#include <db_sybint.h>
#include <aps_db_table.h>
#include <db_active_single_activities.h>
#include <db_phase.h>

char  msg[MSG_LEN]; 


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

Description:    prints usage and exits with an error code.  

Creator:        Lawrence Stevens

Creation Date:  Tue Dec 24 22:06:23 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static usage_exit(char *progname, int code )
{
printf(
"usage:  %s  [-U sybase_Userid] -P sybase_Password \n", progname );
printf(
"           [ {  -p <Permission id> \n");
printf(
"              | -R <number of Re-tries> -S <Seconds between re-tries>  } ]\n");
printf(
"           <satellite>  <phase>\n\n");

printf("\tNOTES:\n");
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

exit(code);


}
/**********************************************************************
* Name : main
*
* Purpose: handles the command line arguments for create_nominal_orbit.for,
*           the main routine for create nominal orbit. 
*
****************************************************************************/
int main( int   argc, char  *argv[])
{

DBPROCESS   *APS_dbproc ;
int         int_APS_dbproc ;
RETCODE     rcode ;
int         nrecs ;

llist       *perm_list = NULL ;

int     j ;
char    dbname[100];
char    sybase_userid[100];
char    password[100];
char    sat[30];
char    phase[30];
int     return_code;

/* nominal orbit program in fortran  */
extern int      create_nominal_orbit_(
                    char *progname, 
                    int  *dbproc, 
                    char *dbname, 
                    char *sybase_userid, 
                    char *password,
                    char *sat, 
                    char *phase ) ;

/* get the sybase_userid (optional) and password (mandatory)    */
/* int  getopt(int argc, char * const *argv, char *optstring);  */
extern  char    *optarg;
extern  int     optind ;
/*  extern  char*   getenv(char*);  */

int     c;      /* used as return character from getopt()       */

int     Pflag = 0;  /* used to check for mandatory password     */
int     Uflag = 0;  /* used to check for optional sybase_userid */
int     pflag = 0;  /* used to check for optional permission_id          */
int     Sflag = 0;  /* used to check for optional n_seconds_retry        */
int     Rflag = 0;  /* used to check for optional n_retries              */

char    flag_list[100] = "P:U:p:S:R:";  /* list of flags for getopts  */
int     cmdpcount;   /* count of cmd line positional parameters; should be 2 */

char    *progname;          /* program from argv[0]         */
char    *env_dbname;        /* dbname from environment      */
char    *env_sybase_userid; /* userid from environment      */
int     permission_id   = 0 ;  /* optional permission_id passed     */
int     n_seconds_retry = 0 ;  /* optional seconds between re-tries */
int     n_retries       = 0 ;  /* optional number of re-tries       */

char    buf[200];

char    *mu_activity_type = MU_SINGLE_ACTIVITY_TYPE ;
char    mu_activity_id[64] ;

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

/* now pick up the positional parameters satellite and phase  */
cmdpcount = 0;
for(; optind < argc; optind++)
{
    if(cmdpcount == 0)
    {
        strncpy(sat, argv[optind], 2);
        sat[2] = '\0';
    }
    if(cmdpcount == 1)
    {
        strncpy(phase, argv[optind], 3);
        phase[3] = '\0';
    }
    cmdpcount++;
    if(cmdpcount > 2)
        usage_exit(progname, APS_EXIT_ERROR);
}

if(cmdpcount != 2)
    usage_exit(progname, APS_EXIT_ERROR);

/* 
-- satellite and phase values extracted.  
-- check them.  
*/
/*
-- see if a phase record exists for
-- the input sat, phase from mu_activity_id:
*/
(void) sprintf( where_clause, "where %s = '%s' and %s = '%s'",
    APS_COL(PHASE, PHASE_SAT), sat,
    APS_COL(PHASE, PHASE_PHASE_NAME), phase ) ;

nrecs = db_num_records( DB_SYBINT_USE_APS_READER_DBPROC,
    APS_TABLE(PHASE), where_clause ) ;
if( nrecs < 0 )
    return MU_DB_ERROR_DURING_PHASE_TABLE_COUNTING ;
if( nrecs == 0 )
{
    /*
    -- no records found; the sat
    -- and phase are therefore not valid:
    */
    sprintf( msg, "ERROR:  sat/phase (%s/%s) not valid.", sat, phase ) ;
    aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
    error_exit( progname, APS_EXIT_ERROR);
}

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
-- DO NOT initialize the vector library; 
-- the create nominal orbit program 
-- looks in a SPECIAL DIRECTORY for the stoicfile, according 
-- to sat/phase values.  
*/

/* 
-- now open the database.  
-- db_open will handle the errors.      
*/
APS_dbproc = db_open(dbname,progname,sybase_userid,password,NULL,
    error_handler_exit,&rcode);
if(rcode != DB_OPEN_OK)
{
    db_open_errs( rcode, dbname, sybase_userid);
    sprintf( msg, "ERROR:  dbname %s could not be opened", dbname ) ;
    aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
    error_exit (progname, APS_EXIT_ERROR);
}
printf("Database '%s' is open and in use.\n", dbname ) ;

/* print out the version date for ID.  */
printf("[%s Version dated %s %s]\n\n", progname, __DATE__, __TIME__ ) ;

/****************************************************************************
*                                                                           *
*                   GET PERMISSIONS                                         *
*                                                                           *
****************************************************************************/
 
/*
-- form the multi-user activity id and type for 
-- this task:
*/
strcpy( mu_activity_id, sat ) ;
strcat( mu_activity_id, phase ) ;
strcat( mu_activity_id, MU_CREATE_NOMINAL_ORBIT ) ;

return_code = mu_get_permission( progname, APS_dbproc, 
    permission_id,
    mu_activity_type,
    mu_activity_id, 
    NULL, NULL, NULL, 0,     /* parameters not used        */
    n_retries,
    n_seconds_retry ) ;
if ( return_code < 0 )
    error_exit (progname, APS_EXIT_ERROR) ;

permission_id = return_code ;


/* permission was obtained; now proceed.  */

/* call the create nominal orbit routine */
int_APS_dbproc = (int) APS_dbproc ;
return_code = create_nominal_orbit_(
    progname, &int_APS_dbproc, dbname, sybase_userid, password,
    sat, phase ) ;
/* return the error code to the shell  */
/* print banner and exit                */
if ( return_code != APS_EXIT_OK )
    error_exit (progname, APS_EXIT_ERROR);

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

/* normal end is already BANNERed by asap.  */
exit(APS_EXIT_OK);

}


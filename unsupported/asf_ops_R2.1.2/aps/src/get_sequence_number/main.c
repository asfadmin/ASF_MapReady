#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    main source file for the NEW DTKID function.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This function is implemented and supported at several levels:

Unix command line                        aps_get_sequence_number
C function with userid/password/dbname   aps_get_sequence_number()
C function with dbproc                   db_get_new_dtkid()
SQL stored procedure                     aps_sp_new_dtkid

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/get_sequence_number/SCCS/s.main.c"

/* 
-- to decode the error code into a string:  
*/
extern char *new_dtkid_error_message[] ;    /* see this table.  */

#include <stdio.h>
#include <stdlib.h>         /* for getopt                           */
#include <string.h>
#include <ctype.h>          /* for isdigit()    */
#include <aps_log_msg.h>    /* for syslog stuff                     */

/* for aps_get_sequence_number() and APS_NEW_DTKID_ERROR_MESSAGE */
#include "aps_get_sequence_number.h"  

static void usage_exit(char *progname)
{
    char    *env_dbname ;
    char    *env_DSQUERY ;
    char    *undefined = "undefined" ;
    env_dbname = getenv("APSDB");
    env_DSQUERY = getenv("DSQUERY");
    if( env_dbname == NULL )
        env_dbname = undefined ;
    if( env_DSQUERY == NULL )
        env_DSQUERY = undefined ;

    (void)fprintf(stderr, 
        "\nUsage: %s  -U Sybase_userid  -P Sybase_password  \n", 
        progname ) ;
    (void)fprintf(stderr,
        "                           [-D APS_dbname]  [-S Sybase_server] \n");
    (void)fprintf(stderr,
        "                           sat  rev\n");
    (void)fprintf(stderr, 
    "\n    This program creates a new dtkid (sequence) number for the input\n");
    (void)fprintf(stderr, 
    "    satellite and rev number, and prints it to standard output.\n\n") ;
    (void)fprintf(stderr, 
    "    sat        satellite or platform value: {A1, E1, E2, J1, R2, etc.]\n");
    (void)fprintf(stderr, 
    "    rev        revolution or orbit number:  integer > 0 \n\n");
    (void)fprintf(stderr, 
    "    Sybase_userid     Userid for Sybase account.  \n" ) ;
    (void)fprintf(stderr, 
    "    Sybase_password   Password for Sybase account.  \n" ) ;
    (void)fprintf(stderr, 
    "    APS_dbname        Name of APS database.  If not given, the value \n");
    (void)fprintf(stderr, 
    "                      is obtained from environment variable APSDB\n") ;
    (void)fprintf(stderr, 
    "                      whose value is currently %s.\n", env_dbname ) ;
    (void)fprintf(stderr, 
    "    Sybase_server     Name of Sybase SQL server.  If not given, the \n");
    (void)fprintf(stderr, 
    "                      value is obtained from environment variable\n") ;
    (void)fprintf(stderr, 
    "                      DSQUERY whose value is currently %s.\n\n", 
        env_DSQUERY ) ;

    (void)fprintf(stderr, "\n\n    %s version:  %s %s   ---------\n\n",
        progname, __DATE__, __TIME__ ) ;
    exit(1) ;
    return  ; /* for lint  */
}

/*==============================================================================
Function:       main()

Description:    main routine for the NEW DTKID function.  
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

    char        *dbname = NULL ;
    char        *sybase_userid = NULL ;
    char        *sybase_password = NULL ;

    /* 
    -- NULL indicates to Sybase use 
    -- env DSQUERY to obtain the 
    -- name of the SQL server
    */
    char        *sybase_server = NULL ;    

    char        *progname ;
    char        *slash_ptr ;

    char        buf[256] ;
    int         pcount ;
    char        *sat ;
    int         rev ;

    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;
     
    int         return_code ;
    int         j ;
    
    char        *env_dbname;        /* dbname from environment      */
    char        *env_sybase_userid; /* userid from environment      */

    int         Uflag = 0;  /* used to check for userid flag.      */
    int         Pflag = 0;  /* used to check for password flag.   */
    int         Sflag = 0;  /* used to check for server flag.   */
    int         Dflag = 0;  /* used to check for database flag.   */

    int         c;      /* used as return character from getopt()       */
     
    char        flag_list[20] = "U:P:D:S:"; /* list of flags for getopt  */
    
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
        -- remove it.  reset the progname pointer to 
        -- the next character after the last slash:  
        */
        progname = slash_ptr + 1 ;
    }

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'D':
                if( Dflag != 0 )
                    usage_exit(progname);
                Dflag ++ ;
                dbname = optarg ;
                break;
            case 'P':
                if( Pflag != 0 )
                    usage_exit(progname);
                Pflag ++ ;
                sybase_password = optarg ;
                break;
            case 'S':
                if( Sflag != 0 )
                    usage_exit(progname);
                Sflag ++ ;
                sybase_server = optarg ;
                break;
            case 'U':
                if( Uflag != 0 )
                    usage_exit(progname);
                Uflag ++ ;
                sybase_userid = optarg ;
                break;
            case '?':
                usage_exit(progname);
                break ;
            default:
                /* do nothing.  */
                break ;
        }

    /* 
    -- now pick up the positional parameters 
    -- satellite and rev number
    */
    pcount = 0;
    for(; optind < argc; optind++)
    {
        if(pcount == 0)
        {
            if( strlen(argv[optind]) != 2 )
            {
                (void)fprintf(stderr, "%s:  Error:  %s is a bad sat value. \n", 
                    progname, argv[optind] ) ;
                exit(1) ;
            }
            sat = argv[optind] ;
        }
        if(pcount == 1)
        {
            for( j = 0 ; j < strlen(argv[optind]) ; j++ )
            {
                if( ! isdigit(argv[optind][j])  )
                {
                    (void)fprintf(stderr, 
                        "%s:  Error:  %s is a non-integer rev value\n", 
                        progname, argv[optind] ) ;
                    exit(1) ;
                }
            }
            return_code = sscanf(argv[optind], "%d", &rev ) ;
            if( return_code != 1 )
            {
                (void)fprintf(stderr, 
                    "%s:  Error:  %s is a non-integer rev value\n", 
                    progname, argv[optind] ) ;
                exit(1) ;
            }
        }
        pcount++;
        if(pcount > 2)
        {
            (void)fprintf(stderr, "%s:  Error:  too many parameters\n", progname ) ;
            exit(1) ;
        }
    }

    if( pcount != 2 )
        usage_exit(progname );
     
    /* mandatory flags:  */
    if ( Pflag == 0 )
    {
        (void)fprintf(stderr, "%s:  Error:  Sybase_password not given.\n", 
            progname ) ;
        exit(1) ;
    }

    /*
    -- All of the values have been obtained from the
    -- command line.  
    */

    /* optional flag    */
    if(Uflag == 0 )
    {
        /* sybase_userid not supplied in command line.      */
        /* obtain from the environment:                     */
        env_sybase_userid = getenv("APS_SYBASE_USERID");
        if(env_sybase_userid == NULL)
        {
            (void)fprintf(stderr, "%s:  Error:  Sybase_userid not given.\n", 
                progname ) ;
            exit(1) ;
        }
        /* use the environment sybase_userid.   */
        sybase_userid = env_sybase_userid ;
    }
 
    /* optional flag    */
    if(Dflag == 0 )
    {
        /* get the database name from the environment   */
        env_dbname = getenv("APSDB");
        if(env_dbname == NULL)
        {
            (void)fprintf(stderr, 
                "%s:  Error:  APS_dbname was given nor was env APSDB set.\n", 
                progname ) ;
            exit(1) ;
        }
        dbname = env_dbname ;
    } 

    /* 
    -- all values have been obtained.  
    -- if sybase_server == NULL, hen DSQUERY is used for 
    -- the server.  
    */
    dberrhandle(db_default_error_handler) ;   /* caller-supplied */
    dbmsghandle(db_default_message_handler) ; /* caller-supplied */

    return_code = aps_get_sequence_number( progname, sybase_userid, 
        sybase_password, dbname, sat, rev, sybase_server ) ;
    if( return_code < -1000 )
    {
        (void)fprintf(stderr, "%s:  Error:  %s\n", progname, 
            APS_NEW_DTKID_ERROR_MESSAGE(return_code) ) ;
        exit(1) ;
    }
    if( return_code < 1 )
    {
        (void)fprintf(stderr, "%s:  Error obtaining new dtkid  \n", progname ) ;
        exit(1) ;
    }

    /* print new dtkid:  */
    (void)printf("%d\n", return_code ) ;

    return 0 ;

}


#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    main routine for rev2time.  Converts sat/rev to time bracket.

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/rev2time/SCCS/s.main.c"

#include <stdio.h>
#include "db_sybint.h"      /* for APS sybase interface routines.       */
#include "aps_db_table.h"   /* for DTK - accessing dtk table.           */
#include "aps_defs.h"       /* for APSDB_ENVVAR and APS_USERID_ENVVAR   */
#include "db_dtk.h"         /* for APS db relation dtk.                 */
#include "timeconv.h"       /* for APS db relation dtk.                 */
#include <string.h>         /* for strrchr()                            */
#include <stdlib.h>         /* for getenv()                             */
#include <unistd.h>         /* for access()                             */

/* GLOBAL VARIABLE IN THIS FILE:  */
char        msg[100] ;

static void error_exit(char *progname)
{
    exit(1) ;

}
static void usage_exit(char *progname)
{
    fprintf(stderr, "\nusage: %s -s sat -r rev \n",
        progname);
    fprintf(stderr,
    "\n    This program prints the time bracket for the input sat/rev.\n") ;
    fprintf(stderr,
    "    It reads from database named $APSDB to get the data.\n") ;
    fprintf(stderr,
    "\n    sat     satellite value:  E1, E2, R2, A1, etc. \n") ;
    fprintf(stderr,
    "    rev     Revolution number for satellite, must be > 0.\n") ; 
    fprintf(stderr, "\n    %s  Version compiled:  %s %s\n\n",
        progname, __DATE__, __TIME__ ) ;
    error_exit(progname) ;
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
        sprintf(msg, "environment variable:  %s is not set.", env_name);
        fprintf( stderr, "%s:\n\n%s\n", progname, msg );
        return FALSE ;
    }
    else
    {
        return TRUE ;
    }
}



/*==============================================================================
Function:       main()

Description:    accepts command with sat, rev and converts to time bracket.

Creator:        Lawrence Stevens

Creation Date:  Tue Sep 24 10:48:29 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
void main (int argc, char *argv[])
{
 
    char        *APS_rootpath = NULL ;
    char        *sat = NULL ;
    char        start_asftime[25] ;
    char        end_asftime[25] ;
    double      et = 0.0 ;
    double      et1 = 0.0 ;
    double      et2 = 0.0 ;
    long int    rev = 0 ; 
    long int    return_code;
    void        *dbproc = (void *) 1 ;
    char        *progname ;
    int         sflag = 0 ;
    int         rflag = 0 ;
 
    int         j ;
 
    /* for getopt()   */
    extern char *optarg ;
    extern int  optind ;
 
 
    char        *env_sybase_userid; /* userid from environment      */
 
    int         c;      /* used as return character from getopt()       */
 
    char        flag_list[20] = "s:r:"; /* list of flags for getopt  */
 
    progname = argv[0] ;

    /* set stdout to unbuffered I/O */
    setbuf( stdout, (char *) NULL ) ;

    if ( argc <= 1 )
        usage_exit(argv[0]);

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 's':
                if( sflag != 0 )
                    usage_exit(argv[0]);
                sflag++ ;
                sat = optarg ;
                break;
            case 'r':
                if( rflag != 0 )
                    usage_exit(argv[0]);
                return_code = sscanf( optarg, "%d", &rev ) ;
                if( return_code != 1)
                {
                    fprintf(stderr, "%s:  rev must be an integer.  = %s\n", 
                        argv[0], optarg ) ;
                    error_exit (argv[0]) ;
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

    /* mandatory values:  */
    if ( sat == NULL )
    {
        strcpy( msg, "sat not given." ) ;
        fprintf(stderr, "%s:  %s\n", argv[0], msg ) ;
        error_exit (argv[0]) ;
    }

    /* mandatory values:  */
    if ( rev <= 0 )
    {
        fprintf(stderr, "%s:  bad rev number:  %d\n", argv[0], rev ) ;
        error_exit (argv[0]) ;
    }

    /*
    -- all of the values have been obtained from the
    -- command line.
    */
 
    if ( init_vec_lib() )
    {
        fprintf(stderr, "%s:\n\nERROR initializing the Stoic file.\n", argv[0]);
        fprintf(stderr, "     check for existance, permissions, and \n" ) ;
        fprintf(stderr, "     value of environment variable APS_DATA\n" ) ;
        error_exit(argv[0]) ;
    }

    /*
    -- check the other needed environment variables 
    -- for this job.  
    -- APSDB_ENVVAR, IMS_DB_ENVVAR,  and IMS_SERVER_ENVVAR.  
    */
    if ( !env_is_set(argv[0], APSDB_ENVVAR ) )
    {
        sprintf(msg, "APSDB environment variable not set." );
        fprintf( stderr, "%s:\n\n%s\n", argv[0], msg );
        error_exit(argv[0] ) ;
    }
    (void) sscvrev2time( 1, sat, &rev, 
        &et1, start_asftime, &et2, end_asftime, 
        &return_code ) ;

    switch (return_code)
    {
        case 0:
            printf("%s  %s\n", start_asftime, end_asftime) ;
            break ;
        case 1:
            fprintf(stderr, "%s:  the rev was before all phases.\n", 
                progname ) ;
            break ;
        case 2:
            fprintf(stderr, "%s:  the rev was between 2 phases .\n", 
                progname ) ;
            break ;
        case 3:
            fprintf(stderr, "%s:  the rev was after all phases.\n", 
                progname ) ;
            break ;
        default:
            fprintf(stderr, 
"%s:  no records were found in the phase relation for this satellite.\n", 
                progname ) ;
            break ;
    }


    exit(0) ;

} /* main */

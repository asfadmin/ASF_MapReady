#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       main.c

Description:    source routines for aps_write2syslog


==============================================================================*/
#pragma ident   "@(#)main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/write2syslog/SCCS/s.main.c"


/*==============================================================================
Function:       main

Description:    main routine for aps_write2syslog which enables 
                a script to write to syslog.  

Creator:        Lawrence Stevens

Creation Date:  Thu Mar  6 20:50:36 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include <unistd.h>
#include <aps_log_msg.h>
#include <string.h>

static void usage_exit(char *progname, int code )
{
    (void) fprintf(stderr,
    "\nusage: %s  -p progname  -l level  -m message  -f printFlag \n",
 progname );
    (void) fprintf(stderr,
    "\n    This program allows a script to write to the APS syslog.  \n\n" ) ;
    (void) fprintf(stderr, "    progname      name of the program.\n" ) ;
    (void) 
    fprintf(stderr, "    level         {APS_DEBUG | APS_INFO | APS_WARNING \n");
    (void) 
    fprintf(stderr, "                   | APS_ERROR | APS_CRITICAL}\n" ) ;
    (void) 
    fprintf(stderr, "    message       message, a string, to print.\n" ) ;
    (void) fprintf(stderr, "    printFlag     {DO_PRINT | NO_PRINT} \n\n" ) ;

	if( code > 0 )
		(void) fprintf(stderr, "    Error code = %d\n", code ) ;
    (void) fprintf(stderr, "    %s  Version compiled:  %s %s\n\n",
        progname, __DATE__, __TIME__ ) ;

    exit(1) ;
}


int main( int argc, char *argv[])
{

    char        *progname ;
    char        *slash_ptr ;

    /* for getopt()   */
    extern char *optarg ;
 
    int         c;      /* used as return character from getopt()       */
    char        flag_list[20] = "p:l:m:f:"; /* list of flags for getopt  */
 
    /* for MU permission */
    int     pflag = 0;  /* used to check for program name          */
    int     lflag = 0;  /* used to check for level        */
    int     mflag = 0;  /* used to check for message              */
    int     fflag = 0;  /* used to check for printFlag              */

    char    *program_name = NULL ;
    char    *level = NULL ;
    char    *message = NULL ;
    char    *printFlag = NULL ;

    int     int_level ;
    int     int_printFlag ;

    /*
    -- get progname as file name without
    -- the directory path.
    */
    progname = argv[0] ;
    slash_ptr = strrchr( progname, '/' ) ;
    if ( slash_ptr )
    {
        /*
        -- if there is a directory path in progname,
        -- remove it.  reset the progname pointer to
        -- the next character after the last slash:
        */
        progname = slash_ptr + 1 ;
    }

	if( argc == 1 )
		usage_exit(progname, 0) ;

    /*
    -- start up syslog messages.
    */
    aps_open_syslog();

    while ((c = getopt(argc, argv, flag_list)) != EOF)
        switch (c)
        {
            case 'p':
                if( pflag != 0 )
                    usage_exit(progname, 1);
                if( program_name != NULL )
                    usage_exit(progname, 2);
                program_name = optarg ;
                pflag++ ;
                break;

            case 'l':
                if( lflag != 0 )
                    usage_exit(progname, 3);
                if( level != NULL )
                    usage_exit(progname, 4);
                level = optarg ;
                lflag++ ;
                break;

            case 'm':
                if( mflag != 0 )
                    usage_exit(progname, 5);
                if( message != NULL )
                    usage_exit(progname, 6);
                message = optarg ;
                mflag++ ;
                break;

            case 'f':
                if( fflag != 0 )
                    usage_exit(progname, 7);
                if( printFlag != NULL )
                    usage_exit(progname, 8);
                printFlag = optarg ;
                fflag++ ;
                break;

            case '?':
                usage_exit(progname, 9);
                break ;

            default:
                usage_exit(progname, 10);
                break ;
        }

    if( program_name == NULL )
        usage_exit(progname, 11);
    if( level == NULL )
        usage_exit(progname, 12);
    if( message == NULL )
        usage_exit(progname, 13);
    if( printFlag == NULL )
        usage_exit(progname, 14);

    /* decode string level to int_level */
    if( strcmp( level, "APS_DEBUG" ) == 0 )
        int_level = APS_DEBUG ;
    else if( strcmp( level, "APS_INFO" ) == 0 )
        int_level = APS_INFO ;
    else if( strcmp( level, "APS_WARNING" ) == 0 )
        int_level = APS_WARNING ;
    else if( strcmp( level, "APS_ERROR" ) == 0 )
        int_level = APS_ERROR ;
    else if( strcmp( level, "APS_CRITICAL" ) == 0 )
        int_level = APS_CRITICAL ;
    else
        usage_exit(progname, 15) ;

    /* decode string printFlag to int_printFlag */
    if( strcmp( printFlag, "DO_PRINT" ) == 0 )
        int_printFlag = DO_PRINT ;
    else if( strcmp( printFlag, "NO_PRINT" ) == 0 )
        int_printFlag = NO_PRINT ;
    else
        usage_exit(progname, 16) ;

    /* now make the call.  */
    aps_log_msg( program_name, int_level, message, DO_SYSLOG, int_printFlag ) ;

    exit(0) ;

}

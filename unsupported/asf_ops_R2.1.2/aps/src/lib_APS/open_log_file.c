#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       open_log_file.c

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
#pragma ident   "@(#)open_log_file.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.open_log_file.c"


/*==============================================================================
Function:       open_log_file()

Description:    accepts a programe name, creates a time-stamped unique 
                log file name, opens the log file for writing, and writes 
                the file name and time into the log file to start.  
                Uses the aps syslog.  

Creator:        Lawrence Stevens

Creation Date:  Mon Sep 23 10:08:51 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

#include <open_log_file.h>   /* for LOG_FILE_NAME_MIN_SIZE   */

#include <stdio.h>   /* for FILE*      */

FILE *open_log_file( 
    char    *progname,      /* name of calling program.                */
    char    *log_filename,  /* output name of log file.                */
    int     log_filename_size, /* output log file name, must be >= MIN_SIZE */
    char    *now_asftime,       /* output asf time, previously allocated.  */
    int     now_asftime_size)   /* size of previously allocated asf time.  */
{

    FILE    *logfp ;        /* file pointer to opened log file.  */
    char    *aps_fullpath_log_filename ;
    int     year, yy ;
    int     doy, hour, min, sec, msec ;
    char    msg[MSG_LEN+1] ;

    /* error checking.  */
    if( progname == NULL )
        return (FILE*)-1 ;

    if( log_filename == NULL )
        return (FILE*)-1 ;

    if( log_filename_size < LOG_FILE_NAME_MIN_SIZE ) 
        return (FILE*)-1 ;

    if( now_asftime == NULL )
        return (FILE*)-1 ;

    if( now_asftime_size < (ASF_TIME_STR_LENGTH + 1) )
        return (FILE*)-1 ;

    /* get system time in asf format */
    tc_systime2asf(now_asftime) ;
 
    /* check asf time */
    if (!tc_parse_asftime(now_asftime,
            &year, &yy, &doy, &hour, &min, &sec, &msec))
    {
        (void) sprintf(msg,
            "ERROR: Unable to create log filename from bad ASF time %s",
            now_asftime ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return (FILE*)-1 ;
    }
 
    /* make file name (progname.yyyyddd_hhmmss.LOG) */
 
    (void)sprintf(log_filename,
        "%s.%04d%03d_%02d%02d%02d.LOG",
        progname, year, doy, hour, min, sec) ;
    /* memory is allocated here, free later:  */
    aps_fullpath_log_filename = aps_fullpath(APS_LOGS, log_filename) ;
 
    /*  Open the file  */
    logfp = fopen(aps_fullpath_log_filename, "a") ;
    if ( logfp == NULL  )
    {
        (void)sprintf(msg,
"Unable to open log file %s; check for directory permissions or full disk.",
            aps_fullpath_log_filename ) ;
        aps_log_msg( progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return (FILE*)-1 ;
    }
 
    /* unbuffer the log file to follow the output easier during testing. */
    setbuf( logfp, (char *) NULL ) ;

    /*
    -- start the log file with some info:
    */
    (void) fprintf(logfp, "LOG FILE %s\n", aps_fullpath_log_filename ) ;
    (void) fprintf(logfp, "created %s\n\n", now_asftime ) ;

    if( strlen( aps_fullpath_log_filename ) >= LOG_FILE_NAME_MIN_SIZE )
        return (FILE*)-1 ;

    /* copy name to output string and free the allocated memory:  */
    strcpy( log_filename, aps_fullpath_log_filename ) ;
    free( aps_fullpath_log_filename ) ;

    return logfp ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       fa_copy_file.c

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)fa_copy_file.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_fileutils/SCCS/s.fa_copy_file.c"


/* FOR FILE UTILITIES DEFINITIONS */
#include "file_utilities.h"
#include <unistd.h>    /* for access(), F_OK   */
#include <errno.h>

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"
 
/* FOR DIAGNOSTIC PRINTING */
#define PRINT_DIAG
#undef  PRINT_DIAG

#include <aps_log_msg.h>

extern char     *file_util_progname ; /* required by libfileutils.a */
extern char     file_util_msg[MSG_LEN]; /* required by libfileutils.a */


/*==============================================================================
Function:       fa_copy_file()

Description:    copy -p the FA input file that was processed to 
                the indicated directory as defined by 
                include/local/apspath.h and 
                src/lib_APS/apspath.c 

                If necessary, adds uniqueness number to file name to 
                avoid writing on an existing file. 

Creator:        Lawrence Stevens

Creation Date:  Thu Aug  8 18:45:00 PDT 1996

Notes:      

    EXAMPLE:  
    #include <apspath.h>
    char *full_path_filename ;
    fa_copy_file( full_path_filename, APS_FA_INPUT_FILES ) ;


    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

int fa_copy_file( 
    char        *fullpath_file, /* the full path name of the file to copy  */
    int         id )     /* integer indicating code for destination path.  */
                         /* used as:    aps_path_table[id].full_path       */
                         /* see:  static APS_PATH_TABLE aps_path_table[]   */
                         /* in src/lib_APS/apspath.c for paths.            */
                         /* see include/local/apspath.h for id value defs  */
{

    char    *file_name ;
    char    *copy_dir ;                   /* directory to copy file.        */
    char    copy_fullpath_file[128] ;    /* full path name of file to write */
    char    buf[256] ;
    int     uniqueness_counter = 0 ;
    int     status ;

    /* 
    -- create the destination directory 
    -- path; must free() it later.  
    */
    copy_dir = aps_fullpath( id, NULL ) ;

    /* 
    -- create the destination filename without 
    -- path.  must free() it later.  
    */
    file_name = aps_pathname2filename( fullpath_file ) ;

    /* first try:  */
    sprintf( copy_fullpath_file, "%s/%s", copy_dir, file_name ) ;

    do 
    {
        status = access( copy_fullpath_file, F_OK ) ;
        if ( !status )
        {
            /* 
            -- the file exists.  add a _n for uniqueness 
            -- and try again:
            */
            sprintf( copy_fullpath_file, "%s/%s_%d", copy_dir, file_name, 
                ++uniqueness_counter ) ;
        }
        if ( uniqueness_counter > 100 )
        {
            sprintf (file_util_msg, 
"%s(%d):  ERROR:  uniqueness_counter > 100 ; will not copy FA file %s to %s\n",
                __FILE__, __LINE__, fullpath_file, copy_dir );
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
                DO_SYSLOG, DO_PRINT);
            free( copy_dir ) ;
            free( file_name ) ;
            return (FALSE) ;
        }

    } while ( !status ) ;

    /*
    -- the copy_fullpath_file does not exist, so we will 
    -- not be overwriting anything.  
    -- now copy the file, preserving the time and other info:  
    */

    sprintf( buf, "cp -p %s %s", fullpath_file, copy_fullpath_file ) ;

	/* 
	-- set errno; it is not re-set on a successful 
	-- system() call.  
	*/
	errno = 0 ;
    system( buf ) ;

	/*
	-- check status of errno, was the copy completed ?
	-- only checks for fork() and exec() failure within 
	-- system() call.  can not check for running out 
	-- of disk space, for example.  
	*/
	if (errno)
	{
		sprintf (file_util_msg, 
"%s(%d):  ERROR CODE:  errno=%d ; error occurred in copying FA file %s to %s\n",
			__FILE__, __LINE__, errno, fullpath_file, copy_dir );
		aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
			DO_SYSLOG, DO_PRINT);
		free( copy_dir ) ;
		free( file_name ) ;
		return (FALSE) ;
	}

    sprintf( file_util_msg, "NOTE:  file %s copied to %s.",
        fullpath_file, copy_fullpath_file ) ;
    aps_log_msg( file_util_progname, APS_INFO, file_util_msg, 
        DO_SYSLOG, DO_PRINT);
    free( copy_dir ) ;
    free( file_name ) ;

    return (TRUE) ;
}

#ifndef OPEN_LOG_FILE_H
#define OPEN_LOG_FILE_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   
Description:    
Creator:    Lawrence Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)open_log_file.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.open_log_file.h"

#include <dapps_defs.h>   /* for ASF_TIME_STR_LENGTH  */
#include <aps_log_msg.h>       /* for aps_log_msg(), APS_CRITICAL etc.   */
#include <apspath.h>      /* for APS_LOGS, aps_fullpath()    */

FILE *open_log_file( 
    char    *progname,      /* name of calling program.                      */
    char    *log_filename,  /* output name of log file, already allocated    */
    int     log_filename_size, /* size of output log file name, 
                                  must be >= LOG_FILE_NAME_MIN_SIZE          */
    char    *asftime,       /* output asf time, previously allocated.        */
    int     asftime_size) ; /* size of previously allocated asf time.
                               must be >= ASF_TIME_STR_LENGTH+1              */

#define LOG_FILE_NAME_MIN_SIZE 200

#endif  /* OPEN_LOG_FILE_H */

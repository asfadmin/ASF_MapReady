#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       db_install_message_handler.c

==============================================================================*/
#pragma ident   "@(#)db_install_message_handler.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_install_message_handler.c"


/*==============================================================================
Function:       db_install_message_handler()

Description:    This installs a message handler for the Datbase server to call 
                when it encounters an error.  The only reason this routine 
                exists is so that all of the Sybase-dependent code is in 
                this directory.  There is a call in the src/framegen code 
                to this routine and we didn't want code there calling 
                sybase routines directly.  

Creator:        Lawrence Stevens

Creation Date:  Mon Sep 23 08:40:09 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "db_sybint.h"

int db_install_message_handler( 
    int     (*new_message_handler)() )      /* pointer to the function  */
{

    /* some error checking.  */
    if ( new_message_handler == NULL)
        return FALSE ;

    /* Install the new MESSAGE HANDLER */
    dbmsghandle(new_message_handler) ; /* caller-supplied */

    return TRUE ;

}

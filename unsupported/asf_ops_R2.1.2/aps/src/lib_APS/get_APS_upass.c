#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       get_APS_upass.c

Description:    
    This module contains the function for decrypting a file to obtain
    username and password information.  The username and password obtained 
    is used by the APS for transferring files to ACS or systems other remote 
    hosts.

External Functions Defined:
    get_APS_upass
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident	"@(#)get_APS_upass.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.get_APS_upass.c"

#include <stdio.h>
#include <string.h>
#include <aps_log_msg.h>    /* for aps_log_msg()  calls  */
#include <sys/types.h>
#include <sys/stat.h>
#include <des_crypt.h>
#include "aps_encrypt.h"   /* for BUFSIZE etc.    */

#ifdef __STDC__
char *get_APS_upass(char *) ;
#else
char *get_APS_upass() ;
#endif


/*==============================================================================
Function:   get_APS_upass

Description:    
    This function decrypts an APS encrypted password file.  The
    information obtained is a string containing a username and password which
    APS uses to transfer files for a particular interface. The format of the
    string is simply the unencoded username followed by the password separated
    by a single comma.  It is assumed that the username does not contain a
    comma.

Parameters:
    char *upassfile - name of password file to decrypt

Returns:    pointer to unencoded username and password string or NULL
Creator:    Norbert Piega   
Creation Date:  12/09/1994
Notes:      
==============================================================================*/

#ifdef __STDC__
char *
get_APS_upass(char *upassfile)
#else
char *
get_APS_upass(upassfile)
   char *upassfile ;
#endif
{
    char    key[KEYSIZE] ;
    char    iv[IVSIZE] ;
    char    buf[BUFSIZE] ;
    char    msg[256] ;
    FILE    *pass_fp ;
    char    *decoded = NULL ;
    int     estatus ;

    memset(key, 0, (size_t)sizeof(key)) ;
    memset(iv, 0, (size_t)sizeof(iv)) ;
    strcpy(key, APS_ENC_KEY) ;
    des_setparity(key) ;
 
    pass_fp = fopen(upassfile,"r") ;
    if ( pass_fp == (FILE *)NULL )
    {
        sprintf(msg, "ERROR:  password file %s could not be opened", 
            upassfile ) ; 
        fprintf( stderr, "%s\n", msg ) ;
        aps_log_msg(" ", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return(NULL) ;
    }

    fread(buf, sizeof(char), BUFSIZE, pass_fp) ;
    fclose(pass_fp) ;
    estatus = cbc_crypt(key, buf, BUFSIZE, DES_DECRYPT | DES_SW, iv);
    switch(estatus)
    {
    case DESERR_NONE:
        break ;

#ifdef SEE_MAN_PAGE_FOR_cbc_crypt
    case DESERR_NOHWDEVICE:
        syslog(LOG_DEBUG, "NOTICE: cbc_crypt status DESERR_NOHWDEVICE\n") ;
        break ;
#endif

    case DESERR_BADPARAM:
        sprintf(msg, "Bad parameters passed to cbc_crypt" ) ;
        fprintf( stderr, "ERROR:  %s\n", msg ) ;
        aps_log_msg(" ", APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        return(NULL) ;
        break ;

    default:
        sprintf(msg, "Unexpected status %d returned by cbc_crypt", estatus ) ;
        fprintf( stderr, "ERROR:  %s\n", msg ) ;
        aps_log_msg(" ", APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        return(NULL) ;
    }

    decoded = (char *)malloc(sizeof(char)*(strlen(buf)+1)) ;

    if ( decoded == (char *)NULL )
    {
        sprintf(msg, "%s(%d):  malloc() failed when decoding password. ", 
            __FILE__, __LINE__ ) ;
        fprintf( stderr, "ERROR:  %s\n", msg ) ;
        aps_log_msg(" ", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
    }
    else
        strcpy(decoded, buf) ;

    return(decoded) ;

} /* get_APS_upass */

/* End of File */

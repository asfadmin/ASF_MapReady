#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   change_to_stoicdir.c

Description:    contains one C function change_to_stoicdir().

External Functions Defined:
int change_to_stoicdir()
int init_vec_lib()
init_vec_lib_message()
init_vec_lib_exit()


File Scope Functions:
    
External Variables Defined:
none.
    
File Scope Variables:
none.
    
Notes:

==============================================================================*/
#pragma ident	"@(#)change_to_stoicdir.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.change_to_stoicdir.c"


/*==============================================================================
Function:       init_vec_lib()

Description:    The fuction initializes the vectory library with data 
from the stoic file.  If there is an error, a non-zero code is returned.
The method is to save the current working directory, then change to 
the stoic file directory.  Then call a routine that will invoke the 
vector library and cause it to initialize with the stoic file.  When 
that is done, change back to the original working directory, then return. 
The calling program should exit if the return code is non-zero.  

Parameters:     none

Returns:        int   
= 0  if no error, 
= 1  if there is an error

Creator:        Lawrence Stevens

Creation Date:  11/30/1994

==============================================================================*/

#include <unistd.h>     /* needed for chdir()       */
#include "timeconv.h"
#include <aps_defs.h>   /* for APS_EXIT_ERROR   */
#include "aps_log_msg.h"

int init_vec_lib()
{
int     rcode;          /* return code from chdir           */
char    *cwd;           /* current working directory        */
double  et;             /* for use in the call to tc_asf2et */

double	days, secs, jd ;       /* returned values from ECONST() in vector lib*/
int 	file_presence_flag ;   /* file_presence_flag == TRUE if present.     */
extern int econst_() ;

/* obtain the current working directory; this is a malloc for up to     */
/* 200 bytes.  we can free this later.                                  */
cwd = getcwd(NULL,200);

/* change to the stoic file directory       */
rcode = change_to_stoicdir();

if(rcode)
    return (rcode);

/* 
-- run a vectory library routine to get the vector library to 
-- initialize   itself.
-- this is to prevent the WARNING messages.  
-- note:  in change_to_stoicdir, we check the file presence already. 
*/
rcode = econst_(&days, &secs, &jd, &file_presence_flag ) ;

/* change back to the original current working directory.           */
rcode = chdir(cwd);
if(rcode)
    return (rcode + 1000);

/* free the cwd memory to be clean.                                 */
free(cwd);

return (0);

}

/*==============================================================================
Function:       init_vec_lib_message(int rcode)

Description:    Print the error message corresponding to the return 
code from init_vec_lib().  

Parameters:     none

Returns:        int   
this routine prints a message.  

Creator:        Lawrence Stevens

Creation Date:  12/07/1994

==============================================================================*/

void init_vec_lib_message(int rcode)
{

printf("\n\n");
printf("Error.  Unable to initialize the Vector Library with a Stoicfile.\n");

if(rcode == 1)
    printf("The stoicfile does not exist.\n");

if(rcode == 2)
    printf("The stoicfile exists but could not be read.\n");

if(rcode == 3)
    printf("The environment variable APS_DATA was not set.\n");

printf("Please check your configuration.\n");

return;
}


/*==============================================================================
Function:       init_vec_lib_exit(int rcode)

Description:    Exit with an error message noting that the vector library 
could not be initialized due to no stoic file found.  

Parameters:     none

Returns:        int   
this routine prints a message and exits.  

Creator:        Lawrence Stevens

Creation Date:  12/02/1994

==============================================================================*/

int init_vec_lib_exit(int rcode)
{
    init_vec_lib_message(rcode);
    system("banner ERROR");
    exit(APS_EXIT_ERROR);
}


/*==============================================================================
Function:       change_to_stoicdir()

Description:    The fuction changes the current working directory 
to the directory that contains the current stoicfile.  

This directory is:
$APS_DATA/stoicfiles/current

Parameters:     none

Returns:        int   
= 0  if no error, 
= -1 if there is an error in chdir.  This is just like the library routine 
     chdir().
=  1 if the stoicfile does not exist.
=  2 if the stoicfile exists but the process can't read the stoicfile.
=  3 if APS_DATA has no value - returns NULL from getenv

Creator:        Lawrence Stevens

Creation Date:  11/30/1994

Notes:      The stoicfile, a file named "stoicfile", contains 
leap second info, earth rotation, and polar motion data.  
This file is read by the Vectory Library routines and 
contains observational data good for a few months into 
the future and back about a year and a half.  Any APS 
function that deals with time or earth rotation needs a 
stoicfile in the current working directory that covers 
the time period in which the function operates.  
This routine was created to satisfy that need for planning 
needs which deal with the future and the recent past.
This routine changes the current working directory to one
containing the latest stoicfile.  
==============================================================================*/


int change_to_stoicdir()
{
extern  aps_access(char *filename, char *access_flags, int *returncode);
char    *base_path;     /* directory path from env APS_DATA */
int     rcode;          /* return code from chdir           */
char    stoicdir[150];  /* directory path containing the current stoicfile */
char    msg[256] ;

/* decode env APS_DATA to obtain the root directory */
base_path = (char *) getenv("APS_DATA");
if(base_path == NULL)
{
    sprintf(msg, "%s(%d):  ERROR:  no value for env APS_DATA.",
        __FILE__, __LINE__ ) ; 
    aps_log_msg( " ", APS_CRITICAL,  msg, DO_SYSLOG, DO_PRINT ) ;
    return 3;
}
    
strcpy(stoicdir, base_path);

/* insure that the last character of stoicdir is a "/" if not already so. */
if ( stoicdir[ strlen(stoicdir) - 1 ] != '/')
    strcat(stoicdir, "/");

/* add to path to get location of the current stoicfile */
strcat(stoicdir, "stoicfiles/current");

/* now chdir to this directory */
rcode = chdir(stoicdir);
if(rcode != 0)
{
    sprintf(msg, "%s(%d):  ERROR changing to stoicfile directory:  %s",
        __FILE__, __LINE__, stoicdir ) ; 
    aps_log_msg( " ", APS_CRITICAL,  msg, DO_SYSLOG, DO_PRINT ) ;
    return rcode;
}

/* so far OK.  now check for existence of stoicfile */
aps_access("stoicfile", "f", &rcode);
if(rcode != 0)
{
    sprintf(msg, 
    "%s(%d):  ERROR:  stoicfile %s/stoicfile does not exist.  You must create this file.",
        __FILE__, __LINE__, stoicdir ) ;
    aps_log_msg( " ", APS_CRITICAL,  msg, DO_SYSLOG, DO_PRINT ) ;
    return 1;
}

/* so far OK.  now check for readability of stoicfile */
aps_access("stoicfile", "r", &rcode);
if(rcode != 0)
{
    sprintf(msg, "%s(%d):  ERROR:  stoicfile %s/stoicfile cannot be read.   Check permissions",
        __FILE__, __LINE__, stoicdir ) ;
    aps_log_msg( " ", APS_CRITICAL,  msg, DO_SYSLOG, DO_PRINT ) ;
    return 2;
}

return (0);

}

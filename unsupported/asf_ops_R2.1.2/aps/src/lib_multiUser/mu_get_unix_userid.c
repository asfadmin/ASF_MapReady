#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_get_unix_userid.c

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
#pragma ident   "@(#)mu_get_unix_userid.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_get_unix_userid.c"



/*==============================================================================
Function:       mu_get_unix_userid()

Description:    obtains the first MU_UNIX_USERID_STRLEN characters of 
                the "effective userid".  

Returns:        TRUE if ok, FALSE if the userid was not gotten.   

Creator:        Lawrence Stevens

Creation Date:  Wed Dec  4 11:55:07 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "mu.h"
#include <pwd.h>  /* for geteuid(), struct passwd, uid_t, and getpwuid().  */
#include <sys/types.h>     /*  for geteuid()    */
#include <unistd.h>        /*  for geteuid()    */


int mu_get_unix_userid(
    char *mu_unix_userid) /* allow MU_UNIX_USERID_STRLEN + 1  chars */
{
    uid_t           effective_uid ;
    struct passwd   *password_struct ;


    /*
    -- get the current userid, the first 
    -- MU_UNIX_USERID_STRLEN characters only:
    */

    /*
    -- step 1:  get the effective uid.  If someone has done a 
    --          switch user, the login userid is unchanged; the effective
    --          userid is the new userid.  We want the effective 
    --          userid.  
    --  geteuid() returns type of uid_t, like a number.  
    --            from this, we can determins userid.  
    */
    effective_uid = geteuid() ;

    /*
    -- step 2:  use the effective_uid to obtain a pointer to 
    --          the password structure
    --     Password entries are represented by the struct passwd 
    --     structure defined in <pwd.h>:
    --          struct passwd {
    --              char *pw_name;    /x user's login name x/
    --              char *pw_passwd;  /x no longer used x/
    --              uid_t pw_uid;        /x user's uid x/
    --              gid_t pw_gid;        /x user's gid x/
    --              char *pw_age;     /x not used x/
    --              char *pw_comment; /x not used x/
    --              char *pw_gecos;   /x typically user's full name x/
    --              char *pw_dir;     /x user's home dir x/
    --              char *pw_shell;   /x user's login shell x/
    --          };
    */
    password_struct = getpwuid( effective_uid ) ;

    /* 
    -- now obtain the first MU_UNIX_USERID_STRLEN characters of 
    -- password_struct->pw_nam plus a null terminator for the 
    -- string.  
    */
    mu_unix_userid[0] = '\0' ;
    (void) strncat(mu_unix_userid, 
        password_struct->pw_name, 
        MU_UNIX_USERID_STRLEN ) ;
 
    if( strlen(mu_unix_userid) < 1 )
        return FALSE ;
 
    return TRUE ;
}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_get_permission.c

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
#pragma ident   "@(#)mu_get_permission.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_get_permission.c"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>       /* for strcmp()   */
#include <unistd.h>       /* for sleep()    */
#include <aps_defs.h>
#include <aps_log_msg.h>
#include <db_sybint.h>
#include <aps_db_table.h>
#include <db_active_dar_activities.h>
#include <db_active_single_activities.h>
#include <db_active_planning_activities.h>
#include <mu_utilities.h>


/*==============================================================================
Function:       mu_get_permission()

Description:    gets or verifies permission, according to whether 
                permission_id == 0, with retries.  

                If permission_id != 0, but if parameters are NOT supplied, 
                then the routine will verify permission.  If parameters 
                are supplied, it will get a new permission.  

                For single activities with permission_id != 0, 
                the routine must always verify permission.  

                This routine is mainly a labor-saving device, to prevent 
                duplication of code in getting permissions and for making 
                re-tries, and is used by several executables (Create nominal 
                orbit, Create nominal coverage, etc.) that write to a log 
                file.  

                NOTE:  the calling program should check the input
                parameters before making the call.
                Error checking is done here, AND the results are printed
                to standard error and syslog, and the return code is
                always -1 in every case except when permission is granted.
                So the calling program does not and cannot print any
                error messages:
 
                if return_code < 0, permission was either NOT valid or
                                    NOT granted, or there was an error
                                    message (already printed); the calling
                                    program will terminate.
                if return_code > 0, then
                   return_code == permission_id
                                    The permission was either valid or
                                    granted.  The calling program then
                                    proceeds normally.

Creator:        Lawrence Stevens

Creation Date:  Sun Jan 12 00:31:41 PST 1997

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int mu_get_permission( 
    char        *progname,           /*  name of executable, for syslog()    */
    DBPROCESS   *APS_dbproc,         /*  Sybase process pointer              */
    int         permission_id,       /*  if != 0, verify this permission.    */
    char        *mu_activity_type,   /*  Multi-user activity type.           */
    char        *mu_activity_id,     /*  Multi-user activity id.             */
    char        *strttime,           /*  start time of planning time bracket.*/
    char        *stoptime,           /*  end time of planning time bracket.  */
    char        *station_id,         /*  station id of planning activity.    */
    int         darid,               /*  DAR id of dar activity.             */
    int         n_retries,           /*  retry logic                         */
    int         n_seconds_retry )    /*  retry logic                         */
{

    llist       *perm_list = NULL ;

    int     j ;
    int     return_code;

    char    buf[200];

    /*
    -- check to see if we should get a new permission, 
    -- or validate an old one.  
    -- If permission_id == 0, then get a new permission.  
    -- If any parameters are supplied, then get a new permission.  
    */

    if ( permission_id == 0 
    ||   strttime   != NULL 
    ||   stoptime   != NULL 
    ||   station_id != NULL 
    ||   darid      != 0    )
    {
        /* 
        -- get permission, do not validate an existing permission.  
        -- If the permission id is 0, you cannot validate it.  
        -- If at least one parameter value is supplied, we should 
        -- get permission, since validation of a permission does 
        -- not need any parameter values at all.  
        --
        -- put the try in a loop 
        -- for retry logic.  
        -- total number of tries is n_retries + 1;
        */
        perm_list = create_dyn_llist() ;
        for ( j = 0 ; j < (n_retries + 1) ; j++ )
        {
            if ( j > 0 )
            {
                /*
                -- wait the number of seconds indicated, 
                -- between retries.  Don't wait at the start.
                */
                (void) sleep( (unsigned int) n_seconds_retry ) ;

                /* remove permission members from previous call:  */
                DEL_ALL( perm_list ) ;
            }
            return_code = mu_permission_request( APS_dbproc, 
                permission_id,
                mu_activity_id, 
                mu_activity_type,
                perm_list,                  /*  blocking_permission_list  */
                strttime, stoptime, station_id, 
                darid ) ;
            if( return_code < 0 )
            {
                (void)fprintf( stderr, 
                    "%s:\n\n%s\n", progname, MU_ERROR_MESSAGE(return_code) ) ;
                (void)sprintf( buf, 
                    "ERROR:  %s", MU_ERROR_MESSAGE(return_code) ) ;
                aps_log_msg( progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
                DEL_LIST(perm_list) ;
                /* indicate to exit.  */
                return -1 ;
            }
            else if (return_code == 0 )
            {
                /* 
                -- permission denied.  
                -- continue retry logic 
                -- until no more tries allowed.  
                */
                continue ;
            }
            else
            {
                /*
                -- permission was granted; no 
                -- more re-tries.  
                -- return_code = permission obtained 
                */
                (void)printf(
"%s(%d):  Multi-user permission for \n\t%s %s activity GRANTED: permission_id = %d\n",
                    __FILE__, __LINE__, 
                    mu_activity_type, mu_activity_id, return_code ) ;

                /* 
                -- llist not needed any more.  
                */
                DEL_LIST(perm_list) ;
                break ;
            }

        }  /* for-loop on tries.  */

        /* 
        -- permission_id was not provided 
        -- in command line.  it was requested
        -- result is stored in return_code.  
        */
        if ( return_code == 0 )
        {
            /* 
            -- permission was not granted.  
            -- retries were exhausted  
            -- print to stderr the blocking permissions
            -- and EXIT.  
            */
            (void)sprintf(buf, 
                "Permission request for %s activity:  %s DENIED",
                mu_activity_type, mu_activity_id ) ;
            if( strttime != NULL )
                (void)sprintf( buf, "%s  %s", buf, strttime ) ;
            if( stoptime != NULL )
                (void)sprintf( buf, "%s  %s", buf, stoptime ) ;
            if( station_id != NULL )
                (void)sprintf( buf, "%s  %s", buf, station_id ) ;
            if( darid != 0 )
                (void)sprintf( buf, "%s  DAR = %d", buf, darid ) ;

            (void)fprintf(stderr, "%s\n", buf ) ;
            aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT) ;
            (void)fprintf(stderr, "Blocking activities:\n" ) ;

            /* print blocking activities:  */
            if ( strcmp(mu_activity_type, MU_SINGLE_ACTIVITY_TYPE ) == 0 )
            {
                db_fprint_list( stderr, perm_list, 
                    APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES) ) ;
            }
            else if ( strcmp(mu_activity_type,MU_PLANNING_ACTIVITY_TYPE) == 0 )
            {
                db_fprint_list( stderr, perm_list, 
                    APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;
            }
            else if ( strcmp(mu_activity_type,MU_DAR_ACTIVITY_TYPE) == 0 )
            {
                db_fprint_list( stderr, perm_list, 
                    APS_CDEFS(ACTIVE_DAR_ACTIVITIES) ) ;
            }

            DEL_LIST(perm_list) ;
            /* indicate to exit.  */
            return -1 ;
        }
    } /*    endif  permission_id == 0  [permission not passed in cmd line]   */
    else
    {
        /* 
        -- permission_id != 0 
        -- permission WAS passed in command line, 
        -- must validate this permission now.  
        */

        return_code = mu_permission_validate(
            permission_id,
            mu_activity_id,
            mu_activity_type ) ;
        if( return_code < 0 )
        {
            (void)fprintf( stderr, 
                "%s:\n\n%s\n", progname, MU_ERROR_MESSAGE(return_code) ) ;
            (void)sprintf( buf, "ERROR:  %s", MU_ERROR_MESSAGE(return_code) ) ;
            aps_log_msg( progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            /* indicate to exit.  */
            return -1 ;
        }
        if ( return_code != permission_id )
        {
            /* 
            -- permission is not valid.  
            -- Error; must exit
            */
            (void)fprintf( stderr, 
            "%s:\n\nPassed permission id %d for %s activity %s WAS NOT VALID\n",
                progname, permission_id, mu_activity_type, mu_activity_id ) ;
            (void)sprintf( buf, 
                "Passed permission id %d for %s activity %s WAS NOT VALID\n", 
                permission_id, mu_activity_type, mu_activity_id ) ;
            aps_log_msg( progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);
            /* indicate to exit.  */
            return -1 ;
        }

        /* 
        -- the permission was valid.  
        -- return_code == permission_id 
        -- OK.  can proceed.  
        */
        (void)printf(
        "%s(%d):  Multi-user permission %d for \n\t%s %s activity VERIFIED\n",
            __FILE__, __LINE__, 
            return_code, mu_activity_type, mu_activity_id ) ;

    }  /* endif permission_id != 0   */

    /* 
    -- normal case.  new or validated permission id 
    -- is stored in return_code 
    -- pass permission_id back to calling routine:  
    -- permission id is stored in return_code, which 
    -- was passed from validation or permission function:
    */
    return return_code ;

}

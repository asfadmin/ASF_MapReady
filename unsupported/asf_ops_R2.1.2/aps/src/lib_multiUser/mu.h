#ifndef _MU_H_
#define _MU_H_

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:       mu.h
Description:    has "private" function prototypes.  
Creator:        Lawrence Stevens
Notes:      
==============================================================================*/
#pragma ident   "@(#)mu.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu.h"

/* USED FREQUENTLY   */
#include <string.h>        /* for strcpy() etc.  */
#include <stdio.h>        /* for cuserid()       */


/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for NULL, TRUE, FALSE, MIN_REV and others  */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "mu_utilities.h"   /* for the mu - library definitions   */

/* 
-- userid length for the Multi-user library.  
-- (internal use.)  
*/
#define MU_UNIX_USERID_STRLEN   8

/*
-- "PRIVATE"  FUNCTION DECLARATIONS for libdtkm.a
*/
int mu_dar_activity_request(
    DBPROCESS   *dbproc,        /* input sybase session pointer              */
    int         permission_id,  /* input permission id, usually = 0          */
    char        *mu_activity_id,/* input mu activity for which permission is
                                   requested.                                */
    int         darid,          /* input darid for permission                */
    llist       *blocking_permission_list ) ;
                                /* output linked list for blocking permissions.
                                   used only if permission is denied.  calling
                                   routine must allocate the list and it must
                                   have no members at input.                 */

int mu_get_new_permission_id(
    DBPROCESS   *dbproc ) ;    /* Pointer to Sybase structure for session. */

int mu_get_unix_userid(
    char *mu_unix_userid );   /* allow MU_UNIX_USERID_STRLEN + 1 chars */

int mu_insert_dar_perm(
    DBPROCESS   *dbproc,         /*  input Sybase session pointer.         */
    char        *mu_activity_id, /*  input Multi-user dar-type activity id */
    int         permission_id,   /*  input:  the permission id to use.     */
    int         darid  ) ;       /*  input:  the darid to use.             */

int mu_insert_planning_perm(
    DBPROCESS   *dbproc,         /*  input Sybase session pointer.        */
    char        *mu_activity_id, /*  input planning-type activity id      */
    int         permission_id ,  /*  input:  the permission id to use.    */
    char        *padded_strttime,/*  input:  the enlarged time bracket    */
    char        *padded_stoptime,/*  input:  the enlarged time bracket    */
    char        *station_id ) ;  /*  input:  the station_id value         */

int mu_insert_single_perm(
    DBPROCESS   *dbproc,         /*  input Sybase session pointer.            */
    char        *mu_activity_id, /*  input Multi-user single-type activity id */
    int         permission_id ); /*  input:  the permission id to use.        */

int mu_pad_time_bracket(
    /* NOTE:  these times are ASF-format times.   */
    char    *strttime,         /*  input planning activity start time         */
    char    *stoptime,         /*  input planning activity stop time          */
    char    *padded_strttime,  /*  output start time of enlarged time bracket */
    char    *padded_stoptime );/*  output end time of enlarged time bracket   */

int mu_planning_activity_request(
    DBPROCESS   *dbproc,       /*  input sybase session pointer              */
    int         permission_id, /*  input permission id, usually = 0          */
    char        *mu_activity_id,/* input mu activity for which permission is
                                   requested.                                */
    char        *strttime,     /*  input start time for activity.            */
    char        *stoptime,     /*  input stop time for activity.             */
    char        *station_id,   /*  input station id (ASF, MCM, or ALL)       */
    llist       *blocking_permission_list ) ;
                               /*  output linked list for blocking permissions.
                                   used only if permission is denied.  calling
                                   routine must allocate the list and it must
                                   have no members at input.                 */

int mu_single_activity_request(
    DBPROCESS   *dbproc,       /*  input sybase session pointer              */
    int         permission_id, /*  input permission id, usually = 0          */
    char        *mu_activity_id,/* input mu activity for which permission is
                                   requested.                                */
    llist       *blocking_permission_list ) ;
                               /*  output linked list for blocking permissions.
                                   used only if permission is denied.  calling
                                   routine must allocate the list and it must
                                   have no members at input.                 */

int mu_count_sysprocesses_khph(
    DBPROCESS   *dbproc,       /*  input sybase session pointer            */
    int         kpid,          /*  input kernel process id in Sybase       */
    char        *hostname,     /*  input nodename                          */
    char        *progname,     /*  input programe name                     */
    char        *process_id ); /*  input process id, a character string    */
 
int mu_get_sysprocesses_khph(
    DBPROCESS   *dbproc,      /* input sybase session pointer             */
    int         *kpid,        /* output current Sybase kernel process id  */
    char        *hostname,    /* output current nodename.  allow 11 chars */
    char        *progname,    /* output current programe name.  allow 17 char*/
    char        *process_id );/* output current process id, 9 chars       */

/* 
-- These 3 validation routines return TRUE if valid, FALSE if not. 
-- They return < 0 if error, when you use MU_ERROR_MESSAGE(code) defined in 
-- include/local/mu_utilities.h
-- print using:  printf("%s\n", MU_ERROR_MESSAGE(code) ) ;
*/
int mu_validate_activity_id(
    char    *activity_type,
    char    *mu_activity_id ) ;
int mu_validate_sat_phase_nominal_orb( char *mu_activity_id ) ;
int mu_validate_station_id( char *station_id ) ;

int mu_verify_dar_perms(
    DBPROCESS   *dbproc,        /* Sybase session pointer.                */
    llist       *perm_list ) ;  /* list of dar activity permissions.  */

int mu_verify_planning_perms(
    DBPROCESS   *dbproc,        /* Sybase session pointer.                */
    llist       *perm_list ) ;  /* list of planning activity permissions.  */

int mu_verify_single_perms(
    DBPROCESS   *dbproc,        /* Sybase session pointer.                */
    llist       *perm_list ) ;  /* list of single activity permissions.  */

#endif  /* _MU_H_ */

#ifndef APS_GET_SEQUENCE_NUMBER_H
#define APS_GET_SEQUENCE_NUMBER_H

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_get_sequence_number.h
Description:    header file for the aps new dtkid functions.  
Creator:        Lawrence Stevens
Notes:          
==============================================================================*/
#pragma ident   "@(#)aps_get_sequence_number.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/global/SCCS/s.aps_get_sequence_number.h"

#include <db_sybint.h>        /* for DBPROCESS       */

/* PROTOTYPES:  */
/* 
-- aps_get_sequence_number() and db_get_new_dtkid() both 
-- return new dtkid (sequence number).  
-- But if the return value < 0, decode it 
-- using APS_NEW_DTKID_ERROR_MESSAGE(return_value)
-- defined below, into an error message.  
*/
int aps_get_sequence_number(
    char    *progname,        /* input progname needed by sybase              */
    char    *sybase_userid,   /* input userid                                 */
    char    *sybase_password, /* input password                               */
    char    *dbname,          /* input db name                                */
    char    *sat,             /* input 2-char satellite or platform           */
    int     rev,              /* input rev number                             */
    char    *sybase_server ); /* input sybase SQL server: NULL uses DSQUERY   */

int db_get_new_dtkid(
    DBPROCESS   *APS_dbproc, /* input sybase process pointer.                */
    char        *sat,        /* input 2-char satellite (platform): E2/R1 etc.*/
    int         rev ) ;      /* input rev number.  > 0                       */

/* ERROR MESSAGES  */

/* a macro to decode the error code into a string:  */
extern char *aps_new_dtkid_error_message[] ;
 
#define APS_NEW_DTKID_ERROR_MESSAGE( code ) \
        aps_new_dtkid_error_message[ -(code+1000) ]

/* 
--  these codes correspond to the files 
-- aps_sp_new_dtkid.crt  and aps_new_dtkid_errors.c 
*/
#define APS_NEW_DTKID_ERROR_INPUT_REV_LE_ZERO                         -1001
#define APS_NEW_DTKID_ERROR_MULTIPLE_ENTRIES_IN_SAT_REV_DTKID         -1002
#define APS_NEW_DTKID_ERROR_DB_ERROR_IN_SELECT_FROM_SAT_REV_DTKID     -1003
#define APS_NEW_DTKID_ERROR_DB_ERROR_DTKID_AT_MAX_VALUE               -1004
#define APS_NEW_DTKID_ERROR_DB_ERROR_IN_INSERT_INTO_SAT_REV_DTKID     -1005
#define APS_NEW_DTKID_ERROR_DB_ERROR_INVALID_DTKID_IN_SAT_REV_DTKID   -1006
#define APS_NEW_DTKID_ERROR_DB_ERROR_IN_UPDATE_SAT_REV_DTKID          -1007
#define APS_NEW_DTKID_ERROR_BAD_INPUT_DTKID                           -1008
#define APS_NEW_DTKID_ERROR_INPUT_ASSIGNED_DTKID_TOO_LOW_TO_BE_USED   -1009

/* these codes correspond to the file aps_new_dtkid_errors.c   */
#define APS_NEW_DTKID_ERROR_DB_NOT_OPENED_OK                          -1010
#define APS_NEW_DTKID_ERROR_INVALID_SAT_VALUE                         -1011
#define APS_NEW_DTKID_ERROR_NO_RETURNED_VALUE_FROM_SP_CALL            -1012
#define APS_NEW_DTKID_ERROR_NULL_DBNAME                               -1013
#define APS_NEW_DTKID_ERROR_NULL_DBPROC                               -1014
#define APS_NEW_DTKID_ERROR_NULL_SAT                                  -1015
#define APS_NEW_DTKID_ERROR_NULL_SYBASE_PASSWORD                      -1016
#define APS_NEW_DTKID_ERROR_NULL_SYBASE_USERID                        -1017
#define APS_NEW_DTKID_ERROR_REV_GE_9999999                            -1018
#define APS_NEW_DTKID_ERROR_SAT_STRLEN_NE_2                           -1019
#define APS_NEW_DTKID_ERROR_INPUT_ASSIGNED_DTKID_LE_ZERO              -1020
#define APS_NEW_DTKID_ERROR_INPUT_ASSIGNED_DTKID_GE_100O              -1021

#endif  /*   APS_GET_SEQUENCE_NUMBER_H     */

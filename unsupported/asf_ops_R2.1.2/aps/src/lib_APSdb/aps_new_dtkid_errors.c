#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_new_dtkid_errors.c

Description:    error strings to decode error messages.  
                Using the macro APS_NEW_DTKID_ERROR_MESSAGE
    
Notes:          
these codes correspond to the file aps_get_sequence_number.h  

==============================================================================*/
#pragma ident   "@(#)aps_new_dtkid_errors.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_APSdb/SCCS/s.aps_new_dtkid_errors.c"

/* 
-- these codes correspond to the files 
-- include/global/aps_get_sequence_number.h
-- and etc/db/tables/aps_sp_new_dtkid.crt     (-1001 to -1007 )
*/

char    *aps_new_dtkid_error_message[] = 
{
    "  0  is not an error code.  ",
    "APS_NEW_DTKID_ERROR_INPUT_REV_LE_ZERO",                         /* -1001 */
    "APS_NEW_DTKID_ERROR_MULTIPLE_ENTRIES_IN_SAT_REV_DTKID",         /* -1002 */
    "APS_NEW_DTKID_ERROR_DB_ERROR_IN_SELECT_FROM_SAT_REV_DTKID",     /* -1003 */
    "APS_NEW_DTKID_ERROR_DB_ERROR_DTKID_AT_MAX_VALUE",               /* -1004 */
    "APS_NEW_DTKID_ERROR_DB_ERROR_IN_INSERT_INTO_SAT_REV_DTKID",     /* -1005 */
    "APS_NEW_DTKID_ERROR_DB_ERROR_INVALID_DTKID_IN_SAT_REV_DTKID",   /* -1006 */
    "APS_NEW_DTKID_ERROR_DB_ERROR_IN_UPDATE_SAT_REV_DTKID",          /* -1007 */
    "APS_NEW_DTKID_ERROR_BAD_INPUT_DTKID",                           /* -1008 */
    "APS_NEW_DTKID_ERROR_INPUT_ASSIGNED_DTKID_TOO_LOW_TO_BE_USED",   /* -1009 */
    "APS_NEW_DTKID_ERROR_DB_NOT_OPENED_OK",                          /* -1010 */
    "APS_NEW_DTKID_ERROR_INVALID_SAT_VALUE",                         /* -1011 */
    "APS_NEW_DTKID_ERROR_NO_RETURNED_VALUE_FROM_SP_CALL",            /* -1012 */
    "APS_NEW_DTKID_ERROR_NULL_DBNAME",                               /* -1013 */
    "APS_NEW_DTKID_ERROR_NULL_DBPROC",                               /* -1014 */
    "APS_NEW_DTKID_ERROR_NULL_SAT",                                  /* -1015 */
    "APS_NEW_DTKID_ERROR_NULL_SYBASE_PASSWORD",                      /* -1016 */
    "APS_NEW_DTKID_ERROR_NULL_SYBASE_USERID",                        /* -1017 */
    "APS_NEW_DTKID_ERROR_REV_GE_9999999",                            /* -1018 */
    "APS_NEW_DTKID_ERROR_SAT_STRLEN_NE_2",                           /* -1019 */
    "APS_NEW_DTKID_ERROR_INPUT_ASSIGNED_DTKID_LE_ZERO",              /* -1020 */
    "APS_NEW_DTKID_ERROR_INPUT_ASSIGNED_DTKID_GE_100O",              /* -1021 */
    "-1022  unknown error code.  ",
    "-1023  unknown error code.  ",
    "-1024  unknown error code.  ",
    "-1025  unknown error code.  ",
    "-1026  unknown error code.  ",
    " END OF THE LIST!!!       "
} ;

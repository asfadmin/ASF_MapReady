#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   mu_error_message.c

Description:    contains the error messages for libmu.a

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_error_message.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_error_message.c"



/*==============================================================================
Function:       mu_error_message.c

Description:    decodes a negative error code into a string.  
                The macro MU_ERROR_MESSAGE() decodes a negative 
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Thu Nov 21 15:10:20 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

char    *mu_error_message[] =
{
"zero is not an error code",
"MU_DB_ERROR_DELETING_FROM_ACTIVE_DAR_ACTIVITIES",                    /*   -1 */
"MU_DB_ERROR_DELETING_FROM_ACTIVE_PLANNING_ACTIVITIES",               /*   -2 */
"MU_DB_ERROR_DELETING_FROM_ACTIVE_SINGLE_ACTIVITIES",                 /*   -3 */
"MU_DB_ERROR_DURING_PHASE_TABLE_COUNTING",                            /*   -4 */
"MU_DB_ERROR_DURING_SATSENSOR_TABLE_COUNTING",                        /*   -5 */
"MU_DB_ERROR_DURING_SYSPROCESSES_COUNTING",                           /*   -6 */
"MU_DB_ERROR_DURING_SYSPROCESSES_RETRIEVE",                           /*   -7 */
"MU_DB_ERROR_INSERTING_DAR_ACTIVITY",                                 /*   -8 */
"MU_DB_ERROR_INSERTING_PLANNING_ACTIVITY",                            /*   -9 */
"MU_DB_ERROR_INSERTING_SINGLE_ACTIVITY",                              /*  -10 */
"MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_DAR_ACTIVITIES_TABLE",            /*  -11 */
"MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_PLANNING_ACTIVITIES_TABLE",       /*  -12 */
"MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_SINGLE_ACTIVITIES_TABLE",         /*  -13 */
"MU_DB_ERROR_RETRIEVING_FROM_PERMISSION_COUNTER_TABLE",               /*  -14 */
"MU_DB_ERROR_STATION_RELATION_QUERY_FAILED",                          /*  -15 */
"MU_DB_ERROR_UPDATING_MULTI_USER_WAIT",                               /*  -16 */
"MU_DB_ERROR_UPDATING_PERM_COUNTER_REC",                              /*  -17 */
"MU_ERROR_COULD_NOT_FIND_SYSPROCESSES_INFO_FOR_CURRENT_PROCESS",      /*  -18 */
"MU_ERROR_COULD_NOT_GET_USERID",                                      /*  -19 */
"MU_ERROR_DATA_CORRUPTION_DURING_VALIDATION",                         /*  -20 */
"MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_DAR_ACTIVITIES",                /*  -21 */
"MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_PLANNING_ACTIVITIES",           /*  -22 */
"MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_SINGLE_ACTIVITIES",             /*  -23 */
"MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_DAR_ACTIVITIES",                 /*  -24 */
"MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_PLANNING_ACTIVITIES",            /*  -25 */
"MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_SINGLE_ACTIVITIES",              /*  -26 */
"MU_ERROR_DURING_BEGIN_TRANSACTION",                                  /*  -27 */
"MU_ERROR_DURING_COMMIT_TRANSACTION",                                 /*  -28 */
"MU_ERROR_DURING_ROLLBACK_TRANSACTION",                               /*  -29 */
"MU_ERROR_FROM_FUNCTION_tc_time_pad",                                 /*  -30 */
"MU_ERROR_INPUT_ACTIVITY_ID_IS_LE_3_CHARS_LONG",                      /*  -31 */
"MU_ERROR_INPUT_ACTIVITY_ID_IS_NULL_PTR",                             /*  -32 */
"MU_ERROR_INPUT_ACTIVITY_TYPE_HAS_ILLEGAL_VALUE",                     /*  -33 */
"MU_ERROR_INPUT_ACTIVITY_TYPE_IS_LT_3_CHARS_LONG",                    /*  -34 */
"MU_ERROR_INPUT_ACTIVITY_TYPE_IS_NULL_PTR",                           /*  -35 */
"MU_ERROR_INPUT_BLOCKING_PERMISSION_LIST_IS_NOT_EMPTY",               /*  -36 */
"MU_ERROR_INPUT_BLOCKING_PERMISSION_LIST_IS_NULL_PTR",                /*  -37 */
"MU_ERROR_INPUT_DARID_IS_LE_ZERO_FOR_DAR_ACTIVITY",                   /*  -38 */
"MU_ERROR_INPUT_DAR_ACTIVITY_ID_HAS_ILLEGAL_VALUE",                   /*  -39 */
"MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NOT_EMPTY",                      /*  -40 */
"MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NULL_PTR",                       /*  -41 */
"MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT",                 /*  -42 */
"MU_ERROR_INPUT_DBPROC_IS_NULL",                                      /*  -43 */
"MU_ERROR_INPUT_HOSTNAME_POINTER_IS_NULL",                            /*  -44 */
"MU_ERROR_INPUT_HOSTNAME_STRING_ZERO_LENGTH",                         /*  -45 */
"MU_ERROR_INPUT_HOSTPROCESS_POINTER_IS_NULL",                         /*  -46 */
"MU_ERROR_INPUT_HOSTPROCESS_STRING_ZERO_LENGTH",                      /*  -47 */
"MU_ERROR_INPUT_KPID_IS_ZERO",                                        /*  -48 */
"MU_ERROR_INPUT_NOMINAL_ORBIT_ACTIVITY_ID_HAS_NON_COVERAGEABLE_SAT",  /*  -49 */
"MU_ERROR_INPUT_NOMINAL_ORBIT_ACTIVITY_ID_HAS_UNKNOWN_SAT_PHASE",     /*  -50 */
"MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO",                     /*  -51 */
"MU_ERROR_INPUT_PERMISSION_ID_IS_ZERO",                               /*  -52 */
"MU_ERROR_INPUT_PLANNING_ACTIVITY_ID_HAS_ILLEGAL_VALUE",              /*  -53 */
"MU_ERROR_INPUT_PLANNING_ACTIVITY_LIST_IS_NOT_EMPTY",                 /*  -54 */
"MU_ERROR_INPUT_PLANNING_ACTIVITY_LIST_IS_NULL_PTR",                  /*  -55 */
"MU_ERROR_INPUT_PROGNAME_POINTER_IS_NULL",                            /*  -56 */
"MU_ERROR_INPUT_PROGNAME_STRING_ZERO_LENGTH",                         /*  -57 */
"MU_ERROR_INPUT_SINGLE_ACTIVITY_ID_HAS_ILLEGAL_VALUE",                /*  -58 */
"MU_ERROR_INPUT_SINGLE_ACTIVITY_LIST_IS_NOT_EMPTY",                   /*  -59 */
"MU_ERROR_INPUT_SINGLE_ACTIVITY_LIST_IS_NULL_PTR",                    /*  -60 */
"MU_ERROR_INPUT_STATION_ID_IS_NULL",                                  /*  -61 */
"MU_ERROR_INPUT_STATION_ID_VALUE_IS_ILLEGAL",                         /*  -62 */
"MU_ERROR_INPUT_STOPTIME_IS_NOT_A_VALID_ASFTIME",                     /*  -63 */
"MU_ERROR_INPUT_STOPTIME_IS_NULL_FOR_PLANNING_ACTIVITY",              /*  -64 */
"MU_ERROR_INPUT_STRTTIME_IS_NOT_A_VALID_ASFTIME",                     /*  -65 */
"MU_ERROR_INPUT_STRTTIME_IS_NOT_BEFORE_STOPTIME",                     /*  -66 */
"MU_ERROR_INPUT_STRTTIME_IS_NULL_FOR_PLANNING_ACTIVITY",              /*  -67 */
"MU_ERROR_IN_MOVING_RECS_TO_LLIST",                                   /*  -68 */
"MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE",                               /*  -69 */
"MU_ERROR_MORE_THAN_ONE_MULTI_USER_WAIT_REC_UPDATED",                 /*  -70 */
"MU_ERROR_MORE_THAN_ONE_PERMISSION_COUNTER_REC_FOUND_IN_TABLE",       /*  -71 */
"MU_ERROR_NOT_ONLY_ONE_PERM_COUNTER_REC_UPDATED",                     /*  -72 */
"MU_ERROR_NO_MULTI_USER_WAIT_REC_UPDATED",                            /*  -73 */
"MU_ERROR_NO_PERMISSION_COUNTER_RECS_FOUND_IN_TABLE",                 /*  -74 */
"MU_ERROR_PADDED_STOPTIME_IS_NULL_FOR_PLANNING_ACTIVITY",             /*  -75 */
"MU_ERROR_PADDED_STRTTIME_IS_NULL_FOR_PLANNING_ACTIVITY",             /*  -76 */
"MU_ERROR_UNLINKING_PERM_RECORD",                                     /*  -77 */
"MU_ERROR_NO_SYSPROCESSES_PROG_NAME_CHECK_DBOPEN_CALL_2ND_ARGUMENT",  /*  -78 */
"-79 unknown error code.  ",
"-80 unknown error code.  ",
"-81 unknown error code.  ",
"-82 unknown error code.  ",
"-83 unknown error code.  ",
"-84 unknown error code.  ",
"-85 unknown error code.  ",
"-86 unknown error code.  ",
"-87 unknown error code.  ",
"-88 unknown error code.  ",
"-89 unknown error code.  ",
"-90 unknown error code.  ",
" END OF THE LIST!!!       "
} ;

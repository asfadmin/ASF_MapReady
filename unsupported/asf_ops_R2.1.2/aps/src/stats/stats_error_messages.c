#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   stats_error_messages.c

Description:    contains the error messages

==============================================================================*/
#pragma ident   "@(#)stats_error_messages.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_error_messages.c"



/*==============================================================================
Function:       stats_error_messages.c

Description:    decodes a negative error code into a string.  
                The macro STATS_ERROR_MESSAGE() decodes a negative 
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Fri Jan 30 11:27:28 PST 1998
==============================================================================*/

char    *stats_error_message[] =
{
    "zero is not an error code",
    "STATS_ERROR_CONDITION_ARG",                                  /*   -1 */
    "STATS_ERROR_STATS_CALLS_REC_NOT_INSERTED",                   /*   -2 */
    "STATS_ERROR_DB_ERROR_INSERTING_STATS_CALLS_REC",             /*   -3 */
    "STATS_ERROR_GETTING_MASK_TIMES",                             /*   -4 */
    "STATS_ERROR_DB_ERROR_COUNTING_STATS_CALLS_RECS",             /*   -5 */
    "STATS_ERROR_DB_ERROR_IN_APSDB_QUERY",                        /*   -6 */
    "STATS_ERROR_NO_ANTENNA_RECS_FOUND_IN_QUERY",                 /*   -7 */
    "STATS_ERROR_NO_DTK_RECS_FOUND_IN_RETRIEVE",                  /*   -8 */
    "STATS_ERROR_GT_1_ANTENNA_RECS_FOUND_IN_QUERY",               /*   -9 */
    "STATS_ERROR_NULL_POINTER_IN_ARGUMENT",                       /*  -10 */
    "STATS_ERROR_MALLOC_FAILED",                                  /*  -11 */
    "STATS_ERROR_TRANSLATING_SENSOR_TO_SENSOR_MODE",              /*  -12 */
    "STATS_ERROR_CONVERTING_ASFTIME_TO_ODL_TIME",                 /*  -13 */
    "STATS_ERROR_IN_CONDITION_PMF_VALUE",                         /*  -14 */
    "STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_AOS_LOS",              /*  -15 */
    "STATS_ERROR_IN_PMF_TIMES_IN_REDUCED_AOS_LOS",                /*  -16 */
    "STATS_ERROR_IN_PMF_TIMES_IN_SCHEDULED_AOS_LOS",              /*  -17 */
    "STATS_ERROR_IN_PMF_DOWNLINK_ACTIVITY_ID",                    /*  -18 */
    "STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_DOWNLINK",             /*  -19 */
    "STATS_ERROR_IN_PMF_TIMES_IN_REDUCED_DOWNLINK",               /*  -20 */
    "STATS_ERROR_IN_PMF_TIMES_IN_CANCELLED_SAR",                  /*  -21 */
    "STATS_ERROR_IN_PMF_TIMES_IN_PLANNED_SAR",                    /*  -22 */
    "STATS_ERROR_UNKNOWN_PMF_TYPE",                               /*  -23 */
    "STATS_ERROR_COULD_NOT_OPEN_PMF_FILE",                        /*  -24 */
    "STATS_ERROR_IMS_ERROR",                                      /*  -25 */
    "STATS_ERROR_IMS_FATAL",                                      /*  -26 */
    "STATS_ERROR_IMS_UNKNOWN_RETURN_CODE",                        /*  -27 */
    "-28  error string unknown.  Update stats_error_message.c", 
    "-29  error string unknown.  Update stats_error_message.c", 
    "-30  error string unknown.  Update stats_error_message.c", 
    "-31  error string unknown.  Update stats_error_message.c", 
    "-32  error string unknown.  Update stats_error_message.c", 
    "-33  error string unknown.  Update stats_error_message.c", 
    "-34  error string unknown.  Update stats_error_message.c", 
    "-35  error string unknown.  Update stats_error_message.c", 
    "-36  error string unknown.  Update stats_error_message.c", 
    "-37  error string unknown.  Update stats_error_message.c", 
    "-38  error string unknown.  Update stats_error_message.c", 
    " END OF THE LIST!!!       "
} ;

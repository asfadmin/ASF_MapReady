#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	phase_utilities.h
Description:	
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)phase_utilities.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.phase_utilities.h"

#ifndef _PHASE_UTILITIES_H_
#define _PHASE_UTILITIES_H_


/* FOR SYBASE INTERFACES   */
#include <db_sybint.h>      /* for APS sybase interface definitions    */
#include <db_phase.h>       /* for APS db table phase                  */

/* FOR APS MACROS, etc...    */
#include <dapps_defs.h>

#define PHASE_FIRST_REV_OK          1
#define PHASE_REV2ASFTIME_OK        1
#define PHASE_REV2ET_OK             1
#define DECODE_ERROR_MESSAGE_OK     1


/* ERROR CODES:  all must be unique,  -2000 < code < -1000   */

/* to decode the error code into a string:  */
extern char *phase_error_message[] ;
#define PHASE_ERROR_MESSAGE( code ) \
	    phase_error_message[ -(code + 1000) ]

/* FOR asftime_2_phase()    */
#define PHASE_APS_READER_SESSION_NOT_OPENED                -1001 
/* available error code.                                   -1002   */
#define NASDA_DB_QUERY_FAILED                              -1003
#define NASDA_NO_PHASE_RECS_FOUND                          -1004
#define NASDA_BAD_PHASE_REC_START_TIME                     -1005
#define NASDA_TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME        -1006
#define NASDA_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS        -1007
#define NASDA_TWO_PHASE_RECS_OVERLAP_IN_TIME               -1008
/* FOR NASDAc_asftime_rsp_angle_2_rev_time()    */
#define NASDA_BAD_VALUE_FOR_SEARCH_FLAG                    -1009
#define NASDA_ERROR_IN_PHASE_START_TIME_OR_ASFTIME         -1010
#define NASDA_ERROR_IN_PHASE_START_TIME                    -1011
#define NASDA_ERROR_IN_COMPUTED_TIME                       -1012
#define NASDA_BAD_ASFTIME                                  -1013
#define NASDA_BAD_RSP_ANGLE                                -1014
#define NASDA_ASFTIME_NOT_WITHIN_ANY_PHASE                 -1015
#define NASDA_NOT_A_NASDA_SATELLITE                        -1016
#define NASDA_BAD_RSP_PATH                                 -1017
#define NASDA_REV_NOT_FOUND_FOR_RSP                        -1018
/* FOR NASDAc_rev_asftime_2_rsp_angle()    */
#define NASDA_REV_NUMBER_NOT_IN_PHASE                      -1019
#define NASDA_INPUT_ASFTIME_NOT_WITHIN_INPUT_REV           -1020
/* FOR NASDAc_phase_rev2rsp()         */
#define NASDA_INPUT_PHASE_REC_IS_NULL                      -1021
#define NASDA_NOT_A_NASDA_PHASE_RECORD                     -1022
/* FOR NASDAc_phase_rsp2firstrev()         */
#define NASDA_RSP_NOT_FOUND_CHECK_PHASE_RELATION           -1023
#define NASDA_PHASE_POINTER_IS_NULL                        -1024
#define NASDA_INPUT_ASFTIME_NOT_WITHIN_INPUT_PHASE         -1025
#define PHASE_BAD_ASFTIME                                  -1026
#define PHASE_ERROR_IN_PHASE_START_TIME                    -1027
#define PHASE_INPUT_ASFTIME_NOT_WITHIN_INPUT_PHASE         -1028
#define PHASE_REV_NUMBER_NOT_IN_PHASE                      -1029
/* FOR asftime_2_phase()                  */
#define PHASE_ASFTIME_2_PHASE_INPUT_TIME_TOO_LOW           -1030
#define PHASE_ASFTIME_2_PHASE_INPUT_TIME_TOO_HIGH          -1031
#define PHASE_DB_QUERY_FAILED                              -1032
#define PHASE_NO_PHASE_RECS_FOUND                          -1033
#define PHASE_BAD_PHASE_REC_START_TIME                     -1034
#define PHASE_TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME        -1035
#define PHASE_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS        -1036
#define PHASE_TWO_PHASE_RECS_OVERLAP_IN_TIME               -1037
#define PHASE_POINTER_IS_NULL                              -1038
#define ERROR_CODE_NOT_LT_ZERO                             -1039

#define PHASE_REV_2_PHASE_INPUT_REV_TOO_LOW                -1040
#define PHASE_REV_2_PHASE_INPUT_REV_TOO_HIGH               -1041
#define PHASE_ASFTIME_NOT_WITHIN_ANY_PHASE                 -1042
#define PHASE_REV_NOT_WITHIN_ANY_PHASE                     -1043
#define ERROR_CODE_UNKNOWN                                 -1044
#define PHASE_ERROR_ET2ASF                                 -1045


/* END OF ERROR CODES    */


/* CONDITION CODES  */



/* FUNCTION PROTOTYPES   */
/* returned by asftime_2_phase():  */
#define PHASE_INPUT_TIME_BEFORE_ALL_PHASES     1
#define PHASE_INPUT_TIME_WITHIN_A_PHASE        2
#define PHASE_INPUT_TIME_BETWEEN_PHASES        3
#define PHASE_INPUT_TIME_AFTER_ALL_PHASES      4
int asftime_2_phase(
	char     	*sat,
	char   	    *asftime,             /* asf time for desired phase */
	DB_RECORD	***phase_rec_found ); /* phase record retrieved.          */

int asftime2rev(
	char     	*sat,       /* input satellite  */
	char   	    *asftime,   /* input asftime    */
	int			*rev ); 	/* output rev number for the sat, asftime   */

int check_rev_asftimes(
	char	*sat,
	int		rev,
	char	*strttime,
	char	*stoptime,
	int		*rev_number_for_strttime,       /* output rev for strttime   */
	char	*strttime_for_rev,              /* output strttime for rev   */
	char	*stoptime_for_rev  ) ;          /* output stoptime for rev   */

int check_rev_times_overlap(
	char	*sat,
	int		rev,
	char	*strttime,
	char	*stoptime,
	int		*rev_number_for_strttime,       /* output rev for strttime   */
	char	*strttime_for_rev,              /* output strttime for rev   */
	char	*stoptime_for_rev  ) ;          /* output stoptime for rev   */

#define PHASE_ASFTIME2REV_OK               1
int phase_asftime2rev(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
    char        *asftime,       /* output rev number of first rev in phase. */
    int         *rev ) ;        /* output rsp of first rev in phase.        */

int phase_rev_in_phase(
     DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
     int        rev ) ;

int phase_first_rev(
	DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
	int        *first_rev_in_phase ) ;

#define PHASE_GET_LIST_OK           1
int phase_get_list(
	char            *sat,           /* input satellite for desired recs.  */
	llist           **phase_list) ; /* output address of linked list      */

int phase_rev2asftime(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
    int         rev ,           /* input rev                                */
    char      	*asftime_start_rev,  /* output ASF time for rev start.     */
    char      	*asftime_end_rev ) ; /* output ASF time for rev end.       */

int phase_rev2et(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
    int         rev ,           /* input rev                                */
    double      *et_start_rev,  /* output ephemeris time for rev start.     */
    double      *et_end_rev ) ; /* output ephemeris time for rev end.       */

int rev2asftime(
	char        *sat,
	int         rev,                /* input rev */
	char        *asftime_start_rev, /* output ASF time for rev start.  */
	char        *asftime_end_rev ); /* output ASF time for rev end.  */


/* returned by rev_2_phase():  */
#define PHASE_INPUT_REV_AFTER_ALL_PHASES                   1
#define PHASE_INPUT_REV_BEFORE_ALL_PHASES                  2
#define PHASE_INPUT_REV_WITHIN_A_PHASE                     3
#define PHASE_INPUT_REV_BETWEEN_PHASES                     4
int rev_2_phase(
	char     	*sat,
	int   	    rev_input,            /* rev for desired phase */
	DB_RECORD	***phase_rec_found ); /* phase record retrieved.          */

#endif /* _PHASE_UTILITIES_H_ */

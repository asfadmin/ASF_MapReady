#ifndef APS_FRAMEGEN_MSG_H
#define APS_FRAMEGEN_MSG_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   aps_framegen_msg.h
Description:    condition and error messages
Creator:    unknown
Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#pragma ident   "@(#)aps_framegen_msg.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.aps_framegen_msg.h"

#define APS_FRAMEGEN_OK                                   0

/* 
-- these non-fatal error codes are >= 0 to indicate that the 
-- program will not terminate.  Also, they must be != APS_FRAMEGEN_FG_OK.
*/
#define APS_FRAMEGEN_FG_INPUT_ERROR                       1
#define APS_FRAMEGEN_FG_ERROR                             2
/* APS_FRAMEGEN_FG_OK MUST be == 0  */
#define APS_FRAMEGEN_FG_OK                                0

extern char    *aps_framegen_error_message[] ;

/* ERROR MESSAGES:  */
#define APS_FRAMEGEN_ERROR_MESSAGE( code ) \
    aps_framegen_error_message[ -(code) ]

#define APS_FRAMEGEN_ERROR_ALLOCATING_MEMORY                       -1
#define APS_FRAMEGEN_ERROR_DECODING_ACTID                          -2
#define APS_FRAMEGEN_ERROR_DECODING_DTKSTAT                        -3
#define APS_FRAMEGEN_ERROR_DECODING_SAT                            -4
#define APS_FRAMEGEN_ERROR_DECODING_SENSOR                         -5
#define APS_FRAMEGEN_ERROR_DECODING_SENSOR_MODE                    -6
#define APS_FRAMEGEN_ERROR_DECODING_STATION_ID                     -7
#define APS_FRAMEGEN_ERROR_DELETING_REC_IN_DB                      -8
#define APS_FRAMEGEN_ERROR_DTK_REC_IS_NULL                         -9
#define APS_FRAMEGEN_ERROR_INSERTING_REC_IN_DB                    -10
#define APS_FRAMEGEN_ERROR_IN_APS_SOURCE_CODE                     -11
#define APS_FRAMEGEN_ERROR_IN_DTKSTAT_VALUE                       -12
#define APS_FRAMEGEN_ERROR_IN_FRAMEGEN_CALLS_RELATION             -13
#define APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY                        -14
#define APS_FRAMEGEN_ERROR_IN_TIME_PAIRS_PTR                      -15
#define APS_FRAMEGEN_ERROR_REJ_DEL_DTK_NOT_PREVIOUSLY_REPORTED    -16
#define APS_FRAMEGEN_ERROR_SENSOR_IS_NOT_A_SAR                    -17
#define APS_FRAMEGEN_FG_FATAL                                     -18
#define APS_FRAMEGEN_FG_UNKNOWN_CODE                              -19
#define APS_FRAMEGEN_ERROR_R1_SENSOR_MODE_UNKNOWN                 -20

#endif  /* APS_FRAMEGEN_MSG_H */

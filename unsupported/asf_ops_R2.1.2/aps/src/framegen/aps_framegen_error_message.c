#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_framegen_error_message.c

Description:    contains the error messages

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)aps_framegen_error_message.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.aps_framegen_error_message.c"


/*==============================================================================
Function:       aps_framegen_error_message

Description:    decodes a negative error code into a string.  
                The macro APS_FRAMEGEN_ERROR_MESSAGE() decodes a negative 
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Wed Mar 27 08:59:01 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

char    *aps_framegen_error_message[] =
{
    "0 is not an error code.  ", 
    "APS_FRAMEGEN_ERROR_ALLOCATING_MEMORY",                    /*   -1  */
    "APS_FRAMEGEN_ERROR_DECODING_ACTID",                       /*   -2  */
    "APS_FRAMEGEN_ERROR_DECODING_DTKSTAT",                     /*   -3  */
    "APS_FRAMEGEN_ERROR_DECODING_SAT",                         /*   -4  */
    "APS_FRAMEGEN_ERROR_DECODING_SENSOR",                      /*   -5  */
    "APS_FRAMEGEN_ERROR_DECODING_SENSOR_MODE",                 /*   -6  */
    "APS_FRAMEGEN_ERROR_DECODING_STATION_ID",                  /*   -7  */
    "APS_FRAMEGEN_ERROR_DELETING_REC_IN_DB",                   /*   -8  */
    "APS_FRAMEGEN_ERROR_DTK_REC_IS_NULL",                      /*   -9  */
    "APS_FRAMEGEN_ERROR_INSERTING_REC_IN_DB",                  /*  -10  */
    "APS_FRAMEGEN_ERROR_IN_APS_SOURCE_CODE",                   /*  -11  */
    "APS_FRAMEGEN_ERROR_IN_DTKSTAT_VALUE",                     /*  -12  */
    "APS_FRAMEGEN_ERROR_IN_FRAMEGEN_CALLS_RELATION",           /*  -13  */
    "APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY",                      /*  -14  */
    "APS_FRAMEGEN_ERROR_IN_TIME_PAIRS_PTR",                    /*  -15  */
    "APS_FRAMEGEN_ERROR_REJ_DEL_DTK_NOT_PREVIOUSLY_REPORTED",  /*  -16  */
    "APS_FRAMEGEN_ERROR_SENSOR_IS_NOT_A_SAR",                  /*  -17  */
    "APS_FRAMEGEN_FG_FATAL",                                   /*  -18  */
    "APS_FRAMEGEN_FG_UNKNOWN_CODE",                            /*  -19  */
    "APS_FRAMEGEN_ERROR_R1_SENSOR_MODE_UNKNOWN",               /*  -20  */
    "-21 unknown error code.  ", 
    "-22 unknown error code.  ", 
    "-23 unknown error code.  ", 
    "-24 unknown error code.  ", 
    "-25 unknown error code.  ", 
    "-26 unknown error code.  ", 
    "-27 unknown error code.  ", 
    "-28 unknown error code.  ", 
    "-29 unknown error code.  ", 
    " END OF THE LIST!!!       "
} ;


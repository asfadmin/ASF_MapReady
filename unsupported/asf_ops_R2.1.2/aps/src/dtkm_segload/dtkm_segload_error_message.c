#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_segload_error_message.c

Description:    contains the error messages for dtkm_segload

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_segload_error_message.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/dtkm_segload/SCCS/s.dtkm_segload_error_message.c"



/*==============================================================================
Function:       dtkm_segload_error_message.c

Description:    decodes a negative error code into a string.
                The macro DTKM_SEGLOAD_ERROR_MESSAGE() decodes a negative
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Fri Dec  8 17:04:09 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

char    *dtkm_segload_error_message[] =
{   "zero is not an error code",
    "DTKM_SEGLOAD_ERROR_BAD_STOPTIME_VALUE",                          /* -1  */
    "DTKM_SEGLOAD_ERROR_BAD_STRTTIME_VALUE",                          /* -2  */
    "DTKM_SEGLOAD_ERROR_DARID_ZERO",                                  /* -3  */
    "DTKM_SEGLOAD_ERROR_DB_QUERY_FAILED",                             /* -4  */
    "DTKM_SEGLOAD_ERROR_DTK_UPDATES_LIST_NOT_EMPTY",                  /* -5  */
    "DTKM_SEGLOAD_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED",            /* -6  */
    "DTKM_SEGLOAD_ERROR_GT_1_SEG_REC_DELETED",                        /* -7  */
    "DTKM_SEGLOAD_ERROR_INPUT_DTK_LIST_NOT_EMPTY",                    /* -8  */
    "DTKM_SEGLOAD_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED",              /* -9  */
    "DTKM_SEGLOAD_ERROR_IN_CODE_dtkmseg_add_segment",                 /* -10 */
    "DTKM_SEGLOAD_ERROR_IN_CODE_dtkmseg_seg_exists",                  /* -11 */
    "DTKM_SEGLOAD_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_CNOMCOV_FOR_DTK",/* -12 */
    "DTKM_SEGLOAD_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV",              /* -13 */
    "DTKM_SEGLOAD_ERROR_NO_STATION_REC_FOR_DTK",                      /* -14 */
    "DTKM_SEGLOAD_ERROR_NULL_DTK_PROPOSAL",                           /* -15 */
    "DTKM_SEGLOAD_ERROR_NULL_PROPOSED_DTK_SEG",                       /* -16 */
    "DTKM_SEGLOAD_ERROR_NULL_RECORD",                                 /* -17 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_ACCEPTED_DTK_LIST_IS_NOT_EMPTY",       /* -18 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_ACCEPTED_DTK_LIST_NOT_INITIALIZED",    /* -19 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_CON_DTK_LIST_IS_NOT_EMPTY",            /* -20 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_CON_DTK_LIST_NOT_INITIALIZED",         /* -21 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_ERROR_DTK_LIST_IS_NOT_EMPTY",          /* -22 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_ERROR_DTK_LIST_NOT_INITIALIZED",       /* -23 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_REJECTED_DTK_LIST_IS_NOT_EMPTY",       /* -24 */
    "DTKM_SEGLOAD_ERROR_OUTPUT_REJECTED_DTK_LIST_NOT_INITIALIZED",    /* -25 */
    "DTKM_SEGLOAD_ERROR_SEGID_ZERO",                                  /* -26 */
    "DTKM_SEGLOAD_ERROR_SEG_NOT_DELETED",                             /* -27 */
    "DTKM_SEGLOAD_ERROR_SEG_NOT_INSERTED",                            /* -28 */
    "DTKM_SEGLOAD_ERROR_SEG_NOT_UPDATED",                             /* -29 */
    "DTKM_SEGLOAD_ERROR_SHOULD_BE_QUE_STATUS",                        /* -30 */
    "DTKM_SEGLOAD_ERROR_SSCANF",                                      /* -31 */
    "DTKM_SEGLOAD_ERROR_TOO_MANY_SEGS_UPDATED",                       /* -32 */
    "DTKM_SEGLOAD_ERROR_ADDING_SEGMENT",                              /* -33 */
    "DTKM_SEGLOAD_ERROR_PROCESSING_LIST",                             /* -34 */
    "DTKM_SEGLOAD_ERROR_IN_SEG_TIMES",                                /* -35 */
    "DTKM_SEGLOAD_UNKNOWN_ERROR_CODE_FROM_dtkm_in_station_mask",      /* -36 */
    "-37 unknown error code.  ",
    "-38 unknown error code.  ",
    "-39 unknown error code.  ",
    "-40 unknown error code.  ",
    " END OF THE LIST!!!  "
} ;

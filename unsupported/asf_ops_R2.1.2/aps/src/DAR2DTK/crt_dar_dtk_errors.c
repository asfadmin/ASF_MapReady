#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   crt_dar_dtk_errors.c

Description:    contains the error messages for libmu.a

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)crt_dar_dtk_errors.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/DAR2DTK/SCCS/s.crt_dar_dtk_errors.c"



/*==============================================================================
Function:       crt_dar_dtk_errors.c

Description:    decodes a negative error code into a string.  
                The macro CRT_DAR_DTK_ERROR_MESSAGE() decodes a negative 
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Thu Jun 19 16:47:05 PDT 1997

==============================================================================*/

char    *crt_dar_dtk_errors[] =
{
"zero is not an error code",
"CRT_DAR_DTK_ERROR_BAD_ENDTIME",                              /*  -1  */
"CRT_DAR_DTK_ERROR_BAD_STRTTIME",                             /*  -2  */
"CRT_DAR_DTK_ERROR_DARID_LE_ZERO",                            /*  -3  */
"CRT_DAR_DTK_ERROR_DAR_NOT_FOUND",                            /*  -4  */
"CRT_DAR_DTK_ERROR_DAR_PERMISSION_NOT_GRANTED",               /*  -5  */
"CRT_DAR_DTK_ERROR_DB_QUERY_ERROR",                           /*  -6  */
"CRT_DAR_DTK_ERROR_DTKSTAT_NOT_PLN_SUB_OR_SCH",               /*  -7  */
"CRT_DAR_DTK_ERROR_GT_ONE_DAR_FOUND",                         /*  -8  */
"CRT_DAR_DTK_ERROR_IN_PROCESSING_DTK_PROPOSAL",               /*  -9  */
"CRT_DAR_DTK_ERROR_PLANNING_PERMISSION_NOT_GRANTED",          /* -10  */
"CRT_DAR_DTK_ERROR_SAT_NOT_R1",                               /* -11  */
"CRT_DAR_DTK_ERROR_SENSOR_NOT_USABLE",                        /* -12  */
"CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_AMM_PHASE",               /* -13  */
"CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_ANY_PHASE",               /* -14  */
"CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_SAME_REV",                /* -15  */
"CRT_DAR_DTK_ERROR_TIME_DURATION_LE_ZERO",                    /* -16  */
"CRT_DAR_DTK_ERROR_TIME_DURATION_TOO_LONG_FOR_AMM",           /* -17  */
"CRT_DAR_DTK_ERROR_GETTING_DEFAULT_VALUES",                   /* -18  */
"CRT_DAR_DTK_ERROR_FROM_DTKM_CHECK_VALUES",                   /* -19  */
"CRT_DAR_DTK_ERROR_MAX_MINUTES_LE_ZERO",                      /* -20  */
"CRT_DAR_DTK_ERROR_PLANNER_QUICKLOOK_NOT_Y_OR_N",             /* -21  */
"CRT_DAR_DTK_ERROR_MASKINOUT_FLAG_NOT_Y_OR_N",                /* -22  */
"CRT_DAR_DTK_ERROR_DETERMINING_MASK",                         /* -23  */
"CRT_DAR_DTK_ERROR_DETERMINING_DELTA_TIME",                   /* -24  */
"-25 unknown error code.  ",
"-26 unknown error code.  ",
"-27 unknown error code.  ",
"-28 unknown error code.  ",
"-29 unknown error code.  ",
"-30 unknown error code.  ",
" END OF THE LIST!!!       "
} ;

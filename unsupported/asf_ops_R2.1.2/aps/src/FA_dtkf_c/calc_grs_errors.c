#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   calc_grs_errors.c

Description:    contains the error messages for libmu.a

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident	"@(#)calc_grs_errors.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_c/SCCS/s.calc_grs_errors.c"


/*==============================================================================
Function:       calc_grs_errors.c

Description:    decodes a negative error code into a string.  
                The macro LOAD_DAR_ERROR_MESSAGE() decodes a negative 
                return code into one of these strings.  

Creator:        Lawrence Stevens

Creation Date:  Thu Jun 19 16:47:05 PDT 1997

==============================================================================*/

char    *calc_grs_errors[] =
{
"zero is not an error code",
"LOAD_DAR_ERROR_BAD_ENDTIME",                              /*  -1  */
"LOAD_DAR_ERROR_BAD_STRTTIME",                             /*  -2  */
"LOAD_DAR_ERROR_DARID_LE_ZERO",                            /*  -3  */
"LOAD_DAR_ERROR_DAR_NOT_FOUND",                            /*  -4  */
"LOAD_DAR_ERROR_DAR_PERMISSION_NOT_GRANTED",               /*  -5  */
"LOAD_DAR_ERROR_DB_QUERY_ERROR",                           /*  -6  */
"LOAD_DAR_ERROR_DTKSTAT_NOT_PLN_SUB_OR_SCH",               /*  -7  */
"LOAD_DAR_ERROR_GT_ONE_DAR_FOUND",                         /*  -8  */
"LOAD_DAR_ERROR_PLANNING_PERMISSION_NOT_GRANTED",          /*  -9  */
"LOAD_DAR_ERROR_SAT_NOT_J1",                               /* -10  */
"LOAD_DAR_ERROR_SENSOR_NOT_SAR",                           /* -11  */
"LOAD_DAR_ERROR_BAD_SHAPE",                                /* -12  */
"LOAD_STATION_ERROR_DB_QUERY_ERROR",                       /* -13  */
"LOAD_STATION_ERROR_STATION_NOT_FOUND",                    /* -14  */
"LOAD_STATION_ERROR_GT_ONE_STATION_FOUND",                 /* -15  */
"STATION_MASK_FLAG_CIRCLE_IN_MASK",                        /* -16  */
"STATION_MASK_FLAG_QUAD_IN_MASK",                          /* -17  */
"STATION_MASK_FLAG_MASK_IN_QUAD_DAR",                      /* -18  */
"REQQ_PHASE_ERROR_PHASES_DO_NOT_MATCH",                    /* -19  */
"CALC_GRS_ERROR_NO_GRS_COORDS",                            /* -20  */
"STATION_MASK_FLAG_QUAD_OVERLAPS_MASK",                    /* -21  */
"STATION_MASK_FLAG_CIRCLE_OVERLAPS_MASK",                  /* -22  */
"LOAD_DAR_ERROR_CHECK_LOG_FILE",                           /* -23  */
"-24 unknown error code.  ",
"-25 unknown error code.  ",
"-26 unknown error code.  ",
"-27 unknown error code.  ",
" END OF THE LIST!!!       "
} ;


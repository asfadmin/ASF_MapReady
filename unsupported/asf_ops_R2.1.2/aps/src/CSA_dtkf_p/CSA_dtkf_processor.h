#ifndef CSA_DTKF_PROCESSOR_H 
#define CSA_DTKF_PROCESSOR_H 

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   fa_csaf_processor.h
Description:header for fa_csaf_processor routines.
Creator:    Miguel Siu
Notes:      
==============================================================================*/
#pragma ident   "@(#)CSA_dtkf_processor.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/CSA_dtkf_p/SCCS/s.CSA_dtkf_processor.h"

#include "fa_defs.h"    /* for FA typedefs and basic definitions */

#define FA_CSAF_ERROR_MESSAGE( code ) \
    fa_csaf_error_message[ -(code) ]

#define FA_CSAF_UNABLE_TO_PARSE_FILE              -1
#define FA_CSAF_HEADER_KEYWORD_NOT_OK             -2
#define FA_CSAF_SEGMENT_COUNT_MISSING             -3
#define FA_CSAF_SEGMENT_MISSING                   -4
#define FA_CSAF_ACTIVITY_MISSING                  -5
#define FA_CSAF_DBRECORD_COPY_FAILED              -6
#define FA_CSAF_AGGREGATE_KEYWORD_NOT_OK          -7
#define FA_CSAF_DTK_DEFAULTS_NOT_FOUND            -8
#define FA_CSAF_DEFAULT_NOT_FOUND                 -9
#define FA_CSAF_PERMISSIONS_DENIED                -10
#define FA_CSAF_PERMISSIONS_FAILED                -11
#define FA_CSAF_INDETERMINATE_FILE_TIMES          -12
#define FA_CSAF_MORE_THAN_ONE_REALTIME_IN_FILE    -13
#define FA_CSAF_MORE_THAN_ONE_TAPEDUMP_IN_FILE    -14
#define FA_CSAF_COULD_NOT_PARSE_RECORDING         -15

static char *fa_csaf_error_message[] = 
{
    "zero is not a valid error code",             /* 0*/
    "FA_CSAF_UNABLE_TO_PARSE_FILE",               /* 1*/
    "FA_CSAF_HEADER_KEYWORD_NOT_OK",              /* 2*/
    "FA_CSAF_SEGMENT_COUNT_MISSING",              /* 3*/
    "FA_CSAF_SEGMENT_MISSING",                    /* 4*/
    "FA_CSAF_ACTIVITY_MISSING",                   /* 5*/
    "FA_CSAF_DBRECORD_COPY_FAILED",               /* 6*/
    "FA_CSAF_AGGREGATE_KEYWORD_NOT_OK",           /* 7*/
    "FA_CSAF_DTK_DEFAULTS_NOT_FOUND",             /* 8*/
    "FA_CSAF_DEFAULT_NOT_FOUND",                  /* 9*/
    "FA_CSAF_PERMISSIONS_DENIED",                 /*10*/
    "FA_CSAF_PERMISSIONS_FAILED",                 /*11*/
    "FA_CSAF_INDETERMINATE_FILE_TIMES",           /*12*/
    "FA_CSAF_MORE_THAN_ONE_REALTIME_IN_FILE",     /*13*/
    "FA_CSAF_MORE_THAN_ONE_TAPEDUMP_IN_FILE",     /*14*/
    "FA_CSAF_COULD_NOT_PARSE_RECORDING",          /*15*/
    "-16 unknown error code.",                    /*16*/
    "-17 unknown error code.",                    /*17*/
    "-18 unknown error code.",                    /*18*/
    "-19 unknown error code.",                    /*19*/
    "-20 unknown error code.",                    /*20*/
    "-21 unknown error code.",                    /*21*/
    "END OF fa_csaf_error_message[] LIST"
};

#endif /* CSA_DTKF_PROCESSOR_H */

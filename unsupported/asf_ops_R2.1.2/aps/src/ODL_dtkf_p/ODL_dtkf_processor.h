#ifndef ODL_DTKF_PROCESSOR_H 
#define ODL_DTKF_PROCESSOR_H 

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	ODL_dtkf_processor.h
Description:header for ODL_dtkf_processor routines.
Creator:	Miguel Siu
Notes:		
==============================================================================*/
#pragma ident	"@(#)ODL_dtkf_processor.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/ODL_dtkf_p/SCCS/s.ODL_dtkf_processor.h"

#include "fa_defs.h"	/* for FA typedefs and basic definitions */

#define FA_ODL_ERROR_MESSAGE( code ) \
	fa_odl_error_message[ -(code) ]

#define FA_ODL_UNABLE_TO_PARSE_FILE		-1
#define FA_ODL_HEADER_KEYWORD_NOT_OK 		-2
#define FA_ODL_SEGMENT_COUNT_MISSING		-3
#define FA_ODL_SEGMENT_MISSING				-4
#define FA_ODL_ACTIVITY_MISSING			-5
#define FA_ODL_DBRECORD_COPY_FAILED		-6
#define FA_ODL_AGGREGATE_KEYWORD_NOT_OK    -7
#define FA_ODL_DTK_DEFAULTS_NOT_FOUND  	-8
#define FA_ODL_DEFAULT_NOT_FOUND			-9

static char *fa_odl_error_message[] = 
{
	"zero is not a valid error code",	/* 0*/
	"FA_ODL_UNABLE_TO_PARSE_FILE",		/* 1*/
	"FA_ODL_HEADER_KEYWORD_NOT_OK",	/* 2*/
	"FA_ODL_SEGMENT_COUNT_MISSING",	/* 3*/
	"FA_ODL_SEGMENT_MISSING",			/* 4*/
	"FA_ODL_ACTIVITY_MISSING",			/* 5*/
	"FA_ODL_DBRECORD_COPY_FAILED",		/* 6*/
	"FA_ODL_AGGREGATE_KEYWORD_NOT_OK", /* 7*/
	"FA_ODL_DTK_DEFAULTS_NOT_FOUND",	/* 8*/
	"FA_ODL_DEFAULT_NOT_FOUND",		/* 9*/
	"-10 unknown error code.",	        /*10*/
	"-11 unknown error code.",	        /*11*/
	"-12 unknown error code.",	        /*12*/
	"END OF fa_odl_error_message[] LIST"
};

#ifdef MIGUEL_COMMENT_OUT
extern EQUIV_TABLE CSA_write_recording_flags[];
extern EQUIV_TABLE CSA_dtk_status[];
extern EQUIV_TABLE CSA_sat_id[];
extern EQUIV_TABLE CSA_facility_id[];
extern EQUIV_TABLE CSA_transmission_id[];
extern EQUIV_TABLE CSA_activity_id[];
extern EQUIV_TABLE CSA_sensor[];
#endif

#endif /* ODL_DTKF_PROCESSOR_H */

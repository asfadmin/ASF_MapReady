/*==============================================================================
Filename:	ESAparse.h
Description:    header for ESAparse routines.
Creator:	Miguel Siu

Notes:		
SCCS Info:

==============================================================================*/

#ifndef _FA_ASCII_REC_PROCESSOR_
#define _FA_ASCII_REC_PROCESSOR_

#include "fa_defs.h"

#define FA_ASCII_REC_ERROR_MESSAGE( code ) \
	fa_ascii_rec_error_message[ -(code) ]

#define FA_ASCII_FILE_UNABLE_TO_OPEN		-1
#define FA_ASCII_REC_HEADER_NOT_OK 		-2
#define FA_ASCII_REC_DBRECORD_COPY_FAILED	-3
#define FA_ASCII_REC_AGGREGATE_NOT_OK    	-4
#define FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND     -5
#define FA_ESAF_DEFAULT_NOT_FOUND		-6 


char *fa_ascii_rec_error_message[] = 
{
	"zero is not a valid error code",	 /* 0*/
	"FA_ASCII_FILE_UNABLE_TO_OPEN",		 /* 1*/
	"FA_ASCII_REC_HEADER_NOT_OK", 		 /* 2*/
	"FA_ASCII_REC_DBRECORD_COPY_FAILED",     /* 3*/
	"FA_ASCII_REC_AGGREGATE_NOT_OK",  	 /* 4*/
	"FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND",   /* 5*/
	"FA_ESAF_DEFAULT_NOT_FOUND"		 /* 6*/
};

extern EQUIV_TABLE ESA_dtk_status[];
extern EQUIV_TABLE ESA_facility_id[];
 
#endif /* _FA_ASCII_REC_PROCESSOR_ */

/* End of File */

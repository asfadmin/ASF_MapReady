#ifndef FA_DTKF_PROCESSOR_H
#define FA_DTKF_PROCESSOR_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	FA_dtkf_processor.h
Description:	header for fa_ascii_rec_processor routines.
Creator:	Miguel Siu
Notes:		
==============================================================================*/
#pragma ident	"@(#)FA_dtkf_processor.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/FA_dtkf_p/SCCS/s.FA_dtkf_processor.h"

#include "fa_defs.h"

#define FA_ASCII_REC_ERROR_MESSAGE( code ) \
	fa_ascii_rec_error_message[ -(code) ]

#define FA_ASCII_FILE_UNABLE_TO_OPEN		-1
#define FA_ASCII_REC_HEADER_NOT_OK 			-2
#define FA_ASCII_REC_DBRECORD_COPY_FAILED	-3
#define FA_ASCII_REC_AGGREGATE_NOT_OK    	-4
#define FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND -5
#define FA_DEFAULT_NOT_FOUND				-6 


char *fa_ascii_rec_error_message[] = 
{
	"zero is not a valid error code",	 /* 0*/
	"FA_ASCII_FILE_UNABLE_TO_OPEN",		 /* 1*/
	"FA_ASCII_REC_HEADER_NOT_OK", 		 /* 2*/
	"FA_ASCII_REC_DBRECORD_COPY_FAILED", /* 3*/
	"FA_ASCII_REC_AGGREGATE_NOT_OK",  	 /* 4*/
	"FA_ASCII_REC_DTK_DEFAULTS_NOT_FOUND",/* 5*/
	"FA_ESAF_DEFAULT_NOT_FOUND"			 /* 6*/
};

#endif /* FA_DTKF_PROCESSOR_H */

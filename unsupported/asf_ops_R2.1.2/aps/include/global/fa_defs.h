#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	fa_defs.h
Description:	
	Definitions for flight agency files and their handling.

Creator:	Miguel Siu

Notes:		
==============================================================================*/
#pragma ident   "@(#)fa_defs.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/global/SCCS/s.fa_defs.h"

#ifndef _fa_def_
#define _fa_def_

#include "dapps_list.h"     /* for llist           */

/*
-- PLACE DEFINITIONS.
-- When used to describe a place, (used as a SOURCE)
-- the following definitions allow the construct
--						while (record.source)
*/
#define FILE_HEADER		1
#define FILE_RECORD		2
#define FA_DEFAULT		3
#define FILE_SUBRECORD	4
#define FILE_DESCRIPTOR	5
#define FILE_TRAILER	6

#define REPORT_RECORD   7 
#define REPORT_HEADER   8
#define REPORT_CONTROL  9 
#define GLOBAL_VARIABLE 10

#define DTK_DEFAULT 101
#define DTK_RECORD  102
#define NON_DTKATTR 103
#define DTK_NEWLINE 104
#define DTK_DEFAULT_STRING   105
#define DTK_FUNCTION         106

/*
-- PROCESSING DEFINITIONS.
*/
#define READ_TO_EOF		-1



/* ========================================================================== */

typedef
	union _DEST_TYPE
	{
		int		index ;
		void	*pointer ;

	} DEST_TYPE;


typedef 
	struct 
	{
	    char	*fa_string;
		char	*aps_string;

    } EQUIV_TABLE;

/* ========================================================================== */

typedef
	struct _OFFSET_VALUE_DEFS
	{
		int				source_code ; /* [	FILE_HEADER  | 
											FILE_RECORD  | 
											FA_DEFAULT   ] */

		int				destination_code; /* [  REPORT_HEADER  | 
												REPORT_CONTROL |
												REPORT_RECORD  ] */
		int				offset ;
		int				length ;
		int				(*conversion)();
		void			*equiv_table ;	
	 	DEST_TYPE		destination;

	} VALUE_DEFS ;



typedef
	struct _FA_FILEDEF
	{
		int 		header_length ;
		int 		record_length ;
		VALUE_DEFS	*field_def ;

	} FA_FILEDEF ;



typedef
	struct _FA_NAMES
	{
		char		*flight_agency ;
		char		*file_type ;
		FA_FILEDEF	*file_definition ;
		FA_FILEDEF	*response_file_definition ;
		char		*response_extension;

	} FA_FILENAME ;

/* ========================================================================== */

typedef
	struct _ODL_VALUE_DEFS
	{
    	int  				source_code;
		int					destination_code;
		char 				*keyword;
		int 				(*conversion)();
		EQUIV_TABLE			*conversion_table ;
    	DEST_TYPE 			destination;
		
	} ODL_VALUEDEFS ;



typedef
	struct _ODL_FILEDEF
	{
		char			*file_object ;
		char			*header_object ;
		char			*data_record_object ;
		ODL_VALUEDEFS	*field_def ;

	} ODL_FILEDEF ;



typedef
	struct _ODL_FILENAMES
	{
		char		*flight_agency ;
		char		*file_type ;
		ODL_FILEDEF	*file_definition ;

	} ODL_FILENAME ;

/* ========================================================================== */

typedef 
	struct _KEYWORD_VALUE_DEFS 
	{
    	int  				source_code;
		int					destination_code;
		char 				*keyword;
		int 				(*conversion)();
		EQUIV_TABLE			*conversion_table ;
    	DEST_TYPE 			destination;
		
	} CSA_VALUEDEF ;

typedef
	struct _CSA_NAMES
	{
		char		*flight_agency ;
		char		*file_type ;
		CSA_VALUEDEF *field_def ;

	} CSA_FILENAME ;

/* ========================================================================== */

typedef struct 
    {
        char sensor_segment[9];
        char planning_ID[6];
 
        /* "SSSNNNNNNXXXXXX"  SSS=sensor  NNNNNN=segment  XXXXXX=planning_ID */
    }   FA_KEY_STRUCT;

#endif /*_fa_def_*/

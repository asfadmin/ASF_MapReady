#ifndef CREATE_SV_FILE_H
#define CREATE_SV_FILE_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	create_sv_file.h
Description:	This contains the data structure definition for state vector
			common header and data record 
Creator:	Tieh Ku
Notes:		
==============================================================================*/
#pragma ident	"@(#)create_sv_file.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/create_sv_files/SCCS/s.create_sv_file.h"
 
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>
#include <unistd.h>
 
 
#include <ims_dbms.h>
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>
 
#include "aps_defs.h"

#include <odldef.h>
#include <odlinter.h>
 
  
#define SV_MAXLENGTH 	30 
#define SV_TIME_LENGTH 	21 

typedef struct command_line
{
    char username[SV_MAXLENGTH];
    char password[SV_MAXLENGTH];
    char progname[SV_MAXLENGTH];
    char platform[SV_MAXLENGTH];
    char precision[SV_MAXLENGTH];
    char startTime[SV_MAXLENGTH];
    char endTime[SV_MAXLENGTH];
    char dest[SV_MAXLENGTH];
    char server[SV_MAXLENGTH];
    char database[SV_MAXLENGTH];
    char odl_creation_time[SV_TIME_LENGTH + 1];
	FILE *SV_file_ptr;
} COMMANDS;
 
/* SV common header structure */

typedef struct ODL_common_header
{
   char file_creation_time[SV_TIME_LENGTH+1] ;   /* File Creation Time */
   /*struct ODLDate date_time ;*/                    /* ODL structure used to
                                                 ** store creation time
                                                 */
   char file_type[SV_MAXLENGTH] ;                     /* File Type */
   char file_dest[SV_MAXLENGTH] ;                     /* File Type */
   char file_source[SV_MAXLENGTH] ;                   /* File Source */
   char number_of_records[SV_MAXLENGTH] ;
 
} ODL_COMMON_HEADER ;


/* SV metadata structure */

typedef struct ODL_SV_metadata
{
   char platform[SV_MAXLENGTH] ;                     /* platform */
   char metadata_type[SV_MAXLENGTH] ;                /* metadata type*/
   char coordinate[SV_MAXLENGTH] ;                   /* coordinate */

} ODL_SV_METADATA ;


/* SV Keywords */

#define SV_BUF_SIZE	     128 
#define SV_OBJECT_NAME       "STATE_VECTOR"
#define SV_RECORD_NAME       "STATE_VECTOR_RECORD"
#define SV_METADATA_NAME     "STATE_VECTOR_METADATA"
#define SV_DATA_NAME         "STATE_VECTOR_DATA"

#define SV_COMMON_HDR        "COMMON_HEADER"
#define SV_TIME              "TIME"
#define SV_MSG_TYPE    	     "MSG_TYPE"
#define SV_DEST        	     "DESTINATION"
#define SV_SRC               "SOURCE"
#define SV_NUMRECS           "NUMBER_OF_RECORDS"
#define SV_PLATFORM          "PLATFORM"
#define SV_METADATA_TYPE     "STATE_VECTOR_PRECISION"
#define SV_COORDINATE        "COORDINATE_SYSTEM"
#define SV_MSG_TYPE_NAME     "EPHEMERIS"
#define SV_SRC_NAME          "APS"
 
 
#define SV_REVOLUTION        "REVOLUTION"
#define SV_X_POS             "X_POSITION"
#define SV_Y_POS             "Y_POSITION"
#define SV_Z_POS             "Z_POSITION"
#define SV_X_VEL             "X_VELOCITY"
#define SV_Y_VEL             "Y_VELOCITY"
#define SV_Z_VEL             "Z_VELOCITY"

#endif	/* CREATE_SV_FILE_H */

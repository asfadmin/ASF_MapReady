#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)stoicfile_metadata.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/stoic/SCCS/s.stoicfile_metadata.c"

/*
** Purpose: 
**  <filename>.M   metadata file
**  <filename>.D   data file
** The base <filename> would be the same for both files.  An example of
** the metadata file for the GHA data file, based on my last email message
** would look like the following
** Example
** OBJECT = DAPPS_FILE_METADATA
** 
**     OBJECT = COMMON_HEADER
**         TIME = 1996-001T12:30:45.000
**         SOURCE = "FAIF"
**         DESTINATION = "IMS"
**         MSG_TYPE = "DAPPS_FILE_METADATA"
**         NUMBER_OF_RECORDS = 1
**     END_OBJECT = COMMON_HEADER
** 
**     OBJECT = CATALOG_METADATA
**         FILE_NAME = "STOIC_FILE.D"
**         FILE_FORMAT = "ORIGINAL"
**         SOURCE = "JPL"
**         DESTINATION = "ASF"
**         FA_FILE_TYPE = "JPL_STOIC"
**         GEN_FILE_TYPE = "STOIC"
**         VALID_START_TIME = 1996-001T12:00:00.000
**         VALID_END_TIME = 1996-005T12:00:00.000
**     END_OBJECT = CATALOG_METADATA
** 
** 
** Author:  Lisa Nguyen (section 334)
** Date:    Jan, 1996
** Modify:
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
 
int wrt_stoic_metadata_(char *infile);

void main()
{
     int  func_DAPPS_FILE_MEATADATA;
     char infile[256];

     sprintf(infile,"%s/stoicfile",getenv("latest_stoicfile"));
     if(!wrt_stoic_metadata_(infile))
          printf("STOIC metadata can not create\n");
}

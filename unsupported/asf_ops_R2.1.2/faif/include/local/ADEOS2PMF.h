/*==============================================================================
Filename:	ADEOSparse.h
Description:	Definitions used in ADEOSparse library	
Creator:	Phil Yurchuk 
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ADEOSPARSE_
#define _ADEOSPARSE_

typedef struct {
	  char    mintime[22];
	  char    maxtime[22];
          char    minrev[6];
	  char    maxrev[6];
	} timespan;

typedef struct {
          char    pstart[22];
          int     iprevs;
          int     iplastrev;
          int     icdays;
          int     icrevs;
	} pdata;

#define HEADER_RECORD_SIZE		128	
#define DATA_RECORD_SIZE		249	

#define HDRRECLEN 128
#define SVRECLEN 90
#define MAXRECLEN 500

#endif

/* End of File */

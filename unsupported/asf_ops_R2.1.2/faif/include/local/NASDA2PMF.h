/*==============================================================================
Filename:	NASDA2PMF.h
Description:	Definitions used in NASDAparse library	
Creator:	Cameron Cooper, Phil Yurchuk
Notes:		
1.  Jan. '97 - R. Hoffman
    Added jers_data_rec and re-named timespan_rec for PR 2351 changes.

SCCS Info:
   @(#)NASDA2PMF.h	1.1
==============================================================================*/

#ifndef _NASDAPARSE_
#define _NASDAPARSE_

#define HEADER_RECORD_SIZE	46	
#define DATA_RECORD_SIZE	249	

typedef struct {	/* Greenwich Mean Time (GMT) record */
	int yr;		/* GMT year */
	int day;	/* GMT day */
	int hr;		/* GMT hour */
	int min;	/* GMT minute */
	float second;	/* GMT second and millisecond */
} GMT, *GMT_PTR;

typedef struct {
	  char    mintime[22];
	  char    maxtime[22];
          char    minrev[6];
	  char    maxrev[6];
	} timespan_rec;
 
typedef struct {
          char    pstart[22];          /* phase data */
          int     iprevs;              /* phase data */
          int     iplastrev;           /* phase data */
          int     icdays;              /* phase data */
          int     icrevs;              /* phase data */
          int     tc_count;            /* Time Correction data count in ELMF */
          int     d0;                  /* SV count in ELMF */
          int     d1;                  /* SV count in ELMF */
          int     d2;                  /* SV count in ELMF */
          int     p1;                  /* SV count in ELMF */
          int     p2;                  /* SV count in ELMF */
          int     p3;                  /* SV count in ELMF */
          timespan_rec      timespan;  /* for an ELMF file */
        } jers_data_rec;

#define HDRRECLEN 128
#define FDRRECLEN 128
#define TCRECLEN 85
#define SVRECLEN 85
#define MAXRECLEN 500

#endif

/* End of File */

/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	CSA2PMF.c

Description:    Contains the funtions necessary for creating the PMF for CSA
                RADARSAT files.

External Functions:
	CSA_ASF_timecmp
	rsh2PMF
	rrq2PMF
	csh2PMF
	pap2PMF
	orb2PMF
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  Feb. '96 - R. Hoffman
    (a) Use CSA_NUM_VECTORS to locate last state vector when determining
        valid_end_time
    (b) Deleted extra carriage returns in "Translating ... to ..." log msg.
2.  March '96 - R. Hoffman
    Changed dataset name of translated state vector file to CSA_SV_HSTR
3.  May '96 - R. Hoffman
    Added calls to delete_tree() throughout (related to P.R. 859)
4.  Dec. '96 - R. Hoffman
    Corrected one syslog message.  Changed WARNING to ERROR throughout.
5.  June '97 - R. Hoffman
    Reverse linked list output; send *.orb to IMS last, after translated files.
==============================================================================*/

static char SccsFile[] = "CSA2PMF.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 May 1996";
static char SccsLastChanger[] = "@(#)CSA2PMF.c	1.6";
static char SccsState[] = "1.6";

#include <stdio.h>
#include <stdlib.h>
#include "PMF.h"
#include "CSAparse.h"
#include "CSA.h"
#include "CSArsh.h"
#include "CSAcsh.h"
#include "CSApap.h"
#include "CSAorb.h"
#include "CSAfiletab.h"
#include "llist_err.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include <time.h>
 
#define ASF_SV_FNAME_LEN       18
#define ASF_SV_FNAME_PREFIX    "RADARSAT_"
#define ASF_PRDSV_FNAME_EXT    "PSV"
#define ASF_RESSV_FNAME_EXT    "RSV"
#define ASF_ORBITNUM_STR_SIZE  5
#define CSA_ORBITNUM_STR_SIZE  7

int CSA_ASF_timecmp(char *, char *) ;
AGGREGATE rsh2PMF(char *) ;
AGGREGATE rrq2PMF(char *) ;
AGGREGATE csh2PMF(char *) ;
AGGREGATE pap2PMF(char *) ;
AGGREGATE orb2PMF(char *, int, ODL_Catalog_Metadata *) ;
int check_CSA_translation(char *, char *, char *, char **, int) ;


/*==============================================================================
Function:       CSA_ASF_timecmp
Description:    Compares two time strings in CSA or ASF format, and returns 
                whether the first is greater than, less than, or equal to the
		second, in the same way strcmp does.
Parameters:     char *time1 - the first time string
                char *time2 - the second time string
Returns:        -1 : time1 is less than time2
                 1 : time1 is greater than time2
		 0 : time1 is equal to time2
Creator:        Philip Yurchuk (phil@ditto.jpl.nasa.gov)
Creation Date:  7/5/95
Notes:
==============================================================================*/
int CSA_ASF_timecmp(char *time1, char *time2)
{
  int year1, year2 ;
  int doy1, doy2 ;
  int hour1, hour2 ;
  int minute1, minute2 ;
  int second1, second2 ;
  int nanosecond1, nanosecond2 ;
  char *t1copy, *t2copy ;
  char *tokens = "-:." ;  /* tokens used by strtok to break up the string */

  /* Work on copies of time1 and time2
  */
  t1copy = (char *)util_do_malloc(sizeof(char)*(strlen(time1)+1)) ;
  strcpy(t1copy, time1) ;

  t2copy = (char *)util_do_malloc(sizeof(char)*(strlen(time2)+1)) ;
  strcpy(t2copy, time2) ;

  /* break up the time strings into their components, using strtok and atoi 
  */
  year1 = (int)atoi((char *)strtok(t1copy, tokens)) ;
  doy1 = (int)atoi((char *)strtok(NULL, tokens)) ;
  hour1 = (int)atoi((char *)strtok(NULL, tokens)) ;
  minute1 = (int)atoi((char *)strtok(NULL, tokens)) ;
  second1 = (int)atoi((char *)strtok(NULL, tokens)) ;
  nanosecond1 = (int)atoi((char *)strtok(NULL, tokens)) ;
  
  year2 = (int)atoi((char *)strtok(t2copy, tokens)) ;
  doy2 = (int)atoi((char *)strtok(NULL, tokens)) ;
  hour2 = (int)atoi((char *)strtok(NULL, tokens)) ;
  minute2 = (int)atoi((char *)strtok(NULL, tokens)) ;
  second2 = (int)atoi((char *)strtok(NULL, tokens)) ;
  nanosecond2 = (int)atoi((char *)strtok(NULL, tokens)) ;

  /* compare each component in order to determine if time1 is less than, 
  -- greater than, or equal to time2
  */
  if (year1 < year2)
     return -1 ;

  if (year1 > year2)
     return 1 ;

  if (doy1 < doy2)
     return -1 ;

  if (doy1 > doy2)
     return  1 ;

  if (hour1 < hour2)
     return -1 ;

  if (hour1 > hour2)
     return  1 ;

  if (minute1 < minute2)
     return -1 ;

  if (minute1 > minute2)
     return  1 ;

  if (second1 < second2)
     return -1 ;

  if (second1 > second2)
     return  1 ;

  if (nanosecond1 < nanosecond2)
     return -1 ;

  if (nanosecond1 > nanosecond2)
     return  1 ;

  /* time1 is equal to time2 
  */
  return 0 ; 

} /* CSA_ASF_timecmp */




/*==============================================================================
Function:       rsh2PMF
Description:    Creates the PMF for a Reception Schedule
Parameters:     char *filename - the name of the Reception Schedule file to be
                                 processed
Returns:        AGGREGATE root - the root of the PMF data structure if 
                                 successful
		NULL - PMF cannot be created
Creator:        Philip Yurchuk (phil@ditto.jpl.nasa.gov)
Creation Date:  7/5/95
Notes:
==============================================================================*/
AGGREGATE rsh2PMF(char *filename)
{
  NODEPTR top ;                     /* root of the CSA parse tree     */
  NODEPTR curr ;                    /* current CSA statement          */
  AGGREGATE root;                   /* root of the PMF data structure */
  ODL_Catalog_Metadata *catrecord ; /* catalog metadata structure     */
  
  char time[TIME_STRING_LEN+1] ;
  char *temp ;
  char *lastslash ;
  int i = 0;
  
  catrecord = 
     (ODL_Catalog_Metadata *)util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; 

  /* parse CSA file and put the root in top 
  */ 
  top = (NODEPTR) create_tree(filename) ;

  /* stuff the static fields of the catalog metadata structure 
  */
  lastslash = (char *)strrchr(filename, '/') ;
  strcpy(catrecord->file_name, lastslash+1) ;
  strcpy(catrecord->file_source, CSA_STR) ;
  strcpy(catrecord->file_dest, ASF_STR) ;
  strcpy(catrecord->satellite, R_SAT) ;
  strcpy(catrecord->number_of_records, NUMRECS_IN_PMF) ;
  
  /* get the date/time stamp off the file 
  */
  strcpy(catrecord->file_arrival_time, PMFfiletime(filename)) ;
  
  /* search through the CSA_Filetype_Table using CSAfiletype as an index, to
  -- determine the FA_file_type and gen_file_type fields
  */
  while(CSA_Filetype_Table[i].CSAfiletype[0])
    {
      if (strcmp(CSA_Filetype_Table[i].CSAfiletype, CSA_FTYPE_RECSCHED) == 0)
	{
	  strcpy(catrecord->FA_file_type, CSA_Filetype_Table[i].FAfiletype) ;
	  strcpy(catrecord->gen_file_type, CSA_Filetype_Table[i].Genfiletype) ;
          strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
	  if ((strcmp(catrecord->gen_file_type, GFTYPE_SVTIMCOR) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_PSV) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_RSV) == 0))
	    strcpy(catrecord->format, ASF_STR) ;
	  else
	    strcpy(catrecord->format, ORIG_STR) ;
	}
      i++ ;
    }

  /* get the earliest IMG_DATA_DL_START from the RADARSAT file - this is the
  -- valid_start_time field of the ODL_catalog_metadata structure
  */
  curr = (NODEPTR) find_ag(CSARSH_RECEPT_ACT_SPECS, top) ;
  if (temp = (char *) find_keyword_value(CSARSH_START_PLAYBACK, curr))
    strcpy(time, temp) ;
  
  while (curr = (NODEPTR) next_ag(curr, top))
    {
      if (temp = (char *) find_keyword_value(CSARSH_START_PLAYBACK, curr))
	{
	  if (time)
	    if(CSA_ASF_timecmp(time, temp) < 0)
	      strcpy(time, temp) ;
	  else
	    strcpy(time, temp) ;
	}
    }
  
  /* place the earliest DL_START time in the C structure and convert it 
  -- to ODL format
  */

  strcpy(catrecord->valid_start_time, time) ;
  catrecord->valid_start_time[8] = 'T' ;
  
  /* get the latest IMG_DATA_DL_STOP from the RADARSAT file - this is the
  -- valid_end_time field of the ODL_catalog_metadata structure
  */
  
  curr = (NODEPTR) find_ag(CSARSH_RECEPT_ACT_SPECS, top) ;
  if (temp = (char *) find_keyword_value(CSARSH_STOP_PLAYBACK, curr))
    strcpy(time, temp) ;
  
  while (curr = (NODEPTR) next_ag(curr, top))
    {
      if (temp = (char *) find_keyword_value(CSARSH_STOP_PLAYBACK, curr))
	{
	  if (time)
	    if(CSA_ASF_timecmp(time, temp) > 0)
	      strcpy(time, temp) ;
	  else
	    strcpy(time, temp) ;
	}
    }
  
  /* place the earliest DL_STOP time in the C structure and convert it 
  -- to ODL format
  */

  strcpy(catrecord->valid_end_time, time) ;
  catrecord->valid_end_time[8] = 'T' ;
  
  /* the start and end revs are the orbit number 
  */
  strcpy(catrecord->start_rev, find_keyword_value(CSARSH_ORBIT_NUM, top)) ;
  strcpy(catrecord->end_rev, find_keyword_value(CSARSH_ORBIT_NUM, top));

  /* File creation time is in CSA file header
  */
  strcpy(catrecord->file_creation_time, 
	 (char *)find_keyword_value(FILE_CREAT_TIME_STR, top)) ;
  catrecord->file_creation_time[8] = 'T' ;
  delete_tree(top);

  root = (AGGREGATE)create_PMF(catrecord);
  return (root);

} /* rsh2PMF */ 
 



/*==============================================================================
Function:       rrq2PMF
Description:    Creates the PMF for a Reception Request
Parameters:     char *filename - the name of the Reception Request file to be
                                 processed
Returns:        AGGREGATE root - the root of the PMF data structure if 
                                 successful
		NULL - PMF cannot be created
Creator:        Philip Yurchuk (phil@ditto.jpl.nasa.gov)
Creation Date:  7/10/95
Notes:
==============================================================================*/
AGGREGATE rrq2PMF(char *filename)
{
  NODEPTR top ;                     /* root of the CSA parse tree     */
  NODEPTR curr ;                    /* current CSA statement          */
  AGGREGATE root;                   /* root of the PMF data structure */
  ODL_Catalog_Metadata *catrecord ; /* catalog metadata structure     */
  
  char time[TIME_STRING_LEN+1] ;
  char *temp ;
  char *lastslash ;
  int i = 0;

  catrecord = 
     (ODL_Catalog_Metadata *) util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; 

  /* parse CSA file and put the root in top 
  */ 
  top = (NODEPTR) create_tree(filename) ;

  /* stuff the static fields of the catalog metadata structure 
  */
  lastslash = (char *)strrchr(filename, '/') ;
  strcpy(catrecord->file_name, lastslash+1) ;
  strcpy(catrecord->file_source, CSA_STR) ;
  strcpy(catrecord->file_dest, ASF_STR) ;
  strcpy(catrecord->satellite, R_SAT) ;
  strcpy(catrecord->number_of_records, NUMRECS_IN_PMF) ;
  
  /* get the date/time stamp off the file 
  */
  strcpy(catrecord->file_arrival_time, PMFfiletime(filename)) ;
  
  /* search through the CSA_Filetype_Table using CSAfiletype as an index, to
  -- determine the FA_file_type and gen_file_type fields
  */

  while(CSA_Filetype_Table[i].CSAfiletype[0])
    {
      if (strcmp(CSA_Filetype_Table[i].CSAfiletype, CSA_FTYPE_RECRQST) == 0)
	{
	  strcpy(catrecord->FA_file_type, CSA_Filetype_Table[i].FAfiletype) ;
	  strcpy(catrecord->gen_file_type, CSA_Filetype_Table[i].Genfiletype) ;
          strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
	  if ((strcmp(catrecord->gen_file_type, GFTYPE_SVTIMCOR) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_PSV) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_RSV) == 0))
	    strcpy(catrecord->format, ASF_STR) ;
	  else
	    strcpy(catrecord->format, ORIG_STR) ;
	}

      i++ ;
    }

  /* get the earliest IMG_DATA_DL_START from the RADARSAT file - this is the
  -- valid_start_time field of the ODL_catalog_metadata structure
  */
  
  curr = (NODEPTR) find_ag(CSARSH_RECEPT_ACT_SPECS, top) ;
  if (temp = (char *) find_keyword_value(CSARSH_START_PLAYBACK, curr))
    strcpy(time, temp) ;
  
  while (curr = (NODEPTR) next_ag(curr, top))
    {
      if (temp = (char *) find_keyword_value(CSARSH_START_PLAYBACK, curr))
	{
	  if (time)
	    if(CSA_ASF_timecmp(time, temp) < 0)
	      strcpy(time, temp) ;
	  else
	    strcpy(time, temp) ;
	}
    }
  
  /* place the earliest DL_START time in the C structure and convert it 
  -- to ODL format
  */

  strcpy(catrecord->valid_start_time, time) ;
  catrecord->valid_start_time[8] = 'T' ;
  
  /* get the latest IMG_DATA_DL_STOP from the RADARSAT file - this is the
  -- valid_end_time field of the ODL_catalog_metadata structure
  */
  
  curr = (NODEPTR) find_ag(CSARSH_RECEPT_ACT_SPECS, top) ;
  if (temp = (char *) find_keyword_value(CSARSH_STOP_PLAYBACK, curr))
    strcpy(time, temp) ;
  
  while (curr = (NODEPTR) next_ag(curr, top))
    {
      if (temp = (char *) find_keyword_value(CSARSH_STOP_PLAYBACK, curr))
	{
	  if (time)
	    if(CSA_ASF_timecmp(time, temp) > 0)
	      strcpy(time, temp) ;
	  else
	    strcpy(time, temp) ;
	}
    }
  
  /* place the earliest DL_STOP time in the C structure and convert it 
  -- to ODL format
  */

  strcpy(catrecord->valid_end_time, time) ;
  catrecord->valid_end_time[8] = 'T' ;
  
  /* the start and end revs are the orbit number 
  */
  strcpy(catrecord->start_rev, find_keyword_value(CSARSH_ORBIT_NUM, top));
  strcpy(catrecord->end_rev, find_keyword_value(CSARSH_ORBIT_NUM, top));

  /* File creation time is in CSA file header
  */
  strcpy(catrecord->file_creation_time, 
	 (char *)find_keyword_value(FILE_CREAT_TIME_STR, top)) ;
  catrecord->file_creation_time[8] = 'T' ;
  delete_tree(top);

  root = (AGGREGATE)create_PMF(catrecord);
  return (root);

} /* rrq2PMF */




/*==============================================================================
Function:       crq2PMF
Description:    Creates the PMF for a Calibration Request
Parameters:     char *filename - the name of the Calibration Request to be
                                 processed
Returns:        AGGREGATE root - the root of the PMF data structure if 
                                 successful
		NULL - PMF cannot be created
Creator:        Norbert Piega (norbert@cogito.jpl.nasa.gov)
Creation Date:  9/14/95
Notes:
==============================================================================*/
AGGREGATE crq2PMF(char *filename)
{
  NODEPTR top ;                     /* root of the CSA parse tree     */
  NODEPTR curr ;                    /* current CSA statement          */
  AGGREGATE root ;                  /* root of the PMF data structure */
  ODL_Catalog_Metadata *catrecord ; /* catalog metadata structure     */

  char time[TIME_STRING_LEN+1] ;
  char *lastslash ;
  int i = 0;

  catrecord = 
     (ODL_Catalog_Metadata *) util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; 

  /* parse CSA file and put the root in top 
  */ 
  top = (NODEPTR) create_tree(filename) ;

  /* stuff the static fields of the catalog metadata structure 
  */
  lastslash = (char *)strrchr(filename, '/') ;
  strcpy(catrecord->file_name, lastslash+1) ;
  strcpy(catrecord->file_source, CSA_STR) ;
  strcpy(catrecord->file_dest, ASF_STR) ;
  strcpy(catrecord->satellite, R_SAT) ;
  strcpy(catrecord->number_of_records, NUMRECS_IN_PMF) ;
  
  /* get the date/time stamp off the file for the arrival time 
  */
  strcpy(catrecord->file_arrival_time, PMFfiletime(filename)) ;
  
  /* search through the CSA_Filetype_Table using CSAfiletype as an index, to
  -- determine the FA_file_type and gen_file_type fields
  */

  while(CSA_Filetype_Table[i].CSAfiletype[0])
    {
      if (strcmp(CSA_Filetype_Table[i].CSAfiletype, CSA_FTYPE_CALRQST) == 0)
	{
	  strcpy(catrecord->FA_file_type, CSA_Filetype_Table[i].FAfiletype) ;
	  strcpy(catrecord->gen_file_type, CSA_Filetype_Table[i].Genfiletype) ;
          strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
	  if ((strcmp(catrecord->gen_file_type, GFTYPE_SVTIMCOR) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_PSV) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_RSV) == 0))
	    strcpy(catrecord->format, ASF_STR) ;
	  else
	    strcpy(catrecord->format, ORIG_STR) ;
	}

      i++ ;
    }

  /* get the valid_start_time and convert it to ODL format 
  */
  strcpy(catrecord->valid_start_time, 
	 find_keyword_value(CSACSH_START_CALTIME, top)) ;
  catrecord->valid_start_time[8] = 'T' ;
  
  /* get the valid_end_time and convert it to ODL format 
  */
  strcpy(catrecord->valid_end_time, 
	 find_keyword_value(CSACSH_STOP_CALTIME, top)) ;
  catrecord->valid_end_time[8] = 'T' ;
  
  /* the start and end revs are the orbit number 
  */
  strcpy(catrecord->start_rev, find_keyword_value(CSACSH_ABS_ORBIT_NUM, top)) ;
  strcpy(catrecord->end_rev, find_keyword_value(CSACSH_ABS_ORBIT_NUM, top));

  /* File creation time is in CSA file header
  */
  strcpy(catrecord->file_creation_time, 
	 (char *)find_keyword_value(FILE_CREAT_TIME_STR, top)) ;
  catrecord->file_creation_time[8] = 'T' ;
  delete_tree(top);

  root = (AGGREGATE)create_PMF(catrecord);
  return (root);

} /* crq2PMF */




/*==============================================================================
Function:       csh2PMF
Description:    Creates the PMF for a Calibration Schedule
Parameters:     char *filename - the name of the Calibration Schedule to be
                                 processed
Returns:        AGGREGATE root - the root of the PMF data structure if 
                                 successful
		NULL - PMF cannot be created
Creator:        Philip Yurchuk (phil@ditto.jpl.nasa.gov)
Creation Date:  7/10/95
Notes:
==============================================================================*/
AGGREGATE csh2PMF(char *filename)
{
  NODEPTR top ;                     /* root of the CSA parse tree     */
  NODEPTR curr ;                    /* current CSA statement          */
  AGGREGATE root ;                  /* root of the PMF data structure */
  ODL_Catalog_Metadata *catrecord ; /* catalog metadata structure     */

  char time[TIME_STRING_LEN+1] ;
  char *lastslash ;
  int i = 0;

  catrecord = 
     (ODL_Catalog_Metadata *) util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; 

  /* parse CSA file and put the root in top 
  */ 
  top = (NODEPTR) create_tree(filename) ;

  /* stuff the static fields of the catalog metadata structure 
  */
  lastslash = (char *)strrchr(filename, '/') ;
  strcpy(catrecord->file_name, lastslash+1) ;
  strcpy(catrecord->file_source, CSA_STR) ;
  strcpy(catrecord->file_dest, ASF_STR) ;
  strcpy(catrecord->satellite, R_SAT) ;
  strcpy(catrecord->number_of_records, NUMRECS_IN_PMF) ;
  
  /* get the date/time stamp off the file for the arrival time 
  */
  strcpy(catrecord->file_arrival_time, PMFfiletime(filename)) ;
  
  /* search through the CSA_Filetype_Table using CSAfiletype as an index, to
  -- determine the FA_file_type and gen_file_type fields
  */

  while(CSA_Filetype_Table[i].CSAfiletype[0])
    {
      if (strcmp(CSA_Filetype_Table[i].CSAfiletype, CSA_FTYPE_CALSCHED) == 0)
	{
	  strcpy(catrecord->FA_file_type, CSA_Filetype_Table[i].FAfiletype) ;
	  strcpy(catrecord->gen_file_type, CSA_Filetype_Table[i].Genfiletype) ;
          strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
	  if ((strcmp(catrecord->gen_file_type, GFTYPE_SVTIMCOR) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_PSV) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_RSV) == 0))
	    strcpy(catrecord->format, ASF_STR) ;
	  else
	    strcpy(catrecord->format, ORIG_STR) ;
	}

      i++ ;
    }

  /* get the valid_start_time and convert it to ODL format 
  */
  strcpy(catrecord->valid_start_time, 
	 find_keyword_value(CSACSH_START_CALTIME, top)) ;
  catrecord->valid_start_time[8] = 'T' ;
  
  /* get the valid_end_time and convert it to ODL format 
  */
  strcpy(catrecord->valid_end_time, 
	 find_keyword_value(CSACSH_STOP_CALTIME, top)) ;
  catrecord->valid_end_time[8] = 'T' ;
  
  /* the start and end revs are the orbit number 
  */
  strcpy(catrecord->start_rev, find_keyword_value(CSACSH_ABS_ORBIT_NUM, top)) ;
  strcpy(catrecord->end_rev, find_keyword_value(CSACSH_ABS_ORBIT_NUM, top));

  /* File creation time is in CSA file header
  */
  strcpy(catrecord->file_creation_time, 
	 (char *)find_keyword_value(FILE_CREAT_TIME_STR, top)) ;
  catrecord->file_creation_time[8] = 'T' ;
  delete_tree(top);

  root = (AGGREGATE)create_PMF(catrecord);
  return (root);

} /* csh2PMF */




/*==============================================================================
Function:       pap2PMF
Description:    Creates the PMF for a Payload Parameters file
Parameters:     char *filename - the name of the Orbit Data file to be
                                 processed
Returns:        AGGREGATE root - the root of the PMF data structure if 
                                 successful
		NULL - PMF cannot be created
Creator:        Philip Yurchuk (phil@ditto.jpl.nasa.gov)
Creation Date:  7/17/95
Notes:
==============================================================================*/
AGGREGATE pap2PMF(char *filename)
{
  NODEPTR top ;                     /* root of the CSA parse tree     */
  NODEPTR curr ;                    /* current CSA statement          */
  AGGREGATE root ;                  /* root of the PMF data structure */
  ODL_Catalog_Metadata *catrecord ; /* catalog metadata structure     */

  char time[TIME_STRING_LEN+1] ;
  char *lastslash ;
  int i = 0;


  catrecord = 
     (ODL_Catalog_Metadata *) util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; 

  /* parse CSA file and put the root in top 
  */ 
  top = (NODEPTR) create_tree(filename) ;

  /* stuff the static fields of the catalog metadata structure 
  */
  lastslash = (char *)strrchr(filename, '/') ;
  strcpy(catrecord->file_name, lastslash+1) ;
  strcpy(catrecord->file_source, CSA_STR) ;
  strcpy(catrecord->file_dest, ASF_STR) ;
  strcpy(catrecord->satellite, R_SAT) ;
  strcpy(catrecord->number_of_records, NUMRECS_IN_PMF) ;
  
  /* get the date/time stamp off the file for the arrival time 
  */
  strcpy(catrecord->file_arrival_time, PMFfiletime(filename)) ;
  
  /* search through the CSA_Filetype_Table using CSAfiletype as an index, to
  -- determine the FA_file_type and gen_file_type fields
  */
  while(CSA_Filetype_Table[i].CSAfiletype[0])
    {
      if (strcmp(CSA_Filetype_Table[i].CSAfiletype, CSA_FTYPE_SARPROCPRM) == 0)
	{
	  strcpy(catrecord->FA_file_type, CSA_Filetype_Table[i].FAfiletype) ;
	  strcpy(catrecord->gen_file_type, CSA_Filetype_Table[i].Genfiletype) ;
          strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
          strcpy(catrecord->start_rev, "\0");
          strcpy(catrecord->end_rev,   "\0");
	  if ((strcmp(catrecord->gen_file_type, GFTYPE_SVTIMCOR) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_PSV) == 0) ||
	      (strcmp(catrecord->gen_file_type, GFTYPE_RSV) == 0))
	    strcpy(catrecord->format, ASF_STR) ;
	  else
	    strcpy(catrecord->format, ORIG_STR) ;
 	}

      i++ ;
    }

  /* valid start time = effective starting time 
  */
  strcpy(catrecord->valid_start_time, 
	 find_keyword_value(CSAPAP_EFFECTIVE_START, top)) ;
  catrecord->valid_start_time[8] = 'T' ;

/* TODO valid end time ?
*/
  strcpy(catrecord->valid_end_time, 
	 find_keyword_value(CSAPAP_EFFECTIVE_START, top)) ;
  catrecord->valid_end_time[8] = 'T' ;

  /* File creation time is in CSA file header
  */
  strcpy(catrecord->file_creation_time, 
     (char *)find_keyword_value(FILE_CREAT_TIME_STR, top)) ;
  catrecord->file_creation_time[8] = 'T' ;
  delete_tree(top);

  root = (AGGREGATE)create_PMF(catrecord);
  return (root);

} /* pap2PMF */




/*==============================================================================
Function:       orb2PMF
Description:    Creates the PMF for a Orbit Data file
Parameters:     char *filename - the name of the Orbit Data file to be
                                 processed
Returns:        AGGREGATE root - the root of the PMF data structure if 
                                 successful
		NULL - PMF cannot be created
Creator:        Philip Yurchuk (phil@ditto.jpl.nasa.gov)
Creation Date:  7/17/95
Notes:
==============================================================================*/
AGGREGATE orb2PMF(char *filename, int filetype, ODL_Catalog_Metadata *catrecord)
{
  NODEPTR top ;                     /* root of the CSA parse tree     */
  NODEPTR curr ;                    /* current CSA statement          */
  AGGREGATE root ;                  /* root of the PMF data structure */

  NODEPTR temp, temp2 ;
  char time[TIME_STRING_LEN+1] ;
  char *lastslash ;
  int i = 0;

/*  catrecord = 
     (ODL_Catalog_Metadata *) util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; */

  /* parse CSA file and put the root in top 
  */ 
  top = (NODEPTR) create_tree(filename) ;

  /* stuff the static fields of the catalog metadata structure 
  */
  lastslash = (char *)strrchr(filename, '/') ;
  strcpy(catrecord->file_name, lastslash+1) ;
  strcpy(catrecord->file_source, CSA_STR) ;
  strcpy(catrecord->file_dest, ASF_STR) ;
  strcpy(catrecord->satellite, R_SAT) ;
  strcpy(catrecord->number_of_records, NUMRECS_IN_PMF) ;
  
  /* get the date/time stamp off the file for the arrival time 
  */
  strcpy(catrecord->file_arrival_time, PMFfiletime(filename)) ;
  
  /* FA and GEN file type
  */
  if (filetype == CSA_PREDORBIT)
  {
     strcpy(catrecord->FA_file_type, CSA_PREDORBIT_STR) ;
     strcpy(catrecord->gen_file_type, GFTYPE_PSV) ;
     strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
  }
  if (filetype == CSA_DEFVORBIT)
  {
     strcpy(catrecord->FA_file_type, CSA_DEFVORBIT_STR) ;
     strcpy(catrecord->gen_file_type, GFTYPE_RSV) ;
     strcpy(catrecord->number_of_records, NUMRECS_IN_PMF);
  }

  /* Format is always original
  */
  strcpy(catrecord->format, ORIG_STR) ;

  /* Get the start and end times 
  */
  temp = top ;
  
  /* valid_start_time is the time of the first vector 
  */
  while(temp->data_type != VECTOR)
    temp = temp->next ;
  strcpy(catrecord->valid_start_time, temp->vector[0]) ;
  catrecord->valid_start_time[8] = 'T' ;
  
  /* valid_end_time is the time of the last vector 
  */
  for (i = 2; i <= CSA_NUM_VECTORS; i++)
  {
    temp = temp->next;    /* the comment line */
    temp = temp->next;    /* the next vector  */
  } 
  strcpy(catrecord->valid_end_time, temp->vector[0]) ;
  catrecord->valid_end_time[8] = 'T' ;
  
  /* orbit number is both start and end revs 
  */
  strcpy(catrecord->start_rev, find_keyword_value(CSAORB_ORBIT_NUMBER, top)) ;
  strcpy(catrecord->end_rev, find_keyword_value(CSAORB_ORBIT_NUMBER, top));

  /* File creation time is in CSA file header
  */
  strcpy(catrecord->file_creation_time,
         (char *)find_keyword_value(FILE_CREAT_TIME_STR, top)) ;
  catrecord->file_creation_time[8] = 'T' ;
  delete_tree(top);

  root = (AGGREGATE)create_PMF(catrecord);

  return (root);

} /* orb2PMF */

  




/*==============================================================================
Function:	gen_CSA_PMF
Description:	Generate PMF file list for CSA file	
Parameters:
	CSA_file - name of CSA file
	path - directory path where CSA file is located
Returns:	linked list (PMF file list)	
Creator:	Norbert Piega
Creation Date:	Thu Sep 14 17:04:20 PDT 1995
Notes:		
==============================================================================*/

llist *gen_CSA_PMF(char *path, char *CSA_file, int filetype)
{
   AGGREGATE PMFroot  ;
   llist *pmflist ;
   PMF_FILE_LIST *fpmfrec ;
   ODL_Catalog_Metadata *pmf_rec ;
   FILE *PMF_file_ptr ;
   char trans_path[MAX_DIRNAME_LEN] ;
   char *fullCSApath ;
   llist         *err_ptr= NULL;
   int  status;
   char *transpath;
   char *pmffile;
   char          *translated_file;
   char *last_slash;
   time_t      current_time;
   time_t      *null_ptr = NULL;
   char tmp_arr_str[22];
   char temp_dataset[256] ;
   AGGREGATE root;

   pmf_rec = 
     (ODL_Catalog_Metadata *) util_do_malloc(sizeof(ODL_Catalog_Metadata)) ; 

   fullCSApath = 
      (char *)util_do_malloc(sizeof(char)*(strlen(path)+1+strlen(CSA_file)+1)) ;
   strcpy(fullCSApath, path) ;
   strcat(fullCSApath, "/") ;
   strcat(fullCSApath, CSA_file) ;

   switch(filetype)
   {
      case CSA_PREDORBIT:  /* Predicted Orbit */
         PMFroot = (AGGREGATE)orb2PMF(fullCSApath, CSA_PREDORBIT, pmf_rec) ;
         strcpy(temp_dataset, CSA_PREDORBIT_STR) ;
	 break ;
      case CSA_DEFVORBIT:  /* Definitive Orbit */
         PMFroot = (AGGREGATE)orb2PMF(fullCSApath, CSA_DEFVORBIT, pmf_rec) ;
         strcpy(temp_dataset, CSA_DEFVORBIT_STR) ;
         break ;
      case CSA_RECRQST:    /* Reception Request */
      case CSA_RRQ_MCM:
         PMFroot = (AGGREGATE)rrq2PMF(fullCSApath) ;
         strcpy(temp_dataset, CSA_RECRQST_STR) ;
         break ;
      case CSA_RECSCHED:   /* Reception Schedule */
      case CSA_RSH_MCM:
         PMFroot = (AGGREGATE)rsh2PMF(fullCSApath) ;
         strcpy(temp_dataset, CSA_RECSCHED_STR) ;
         break ;
      case CSA_CALIBRQST:  /* Calibration Request */
         PMFroot = (AGGREGATE)crq2PMF(fullCSApath) ;
         strcpy(temp_dataset, CSA_CALIBRQST_STR) ;
         break ;
      case CSA_CALIBSCHED: /* Calibration Schedule */
         PMFroot = (AGGREGATE)csh2PMF(fullCSApath) ;
         strcpy(temp_dataset, CSA_CALIBSCHED_STR) ;
         break ;
      case CSA_SARPROCPRM: /* SAR Processing Parameters */
         PMFroot = (AGGREGATE)pap2PMF(fullCSApath) ;
         strcpy(temp_dataset, CSA_SARPROCPRM_STR) ;
         break ;
      default:
         syslog(LOG_ERR,
                 "ERROR: Unrecognized file.  Unable to generate PMF for file %s\n",
                  CSA_file) ;
         return(NULL) ;

   } /* endswitch */

   /* Write the PMF out to the PMF file 
   */
   pmffile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+1+4+
					 strlen(CSA_file)+ 3));
   strcpy(pmffile,path);
   strcat(pmffile,"/PMF/");
   strcat(pmffile, CSA_file);
   strcat(pmffile, ".");
   strcat(pmffile, PMF_FILE_EXT);

   if ((PMF_file_ptr = fopen(pmffile, "w")) == (FILE *)NULL)
   {
      syslog(LOG_ERR,
         "ERROR: Unable to open PMF for file %s\n",
          CSA_file) ;
      return(NULL) ;
   }
   WriteLabel (PMF_file_ptr, PMFroot);
   fclose(PMF_file_ptr);

   /* Make a link list that has filename and PMF filename 
   */
   pmflist = create_dyn_llist();
  
   /* Create and append an entry to linked list
   */
   fpmfrec = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
   strcpy(fpmfrec->file_name, CSA_file);
   last_slash = (char *)strrchr(pmffile, '/');
   sprintf(fpmfrec->PMF_file_name,"%s",last_slash + 1);
   strcpy(fpmfrec->dataset_suffix, temp_dataset) ;
   fpmfrec->orig_not_tran = TRUE ;
   APPEND(pmflist, fpmfrec, free, fpmfrec);
 
   /* If orbit file, need to translate
   -- Generate PMF for translated file as well
   */
   if (filetype == CSA_PREDORBIT || filetype == CSA_DEFVORBIT)
   {
      /* Perform format translation (CSA to ASF format)
      */
      transpath = (char *)util_do_malloc(sizeof(char)*
                                    (strlen(path)+
				     strlen("/tran")+ 1));
      strcpy(transpath, path) ;
      strcat(transpath, "/tran") ;

      status = check_CSA_translation(CSA_file, path, transpath,
                 &translated_file, filetype) ;

      if (status == ERROR)
      {
         syslog(LOG_ERR,
            "ERROR: Error in translation of CSA orbit file %s\n",
             CSA_file) ;
         return(NULL) ;
      }

      /* Fill in PMF record info for translated file
      */
      current_time = (time_t)time(null_ptr);
      status = cftime(tmp_arr_str, "%Y-%jT%H:%M:%S.000", &current_time);
      strcpy (pmf_rec->file_arrival_time, tmp_arr_str);
      strcpy (pmf_rec->file_creation_time, tmp_arr_str);
      strcpy (pmf_rec->format, ASF_STR);

      /* Create name of PMF filename for translated file
      */
      last_slash = (char *)strrchr(translated_file, '/');
      pmffile = (char *)util_do_malloc(sizeof(char)*
					(strlen(path)+1+4+
					 strlen(last_slash)+ 3));
      strcpy(pmffile,path);
      strcat(pmffile,"/PMF/");
      strcat(pmffile, last_slash);
      strcat(pmffile, ".");
      strcat(pmffile, PMF_FILE_EXT);

      strcpy (pmf_rec->file_name, last_slash+1);

      PMFroot = (AGGREGATE)create_PMF(pmf_rec);
      fpmfrec = (PMF_FILE_LIST *) NEW(sizeof(PMF_FILE_LIST));
      strcpy (fpmfrec->file_name, last_slash + 1);
      sprintf(fpmfrec->PMF_file_name,"%s.%s", last_slash + 1, PMF_FILE_EXT);
      strcpy(fpmfrec->dataset_suffix, CSA_SV_HSTR) ;
      fpmfrec->orig_not_tran = FALSE ;
      PREPEND(pmflist, fpmfrec, free, fpmfrec);

      PMF_file_ptr = fopen(pmffile,"w");
      if (PMF_file_ptr == (FILE *)NULL)
      {
         syslog(LOG_ERR,
          "ERROR: Unable to open output translated PMF file\n") ;
         return(err_ptr) ;
      }

      WriteLabel (PMF_file_ptr, PMFroot);
      fclose(PMF_file_ptr);

   }

      RemoveAggregate(PMFroot);
      return(pmflist) ;

} /* gen_CSA_PMF */





/*==============================================================================
Function:	int check_CSA_translation(char *filename, char *srcdir, 
                   char *transdir, char **translated_file, int filetype)

Description:	
	This file checks if a file is a type of file that needs to be
translated from a flight agency format to an ASF format.  If the file
does not need to be translated, OK is returned.  Otherwise, the
status of the translation procedures is returned.  The procedure
includes determining the translation input and output filenames and
the call to the appropriate translation routine.

Parameters:
	char *filename - file record for the file to be checked
for translation

	char *srcdir - This is the directory containing the file to be
translated, if necessary

	char *translated_file - This is used to pass back the standard
name for the translated file

	int filetype - integer code corresponding to the file type of the
CSA file to translate

Returns:	
	ERROR - if there is an error in determining the filename of the
translated version or if there is an error in the translation

	OK - checked if translation is needed and successfully performed
translation on files that need it

Creator:	Norbert Piega	
Creation Date:	08/17/1994
Notes:		
	This function is for checking for CSA state vector files. 
==============================================================================*/
#ifdef __STDC__
int
check_CSA_translation(char *filename, char *srcdir, char *transdir,
   char **translated_file, int filetype)
#else
int
check_CSA_translation(filename, srcdir, transdir, translated_file, filetype)
   char *filename ;
   char *srcdir ;
   char *transdir ;
   char **translated_file ;
   int  filetype ;
#endif
{
   int status = OK ;
   int i ;
   char *ASF_stvec_filename ;
   char *CSA_stvec_filename ;
   char CSA_orbitnum_str[CSA_ORBITNUM_STR_SIZE+1] ;

   /* If file is a CSA state vector file, generate ASF translation version
   */
   if (filetype == CSA_PREDORBIT || filetype == CSA_DEFVORBIT)
   {
      /* CSA orbit number starts from the 2nd
      -- character to CSA_ORBITNUM_STR_SIZE 
      -- characters after that.
      */
      if (isdigit(filename[1])) 
      {
	 for (i=0; i<CSA_ORBITNUM_STR_SIZE ; i++)
	    CSA_orbitnum_str[i] = filename[i+1] ;
         CSA_orbitnum_str[i] = '\0' ;
      }

      /* Generate ASF file name: 
      --   TranslationDir/RADARSAT_XXXXX.YYY
      --
      --  where TranslationDir is the directory where the
      --  translation output is stored, XXXXX is the orbit 
      --  number derived above and YYY is "PSV" for predicted
      --  and "RSV" for restituted.
      --
      --  Note that the XXXXX is from the 5 righmost digits
      -- from the CSA orbit number.  The 2 leftmost digits
      -- from the CSA orbit number are ignored.
      */

      /* If translation directory name is NULL, error
      */
      if (transdir == (char *)NULL)
      {
	 syslog(LOG_ERR, 
	   "ERROR: Undefined CSA translation directory\n") ;
	 return(ERROR) ;
      }
      else
      {
         ASF_stvec_filename = 
            (char *) util_do_malloc(sizeof(char)*
		     (strlen(transdir)+ 1 /* for the '/' */ +
		      ASF_SV_FNAME_LEN + 1)) ;
         strcpy(ASF_stvec_filename, transdir) ;
      }
      strcat(ASF_stvec_filename, "/") ;
      strcat(ASF_stvec_filename, ASF_SV_FNAME_PREFIX) ;
      strcat(ASF_stvec_filename, 
	 CSA_orbitnum_str+ (CSA_ORBITNUM_STR_SIZE-ASF_ORBITNUM_STR_SIZE)) ;
      strcat(ASF_stvec_filename, ".") ;
      if (filetype == CSA_PREDORBIT) 
         strcat(ASF_stvec_filename, ASF_PRDSV_FNAME_EXT) ;
      else
         strcat(ASF_stvec_filename, ASF_RESSV_FNAME_EXT) ;

      CSA_stvec_filename = 
         (char *) util_do_malloc(sizeof(char)*
	              (strlen(srcdir)+1+strlen(filename)+1)) ;
      strcpy(CSA_stvec_filename, srcdir) ;
      strcat(CSA_stvec_filename, "/") ;
      strcat(CSA_stvec_filename, filename) ;

      syslog(LOG_NOTICE, "NOTICE: Translating %s to %s\n",
	 CSA_stvec_filename, ASF_stvec_filename) ;

      /* Call the PPF state vector translation function
      */
      status = translate_CSA_stvec(CSA_stvec_filename, ASF_stvec_filename) ;

      *translated_file = (char *)util_do_malloc(sizeof(char)*
                         (strlen(ASF_stvec_filename)+1)) ;
      strcpy(*translated_file, ASF_stvec_filename) ;

   }

   return(status) ;

} /* check_CSA_translation */

/* End of File */

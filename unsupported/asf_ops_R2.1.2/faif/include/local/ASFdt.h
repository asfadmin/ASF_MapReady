/*==============================================================================
Filename:	ASFdt.h
Description:	
	Header file for ASF datatake message file functions.  

	This include file contains the definitions of the ASF Datatake
Message related data structures and #defines.
 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF RGS Interface Memo by Navid Dehghani for a
description of the ASF Datatake Message file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ASFDT_
#define _ASFDT_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "ASF.h"

#define ASFDT_DATATAKEID    13
#define ASFDT_ACTID          6
#define ASFDT_TAKENFLAG      3
#define ASFDT_MEDIAID        6
#define ASFDT_STARTENDTAPE  16
#define ASFDT_RECORDID       2
#define ASFDT_FSTENADDR      4

#define ASFDT_RECLEN ((ASFDT_DATATAKEID)+1 + (ASFDT_ACTID)+1 + \
		       (ASFDT_TAKENFLAG)+1 + (ASFDT_MEDIAID)+1 + \
		       (2*((ASFDT_STARTENDTAPE)+1)) + \
		       (2*((ASFHDR_YEAR)+1)) + (2*((ASFHDR_TIME)+1)) + \
		       (ASFDT_RECORDID)+1 + (2*((ASFDT_FSTENADDR)+1)))

typedef struct asf_dt_record
{
   char datatake_id[ASFDT_DATATAKEID+1] ;
   char activity_id[ASFDT_ACTID+1] ;
   char taken_flag[ASFDT_TAKENFLAG+1] ;
   char media_id[ASFDT_MEDIAID+1] ;
   char start_tape[ASFDT_STARTENDTAPE+1] ;
   char end_tape[ASFDT_STARTENDTAPE+1] ;
   char on_year[ASFHDR_YEAR+1] ;
   char off_year[ASFHDR_YEAR+1] ;
   char on_time[ASFHDR_TIME+1] ;
   char off_time[ASFHDR_TIME+1] ;
   char recorder_id[ASFDT_RECORDID+1] ;
   char fst_addr[ASFDT_FSTENADDR+1] ;
   char fen_addr[ASFDT_FSTENADDR+1] ;

} ASF_DT_Record ;

#endif /* _ASFDT_ */

/* End of File */


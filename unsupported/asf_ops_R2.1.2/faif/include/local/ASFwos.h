/*==============================================================================
Filename:	ASFwos.h
Description:	
	Header file for ASF WOS file functions.
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF RGS Interface Memo by Navid Dehghani for a
description of the ASF WOS file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ASFWOS_
#define _ASFWOS_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "ASF.h"

#define ASFWOS_DATATAKEID    13
#define ASFWOS_ACTID          6
#define ASFWOS_TRANSID        2
#define ASFWOS_SITENAME      32
#define ASFWOS_MODE          15
#define ASFWOS_SPARE         22

#define ASFWOS_RECLEN ((ASFWOS_DATATAKEID)+1 + \
		       (ASFWOS_ACTID)+1 + \
		       (2*((ASFHDR_YEAR)+1)) + \
		       (2*((ASFHDR_TIME)+1)) + \
		       (ASFWOS_TRANSID)+1 + \
		       (ASFWOS_SITENAME)+1 + \
		       (ASFWOS_MODE) + \
		       (ASFWOS_SPARE))

typedef struct asf_wos_record
{
   char datatake_id[ASFWOS_DATATAKEID+1] ;
   char activity_id[ASFWOS_ACTID+1] ;
   char on_year[ASFHDR_YEAR+1] ;
   char off_year[ASFHDR_YEAR+1] ;
   char on_time[ASFHDR_TIME+1] ;
   char off_time[ASFHDR_TIME+1] ;
   char trans_id[ASFWOS_TRANSID+1] ;
   char site_name[ASFWOS_SITENAME+1] ;
   char mode[ASFWOS_MODE+1] ;
   char spare[ASFWOS_SPARE+1] ;

} ASF_WOS_Record ;

#endif /* _ASFWOS_ */

/* End of File */

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_blank_values.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_blank_values.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_blank_values.c"



/*==============================================================================
Function:       dtkm_blank_values()

Description:    puts blank startup values into a dtk DB_RECORD to 
				make printing a dtk record without certain values.  
				we were getting an early termination of a dtk_string 
				due to 0 values for single-character fields.  

Creator:        Lawrence Stevens

Creation Date:  Fri Dec 15 23:09:21 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

#include <string.h>

int dtkm_blank_values(
	DB_RECORD	**input_dtk,
	DB_RECORD	**result_dtk )
{

	/* quick error check */
    if ( input_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

	db_copy_record( APS_CDEFS(DTK), result_dtk, input_dtk ) ; 

	strcpy( CAST_DTK_SAT result_dtk[DTK_SAT], "" ) ;
	strcpy( CAST_DTK_SENSOR result_dtk[DTK_SENSOR], "" ) ;
	CAST_DTK_REV result_dtk[DTK_REV] = 0 ;
	CAST_DTK_DTKID result_dtk[DTK_DTKID] = 0 ;
	strcpy( CAST_DTK_FADTKID result_dtk[DTK_FADTKID], "" ) ;
	CAST_DTK_DARID result_dtk[DTK_DARID] = 0 ;
	strcpy( CAST_DTK_ACTID result_dtk[DTK_ACTID], "" ) ;
	CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] = ' ' ;
	strcpy( CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME], "" ) ;
	strcpy( CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME], "" ) ;

	CAST_DTK_LOOKANGL result_dtk[DTK_LOOKANGL] = 0.0 ;
	strcpy( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "" ) ;
	strcpy( CAST_DTK_PROPOSED_DTKSTAT result_dtk[DTK_PROPOSED_DTKSTAT], "" ) ;
	strcpy( CAST_DTK_TRANSID result_dtk[DTK_TRANSID], "" ) ;
	strcpy( CAST_DTK_SITENAME result_dtk[DTK_SITENAME], "" ) ;
	strcpy( CAST_DTK_NOTES result_dtk[DTK_NOTES], "" ) ;
	strcpy( CAST_DTK_DTKDATE result_dtk[DTK_DTKDATE], "" ) ;

	strcpy( CAST_DTK_STATION_ID result_dtk[DTK_STATION_ID], "" ) ;

#ifdef CAST_DTK_FA_SCHEDULE_LINK
	strcpy( CAST_DTK_FA_SCHEDULE_LINK result_dtk[DTK_FA_SCHEDULE_LINK], "" ) ;
#endif

	CAST_DTK_PLANNER_QUICKLOOK result_dtk[DTK_PLANNER_QUICKLOOK] = ' ' ;
	CAST_DTK_SCIENCE_QUICKLOOK result_dtk[DTK_SCIENCE_QUICKLOOK] = ' ' ;

	strcpy( CAST_DTK_SUBMIT_TIME result_dtk[DTK_SUBMIT_TIME], "" ) ;
	CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] = 0 ;

	return TRUE ;
}

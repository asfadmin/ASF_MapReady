#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_create_same_sat_time_bracket.c

Description:	

External Functions Defined:
dtkm_create_same_sat_time_bracket()
	
File Scope Functions:
dtkm_time_bracket_dtk()
	
External Variables Defined:
	
File Scope Variables:
	
Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_create_same_sat_time_bracket.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_create_same_sat_time_bracket.c"

#include "dapps_list.h"
#include "dapps_defs.h"
#include "db_sybint.h"
#include "dtkm.h"
#include "db_dtk.h"

#include <string.h>		/* for strcpy()  */


/*==============================================================================
Function:       dtkm_create_same_sat_time_bracket()

Description:    determine the time bracket to use when comparing 
				a dtk_proposal with existing data-takes when 
				trying to detect conflicts at the same antenna.
Returns:        TRUE, FALSE

Creator:        Lawrence Stevens

Creation Date:  Sat Nov 11 15:22:32 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_create_same_sat_time_bracket( 
	DB_RECORD	**dtk_proposal,
	llist		*dtk_similars, 	/* dtks overlapping, similar function.  */
	llist		*dtk_parallels, /* dtks overlapping, different function.*/
	llist		*dtk_same_pass,	/* dtks with same downlink, not overlapping.  */
	char 		*ss_strttime, 	/* earliest start time of all data-takes.    */
	char 		*ss_stoptime )	/* latest stop time of all data-takes.    */
{

	/* 
	-- initialize the same sat (ss) time 
	-- bracket:  
	*/
	strcpy( ss_strttime, CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) ;
	strcpy( ss_stoptime, CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) ;

	/* 
	-- expand the time bracket to include 
	-- the data-takes on these lists: 
	*/

	if (!dtkm_time_bracket_dtk_list(dtk_similars,  ss_strttime, ss_stoptime ) )
		return FALSE ;

	if (!dtkm_time_bracket_dtk_list(dtk_parallels, ss_strttime, ss_stoptime ) )
		return FALSE ;

	if (!dtkm_time_bracket_dtk_list(dtk_same_pass, ss_strttime, ss_stoptime ) )
		return FALSE ;

	return TRUE ;

}

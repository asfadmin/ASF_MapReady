#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_sat_has_recorder.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_sat_has_recorder.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_sat_has_recorder.c"



/*==============================================================================
Function:	dtkm_sat_has_recorder

Description:  returns TRUE if the satellite has a tape recorder on board.  

Parameters:		
	DB_RECORD    **dtk_record  record to check.

Returns:   	
	TRUE   (1)  the satellite DOES have a recorder.  
	FALSE  (0)  the satellite does NOT have a recorder.  
	< 0         ERROR
		        DTKM_ERROR_NULL_RECORD ;
		        DTKM_ERROR_UNKNOWN_SAT ;

Creator:	Lawrence Stevens

Creation Date:	Mon Nov  6 12:59:11 PST 1995

Notes:		
	SAMPLE:  dtkm_sat_has_recorder(dtk_record ) ;
==============================================================================*/

#include "dtkm.h"
#include "db_dtk.h"		/* for DTK relation stuff.  */

#include <string.h>		/* for strncmp()  			*/

int dtkm_sat_has_recorder( DB_RECORD **dtk_rec )
{

	if (dtk_rec == NULL)
		return DTKM_ERROR_NULL_RECORD ;

	if ( strncmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E1", 2) == 0 )
		return FALSE ;
	else if ( strncmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E2", 2) == 0 )
		return FALSE ;
	else if ( strncmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "J1", 2) == 0 )
		return TRUE ;
	else if ( strncmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1", 2) == 0 )
		return TRUE ;
	else if ( strncmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "A1", 2) == 0 )
		return TRUE ;
	else
		return DTKM_ERROR_UNKNOWN_SAT ;
}


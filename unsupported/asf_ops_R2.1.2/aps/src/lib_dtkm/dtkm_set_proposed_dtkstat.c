#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_set_proposed_dtkstat.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_set_proposed_dtkstat.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_set_proposed_dtkstat.c"



/*==============================================================================
Function:       dtkm_set_proposed_dtkstat()

Description:    set the dtk.proposed_dtkstat value in a DB_RECORD 
				from its dtk.dtkstat value.  
				This is used when we are about to deny a dtk DB_RECORD 
				its proposed status, and before we clobber that value, 
				we want to save the desired/proposed_dtkstat value 
				into the dtk.proposed_dtkstat field.  
				Because of the "if" statement, we put this in a 
				separate routine to that a maintenance programmer 
				does not have to encounter the if everywhere.  

Creator:        Lawrence Stevens

Creation Date:  Wed Dec  6 11:38:39 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>

int dtkm_set_proposed_dtkstat(
	DB_RECORD **dtk_proposal, /* readonly input DB_RECORD                     */
	DB_RECORD **result_dtk  ) /* this pointer can be the same as dtk_proposal */
{

	if ( dtk_proposal == NULL )
		return DTKM_ERROR_NULL_DTK_PROPOSAL ;

	if ( result_dtk == NULL )
		return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

	db_copy_record ( APS_CDEFS(DTK), result_dtk, dtk_proposal ) ;

	if ( strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "QUE" ) == 0
	||   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SUB" ) == 0
	||   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN" ) == 0
	||   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH" ) == 0 )
	{
		/* 
		-- we only want to save this value here if it is 
		-- QUE, SUB, PLN, or SCH.
		-- this is the only place where this "if" statement 
		-- is located.  
		*/
		strcpy( CAST_DTK_PROPOSED_DTKSTAT result_dtk[DTK_PROPOSED_DTKSTAT],
				CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT] ) ;
	}

	return TRUE ;
}

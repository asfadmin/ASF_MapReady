#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_update_best_antenna.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_update_best_antenna.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_best_antenna.c"

#include "dtkm.h"
#include "db_dtk.h"


/*==============================================================================
Function:       dtkm_update_best_antenna()

Description:    the current antenna has been found to be better than 
				the previous best antenna.  record the current antenna 
				data as the best antenna.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 17:35:54 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_update_best_antenna( 
	DBPROCESS			*APS_dbproc,
	DB_RECORD			**dtk_proposal,
	llist				*dtk_conflicts,
	struct Best_Antenna *p )
{

	llist	*list_check = NULL ;
	int		return_code ;


	/* antenna_id for new antenna.  */
	p->antenna_id = CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID] ;

	/* antenna preference for satellite, new antenna.  */
	return_code = dtkm_get_antenna_preference(APS_dbproc, dtk_proposal,
		&(p->preference) ) ;
	if ( return_code < 0 )
		return return_code ;

	/* 
	-- current data-takes that conflict with the 
	-- dtk_proposal.  all of them can be bumped by 
	-- dtk_proposal.  
	-- destroy the members of the current best antenna conflicts 
	-- list and move the current conflicts into it.  
	*/
	DEL_ALL( p->dtk_conflicts ) ;
	list_check = db_record_llist_move( p->dtk_conflicts, dtk_conflicts ) ;
	if ( list_check!= p->dtk_conflicts )
		return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;

	return TRUE ;

}


/*==============================================================================
Function:       dtkm_compare_best_antenna()

Description:    compare the current antenna with the best antenna to see 
				which is preferable.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 17:48:40 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_compare_best_antenna(
	DBPROCESS				*APS_dbproc,
	DB_RECORD				**dtk_proposal,
	llist					*dtk_conflicts,
	struct Best_Antenna		*p )
{
	int		return_code ;
	int		dtk_proposal_preference ;

	/* if there is no previous best antenna, the new one is best.  */
	if ( p->antenna_id <= 0 )
		return DTKM_NEW_BEST_ANTENNA ;

	/* get the preference of the dtk_proposal on the current antenna.  */
	return_code = dtkm_get_antenna_preference(APS_dbproc, dtk_proposal,
		&dtk_proposal_preference ) ;
	if ( return_code < 0 )
		return return_code ;

	/*
	-- STEP 8
	-- An antenna is considered to be "better" if the antenna 
	-- preference is LOWER.  
	*/
	if ( dtk_proposal_preference < p->preference )
		return DTKM_NEW_BEST_ANTENNA ;

	/*
	-- In addition, an antenna is considered to be "better" if 
	-- the antenna preference is equal AND if the number of 
	-- conflicts is lower.
	*/
	if ( dtk_proposal_preference == p->preference 
	&&   NUMELTS(dtk_conflicts)  <  NUMELTS(p->dtk_conflicts)  )
		return DTKM_NEW_BEST_ANTENNA ;

	return DTKM_NO_NEW_BEST_ANTENNA ;

}

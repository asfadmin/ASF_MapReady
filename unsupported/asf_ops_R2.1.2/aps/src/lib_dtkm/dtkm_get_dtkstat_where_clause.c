#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_get_dtkstat_where_clause.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_dtkstat_where_clause.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_dtkstat_where_clause.c"



/*==============================================================================
Function:       dtkm_get_dtkstat_where_clause()

Description:    write the appropriate where_clause fragment for the 
				dtkstat field according to the dtk_proposal status.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sun Nov 12 20:07:11 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"  

#include <string.h>  	/* for strcmp()	*/

int dtkm_get_dtkstat_where_clause( 
	DB_RECORD	**dtk_proposal, 
	char		*dtkstat_where_clause ) 
{

	if ( dtk_proposal == NULL )
		return DTKM_ERROR_NULL_DTK_PROPOSAL ;

	/* 
	--  according to the status if the proposal, adjust the 
	--  DTKSTAT qualifiers in the retrieve from the database.  
	--
    --  When checking the dtk relation, the status of the dtk proposal
    --  limits which records are retrieved:
    --
    --  dtk proposal    check only dtks
    --  status          with status of:
    --  ------------    ---------------
    --  QUE             SCH, SUB, PLN, QUE
    --  SUB             SCH, SUB, PLN, QUE
    --  PLN             SCH, SUB, PLN
    --  REQ             SCH, SUB, PLN
    --  SCH             SCH, PLN
    --
    --  Thus when processing a dtk proposal with status = SCH, all records
    --  in the dtk relation with a status other than SCH and PLN are
    --  completely ignored.
	*/
 
	if ( strcmp (CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "QUE") == 0
	||   strcmp (CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "SUB") == 0 )
	{
		sprintf(dtkstat_where_clause, 
			" and ( %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' ) ",
			APS_COL(DTK, DTK_DTKSTAT), "QUE",
			APS_COL(DTK, DTK_DTKSTAT), "SUB",
			APS_COL(DTK, DTK_DTKSTAT), "SCH",
			APS_COL(DTK, DTK_DTKSTAT), "PLN" ) ;
	}
	else if ( strcmp (CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "PLN") == 0
		 ||   strcmp (CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "REQ") == 0 )
	{
		sprintf(dtkstat_where_clause, 
			" and ( %s = '%s' or %s = '%s' or %s = '%s' ) ",
			APS_COL(DTK, DTK_DTKSTAT), "SUB",
			APS_COL(DTK, DTK_DTKSTAT), "SCH",
			APS_COL(DTK, DTK_DTKSTAT), "PLN" ) ;
	}
	else if ( strcmp (CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "SCH") == 0 )
	{
		sprintf(dtkstat_where_clause, 
			" and ( %s = '%s' or %s = '%s' ) ",
			APS_COL(DTK, DTK_DTKSTAT), "SCH",
			APS_COL(DTK, DTK_DTKSTAT), "PLN" ) ;
	}
	else 
		return DTKM_ERROR_STATUS_NOT_QUE_PLN_REQ_OR_SCH ;

	return TRUE ;
}

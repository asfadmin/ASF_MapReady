#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#ifdef THIS_IS_OBSOLETE_CODE

/*==============================================================================
Filename:		dtkm_update_aos_los_fields.c

Description:	updates the aos/los fields in a list from a dtk.  

Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_update_aos_los_fields.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_aos_los_fields.c"


/*==============================================================================
Function:       dtkm_update_aos_los_fields()

Description:    updates the aos/los fields in a list from a dtk.
				the aos-los time bracket should contain the 
				strttime-stoptime bracket.  

Creator:        Lawrence Stevens

Creation Date:  Wed May  1 17:14:07 PDT 1996

Notes:		
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>

int dtkm_update_aos_los_fields(
	DBPROCESS   *APS_dbproc,
	DB_RECORD   **dtk_proposal,   /* read aos/los here                       */
	llist       *dtk_list,        /* update aos/los fields from proposal     */
	llist       *dtk_updates )    /* updates are copied into list if != NULL */
{

	DB_RECORD	**dtk_rec ;
	cursor		dtk_list_ptr ;
	int			update_flag ;
	int			return_code ;
 
    /* error checking  */
	if ( dtk_proposal == NULL )
		return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
 
    if ( NUMELTS( dtk_list ) == 0 )
        return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

	/* 
	-- check each dtk in the list for updating.  
	*/
	for ( 	dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
			dtk_rec != NULL ;
			dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)
		)
	{
		/* keep a flag for updating.  */
		update_flag = 0 ;

		if ( strcmp(CAST_DTK_AOS_TIME dtk_rec[DTK_AOS_TIME],
					CAST_DTK_AOS_TIME dtk_proposal[DTK_AOS_TIME]) != 0 
		&&	 strcmp(CAST_DTK_AOS_TIME dtk_proposal[DTK_AOS_TIME],
					CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME]) < 0 )
		{
			/* 
			-- the proposal AOS_TIME is different from this dtk_rec
			-- also, the proposal AOS_TIME is earlier than the 
			-- dtk_rec start time.  Therefore, we can update the AOS_TIME 
			-- for the dtk_rec.  
			*/
			memmove( CAST_DTK_AOS_TIME dtk_rec[DTK_AOS_TIME],
				CAST_DTK_AOS_TIME dtk_proposal[DTK_AOS_TIME],
				ASF_TIME_STR_LENGTH ) ;
			*((CAST_DTK_AOS_TIME dtk_rec[DTK_AOS_TIME])+ASF_TIME_STR_LENGTH)
				= '\0' ;
			update_flag++ ;
		}

		if ( strcmp(CAST_DTK_LOS_TIME dtk_rec[DTK_LOS_TIME],
					CAST_DTK_LOS_TIME dtk_proposal[DTK_LOS_TIME]) != 0 
		&&	 strcmp(CAST_DTK_LOS_TIME dtk_proposal[DTK_LOS_TIME],
					CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME]) > 0 )
		{
			/* 
			-- the proposal LOS_TIME is different from this dtk_rec
			-- also, the proposal LOS_TIME is later than the 
			-- dtk_rec stop time.  Therefore, we can update the LOS_TIME 
			-- for the dtk_rec.  
			*/
			memmove( CAST_DTK_LOS_TIME dtk_rec[DTK_LOS_TIME],
				CAST_DTK_LOS_TIME dtk_proposal[DTK_LOS_TIME],
				ASF_TIME_STR_LENGTH ) ;
			*((CAST_DTK_LOS_TIME dtk_rec[DTK_LOS_TIME])+ASF_TIME_STR_LENGTH)
				= '\0' ;
			update_flag++ ;
		}

		if ( update_flag > 0 )
		{
			return_code = dtkm_update_dtk_record( APS_dbproc,
				dtk_rec, dtk_rec, dtk_updates ) ;
			if ( return_code < 0 )
				return return_code ;
		}
	}

	return TRUE ;

}
#endif

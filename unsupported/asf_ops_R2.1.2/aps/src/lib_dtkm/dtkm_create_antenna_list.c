#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_create_antenna_list.c

Description:	routines to handle antenna lists.  

External Functions Defined:
	int dtkm_create_antenna_list()
	int dtkm_select_next_untried_antenna()
	int dtkm_check_off_antenna( )

File Scope Functions:
	
External Variables Defined:
	the antenna list is allocated here and can be accessed and must 
	be freed via the return pointer from dtkm_create_antenna_list().  
	
File Scope Variables:
	
Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_create_antenna_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_create_antenna_list.c"

#include "dtkm.h"
#include "db_antenna_pref.h"
#include "db_dtk.h"

extern void *malloc();	/* conflict with <stdlib.h> and <macros.h> over abs() */

#include <string.h>		/* for strcpy()   */



/*==============================================================================
Function:       dtkm_create_antenna_list()

Description:    allocates memory for the list of antennas and populates it.  
				the list is a zero-terminated array of integers.  each 
				integer specifies a possible antenna_id for the data-take. 
				when one antenna has been considered, it is checked off by 
				setting its value to -1.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Thu Nov  2 11:05:20 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

int dtkm_create_antenna_list(
	DBPROCESS	*APS_dbproc, 
	DB_RECORD	**dtk_proposal, 	
	int			**new_antenna_list ) /* address of new list of antennas.  */
{

	DB_RECORD       **antenna_pref_rec ;
	llist           *antenna_pref_list = NULL ;
	cursor          antenna_pref_list_ptr ;
	int				offset ;			/* counter for filling list.  */

	/*
	-- get the list of antenna_ids for the dtk_proposal, sort 
	-- them by preference.  Most preferred goes first.  
	*/
	sprintf(where_clause, "where %s = '%s' and %s = '%s'", 
		APS_COL(ANTENNA_PREF, ANTENNA_PREF_SAT), 
			CAST_DTK_SAT dtk_proposal[DTK_SAT],
		APS_COL(ANTENNA_PREF, ANTENNA_PREF_STATION_ID), 
			CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID] ) ;

	strcpy(orderby_cols, APS_COL(ANTENNA_PREF, ANTENNA_PREF_PREFERENCE) ) ;

	antenna_pref_list = db_get_records( APS_dbproc, APS_TABLE(ANTENNA_PREF), 
		where_clause, orderby_cols, APS_CDEFS(ANTENNA_PREF), ALL_COLS) ;

	if (antenna_pref_list == NULL)
		return DTKM_ERROR_DB_QUERY_FAILED ;

	if ( NUMELTS( antenna_pref_list ) == 0 )
	{
		DEL_LIST( antenna_pref_list ) ;
		return DTKM_ERROR_NO_ANTENNAS_FOUND_IN_ANTENNA_PREF_TABLE ;
	}

	/* 
	-- get one int per antenna, plus one int for a zero termination. 
	*/

	*new_antenna_list = (int *) malloc( 
		( NUMELTS( antenna_pref_list ) + 1) * sizeof(int) ) ;

	/*
	-- fill the list:
	*/
	offset = 0 ;
	for ( antenna_pref_rec = (DB_RECORD **) 
				FIRST(antenna_pref_list, antenna_pref_list_ptr);
		  antenna_pref_rec != NULL ;
		  antenna_pref_rec = (DB_RECORD **) 
				NEXT(antenna_pref_list, antenna_pref_list_ptr)   )
	{
			/* process the current antenna_pref_rec right here.  */
		(*new_antenna_list)[offset++] = 
		CAST_ANTENNA_PREF_ANTENNA_ID antenna_pref_rec[ANTENNA_PREF_ANTENNA_ID] ;
	 
	}

	/* terminate the list:  */
	(*new_antenna_list)[offset] = 0 ;

	DEL_LIST( antenna_pref_list ) ;
	return DTKM_CREATE_ANTENNA_LIST_OK ;

}


/*==============================================================================
Function:       dtkm_check_off_antenna()

Description:    searches an antenna list for the input value; then mulitplies
				it by -1 to indicate that it has been checked.  

Creator:        Lawrence Stevens

Creation Date:  Fri Nov  3 14:33:40 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_check_off_antenna( 
	int		antenna_id, 			/* input antenna_id   					*/
	int 	*antenna_list )			/* zero-terminated list of antenna_id's */
{

	int		j ;

	if ( antenna_id <= 0 )
		return DTKM_ERROR_ANTENNA_ID_LE_ZERO ;

	if ( antenna_list == NULL )
		return DTKM_ERROR_NULL_ANTENNA_LIST_PTR ;

	for ( j = 0 ; antenna_list[j] != 0 ; j ++ )
	{
		if ( antenna_list[j] == antenna_id )
		{
			/* found.  mark the antenna id and return.  */
			antenna_list[j] *= -1 ;
			return DTKM_CHECK_OFF_ANTENNA_OK ;
		}
	}

	return DTKM_ERROR_ANTENNA_NOT_FOUND_IN_LIST ;

}


/*==============================================================================
Function:       dtkm_select_next_untried_antenna()

Description:    given a list of integer antenna_id's, it selects the first 
				> 0 entry.  If none, it gives a zero value.  

Creator:        Lawrence Stevens

Creation Date:  Fri Nov  3 14:41:45 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_select_next_untried_antenna(
	int		*antenna_list,				/* read this list      */
	int		*next_untried_antenna )     /* return this member  */
{

	int		j ;

	if ( antenna_list == NULL )
		return DTKM_ERROR_NULL_ANTENNA_LIST_PTR ;

	/*
	-- loop through the list, pick up the first > 0 element.  
	*/
	for ( j = 0 ; antenna_list[j] != 0 ; j ++ )
		if ( antenna_list[j] > 0 )
		{
			*next_untried_antenna = antenna_list[j] ;
			return DTKM_SELECT_NEXT_UNTRIED_ANTENNA_OK ;
		}

	/*
	-- no > 0 elements found; return a zero value 
	-- for *next_untried_antenna
	*/
	*next_untried_antenna = 0 ;
	return DTKM_NO_MORE_UNTRIED_ANTENNAS ;

}

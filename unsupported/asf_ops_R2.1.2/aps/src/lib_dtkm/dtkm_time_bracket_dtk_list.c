#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_time_bracket_dtk_list.c

dtkm_time_bracket_dtk()
dtkm_time_bracket_dtk_list()

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_time_bracket_dtk_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_time_bracket_dtk_list.c"

#include "dtkm.h"
#include "dapps_list.h"
#include "dapps_defs.h"
#include "db_sybint.h"
#include "db_dtk.h"
#include "timeconv.h"

#include <string.h>		/* for strlen strcmp strcpy 		*/


/*==============================================================================
Function:       dtkm_time_bracket_dtk()

Description:    if the dtk_proposal EXPANDS the time bracket, update 
				the time bracket.  

Returns:        TRUE, < 0 if there is an error.  

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 11:00:36 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_time_bracket_dtk(
	DB_RECORD	**dtk_proposal,
	char		*strttime,
	char		*stoptime ) 
{

	/*
	-- do some superficial, fast, error checking
	*/
	if ( strlen(strttime) != ASF_TIME_STR_LENGTH )
		return DTKM_ERROR_NO_STRTTIME ;
	if ( strlen(stoptime) != ASF_TIME_STR_LENGTH )
		return DTKM_ERROR_NO_STOPTIME ;

	/*
	-- update the time bracket to include the dtk_proposal
	*/
	if (strcmp( strttime, CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) > 0 )
		strcpy( strttime, CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) ;
	if (strcmp( stoptime, CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) < 0 )
		strcpy( stoptime, CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) ;

	return TRUE ;

}

/*==============================================================================
Function:       dtkm_time_bracket_dtk_list()

Description:    if the dtk_list EXPANDS the time bracket, update 
				the time bracket.  

Returns:        TRUE, < 0 if there is an error.  

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 11:00:36 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_time_bracket_dtk_list(
	llist		*dtk_list,
	char		*strttime,   /* you must supply this value.  */
	char		*stoptime )  /* you must supply this value.  */
{
	DB_RECORD	**dtk_rec = NULL ;
	cursor		dtk_list_ptr ;
	int			return_code ;

	/*
	-- do some superficial, fast, error checking
	*/
	if ( dtk_list == NULL )
		return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

	if ( strlen( strttime ) != ASF_TIME_STR_LENGTH )
		return DTKM_ERROR_NO_STRTTIME ;

	if ( strlen( stoptime ) != ASF_TIME_STR_LENGTH )
		return DTKM_ERROR_NO_STOPTIME ;

	if ( NUMELTS( dtk_list ) == 0 )
	{
		/* this list cannot expand the time bracket.  no problem.  */
		return TRUE ;
	}

	/* 
	-- go through each element and update the time 
	-- bracket to include each data-take.  
	*/

	for (
		dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
		dtk_rec ;
		dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr) 
		)
	{
		return_code = dtkm_time_bracket_dtk( dtk_rec, strttime, stoptime ) ;
		if ( return_code < 0 )
			return return_code ;
	}

	return TRUE ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*
	#define PRINT_DIAG
*/

#include <string.h>       /* for strcmp, strncmp argument checks  */
#include <stdio.h>        /* for fprintf etc...                   */


/* INCLUDES  */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "dapps_list.h"     /* for APS linked list macros           */

/* FOR DATABASE TABLES        */
#include "db_dtk.h"          /* for dtk table             */
#include "db_phase.h"        /* for station table         */

#include "timeconv.h"
#include "phase_utilities.h"


/*==============================================================================
Function:       check_rev_asftimes

Description:    checks to see if the start and stop times are within the 
				rev.  

Parameters:     
int check_rev_asftimes ( 
	char		*sat,
	int			rev,
	char		*strttime, 
	char		*stoptime, 
	int         *rev_number_for_strttime,		output rev for strttime   
	char        *strttime_for_rev, 				output strttime for rev  
	char        *stoptime_for_rev  ) 			output stoptime for rev 

Returns:        
	> 0:  TRUE  if the strttime and stoptime are within the input rev.  
	= 0:  FALSE if the strttime and stoptime are not both within the input rev.
	< 0 : Some error occurred:
			PHASE_ASFTIME_NOT_WITHIN_ANY_PHASE
			PHASE_REV_NOT_WITHIN_ANY_PHASE
			other errors detected by called routines.  


Creator:        Lawrence Stevens

Creation Date:  Tue Jun 27 13:37:18 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)check_rev_asftimes.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.check_rev_asftimes.c"

int check_rev_asftimes ( 
	char		*sat,
	int			rev,
	char		*strttime, 
	char		*stoptime, 
	int         *rev_number_for_strttime,		/* output rev for strttime   */
	char        *strttime_for_rev, 				/* output strttime for rev   */
	char        *stoptime_for_rev  ) 			/* output stoptime for rev   */
{
	DB_RECORD		**phase_rec ;

	int		return_code ;

	/* 
	-- assign default values to output variables in case 
	-- of an early return:  
	*/
	*rev_number_for_strttime = 0 ;
	(void)strcpy(strttime_for_rev, "" ) ;
	(void)strcpy(stoptime_for_rev, "" ) ;

	/* 
	-- get info pertaining to the strttime:
	*/
	return_code = asftime_2_phase( sat, strttime, &phase_rec ) ;
	if ( return_code < 0 )
		return return_code ;

	if ( return_code != PHASE_INPUT_TIME_WITHIN_A_PHASE )
	{
		free_db_record(phase_rec) ;
		return PHASE_ASFTIME_NOT_WITHIN_ANY_PHASE ;
	}

	/*
	-- get the (output) rev number pertaining to
	-- the (input) strttime.  
	*/
	return_code = phase_asftime2rev(phase_rec, strttime, 
		rev_number_for_strttime ) ;
	if (return_code < 0 )
	{
		free_db_record(phase_rec) ;
		return return_code ;
	}

	/* 
	-- now get info pertaining to the input rev; first, 
	-- free the old phase rec:
	*/
	free_db_record(phase_rec) ;
	return_code = rev_2_phase( sat, rev, &phase_rec ) ;
	if ( return_code != PHASE_INPUT_REV_WITHIN_A_PHASE )
	{
		free_db_record(phase_rec) ;
		return PHASE_REV_NOT_WITHIN_ANY_PHASE ;
	}

	/*
	-- get the asf times pertaining to
	-- the rev.  
	*/
	return_code = phase_rev2asftime(phase_rec, rev,
		strttime_for_rev, stoptime_for_rev  ) ;

	/* 
	-- phase record is not needed after here.  
	-- postpone the return_code check while we free it:
	*/
	free_db_record(phase_rec) ;

	/*
	-- now check the return code from phase_rev2asftime()
	*/
	if ( return_code < 0 )
		return return_code ;

	/* test strttime    */
	/*
	-- note that start time can be at the start of a rev, but not 
	-- at the exact end of a rev.  
	*/
	if ( strcmp(strttime, strttime_for_rev ) < 0 
	||   strcmp(strttime, stoptime_for_rev ) >= 0 )
		return FALSE ;

	/* test stoptime    */
	/*
	-- note that stop time cannot be at the exact start of a rev, but can 
	-- be at the exact end of a rev.  
	*/
	if ( strcmp(stoptime, strttime_for_rev ) <= 0 
	||   strcmp(stoptime, stoptime_for_rev ) > 0 )
		return FALSE ;

	return TRUE ;

}

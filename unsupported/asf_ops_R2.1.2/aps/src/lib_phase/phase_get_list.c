#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       phase_get_list()

Description:    this routine retrieves all of the phase records for 
				a particular satellite, sorted by start time of phase.
				it then checks the list for time and rev overlaps, a type 
				of error which has created hard-to-solve problems that 
				have occurred in the past.  
				The routine returns the phase list and the status of the 
				check to the calling routine.
				It is mainly a "private" type of routine in that it is used 
				by utilitiy routines to help find the right phase record.  

Parameters:     
int phase_get_list(
	char            *sat,
	llist           **phase_list_output )  output address of linked list 

Returns:        
	>= 0   :  No error
		PHASE_GET_LIST_OK 

	< 0    :  Error codes:  
		PHASE_DB_QUERY_FAILED
		PHASE_NO_PHASE_RECS_FOUND      we expect at least one record.  
		PHASE_BAD_PHASE_REC_START_TIME
		PHASE_TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME
		PHASE_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS
		PHASE_TWO_PHASE_RECS_OVERLAP_IN_TIME

Creator:        Lawrence Stevens

Creation Date:  Fri Jun 30 14:39:48 PDT 1995

Notes:		
A SPECIAL NOTE:
This routine does some gratuitous checking for time overlaps between
phase records.  It takes up most of the code in the routine, but 
is here because of past problems when editing or changing phase db 
records when new phases are announced.  Sometimes, the changes are 
made, but there is a mistake which causes the time brackets for 2 phases 
to overlap.  This has caused hard-to-detect problems later on in the 
use of the APS.  Because these problems have been experienced in the 
past, we put these checks in here.  

==============================================================================*/
#pragma ident	"@(#)phase_get_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.phase_get_list.c"


/* FOR SYBASE INTERFACES */
#include "db_sybint.h"       /* for APS sybase interface routines    */
#include "aps_db_table.h"    /* for APS DB tables sybase interface   */
#include "dapps_list.h"      /* for APS linked list macros           */
#include "dapps_defs.h"      /* for MIN_REV, MAX_REV                 */

/* FOR PHASE UTILITIES   */
#include "phase_utilities.h"

/* FOR TIME CONVERSION   */
#include "timeconv.h"

int phase_get_list(       
	char			*sat,
	llist			**phase_list_output )  /* output address of linked list */
{

	int			return_code ;

	DB_RECORD	**phase_rec ;
	cursor		phase_list_ptr ;
	llist		*phase_list = NULL ;

	/* 
	-- used in checking:  
	-- previous phase values that must not ever match any 
	-- phase record values:  
	*/
	char	previous_phase_name = ' ' ;
	double	previous_et_phase_end = -1.0 ;
	int		previous_last_rev = -1 ;

	/* computed results from current phase record:  */
	int		first_rev ;
	double	et_phase_start ;
	double	et_phase_end ;
	double	days_per_rev ;

	(void)sprintf(where_clause, "where %s = '%s' ", 
		APS_COL(PHASE, PHASE_SAT), sat ) ;
	(void)sprintf(orderby_cols, "%s", 
		APS_COL(PHASE, PHASE_PHASE_START) ) ;

	phase_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
		APS_TABLE(PHASE),
		where_clause, orderby_cols, APS_CDEFS(PHASE), ALL_COLS) ;

	if (phase_list == NULL)
		return PHASE_DB_QUERY_FAILED ;

	if ( NUMELTS( phase_list ) == 0 )
	{
		DEL_LIST( phase_list ) ;
		return PHASE_NO_PHASE_RECS_FOUND ;
	}

	/* check each record in order retrieved.  */
	phase_rec = (DB_RECORD **) FIRST(phase_list, phase_list_ptr) ;
	do 
	{
		return_code = phase_first_rev(phase_rec, &first_rev ) ;
		if ( return_code < 0 )
		{
			DEL_LIST( phase_list ) ;
			return return_code ;
		}

		if (!tc_asf2et( CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], 
						&et_phase_start ) ) 
		{
			DEL_LIST( phase_list ) ;
			return PHASE_BAD_PHASE_REC_START_TIME ;
		}

		/* 
		-- compute nodal period (the float variable days_per_rev), 
		-- and end of phase.  this is for time overlap checking. 
		*/
		days_per_rev = 
			  (double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] 
			/ (double) CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

		et_phase_end = et_phase_start + 
			days_per_rev 
			* CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] ;

		/* check for 2 records with same phase name  */
		if ( CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME] 
		  == previous_phase_name )
		{
			DEL_LIST( phase_list ) ;
			return PHASE_TWO_PHASE_RECS_WITH_SAME_SAT_AND_NAME ;
		}

		/* check for rev number overlap.  */
		/*
		-- since the retrieve of the phase recs was in
		-- chronological order, these few checks are, surprisingly,
		-- sufficient:
		*/
		if ( first_rev <= previous_last_rev )
		{
			DEL_LIST( phase_list ) ;
			return PHASE_TWO_PHASE_RECS_OVERLAP_IN_REV_NUMBERS ;
		}

		/* check for time overlap.  */
		if ( et_phase_start <= previous_et_phase_end )
		{
			DEL_LIST( phase_list ) ;
			return PHASE_TWO_PHASE_RECS_OVERLAP_IN_TIME ;
		}

		/* overlap checking is done; now set the previous phase data */
		previous_phase_name 
			= CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME] ; 
		previous_et_phase_end = et_phase_end ;
		previous_last_rev = CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] ;

	}  while (  (phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr))
			 != NULL ) ;

	*phase_list_output = phase_list ;

	return PHASE_GET_LIST_OK ;

}

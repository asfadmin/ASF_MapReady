#undef PRINT_DIAG 

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dtkm_in_station_mask.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_in_station_mask.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_in_station_mask.c"

#include "dtkm.h"
#include "db_dtk.h"
#include "db_maskinout.h"
#include "timeconv.h"

#include <string.h>


/*==============================================================================
Function:	dtkm_in_station_mask

Description:	determines if a data-take takes place while the satellite is
within the indicated station mask.  

Parameters:		

	char 		*station_id         check to see if the data-take is within
									this mask station_id. 
	DB_RECORD	**proposed_dtk      data-take to be checked.  
	int 		secs,               criterion for minimum data-take.
	                                if the data-take is shorter than this, 
	                                or if the data-take is in the mask less
	                                time than this, the routine will report
	                                that the data-take is not in the mask - 
	                                the data-take is not approved as a real-
	                                time data-take.  

	char 		*reduced_strttime,  if the dtk isn't all within the mask,
	char 		*reduced_stoptime   reduced times are offered.  The reduced
	                                times indicate the times during the 
	                                proposed data-take that the satellite 
	                                is in the station_id mask and, are 
                                    given even when the entire data-take is in 
	                                the station_id mask.  

Returns:     	
	> 0 
		DTKM_SEGLOAD_DTK_HAS_TIME_IN_MASK   

	< 0
		DTKM_NO_MASKINOUT_RECS_FOUND   
					(possibly because there was no pass, i.e.,
		             the satellite did not enter the mask on that
		             rev.)
		DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK
		             an enter and exit event were not both found
		             for this satellite/rev.  Or more than these
		             records were found.  In any case, the 
		             the problem is solved by re-running 
		             coverage for that rev.  
		             This condition can arise when the planner 
		             inadvertantly runs create nominal coverage
		             with a time period that ends in the middle
		             of a pass, (when the satellite is in the
		             ASF mask) and the proposed data-take is 
		             in that rev.  

Creator:		Lawrence Stevens

Creation Date:	03/06/1995

Notes:		
==============================================================================*/

int dtkm_in_station_mask ( 
	char 		*station_id,         /* input station_id of mask             */
	DB_RECORD	**proposed_dtk,      /* input proposed dtk                   */
	int 		secs,                /* input min criterion for dtk length.  */
	char 		*reduced_strttime,   /* output reduced data-take             */
	char 		*reduced_stoptime )  /* time bracket fit in mask             */
{

	DB_RECORD	**maskinout_rec ;
	llist		*maskinout_list = NULL ;
	cursor		maskinout_list_ptr ;

	int 		in_rec_count = 0 ;

	int			return_code;

	/* these double precision numbers are for ephemeris times in days. */
	double 		et_inmask ; 	/* time of mask entry                  */
	double		et_outmask  ; 	/* time of mask exit                   */
	double		et_dtk_start ; 	/* max time of dtk start and mask entry */
	double		et_dtk_stop ; 	/* min time of dtk end and mask exit    */
	double		et_overlap ;    /* time overlap of data-take and mask   */
	double		et_dtk_strttime ; /* time of dtk strttime     */
	double		et_dtk_stoptime ; /* time of dtk stoptime     */
	double		xsecs ;

	if ( proposed_dtk == NULL ) 
		return DTKM_ERROR_NULL_DTK_PROPOSAL ;

	strcpy(reduced_strttime, "" ) ;
	strcpy(reduced_stoptime, "" ) ;

	sprintf(where_clause, "where %s = '%s' and %s = '%s' and %s = %ld",
		APS_COL(MASKINOUT, MASKINOUT_STATIONID), station_id,
		APS_COL(MASKINOUT, MASKINOUT_SAT),  CAST_DTK_SAT proposed_dtk[DTK_SAT], 
		APS_COL(MASKINOUT, MASKINOUT_REV),  CAST_DTK_REV proposed_dtk[DTK_REV]);

	maskinout_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
		APS_TABLE(MASKINOUT), 
		where_clause, NULL, APS_CDEFS(MASKINOUT), ALL_COLS ) ;

	if ( maskinout_list == NULL ) 
		return DTKM_ERROR_DB_QUERY_FAILED ;

	if ( NUMELTS( maskinout_list ) == 0 )
	{
		DEL_LIST( maskinout_list ) ;
		return DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV ;
	}

	if ( NUMELTS( maskinout_list ) != 2 )
	{
		DEL_LIST( maskinout_list ) ;
		return DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK ;
	}

#ifdef PRINT_DIAG
	printf("%s(%d):  OK 2 maskinout recs were found.\n", __FILE__, __LINE__ ) ;
#endif

	for (
		maskinout_rec = 
			(DB_RECORD **) FIRST(maskinout_list, maskinout_list_ptr) ; 
		maskinout_rec ; 
		maskinout_rec = (DB_RECORD **)NEXT(maskinout_list, maskinout_list_ptr)
		)
	{

#ifdef PRINT_DIAG
		tc_et2asf( CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE], 
				asftime);
		printf("%s(%d).c:  t = %f, inout = %s\n", __FILE__, __LINE__, 
			CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE],
			CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT]);
		printf("                asftime = %s\n", asftime ) ;
#endif

		if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT], 
		"IN", 2 ) == 0 )
		{
			in_rec_count ++ ;
			et_inmask = CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] ;
		}
		else if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT],
		"OUT", 3 ) == 0 )
		{
			et_outmask = CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] ;
		}
	}

	/* 
	-- we are now done with the maskinout list.  
	-- we use et_inmask, et_outmask for the times from the records.  
	*/
	DEL_LIST( maskinout_list ) ;

	/* 
	-- we should get ONE in and ONE out 
	-- record from the relation.  
	-- because of the above code.  
	*/
	if ( in_rec_count != 1 ) 
	{
		/* 
		-- ERROR in maskinout relation; 1 IN and 1 OUT rec expected 
		*/
		return DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK ;
	}

	/* O.K.  we have both records as expected.  */

	/* now compute the overlap between the sat pass and the data-take */

	/* 
	-- get the later start time (et_dtk_start) between start of pass 		
	-- and start of data-take:  
	*/

	/* dtk start time in ephemeris time  */
	return_code = tc_asf2et(CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME], 
		&et_dtk_strttime ) ;
	if ( !return_code )
		return DTKM_ERROR_BAD_STRTTIME_VALUE ;

	et_dtk_start = MAX( et_inmask, et_dtk_strttime ) ;

	/* 
	-- get the earlier start time (et_dtk_stop) between end of pass 		
	-- and end of data-take:  
	*/

	/* dtk stop time in ephemeris time  */
	return_code = tc_asf2et(CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME], 
		&et_dtk_stoptime ) ;
	if ( !return_code )
		return DTKM_ERROR_BAD_STOPTIME_VALUE ;

	et_dtk_stop = MIN( et_outmask, et_dtk_stoptime ) ;

	/* subtract to get the overlap, if any, in days:  */
	et_overlap = et_dtk_stop - et_dtk_start ;

	/* change to seconds units:  */
	et_overlap = et_overlap * 24.00 * 3600.00 ;

	/* 
	-- if there is secs [secs is an input criterion] seconds or 
	-- more of overlap time between the station_id mask pass and 
	-- the data-take, then it is a station_id data-take.  
	*/

	/* use the criterion:  */
	xsecs = secs ;

	if ( et_overlap > xsecs ) 
	{
		/* 
		-- set up the reduced times - the start and stop 
		-- time of the dtk - pass overlap. 
		*/
		tc_et2asf(et_dtk_start, reduced_strttime);
		tc_et2asf(et_dtk_stop, reduced_stoptime);
		return DTKM_DTK_HAS_TIME_IN_MASK ;
	}
	return DTKM_DTK_HAS_NO_TIME_IN_MASK ;
}

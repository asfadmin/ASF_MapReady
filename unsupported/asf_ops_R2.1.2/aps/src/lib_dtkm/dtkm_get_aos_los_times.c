#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif
#ifdef THIS_IS_OBSOLETE_CODE
/*==============================================================================
Filename:	dtkm_get_aos_los_times.c

Notes:
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)dtkm_get_aos_los_times.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_aos_los_times.c"



/*==============================================================================
Function:       dtkm_get_aos_los_times()

Description:    given a dtk DB_RECORD, UPDATE the dtk.aos_time and 
				the dtk.los_time fields for the indicated station 
				mask from the maskinout relation.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  9 19:59:42 PST 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include "db_maskinout.h"
#include "timeconv.h"   /* for tc_et2asf()   */
#include "string.h"   	/* for strncmp()   */

int dtkm_get_aos_los_times(
	DB_RECORD	**input_dtk,
	DB_RECORD	**result_dtk )
{
	int			in_count ;
	int			return_code ;

	llist		*maskinout_list = NULL ;
	cursor		maskinout_list_ptr ;
	DB_RECORD	**maskinout_rec ;


	if ( input_dtk == NULL )
		return DTKM_ERROR_NULL_DTK_PROPOSAL ;
	if ( result_dtk == NULL )
		return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

	/* initialize result record with input values.  */
	db_copy_record( APS_CDEFS(DTK), result_dtk, input_dtk ) ;

	/*
	-- if this data-take is a recording, 
	-- there may never be any AOS-LOS in this 
	-- rev.  Therefore, just copy the dtk times.  
	*/
	if ( !dtkm_is_a_downlink( result_dtk ) )
	{
		/* a recording.  */
		strcpy( CAST_DTK_AOS_TIME result_dtk[DTK_AOS_TIME], 
			CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) ;
		strcpy( CAST_DTK_LOS_TIME result_dtk[DTK_LOS_TIME], 
			CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) ;
		return TRUE ;
	}

	return_code = dtkm_aps_does_cvrg4sat( result_dtk ) ;
	if ( return_code < 0 )
		return return_code ;

	if ( return_code != TRUE )
	{
		/* 
		-- the APS does NOT do coverage at all.  
		-- just copy in the dtk times to the field.  
		*/
		strcpy( CAST_DTK_AOS_TIME result_dtk[DTK_AOS_TIME], 
				CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) ;

		strcpy( CAST_DTK_LOS_TIME result_dtk[DTK_LOS_TIME],
				CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) ;
		return TRUE ;
	}

	/* 
	-- retrieve the in and out mask records for the 
	-- dtk sat, rev, and station_id
	*/
    sprintf(where_clause, "where %s = '%s' and %s = '%s' and %s = %ld",
        APS_COL(MASKINOUT, MASKINOUT_STATIONID), 
			CAST_DTK_STATION_ID input_dtk[DTK_STATION_ID],
        APS_COL(MASKINOUT, MASKINOUT_SAT),  
			CAST_DTK_SAT input_dtk[DTK_SAT],
        APS_COL(MASKINOUT, MASKINOUT_REV),  
			CAST_DTK_REV input_dtk[DTK_REV] ) ;
 
    maskinout_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(MASKINOUT),
        where_clause, NULL, APS_CDEFS(MASKINOUT), ALL_COLS ) ;

	if ( maskinout_list == NULL )
		return DTKM_ERROR_DB_QUERY_FAILED ;

	if ( NUMELTS( maskinout_list ) <= 0 )
	{
		DEL_LIST( maskinout_list ) ;
		return DTKM_ERROR_NO_MASKINOUT_RECS_FOR_DOWNLINK_MUST_RUN_COVERAGE ;
	}

	if ( NUMELTS( maskinout_list ) != 2 )
	{
		DEL_LIST( maskinout_list ) ;
		return DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_CNOMCOV_FOR_DTK ;
	}

	/*
	-- retrieved 2 records; should be one in event 
	-- and one out event.  
	-- the "in" event is satellite entering mask; 
	-- the "out" event is satellite exiting mask. 
	-- the in event is the acquisition of signal, or AOS.  
	-- the in event is the loss of signal, or LOS.  
	*/
	in_count = 0 ;
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
			/* 
			-- mask in event ==> Acquisition Of Signal.
			-- maskinout_rec time is in ephemeris time (et) 
			-- convert to ASF time:
			*/
			in_count++ ;
			if (!tc_et2asf(
				CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE], 
				CAST_DTK_AOS_TIME result_dtk[DTK_AOS_TIME] ) ) 
			{
				DEL_LIST( maskinout_list ) ;
				return DTKM_ERROR_CONVERTING_ET2ASF ;
			}
        }
        else if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT],
        					"OUT", 3 ) == 0 )
        {
			/* 
			-- mask out event ==> Loss Of Signal.  
			-- maskinout_rec time is in ephemeris time (et) 
			-- convert to ASF time:
			*/
			if (!tc_et2asf(
				CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE], 
				CAST_DTK_LOS_TIME result_dtk[DTK_LOS_TIME] ) ) 
			{
				DEL_LIST( maskinout_list ) ;
				return DTKM_ERROR_CONVERTING_ET2ASF ;
			}
        }
		else
		{
			DEL_LIST( maskinout_list ) ;
			return DTKM_ERROR_REC_NEITHER_IN_NOR_OUT_IN_MASKINOUT ;
		}

    }
	DEL_LIST( maskinout_list ) ;

	if ( in_count != 1 )
		return DTKM_ERROR_NE_1_INMASK_EVENT_IN_REV ;

	/*
	-- last check before we go.  the aos/los must bracket the 
	-- start and stop times for SCH and PLN downlink data-takes.  
	-- therefore, if necessary, expand the aos/los to include 
	-- the data-take.  this can happen due to the aos/los being 
	-- nominal but the times may be actual, from the flight 
	-- agency plan or schedule.  
	*/

	if ( dtkm_is_a_downlink( result_dtk ) )
		if ( strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH" ) == 0 
		||   strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN" ) == 0 )
		{
			/*
			-- a downlink dtk that is either SCH or PLN:  
			-- so we expand the aos-los if necessary, to include the 
			-- dtk times.
			*/
			if ( strcmp(CAST_DTK_AOS_TIME result_dtk[DTK_AOS_TIME],
						CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) > 0 )
				strcpy( CAST_DTK_AOS_TIME result_dtk[DTK_AOS_TIME],
						CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) ;

			if ( strcmp(CAST_DTK_LOS_TIME result_dtk[DTK_LOS_TIME],
						CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) > 0 )
				strcpy( CAST_DTK_LOS_TIME result_dtk[DTK_LOS_TIME],
						CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) ;
		}

	return TRUE ;

}
#endif

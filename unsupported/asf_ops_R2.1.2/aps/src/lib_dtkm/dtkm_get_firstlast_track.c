#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_get_firstlast_track.c

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_get_firstlast_track.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_firstlast_track.c"


/*==============================================================================
Function:       dtkm_get_firstlast_track()

Description:    get the track start of the 1st rec and 
                track end of last rec from the input list.  

Creator:        Miguel Siu

Creation Date:  Thu Jul 18 08:59:22 PDT 1996

Notes:      
    We will be using routines gen_rev2trackstart() and
    gen_rev2trackend().  These routines make use of global variables
    fa_sat_id, fa_station_id, fa_antenna_id.

    The variables fa_sat_id and fa_antenna_id may be different for various
    entries in the input dtk_list, so we have to initialize them immediately
    before the gen_rev2trackstart() and gen_rev2trackend() routines.

    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include <timeconv.h> 	/* for tc_odl2asf */
#include "dtkm.h"
#include "db_dtk.h"
#include "GENconversions.h" /* for globals fa_sat_id, fa_antenna_id, fa_station_id */

int dtkm_get_firstlast_track(
    llist   *dtk_list,
    char    *first_track_start,
    char    *last_track_end )
{
    DB_RECORD       **dtk_rec = NULL ;
    cursor          dtk_list_ptr ;
    int             status1, status2;

    /* quick error checking.  */
    if ( dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    if ( NUMELTS( dtk_list ) <= 0 )
        return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

    /* 
	-- use routine get_aos_los_start_stop(); first set up globals 
	-- (routine gen_rev2trackstart is still called within the new routine)
	-- Note that the resulting time is in ODL format, we change to ASF format
	-- in order to keep both return arguments in the same ASF format.
	*/
    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
    fa_antenna_id = CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;
    strcpy( fa_sat_id, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
    fa_dtk_report_list = dtk_list ;

	status1 = get_aos_los_start_stop(NULL, &dtk_rec, first_track_start) ;
	tc_odl2asf(first_track_start, first_track_start) ; 

    /* 
	-- use routine get_aos_los_start_stop to get last_track_end.
	-- (routine gen_rev2trackend is still called within the new routine)
	-- The last_track_end value is placed in the fa_los_track_end
	-- global variable, from where it must be retrieved.
	*/
    dtk_rec = (DB_RECORD **) LAST(dtk_list, dtk_list_ptr);
    fa_antenna_id = CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;
    strcpy( fa_sat_id, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;

	status2 = get_aos_los_start_stop(NULL, &dtk_rec, last_track_end) ;
	strcpy (last_track_end, fa_los_track_end) ;

    if (status1 && status2)
        return TRUE ;
    else
        return FALSE ;
}

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_update_obs_times_from_rdl.c

Description:    source file.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_obs_times_from_rdl.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_obs_times_from_rdl.c"


/*==============================================================================
Function:       dtkm_update_obs_times_from_rdl()

Description:    for the input Realtime DownLink data-take, find 
                its real-time observation data-takes, and adjust 
                their times so that the earliest strttime in the 
                group is the same as the downlink strttime and 
                the latest stoptime in the group is the same 
                as the downlink stoptime.  
                Also, make sure that none of the times are outside 
                of the downlink times.  

                NOTE:  don't do this if the sensor is a low_bit_rate 
                       sensor.  Use dtkm_sat_sensor_is_low_bit_rate()

Creator:        Lawrence Stevens

Creation Date:  Wed Mar 12 16:02:03 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>

int dtkm_update_obs_times_from_rdl(
    DBPROCESS       *APS_dbproc,    /* Sybase db process info struct          */
    DB_RECORD       **dl_dtk_rec,   /* input downlink data-take record        */
    llist           *dtk_updates )  /* append any changed dtks to this list.  */
{

    llist       *obs_dtk_list ;
    DB_RECORD   **obs_dtk_rec ;
    cursor      obs_dtk_list_ptr ;

    int         return_code ;
    int         obs_update_flag = 0 ;

    /* quick error checking.  */
 
    if ( dl_dtk_rec == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;
 
    if ( !(dtkm_is_a_realtime_downlink( dl_dtk_rec ) == TRUE)   )
        return DTKM_ERROR_INPUT_DTK_IS_NOT_RDL ;

    /* initialize linked list to obtain data-takes.  */
    obs_dtk_list = create_dyn_llist() ;
 
    /*
    -- find the corresponding observation data-takes
    -- for this RDL (realtime downlink):
    */
    return_code = dtkm_dl2obs( dl_dtk_rec, obs_dtk_list ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( obs_dtk_list ) ;
        return return_code ;
    }
 
    if ( NUMELTS( obs_dtk_list ) == 0 )
    {
        /* no observations found; this result is OK.  */
        DEL_LIST( obs_dtk_list ) ;
        return TRUE ;
    }

    /* 
    -- now, if necessary, trim each dtk.  Here are examples 
    -- of how to trim them.  In the case where the observation 
    -- is completely outside the downlink, the times were just 
    -- adjusted to match.  It did get complicated considering 
    -- whether or not to delete the observation, which would 
    -- be wrong if there were other observations inside the 
    -- data-take etc. etc.  Then we would be comparing the observation 
    -- with other observations.  
    --
    -- With this implementation, we compare the observation only with 
    -- the downlink.  
    --
    -- If part of the observation is inside the data-take, 
    -- then the strttime and/or stoptimes are trimmed if necessary 
    -- so that all of the data-take is inside the downlink times.  
    --
    --   Here is a picture of a realtime dtk time bracket with 
    --   the observation times both before and after trimming.  
    --
    --
    --    NOTE:  the dtk proposal, in these cases, is always a 
    --           realtime downlink.  Otherwise, this routine is 
    --           not called.  
    --
    --   TIME  --->
    --
    --    Downlink dtk proposal:           |------|
    --  1 Observation before:        |-|
    --    Observation after:         DEL   
    -- 
    --    Downlink dtk proposal:        |------|
    --  2 Observation before:     |-----|
    --    Observation after:        DEL
    -- 
    --    Downlink dtk proposal:        |------|
    --  3 Observation before:      |------|
    --    Observation after:            |-|       (trim strttime)
    -- 
    --    Downlink dtk proposal:     |------|
    --  4 Observation before:        |--|
    --    Observation after:         |--|  (don't trim if inside dl)
    -- 
    --    Downlink dtk proposal:     |------|
    --  5 Observation before:         |--|
    --    Observation after:          |--| (don't trim if inside dl)
    -- 
    --    Downlink dtk proposal:     |------|
    --  6 Observation before:            |--|
    --    Observation after:             |--| (don't trim if inside dl)
    -- 
    --    Downlink dtk proposal:     |------|
    --  7 Observation before:            |------|
    --    Observation after:             |--| (trim stoptime)
    -- 
    --    Downlink dtk proposal:     |------|
    --  8 Observation before:               |----|
    --    Observation after:                 DEL
    -- 
    --    Downlink dtk proposal:     |------|
    --  9 Observation before:                 |--|
    --    Observation after:                  DEL
    -- 
    --    Downlink dtk proposal:     |------|
    -- 10 Observation before:      |-------------|
    --    Observation after:         |------| (trim both strttime and stoptime)
    -- 
    */

    /*
    -- Now go through the list and DEL or trim the 
    -- observation data-takes (if needed) so that they fit within the 
    -- times of the realtime downlink record.  
    -- We need the group to reflect the start and 
    -- end times of the downlink, since the observation 
    -- and downlinking is taking place at the same time.  
    */
    for (   obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, obs_dtk_list_ptr);
            obs_dtk_rec != NULL ;
            obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, obs_dtk_list_ptr)  
        )
    {
        /* process the current obs_dtk_rec right here.  */
        /* 
        -- check if a low_bit_rate sensor, which allows 
        -- time compression of downlinked data.  
        */
        return_code = dtkm_sat_sensor_is_low_bit_rate( 
                        CAST_DTK_SAT obs_dtk_rec[DTK_SAT],
                        CAST_DTK_SENSOR obs_dtk_rec[DTK_SENSOR] ) ;
        if( return_code < 0 )
            return return_code ;
        if( return_code == TRUE )
            continue ;

        /* 
        -- reset the update flag which will indicate 
        -- a change to the data-take if != 0  
        */
        obs_update_flag = 0 ;

        /* 
        -- check obs_dtk_rec to see if it does not overlap the 
        -- downlink data-take times in dl_dtk_rec.  
        -- if there is no overlap, then change the status 
        -- of the observation to DEL.  
        */
        if( strcmp( CAST_DTK_STRTTIME obs_dtk_rec[DTK_STRTTIME],
                    CAST_DTK_STOPTIME dl_dtk_rec[DTK_STOPTIME] ) >= 0 
        ||  strcmp( CAST_DTK_STOPTIME obs_dtk_rec[DTK_STOPTIME],
                    CAST_DTK_STRTTIME dl_dtk_rec[DTK_STRTTIME] ) <= 0 )
        {
            /* 
            -- no time overlap between DL and OBS.  
            --
            -- we have either the obs end before the dl start, 
            -- or else the dl end before the obs start.  
            -- either way, we need to update the data-take 
            -- to DEL.  This will result in dis-connecting it 
            -- to the downlink - deleting the dl2obs record.  
            */
            obs_update_flag ++ ;
            strcpy( CAST_DTK_DTKSTAT obs_dtk_rec[DTK_DTKSTAT], "DEL" ) ;
        }
        else 
        {
            /* 
            -- There is some overlap between DL and OBS.
            -- Preserve the observation, but trim 
            -- its start and/or start times if necessary.  
            */
            if( strcmp( CAST_DTK_STRTTIME obs_dtk_rec[DTK_STRTTIME],
                        CAST_DTK_STRTTIME dl_dtk_rec[DTK_STRTTIME] ) < 0 )
            {
                /* trim the dtk at the start to match:  */
                obs_update_flag ++ ;
                strcpy( CAST_DTK_STRTTIME obs_dtk_rec[DTK_STRTTIME],
                        CAST_DTK_STRTTIME  dl_dtk_rec[DTK_STRTTIME] ) ;
            }
            if( strcmp( CAST_DTK_STOPTIME obs_dtk_rec[DTK_STOPTIME],
                        CAST_DTK_STOPTIME  dl_dtk_rec[DTK_STOPTIME] ) > 0 )
            {
                /* trim the dtk at the end to match:  */
                obs_update_flag ++ ;
                strcpy( CAST_DTK_STOPTIME obs_dtk_rec[DTK_STOPTIME],
                        CAST_DTK_STOPTIME  dl_dtk_rec[DTK_STOPTIME] ) ;
            }
        }

        if( obs_update_flag )
        {
            /* 
            -- must update the observation record 
            -- in the database to reflect the above change(s).  
            */
            return_code = dtkm_update_dtk_record( APS_dbproc, 
                obs_dtk_rec, obs_dtk_rec, dtk_updates ) ;
        }

    }   /* end loop for each rec in obs_dtk_list    */

    /* done.  now clean up.  */
    DEL_LIST( obs_dtk_list ) ;

    return TRUE ;

}

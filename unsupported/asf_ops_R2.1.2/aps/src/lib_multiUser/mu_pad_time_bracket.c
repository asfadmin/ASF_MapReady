#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_pad_time_bracket.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_pad_time_bracket.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_pad_time_bracket.c"


/*==============================================================================
Function:       mu_pad_time_bracket()

Description:    pad the input planning activity time bracket to 
                create the "enlarged time bracket", as per 
                SSD Section 3.1.3.1.1.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  7 21:28:38 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

        From the SSD:                                              
        3.1.3.1.1 Enlarged Time Bracket                            
                                                                   
        Due to the possibility of a data-take bumping another      
        data-take from an antenna, the effects of a planning       
        activity can extend beyond the time bracket of the         
        original data-take(s) being changed.  For that reason,     
        to create the strttime and stoptime in the planning        
        activities table, the original time bracket needs to       
        be padded by an appropriate amount of time.                
        ...                                                        
                                                                   
        The amount of time to pad is the smaller of these two      
        time durations:                                            
                                                                   
        1.  the time of one satellite orbit (about 100             
            minutes) minus the maximum time in the station mask    
            for that satellite (about 15 minutes):  we use 90      
            minutes for safety.                                    
                                                                   
        2.  the amount of time that n-1 satellites can spend,      
            sequentially, in the station mask, or just over        
            (n-1)*15 minutes, where n = the number of satellites,  
                                                                   
        The number of satellites is best determined by counting    
        the number of unique values in the sat field in the        
        phase relation in the APSDB; the phase relation is         
        smaller than other relations that could be used, such      
        as the satsensor relation.                                 
                                                                   
        The "enlarged time bracket" is the activity time bracket   
        padded with this time.                                     

==============================================================================*/
#include "mu.h"
#include "db_phase.h"
#include "timeconv.h"   /* for tc_time_pad() etc.  */

int mu_pad_time_bracket( 
    /* NOTE:  these times are ASF-format times.   */
    char    *strttime,         /*  input planning activity start time         */
    char    *stoptime,         /*  input planning activity stop time          */
    char    *padded_strttime,  /*  output start time of enlarged time bracket */
    char    *padded_stoptime  )/*  output end time of enlarged time bracket   */
    /* NOTE:  these times are ASF-format times.   */
{
    int         return_code ;
    int         n_sats ;            /* the number of satellites   */

    /**************************************************************
    *                                                             *
    *   The amount of time to pad is the smaller of these two     *
    *   time durations:                                           *
    *                                                             *
    *   1.  the time of one satellite orbit (about 100            *
    *       minutes) minus the maximum time in the station mask   *
    *       for that satellite (about 15 minutes):  we use 90     *
    *       minutes for safety.                                   *
    *                                                             *
    **************************************************************/
    int     time1_minutes = 90 ;
    int     time2_minutes = 10000 ;
    int     pad_minutes ;

    /* brief error checking.  */
    /*
    -- validate strttime, stoptime
    */
    if( strttime == NULL )
        return MU_ERROR_INPUT_STRTTIME_IS_NULL_FOR_PLANNING_ACTIVITY ;
    return_code = tc_validate_asf_datetime(strttime) ;
    if( return_code != TRUE )
        return MU_ERROR_INPUT_STRTTIME_IS_NOT_A_VALID_ASFTIME ;
    if( stoptime == NULL )
        return MU_ERROR_INPUT_STOPTIME_IS_NULL_FOR_PLANNING_ACTIVITY ;
    return_code = tc_validate_asf_datetime(stoptime) ;
    if( return_code != TRUE )
        return MU_ERROR_INPUT_STOPTIME_IS_NOT_A_VALID_ASFTIME ;
    if( strcmp(strttime,stoptime) >= 0 )
        return MU_ERROR_INPUT_STRTTIME_IS_NOT_BEFORE_STOPTIME ;

    if( padded_strttime == NULL )
        return MU_ERROR_PADDED_STRTTIME_IS_NULL_FOR_PLANNING_ACTIVITY ;
    if( padded_stoptime == NULL )
        return MU_ERROR_PADDED_STOPTIME_IS_NULL_FOR_PLANNING_ACTIVITY ;

    /**************************************************************
    *                                                             *
    *   The amount of time to pad is the smaller of these two     *
    *   time durations:                                           *
    *                                                             *
    *   1.  the time of one satellite orbit (about 100            *
    *       minutes) minus the maximum time in the station mask   *
    *       for that satellite (about 15 minutes):  we use 90     *
    *       minutes for safety.                                   *
    *                                                             *
    *   2.  the amount of time that n-1 satellites can spend,     *
    *       sequentially, in the station mask, or just over       *
    *       (n-1)*15 minutes, where n = the number of satellites, *
    *                                                             *
    *   The number of satellites is best determined by counting   *
    *   the number of unique values in the sat field in the       *
    *   phase relation in the APSDB; the phase relation is        *
    *   smaller than other relations that could be used, such     *
    *   as the satsensor relation.                                *
    *                                                             *
    *   The "enlarged time bracket" is the activity time bracket  *
    *   padded with this time.                                    *
    **************************************************************/

    /* time1_minutes was already initialized above.  */
  
    /* 
    -- count the number of different satellites in 
    -- the phase relation:  
    */
    n_sats = db_count_distinct_vals( DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(PHASE), PHASE_SAT, APS_CDEFS(PHASE), NULL ) ;

    time2_minutes = (n_sats - 1)*15 ;

    pad_minutes = time1_minutes ;
    if( time2_minutes < time1_minutes ) 
        pad_minutes = time2_minutes ;

    return_code = tc_time_pad( strttime, stoptime, (float) pad_minutes, 
        padded_strttime, padded_stoptime ) ;
    if( return_code != TRUE )
        return MU_ERROR_FROM_FUNCTION_tc_time_pad ;

    return TRUE ;

}

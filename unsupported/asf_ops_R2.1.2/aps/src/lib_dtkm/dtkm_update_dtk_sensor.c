#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_update_dtk_sensor.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dtk_sensor.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dtk_sensor.c"


/*==============================================================================
Function:       dtkm_update_dtk_sensor()

Description:    changes the value of a sensor in a dtk record.  

                With sat/rev/dtkid the new unique identifier, 
                the sensor value can now be changed.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  2 23:14:40 PST 1995

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include <string.h>     /* for strlen strcpy   */

int dtkm_update_dtk_sensor(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **input_dtk,
    char        *sensor,
    DB_RECORD   **result_dtk,
    llist       *dtk_updates ) 
{
    int     return_code ;

    /* quick, brief error checking */
    if ( input_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( (int) strlen( sensor ) < 3 )
        return DTKM_ERROR_BAD_SENSOR_VALUE ; 

    if ( result_dtk  == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    /*
    -- we first change the sensor value in the DB_RECORD 
    -- and then update the dtk record in the DB.  
    */
    return_code = db_copy_record( APS_CDEFS(DTK), result_dtk, input_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    strcpy( CAST_DTK_SENSOR result_dtk[DTK_SENSOR], sensor ) ;
    return_code = dtkm_update_dtk_record( APS_dbproc, result_dtk, result_dtk,
        dtk_updates ) ;

    if ( return_code < 0 )
        return return_code ;

    return TRUE ;

}

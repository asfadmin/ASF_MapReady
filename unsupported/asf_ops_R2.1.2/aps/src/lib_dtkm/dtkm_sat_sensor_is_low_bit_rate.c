#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_sat_sensor_is_low_bit_rate.c

Notes:          

==============================================================================*/
#pragma ident   "@(#)dtkm_sat_sensor_is_low_bit_rate.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_sat_sensor_is_low_bit_rate.c"

#include "dtkm.h"
#include <string.h>
#include <db_sybint.h>
#include <db_satsensor.h>


/*==============================================================================
Function:       dtkm_sat_sensor_is_low_bit_rate()

Description:    checks the satsensor relation to see if the sensor 
                has a low_bit_rate flag = Y.  
                if so, returns TRUE.  If N, then returns FALSE
                if bad input, it returns a code < 0.  

Creator:        Lawrence Stevens

Creation Date:  Wed Mar 12 18:11:25 PST 1997

Notes:          
==============================================================================*/

/* data cache for the small relation, also a previous call cache.  */
static llist *satsensor_list = NULL ;
static char *previous_sat = NULL ;
static char *previous_sensor = NULL ;
static char previous_low_bit_rate_flag = '0' ;

int dtkm_sat_sensor_is_low_bit_rate( char *sat, char *sensor )
{
    DB_RECORD   **satsensor_rec ;
    cursor      *satsensor_list_csr ;
    int         search_list_flag ;

    if( satsensor_list == NULL )
    {
        satsensor_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(SATSENSOR), NULL, NULL, APS_CDEFS(SATSENSOR), 
            ALL_COLS) ;
        if( satsensor_list == NULL )
            return DTKM_ERROR_DB_QUERY_FAILED ;
        if( NUMELTS( satsensor_list ) == 0 )
            return DTKM_ERROR_NO_SATSENSOR_RECORDS_FOUND ;
    }

    search_list_flag = 0 ;

    if( previous_sat == NULL )
        search_list_flag = 1 ;
    else
    {
        /* there has been a previous call.  */
        if( strcmp(previous_sat, sat) != 0 
        ||  strcmp(previous_sensor, sensor) != 0 )
        {
            /* the previous call was not the same sat/sensor.  */
            search_list_flag = 1 ;
        }
    }

    if( search_list_flag )
    {

        /* reset and search again:  */
        previous_sat = NULL ;
        previous_sensor = NULL ;
        previous_low_bit_rate_flag = '0' ;

        for ( satsensor_rec = (DB_RECORD **) FIRST(satsensor_list, 
                                                    satsensor_list_csr);
              satsensor_rec != NULL ;
              satsensor_rec = (DB_RECORD **) NEXT(satsensor_list, 
                                                    satsensor_list_csr)  
            )
        {
            /* process the current satsensor_rec right here.  */
            if( strcmp( CAST_SATSENSOR_SAT satsensor_rec[SATSENSOR_SAT], sat ) )
            {
                continue ;   /* not ==     */
            }

            if( strcmp( CAST_SATSENSOR_SENSOR satsensor_rec[SATSENSOR_SENSOR], 
                sensor ) )
            {
                continue ;   /* not ==     */
            }

            /* OK.  sat and sensor are found.  */
            previous_sensor = 
                CAST_SATSENSOR_SENSOR satsensor_rec[SATSENSOR_SENSOR] ;
            previous_low_bit_rate_flag = 
                CAST_SATSENSOR_LOW_BIT_RATE_FLAG 
                satsensor_rec[SATSENSOR_LOW_BIT_RATE_FLAG] ;
            previous_sat = CAST_SATSENSOR_SAT satsensor_rec[SATSENSOR_SAT] ;
        }
        if( previous_sat == NULL )
        {
            /* 
            -- the current sat/sensor values were 
            -- not found in the db search.  
            */
            return DTKM_ERROR_BAD_SAT_SENSOR_VALUES ;
        }
    } 

    /* we have found our input sat/sensor.  */
    if( previous_low_bit_rate_flag == 'Y' )
        return TRUE ;
    else
        return FALSE ;

}

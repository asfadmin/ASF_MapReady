#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_delete_dtk.c

Description:    delete a dtk from the database.  and other supporting 
                stuff if needed.  

==============================================================================*/
#pragma ident   "@(#)dtkm_delete_dtk.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_delete_dtk.c"


/*==============================================================================
Function:       dtkm_delete_dtk()

Description:    delete a data-take from the APSDB.  And also updated 
                any other info needed, too.  

Creator:        Lawrence Stevens

Creation Date:  Thu Apr 24 15:59:43 PDT 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include <string.h>       /* for strlen() etc.                       */
#include <db_sybint.h>    /* for DB_RECORD                           */
#include <db_dtk.h>       /* for CAST_DTK_SAT etc.                   */
#include <aps_db_table.h> /* for DTK                                 */
#include "dtkm.h"         /* for dtkm_delete_dtk_record()            */

int dtkm_delete_dtk(            /* returns no. of dtks deleted (1)       */
    DBPROCESS   *APS_dbproc,    /* Sybase process id.                    */
                                /* identify dtk by sat/sensor/rev/dtkid: */
    char        *sat,           /* input satellite:  E1, E2, R1, etc.    */
    char        *sensor,        /* input sensor:  SAR, ST1, RDL, DMP...  */
    int         rev,            /* input rev number                      */
    int         dtkid )         /* input dtkid                           */
{

    int         return_code ;
    DB_RECORD   **delete_dtk_rec ;

    /* 
    -- brief error checking.
    */
    if( strlen(sat) < 2 )
        return DTKM_ERROR_BAD_SAT_VALUE ;
    if( strlen(sensor) < 3 )
        return DTKM_ERROR_BAD_SENSOR_VALUE ;
    if( rev <= 0 )
        return DTKM_ERROR_BAD_REV_VALUE ;
    if( dtkid <= 0 )
        return DTKM_ERROR_BAD_DTKID_VALUE ;

    delete_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;

    /* 
    -- load data into delete_dtk_rec 
    -- for call to function.  
    */
    strcpy( CAST_DTK_SAT    delete_dtk_rec[DTK_SAT],    sat ) ;
    strcpy( CAST_DTK_SENSOR delete_dtk_rec[DTK_SENSOR], sensor ) ;
    CAST_DTK_REV   delete_dtk_rec[DTK_REV]   =  rev ;
    CAST_DTK_DTKID delete_dtk_rec[DTK_DTKID] =  dtkid ;

    return_code = dtkm_delete_dtk_record( APS_dbproc, delete_dtk_rec ) ;

    /* clean up allocated memory.  */
    free_db_record( delete_dtk_rec ) ;

    return ( return_code ) ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_validate_station_id.c

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_validate_station_id.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_validate_station_id.c"


/*==============================================================================
Function:       mu_validate_station_id()

Description:    validates station_id as an input to an mu_..() routine.  

Creator:        Lawrence Stevens

Creation Date:  Thu Nov 21 20:46:37 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include <stdio.h>    /* for sprintf()   */

#include "mu.h"
#include "db_station.h"

int mu_validate_station_id( char *station_id )
{

    int     return_code ;

    if( station_id == NULL )
        return MU_ERROR_INPUT_STATION_ID_IS_NULL ;

    if( strcmp( station_id, "ALL" )  == 0 )
        return TRUE ;

    (void) sprintf(where_clause, "where %s = '%s' ",
        APS_COL(STATION, STATION_STATIONID),   station_id ) ;

#ifdef PRINT_DIAG
    (void) printf("%s(%d): where_clause = \n%s\n", __FILE__, __LINE__, where_clause ) ;
#endif

    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(STATION), where_clause ) ;
    if ( return_code < 0 )
        return MU_DB_ERROR_STATION_RELATION_QUERY_FAILED ;
 
    if ( return_code == 0 )
        return MU_ERROR_INPUT_STATION_ID_VALUE_IS_ILLEGAL ;

    return TRUE ; 
}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_validate_sat_phase_nominal_orb.c

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
#pragma ident   "@(#)mu_validate_sat_phase_nominal_orb.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_validate_sat_phase_nominal_orb.c"


/*==============================================================================
Function:       mu_validate_sat_phase_nominal_orb()

Description:    validate mu_activity_id, which should be in the
                form "<sat><phase>CreateNominalOrbit"
                we check to make sure that the sat and phase 
                exist in the phase relation AND that there is 
                at least one "coverageable" sensor for this 
                satellite in the satsensor relation.  

Returns:        TRUE of OK, < 0 if not valid, or if an error.  

Creator:        Lawrence Stevens

Creation Date:  Sun Dec  8 14:46:36 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include <libgen.h>         /* for strfind()      */

#include "mu.h"

#include "db_phase.h"
#include "db_satsensor.h"

int mu_validate_sat_phase_nominal_orb( char *mu_activity_id ) 
{
    int     return_code ;
    char    sat[3] ;
    char    phase_name ;
    int     nrecs ;

    /* 
    -- brief error checking:  
    -- validate this call:  
    */
    return_code = strfind( mu_activity_id, MU_CREATE_NOMINAL_ORBIT ) ;
    if( return_code < 3 )
    {
        (void) printf(
            "%s(%d):  error in code; review recent modifications of APS.\n",
            __FILE__, __LINE__ ) ;
        return MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE ;
    }

    /* 
    -- put the first 2 chars of mu_activity_id into 
    -- sat along with the null string terminater.  
    */
    sat[0] = '\0' ;
    (void) strncat( sat, mu_activity_id, 2 ) ;

    /* 
    -- put the 3rd character of mu_activity_id 
    -- into phase_name.
    */
    phase_name = mu_activity_id[2] ;

    /*
    -- see if a phase record exists for 
    -- the input sat, phase from mu_activity_id:  
    */
    (void) sprintf( where_clause, "where %s = '%s' and %s = '%c'",
        APS_COL(PHASE, PHASE_SAT), sat,
        APS_COL(PHASE, PHASE_PHASE_NAME), phase_name ) ;

    nrecs = db_num_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(PHASE), where_clause ) ;
    if( nrecs < 0 )
        return MU_DB_ERROR_DURING_PHASE_TABLE_COUNTING ;
    if( nrecs == 0 )
    {
        /* 
        -- no records found; the sat 
        -- and phase are therefore not valid:  
        */
        return MU_ERROR_INPUT_NOMINAL_ORBIT_ACTIVITY_ID_HAS_UNKNOWN_SAT_PHASE ;
    }

    /* mu_activity_id is valid.  */
    return TRUE ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_validate_activity_id.c

Description:    validates mu_activity_id and activity_type values.  
                also, contains all the valid values.  

External Functions Defined:
mu_validate_activity_id()
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_validate_activity_id.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_validate_activity_id.c"

#include <string.h>    /* for strcmp() etc.  */
#include <libgen.h>    /* for strfind()      */

#include "mu.h"

/* the following are the valid defined strings:  */
static char *activity_type_list[] = 
{
    MU_DAR_ACTIVITY_TYPE        ,
    MU_SINGLE_ACTIVITY_TYPE     ,
    MU_PLANNING_ACTIVITY_TYPE   ,

    NULL   /* end of list   */
} ;

static char *planning_activity_id[] = 
{
    MU_CREATE_NOMINAL_COVERAGE           ,
    MU_CREATE_NOMINAL_MASKINOUT          ,
    MU_CREATE_EDIT_OR_DELETE_DTK         ,
    MU_AWOS                              ,
    MU_MWOS                              ,
    MU_AREQ                              ,
    MU_ADDM                              ,
    MU_MDDM                              ,
    MU_REQW                              ,
    MU_REQQ                              ,
    MU_ARES                              ,
    MU_MPSG                              ,
    MU_SHAQ                              ,
    MU_SHAQP                             ,
    MU_REQR_STGS                         ,
    MU_OPL1                              ,
    MU_REQM_msge                         ,
    MU_REQA                              ,
    MU_OPLN                              ,
    MU_CRRA                              ,
    MU_CRSA                              ,
    MU_CRRM                              ,
    MU_CRSM                              ,
    MU_CREATE_DATATAKES_FROM_DAR         ,
    MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
    MU_CON_ROUNDUP                       ,

    NULL   /* end of list   */
} ;

static char *dar_activity_id[] = 
{
    MU_CREATE_SITE_COV_FOR_DAR       ,
    MU_SELECT_SITE_COV_FOR_DAR       ,
    MU_EDIT_OR_DELETE_DAR            ,
    MU_CREATE_DATATAKES_FROM_DAR     ,
    MU_DELETE_DAR                    ,

    NULL   /* end of list   */
} ;

static char *single_activity_id[] = 
{
    MU_AWOS                         ,
    MU_MWOS                         ,
    MU_AREQ                         ,
    MU_ADDM                         ,
    MU_MDDM                         ,
    MU_AE1E                         ,
    MU_AE2E                         ,
    MU_AJ1E                         ,
    MU_AA1E                         ,
    MU_AR1E                         ,
    MU_ME1E                         ,
    MU_ME2E                         ,
    MU_MR1E                         ,
    MU_REQW                         ,
    MU_REQQ                         ,
    MU_REUG                         ,
    MU_CRAR                         ,
    MU_MSGF                         ,
    MU_ARES                         ,
    MU_MPSG                         ,
    MU_SHAQ                         ,
    MU_SHAQP                        ,
    MU_REQR_STGS                    ,
    MU_OPL1                         ,
    MU_REQM_msge                    ,
    MU_REQA                         ,
    MU_MSGN                         ,
    MU_OPLN                         ,
    MU_CRRA                         ,
    MU_CRSA                         ,
    MU_CRRM                         ,
    MU_CRSM                         ,
    MU_APS_STATISTICS               ,
    MU_DAR_STATISTICS               ,
    MU_FRAME_GENERATION             ,
    MU_STATION_DOWN_TIMES           ,

    NULL   /* end of list:  
              Create Nominal Orbit values are checked using 
              the phase relation in the DB, so are not in this list  */

} ;



/*==============================================================================
Function:       mu_validate_activity_id()

Description:    validates activity_type and mu_activity_id against 
                the above lists of values.  

Returns:        ERROR messages if not valid, TRUE if OK.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec  5 19:28:20 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

int mu_validate_activity_id(
    char    *activity_type,    
    char    *mu_activity_id ) 
{

    int     return_code ;
    int     j ;
    int     activity_type_is_valid = FALSE ;

    /* brief error checking   */
    if ( activity_type == NULL )
        return MU_ERROR_INPUT_ACTIVITY_TYPE_IS_NULL_PTR ;
    if ( strlen(activity_type) < 3 )
        return MU_ERROR_INPUT_ACTIVITY_TYPE_IS_LT_3_CHARS_LONG ;

    if ( mu_activity_id == NULL )
        return MU_ERROR_INPUT_ACTIVITY_ID_IS_NULL_PTR ;
    if ( strlen(mu_activity_id) < 3 )
        return MU_ERROR_INPUT_ACTIVITY_ID_IS_LE_3_CHARS_LONG ;

    j = -1 ;
    while( activity_type_list[++j] )
    {
        if( strcmp(activity_type, activity_type_list[j] ) == 0 )
        {
            activity_type_is_valid = TRUE ;
            break ;
        }
    }
    if ( activity_type_is_valid != TRUE )
        return MU_ERROR_INPUT_ACTIVITY_TYPE_HAS_ILLEGAL_VALUE ;

    /*
    -- compare input activity_id to elements in the 
    -- appropriate MULL-terminated list.  If found, return 
    -- TRUE to indicate validation.  
    */
    if( strcmp(activity_type, MU_PLANNING_ACTIVITY_TYPE) == 0 )
    {
        j = -1 ;
        while( planning_activity_id[++j] )
            if( strcmp(mu_activity_id, planning_activity_id[j] ) == 0 )
                return TRUE ;
        return MU_ERROR_INPUT_PLANNING_ACTIVITY_ID_HAS_ILLEGAL_VALUE ;
    }

    if( strcmp(activity_type, MU_SINGLE_ACTIVITY_TYPE) == 0 )
    {
        /*
        -- if this is a CreateNominalOrbit activity, 
        -- validate the sat and phase values 
        -- using the Phase relation:   
        */
        return_code = strfind( mu_activity_id, MU_CREATE_NOMINAL_ORBIT ) ;
        if( return_code >= 3 )
        {
            /* 
            -- a CreateNominalOrbit activity, check sat 
            -- and phase values, chars [0-1] and [2] of
            -- mu_activity_id: 
            */
            return_code = mu_validate_sat_phase_nominal_orb( mu_activity_id ) ;
            if( return_code < 0 )
                return return_code ;
            return TRUE ;        
        }

        /* 
        -- not a CreateNominalOrbit activity; go 
        -- through the list of valid activity id's, 
        -- not including CreateNominalOrbit activities. 
        -- a match confirms validity:  
        */
        j = -1 ;
        while( single_activity_id[++j] )
            if( strcmp(mu_activity_id, single_activity_id[j] ) == 0 )
                return TRUE ;

        return MU_ERROR_INPUT_SINGLE_ACTIVITY_ID_HAS_ILLEGAL_VALUE ;
    }

    if( strcmp(activity_type, MU_DAR_ACTIVITY_TYPE) == 0 )
    {
        j = -1 ;
        while( dar_activity_id[++j] )
            if( strcmp(mu_activity_id, dar_activity_id[j] ) == 0 )
                return TRUE ;
        return MU_ERROR_INPUT_DAR_ACTIVITY_ID_HAS_ILLEGAL_VALUE ;
    }

    /* 
    -- this should not happen.  
    */
    (void) printf("%s(%d):  error in code; review recent modifications of APS.  \n", 
        __FILE__, __LINE__ ) ;
    return FALSE ;

}

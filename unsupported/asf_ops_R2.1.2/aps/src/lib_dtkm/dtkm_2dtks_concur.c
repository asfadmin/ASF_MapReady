#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_2dtks_concur.c

==============================================================================*/
#pragma ident   "@(#)dtkm_2dtks_concur.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_2dtks_concur.c"


/*==============================================================================
Function:       dtkm_2dtks_concur()

Description:    returns TRUE if the 2 dtks concur, FALSE if not, 
                < 0 if ERROR.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec 16 12:52:42 PST 1995

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

#include <string.h>

int dtkm_2dtks_concur(
    DB_RECORD   **dtk_rec1, 
    DB_RECORD   **dtk_rec2 ) 
{

    /* quick error checking.  */
    if ( dtk_rec1 == NULL || dtk_rec2 == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if (strcmp( CAST_DTK_SAT dtk_rec1[DTK_SAT],
                CAST_DTK_SAT dtk_rec2[DTK_SAT] ) != 0 )
    {
        return FALSE ;
    }

    if ( CAST_DTK_REV dtk_rec1[DTK_REV] != CAST_DTK_REV dtk_rec2[DTK_REV] )
    {
        return FALSE ;
    }

    /* 
    -- sats and revs match  
    */
    if ( CAST_DTK_DTKID dtk_rec1[DTK_DTKID] != 0 
    &&   CAST_DTK_DTKID dtk_rec2[DTK_DTKID] != 0 )
    {
        /*
        -- both dtkids are != 0 
        -- now only one test to do.  
        */
        if (CAST_DTK_DTKID dtk_rec1[DTK_DTKID] == 
            CAST_DTK_DTKID dtk_rec2[DTK_DTKID] )
        {
            /* 
            -- dtkids are !=0 AND 
            -- they are = to each other.  
            -- they are concurring data-takes.  
            -- due to sat, rev, dtkid all equal 
            -- and not zero.  
            */
            return TRUE ;
        }
        else
        {
            /* 
            -- dtkid's are !=0 and not equal to 
            -- each other.  
            */
            return FALSE ;
        }
    }

    /* 
    -- the sat and revs match, but 
    -- at least one of the dtks has 
    -- a zero dtkid.  This could still be a 
    -- concur if they are the same activity and 
    -- if fadtkid values match.  
    */

    /* check for differing dtk.actid values.  realtime v record. */
    if (strcmp( CAST_DTK_ACTID dtk_rec1[DTK_ACTID],
                CAST_DTK_ACTID dtk_rec2[DTK_ACTID] ) != 0   )
    {
        /* different ACTID values  */
        return FALSE ;
    }

    /*
    -- With the same ACTID values, 
    -- we now check to see if the fa_dtkid
    -- values are non-null and matching:  
    */
    if ( (int) strlen(CAST_DTK_FADTKID dtk_rec1[DTK_FADTKID]) <= 0 )
        return FALSE ;

    if ( (int) strlen(CAST_DTK_FADTKID dtk_rec2[DTK_FADTKID]) <= 0 )
        return FALSE ;

    /* 
    -- both fadtkid strings are non-null.  
    */
    if (strcmp( CAST_DTK_FADTKID dtk_rec1[DTK_FADTKID],
                CAST_DTK_FADTKID dtk_rec2[DTK_FADTKID] ) == 0   )
    {
        /* 
        -- values are non-null 
        -- and matching.  
        -- this is a concurring data-take 
        -- because the sat and revs and activity id's  match 
        -- AND at least one of the dtkid's is 0 
        -- BUT the fadtkid's are non-null AND they match.   
        */
        return TRUE ;
    }
    /* 
    -- either the fadtkid's don't 
    -- match, or they are both
    -- null strings.  not a concur.  
    */
    return FALSE ;

}

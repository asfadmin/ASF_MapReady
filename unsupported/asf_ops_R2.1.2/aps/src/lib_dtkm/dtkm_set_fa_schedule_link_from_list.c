#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_set_fa_schedule_link_from_list.c

Description:    
External Functions Defined:
File Scope Functions:
External Variables Defined:
File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)dtkm_set_fa_schedule_link_from_list.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_set_fa_schedule_link_from_list.c"

#include "dtkm.h"            

/* FOR DATABASE TABLES        */
#include "db_dtk.h"            /* for dtk table             */

#include <string.h>     /* for strcmp strcpy        */


/*==============================================================================
Function:       dtkm_set_fa_schedule_link_from_list()

Description:    look in the dtk_list for a value in DTK_FA_SCHEDULE_LINK.  
                If found, copy that value to the output dtk and return.  

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 21 16:29:34 PDT 1997

==============================================================================*/
int dtkm_set_fa_schedule_link_from_list( 
    llist       *dtk_list,          /* search in this list for a value.       */
    DB_RECORD   **output_dtk_rec )  /* write the found value into this dtk.   */
{
    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr ;

    if ( dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    if ( output_dtk_rec == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    for ( dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
          dtk_rec ;
          dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr) )
    {
        if( strlen( CAST_DTK_FA_SCHEDULE_LINK dtk_rec[DTK_FA_SCHEDULE_LINK])
            > 0 )
        {
            (void) strcpy( CAST_DTK_FA_SCHEDULE_LINK 
                                    output_dtk_rec[DTK_FA_SCHEDULE_LINK],
                           CAST_DTK_FA_SCHEDULE_LINK 
                                    dtk_rec[DTK_FA_SCHEDULE_LINK] );
            return TRUE ;
        }
    }

    return TRUE ;

}

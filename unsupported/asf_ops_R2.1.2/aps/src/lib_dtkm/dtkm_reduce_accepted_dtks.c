#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_reduce_accepted_dtks.c

Notes:          

==============================================================================*/
#pragma ident   "@(#)dtkm_reduce_accepted_dtks.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_reduce_accepted_dtks.c"


/*==============================================================================
Function:       dtkm_reduce_accepted_dtks()

Description:    Look in the input_dtks list for REJ, CON, and DEL data-takes.  
                if any of these data-takes are found in the list 
                of accepted_dtks, then remove them from accepted_dtks.  

Creator:        Lawrence Stevens

Creation Date:  Mon Mar 17 17:15:55 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>

int dtkm_reduce_accepted_dtks(
    llist       *accepted_dtks, /*  input/output list of accepted dtks       */
    llist       *input_dtks )   /*  input list - look for REJ/CON/DEL dtks.  */
{

    llist       *reduce_dtks = NULL ;
    int         return_code ;

    reduce_dtks = create_dyn_llist() ;

    /*
    -- copy any data-takes with DTKSTAT = DEL from input_dtks 
    -- to reduce_dtks.  
    */
    return_code = dtkm_copy_dtks_value( DTK_DTKSTAT, "DEL", 
        input_dtks, reduce_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    return_code = dtkm_copy_dtks_value( DTK_DTKSTAT, "CON", 
        input_dtks, reduce_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    return_code = dtkm_copy_dtks_value( DTK_DTKSTAT, "REJ", 
        input_dtks, reduce_dtks ) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    --  if any data-takes are found in the reduce_dtks list,
    --  remove them from the accepted_dtks list if they are 
    --  in there, too.
    */
    return_code = dtkm_remove_dtks_from_list( reduce_dtks, accepted_dtks ) ;
    DEL_LIST( reduce_dtks ) ;

    /* 
    -- return the code from dtkm_remove_dtks_from_list()  
    */
    return return_code ;

}

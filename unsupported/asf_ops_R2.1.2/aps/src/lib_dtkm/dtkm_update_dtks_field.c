#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_update_dtks_field.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dtks_field.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dtks_field.c"


/*==============================================================================
Function:       dtkm_update_dtks_field()

Description:    change a single data field in a list of dtk 
                relation DB_RECORDS, then update the database 
                with all of the values of the record if the field 
                is not equal to the desired value.  

                CAUTION:  
                CAUTION:  this is not really a great tool; it does 
                what is needed in the dtkm library but maybe not 
                in your application.  Be careful that you check 
                this routine out before you use it.  Maybe you 
                would rather make a better routine instead.  

                This routine updates dtk.dtkdate = <today> in addition 
                to the requested (non-key) data field, 
                field and value indicated by the 
                parameters for each DB_RECORD in the list.  

                NOTE:
                If the DB_RECORD already has the value, there is no 
                update done; it is not needed.  

Creator:        Lawrence Stevens

Creation Date:  Tue Oct 31 17:15:12 PST 1995

Notes:      
    The parameter value_ptr is the address of memory containing the 
    value that will update the db field.  Make sure this area is as 
    large as the db field.  Memory is copied from this area according 
    to the length of the db field.  

    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

#include <string.h>     /* for memcmp() memmove()       */

int dtkm_update_dtks_field(
    DBPROCESS   *APS_dbproc, 
    llist       *dtk_list,      /* list of data-takes.   */
    int         col_number,     /* column to update, e.g. DTK_STRTTIME */
    void        *value_ptr,     /* &(field value); make sure it's long enough */
    llist       *dtk_updates ) 
{
    int         return_code ;
    DB_RECORD   **dtk_rec = NULL ;
    cursor      dtk_list_ptr ;

    /* error checking  */
    if ( dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ; 

    if ( NUMELTS( dtk_list ) == 0 )
        return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ; 

    if ( col_number <= DTK_DTKID )
        return DTKM_ERROR_ATTEMPT_TO_UPDATE_PRIMARY_KEY ;

    if ( value_ptr == NULL )
        return DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE ;

    if ( dtk_updates == NULL )
        return DTKM_ERROR_NULL_OUTPUT_DTK_LIST ;

    for ( dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
          dtk_rec != NULL ;
          dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)   )
    {
        /* 
        -- compare the value in the argument to the 
        -- value in the DB_RECORD:
        */
        if ( 
            memcmp( dtk_rec[col_number], value_ptr, APS_SIZE(DTK, col_number) )
            == 0 )
        {
            /* 
            -- the values are equal; skip 
            -- to the next record; no need to 
            -- update this record.  
            */
            continue ;
        }

        /* 
        -- change the indicated data value:  
        */

        /*
        -- memmove() is a more robust version of memcpy() which 
        -- allows the source and destination to overlap.  
        */
        memmove( dtk_rec[col_number], 
            value_ptr, APS_SIZE(DTK, col_number) ) ;

        /* 
        -- the indicated field in the DB_RECORD is now updated with the 
        -- input value.  now update the database.  
        -- the 2nd dtk_rec in the parameter is the output/result dtk; 
        -- it is OK for them to be the same.  
        -- the change is inserted into the dtk_updates data-takes list.  
        */
        return_code = dtkm_update_dtk_record( APS_dbproc, 
            dtk_rec, dtk_rec, dtk_updates ) ;
        if ( return_code < 0 )
            return return_code ;
    }

    return DTKM_UPDATE_DTKS_FIELD_OK ;

}


#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       reqq_phase.c

Description:    routines to deal with the REQQ phase.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
==============================================================================*/
#pragma ident   "@(#)reqq_phase.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.reqq_phase.c"


#include <dapps_defs.h>        /* for REQQ_PHASE  */
#include <db_sybint.h>         /* for APS sybase interface routines.  includes
                                  dapps_list.h     */

#include <aps_db_table.h>       /* for REQQ_PHASE  */
#include "db_reqq_phase.h"    
#include <timeconv.h>    
#include <string.h>    


/*==============================================================================
Function:       get_reqq_phase_no()

Description:    Given a time, return the REQQ phase number.  This 
                is the earliest phase that ends after or == to the 
                input ASF time.  

Creator:        Lawrence Stevens

Creation Date:  Tue Sep 23 16:17:41 PDT 1997

==============================================================================*/
int get_reqq_phase_no( char *asftime )
{
    llist        *reqq_phase_list ;
    cursor      reqq_phase_list_ptr ;
    DB_RECORD   **reqq_phase_rec ; 

    int         phase_number ;

    /* a little error checking.  */
    if( tc_validate_asf_datetime(asftime) != TRUE )
        return -1 ;

    (void) sprintf(where_clause, "where %s >= '%s' ", 
        APS_COL(REQQ_PHASE, REQQ_PHASE_STOPTIME), asftime ) ;

    (void) sprintf( orderby_cols, "%s ", 
        APS_COL(REQQ_PHASE, REQQ_PHASE_STOPTIME) ) ;

    reqq_phase_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(REQQ_PHASE), where_clause, orderby_cols, 
        APS_CDEFS(REQQ_PHASE), ALL_COLS) ;
    if (reqq_phase_list == NULL)
        return -2 ;

    if( NUMELTS( reqq_phase_list ) < 1 )
    {
        DEL_LIST( reqq_phase_list ) ;
        return -3 ;
    }

    reqq_phase_rec = (DB_RECORD **) FIRST(reqq_phase_list, reqq_phase_list_ptr);

    phase_number = CAST_REQQ_PHASE_REQQ_ID reqq_phase_rec[REQQ_PHASE_REQQ_ID] ;

    DEL_LIST( reqq_phase_list ) ;

    return phase_number ;

}


/*==============================================================================
Function:       get_current_reqq_phase()

Description:    using current time, get the info for the current 
                REQQ phase which is determined by the next (future) 
                REQQ due date.  This is the next REQQ to be done.  

Creator:        Lawrence Stevens

Creation Date:  Tue Sep 23 16:16:01 PDT 1997

==============================================================================*/
int get_current_reqq_phase( 
    int     *reqq_id,      /* output REQQ phase number  */
    char    *due_date,     /* output due date           */
    char    *strttime,     /* output start time         */
    char    *stoptime )    /* output stop time          */
{

    llist        *reqq_phase_list ;
    cursor      reqq_phase_list_ptr ;
    DB_RECORD   **reqq_phase_rec ; 

    char    current_asftime[ASF_TIME_STR_LENGTH+1] ;

    /* get current time:  */
    tc_systime2asf( current_asftime ) ;

    (void) sprintf(where_clause, "where %s > '%s' ", 
        APS_COL(REQQ_PHASE, REQQ_PHASE_DUE_DATE), current_asftime ) ;

    (void) sprintf( orderby_cols, "%s ", 
        APS_COL(REQQ_PHASE, REQQ_PHASE_STOPTIME) ) ;

    reqq_phase_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(REQQ_PHASE), where_clause, orderby_cols, 
        APS_CDEFS(REQQ_PHASE), ALL_COLS) ;
    if (reqq_phase_list == NULL)
        return -2 ;

    if( NUMELTS( reqq_phase_list ) < 1 )
    {
        DEL_LIST( reqq_phase_list ) ;
        return -3 ;
    }

    reqq_phase_rec = (DB_RECORD **) FIRST(reqq_phase_list, reqq_phase_list_ptr);

    /* A.K.A.  REQQ phase number:  */
    *reqq_id = CAST_REQQ_PHASE_REQQ_ID reqq_phase_rec[REQQ_PHASE_REQQ_ID] ;

    (void) strcpy(due_date, 
        CAST_REQQ_PHASE_DUE_DATE reqq_phase_rec[REQQ_PHASE_DUE_DATE] ) ;

    (void) strcpy( strttime, 
        CAST_REQQ_PHASE_STRTTIME reqq_phase_rec[REQQ_PHASE_STRTTIME] ) ;

    (void) strcpy( stoptime, 
        CAST_REQQ_PHASE_STOPTIME reqq_phase_rec[REQQ_PHASE_STOPTIME] ) ;

    DEL_LIST( reqq_phase_list ) ;

    return TRUE ;

}

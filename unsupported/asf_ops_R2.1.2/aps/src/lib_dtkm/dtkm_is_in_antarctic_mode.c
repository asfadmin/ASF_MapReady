#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_is_in_antarctic_mode.c

==============================================================================*/
#pragma ident   "@(#)dtkm_is_in_antarctic_mode.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_is_in_antarctic_mode.c"

#include <db_sybint.h>          /*  for DB_RECORD                         */
#include <phase_utilities.h>    /*  for asftime_2_phase()                 */
#include <dtkm_utilities.h>     /*  for DTKM_ERROR_NULL_RECORD etc.       */
#include <db_dtk.h>             /*  for CAST_DTK_SAT etc.                 */
#include <db_phase.h>           /*  for CAST_PHASE_ANTARCTIC_MODE         */


/*==============================================================================
Function:       dtkm_is_in_antarctic_mode()

Description:    checks the phase relation to see if the data-take is 
                within an antarctic mode phase.  It uses the dtk start time 
                to determine phase.  Also the satellite value.  
                returns < 0 for error, while TRUE, and FALSE and the 
                non-error codes.  

Creator:        Lawrence Stevens

Creation Date:  Tue Apr  8 17:44:21 PDT 1997

==============================================================================*/
int dtkm_is_in_antarctic_mode( DB_RECORD **dtk_rec ) 
{

    DB_RECORD   **phase_rec = NULL ;  /* initialize to check later.  */
    int         return_code ;
    int         answer = 0 ;

    if( dtk_rec == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    return_code = asftime_2_phase( 
        CAST_DTK_SAT dtk_rec[DTK_SAT], 
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME], 
        &phase_rec ) ;
    if( return_code < 0 )
    {
        fprintf(stderr, "%s(%d):  %s\n", __FILE__, __LINE__, 
            PHASE_ERROR_MESSAGE( return_code ) ) ;
        dtkm_print( stderr, dtk_rec ) ;
        return DTKM_ERROR_DETERMINING_PHASE_FOR_DTK ;
    }
    if( return_code != PHASE_INPUT_TIME_WITHIN_A_PHASE )
    {
        fprintf(stderr, "%s(%d):  DTK not within any phase.\n", 
            __FILE__, __LINE__ ) ;
        dtkm_print( stderr, dtk_rec ) ;

        /* possibly clean up:  */
        if( phase_rec != NULL )
            free_db_record( phase_rec ) ;

        return DTKM_ERROR_DETERMINING_PHASE_FOR_DTK ;
    }

    /*
    -- OK.  
    -- PHASE_INPUT_TIME_WITHIN_A_PHASE
    */
    if( CAST_PHASE_ANTARCTIC_MODE phase_rec[PHASE_ANTARCTIC_MODE] == 'Y' )
        answer = TRUE ;
    else 
        answer = FALSE ;

    /* clean up allocated storage.  */
    free_db_record( phase_rec ) ;

    return answer ;

}

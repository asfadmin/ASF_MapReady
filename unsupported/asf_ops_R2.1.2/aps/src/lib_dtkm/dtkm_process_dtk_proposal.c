#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

#pragma ident   "@(#)dtkm_process_dtk_proposal.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_process_dtk_proposal.c"


/*==============================================================================
Function:       dtkm_process_dtk_proposal()

Description:    performs the multi-antenna algorithm to process a dtk proposal.
                It processes the proposal and will then process any bumped
                data-takes if necessary until all processing is done.  

The expected status of the dtk proposal is: 

DEL, which indicates a deletion by either the planner or an FA
REJ, which indicates a rejection by either the planner or an FA (handled as DEL)
REQ, which indicates an FA request (it will have a PLN status when processed), 
PLN, which indicates a data-take from an FA plan or an FA 
     confirmation of an ASF request, or 
SCH, which indicates a data-take from an FA schedule.  
QUE, which indicates a not-yet-requested data-take from the planner.  


Returns:        
    int
    >= 0 normal:

        DTKM_DTK_DELETED_OK  
            Note:  there may be dtk_updates - re-instated dtks

        DTKM_DTK_PROPOSAL_REJ 
            Note: there will be dtk_sat_down_times and/or dtk_conflicts 
            to explain the cause of the rejection.  

        DTKM_DTK_PROPOSAL_CONFLICT ;
            Note:  there will be dtk_conflicts and/or antenna_down_times to 
            explain the conflicts that prevent the data-take from being 
            placed into the schedule.  

        DTKM_DTK_ACCEPTED ;
            Note:  there may be antenna_down_times and dtk_updates
            the antenna down_times are for informational purposes to show 
            that they were encountered for the time of the data-take.  
            
            dtk_updates lists every data-take that was changed in the 
            process of accepting the data-take proposal.  
            It is possible that same-satellite data-takes were 
            moved to a different antenna together with the dtk proposal.  
            It is also possible that the dtk_proposal bumped other 
            data-takes from the schedule to a status of CON.  Any and all 
            of the changed data-takes will be in the dtk_updates 
            list.  

        DTKM_DTK_ACCEPTED_INHERITANCE ;
            Note:  these data-take proposals could be classed under 
            DTKM_DTK_ACCEPTED but for one difference, the status of 
            this 'recording' proposal is inherited from its corresponding
            'downlink' data-take already in the database.

    < 0 ERROR:  

Creator:        Lawrence Stevens

Creation Date:  Sat Oct 28 09:16:41 PDT 1995

Notes:      
See etc/notes/Multi-antenna.txt for the algorighm.  

==============================================================================*/

#include "dtkm.h"  /* each dtkm source file will have this include file. */
#include "db_dtk.h"     /* for CAST_DTK_DTKSTAT  etc. */
#include <string.h>     /* for strcpy()   */
int dtkm_process_dtk_proposal(
    DBPROCESS   *APS_dbproc,     /* Sybase db process   */
    DB_RECORD   **dtk_proposal,  /* input data-take proposal.          */
    DB_RECORD   **result_dtk ,   /* data-take proposal with updated info     */
    DB_RECORD   **parent_dtk ,   /* may contain downlink data-take associated
                                    with a recording data-take proposal */

            /* 
            -- if the dtk_proposal REJ accepted you 
            -- will see entries in dtk_sat_down_times and/or dtk_concurs
            */
    llist       *input_dtk_sat_down_times, 
                    /* list of conflicting satellite down times in the form 
                       of dtk DB_RECORDS;  can be NULL */

    llist       *input_antenna_down_times, 
                    /* antenna down times encountered can be NULL */

    llist       *input_dtk_concurs,   
                    /* concurs found processing original proposal, can be NULL*/

    llist       *input_dtk_similars,  
                    /* similars found processing original. can be NULL */

    llist       *input_dtk_conflicts, 
                    /* list of conflicting data-takes, can be NULL */

            /* if the dtk_proposal WAS accepted, there will be entries:   */
    llist       *input_dtk_updates )   
                    /* all data-takes updated, can be NULL */
{

    int     return_code ;
    int     original_dtk_proposal_return_code = 0 ;

    DB_RECORD   **dtk_unlinked_rec = NULL ; 

    llist       *dtk_bumps = NULL ;  /* list of bumps.  processed in a loop.  */
    cursor      dtk_bumps_ptr ;

    llist       *list_check = NULL ;

    /* 
    -- lists to be used for processing 
    -- original data-takes:  
    */
    llist       *dtk_parallels = NULL ;
    llist       *dtk_same_pass = NULL ;


    DB_RECORD   **dtk_bumped = NULL ;   /* dtk record when doing bumps       */
    DB_RECORD   **b_dtk_proposal = NULL;/* proposed dtk when processing bumps*/
    DB_RECORD   **b_result_dtk = NULL ; /* result dtk when processing bumps. */

    /* 
    -- lists to be used for processing the 
    -- original data-take proposal:
    */
    llist       *antenna_down_times = NULL ;
    llist       *dtk_sat_down_times = NULL ;
    llist       *dtk_concurs = NULL ;
    llist       *dtk_similars = NULL ;
    llist       *dtk_conflicts = NULL ;
    llist       *dtk_updates = NULL ;

    /* 
    -- lists to be re-used over and over for processing 
    -- bumped data-takes:  
    */
    llist       *b_dtk_sat_down_times = NULL ;
    llist       *b_antenna_down_times = NULL ;
    llist       *b_dtk_conflicts = NULL ;
    llist       *b_dtk_updates = NULL ;
    llist       *b_dtk_bumps  = NULL ;
    llist       *b_dtk_parallels = NULL ;
    llist       *b_dtk_same_pass = NULL ;
    llist       *b_dtk_concurs = NULL ;
    llist       *b_dtk_similars = NULL ;


    /* 
    -- parameter error checking  
    */
    if ( dtk_proposal == NULL ) 
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( result_dtk == NULL ) 
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    /* 
    -- the lists can be input as NULL.  If not, they 
    -- must be empty.  If they are null, this will be 
    -- handled during initiation, next.  
    */
    if ( input_antenna_down_times != NULL )
        if ( NUMELTS( input_antenna_down_times ) != 0 )
            return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY ;

    if ( input_dtk_sat_down_times != NULL )
        if ( NUMELTS( input_dtk_sat_down_times ) != 0 )
            return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY ;

    if ( input_dtk_concurs != NULL )
        if ( NUMELTS( input_dtk_concurs ) != 0 )
            return DTKM_ERROR_CONCURS_LIST_NOT_EMPTY ;

    if ( input_dtk_similars != NULL )
        if ( NUMELTS( input_dtk_similars ) != 0 )
            return DTKM_ERROR_SIMILARS_LIST_NOT_EMPTY ;

    if ( input_dtk_conflicts != NULL )
        if ( NUMELTS( input_dtk_conflicts ) != 0 )
            return DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY ;

    if ( input_dtk_updates != NULL )
        if ( NUMELTS( input_dtk_updates ) != 0 )
            return DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY ;

    /*
    -- set up lists for use by the original dtk_proposal.  
    -- if provided by the calling routine, use the lists provided.  
    -- otherwise, create the lists.  
    */
    /* set up antenna_down_times  */
    if ( input_antenna_down_times == NULL )
        antenna_down_times = create_dyn_llist() ;
    else
        antenna_down_times = input_antenna_down_times ;

    /* set up dtk_sat_down_times  */
    if ( input_dtk_sat_down_times == NULL )
        dtk_sat_down_times = create_dyn_llist() ;
    else
        dtk_sat_down_times = input_dtk_sat_down_times ;

    /* set up dtk_concurs  */
    if ( input_dtk_concurs == NULL )
        dtk_concurs = create_dyn_llist() ;
    else
        dtk_concurs = input_dtk_concurs ;

    /* set up dtk_similars  */
    if ( input_dtk_similars == NULL )
        dtk_similars = create_dyn_llist() ;
    else
        dtk_similars = input_dtk_similars ;

    /* set up dtk_conflicts  */
    if ( input_dtk_conflicts == NULL )
        dtk_conflicts = create_dyn_llist() ;
    else
        dtk_conflicts = input_dtk_conflicts ;

    /* set up dtk_updates  */
    if ( input_dtk_updates == NULL )
        dtk_updates = create_dyn_llist() ;
    else
        dtk_updates = input_dtk_updates ;

    /* set up dtk_parallels  */
    dtk_parallels = create_dyn_llist() ;

    /* set up dtk_same_pass  */
    dtk_same_pass = create_dyn_llist() ;

    /*
    -- STEP 1.
    -- set up the next-dtk proposal list, i.e., the list 
    -- of bumped data-takes that might result from the dtk_proposal
    -- being accepted.  
    */
#ifdef PRINT_DIAG
    printf("%s:(%d):  STEP 1. set up next-dtk list (dtk_bumps)\n",
        __FILE__, __LINE__ ) ;
#endif

    /* set up dtk_bumps  */
    dtk_bumps = create_dyn_llist() ;

    /*
    -- STEPS 2-9.
    -- process the ORIGINAL data-take proposal:  
    */
    return_code = dtkm_process_dtk( APS_dbproc, 
        dtk_proposal,  result_dtk, parent_dtk, 
        dtk_sat_down_times, antenna_down_times, dtk_concurs, dtk_similars,  
        dtk_conflicts, dtk_parallels, dtk_same_pass, 
        dtk_updates, dtk_bumps ) ;
#ifdef PRINT_DIAG
        printf("AFTER dtkm_process_dtk() original dtk code return_code = %d\n", 
            return_code ) ;
        if ( return_code < 0 )
            printf("********  %s\n", DTKM_ERROR_MESSAGE( return_code )  ) ;
        else
        {
            switch( return_code )
            {
            case DTKM_DTK_ACCEPTED :
                printf("------- DTKM_DTK_ACCEPTED\n" ) ;
                break ;
            case DTKM_DTK_PROPOSAL_CONFLICT :
                printf("------- DTKM_DTK_PROPOSAL_CONFLICT\n" ) ;
                break ;
            case DTKM_DTK_PROPOSAL_REJ :
                printf("------- DTKM_DTK_PROPOSAL_REJ\n" ) ;
                break ;
            case DTKM_DTK_DELETED_OK :
                printf("------- DTKM_DTK_DELETED_OK\n" ) ;
                break ;
            default:
                printf("############################# UNKNOWN RETURN CODE\n" ) ;
                break ;
            }
        }
        printf("RESULT DTK:\n" ) ;
        dtkm_print(stdout, result_dtk ) ;
        printf("SAT DOWN TIMES:\n" ) ;
        dtkm_print_list(stdout, dtk_sat_down_times ) ;
        printf("ANTENNA DOWN TIMES:\n" ) ;
        dtkm_print_antenna_down_times_list(stdout, antenna_down_times ) ;
        printf("CONCURS:\n" ) ;
        dtkm_print_list(stdout, dtk_concurs ) ;
        printf("SIMILARS:\n" ) ;
        dtkm_print_list(stdout, dtk_similars ) ;
        printf("CONFLICTS:\n" ) ;
        dtkm_print_list(stdout, dtk_conflicts ) ;
        printf("PARALLELS:\n" ) ;
        dtkm_print_list(stdout, dtk_parallels ) ;
        printf("SAME PASS:\n" ) ;
        dtkm_print_list(stdout, dtk_same_pass ) ;
        printf("UPDATES:\n" ) ;
        dtkm_print_list(stdout, dtk_updates ) ;
        printf("BUMPS:\n" ) ;
        dtkm_print_list(stdout, dtk_bumps ) ;
#endif

    if (return_code < 0 )
    {
        /* free allocated memory.  */
        if ( input_antenna_down_times == NULL )
            DEL_LIST( antenna_down_times ) ;
        if ( input_dtk_sat_down_times == NULL )
            DEL_LIST( dtk_sat_down_times ) ;
        if ( input_dtk_concurs == NULL )
            DEL_LIST( dtk_concurs ) ;
        if ( input_dtk_similars == NULL )
            DEL_LIST( dtk_similars ) ;
        if ( input_dtk_conflicts == NULL )
            DEL_LIST( dtk_conflicts ) ;
        if ( input_dtk_updates == NULL )
            DEL_LIST( dtk_updates ) ;
        DEL_LIST( dtk_parallels ) ;
        DEL_LIST( dtk_same_pass ) ;
        DEL_LIST( dtk_bumps ) ;
        return return_code ;
    }

    switch( return_code )
    {
    case DTKM_DTK_PROPOSAL_REJ :
    case DTKM_DTK_PROPOSAL_CONFLICT :
        if ( NUMELTS(dtk_bumps) > 0 )
        {
            /* 
            -- there should have been NO bumps by 
            -- the original proposal, if the return 
            -- code was DTKM_DTK_PROPOSAL_REJ 
            -- or DTKM_DTK_PROPOSAL_CONFLICT.
            */
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( dtk_parallels ) ;
            DEL_LIST( dtk_same_pass ) ;
            DEL_LIST( dtk_bumps ) ;
            return DTKM_ERROR_IN_CODE_DTK_CREATED_BUMPS ;
        }
        /* FALLTHROUGH */
    case DTKM_DTK_DELETED_OK :
        /* 
        -- if a dtk is REJ or DEL, then there could 
        -- be data-takes that should be shifted to 
        -- the newly vacated antenna at that time.  
        -- these are also called bumps.  But it is 
        -- a happy bump; the data-take will ONLY move 
        -- to a more preferred antenna.  
        */
        /* FALLTHROUGH */
    case DTKM_DTK_ACCEPTED_INHERITANCE :
        /* FALLTHROUGH */
    case DTKM_DTK_ACCEPTED :
        /* note the fall through from the cases above.  */
        /*
        -- the the original dtk proposal has been processed; were there 
        -- any bumped data-takes?
        */
        if ( NUMELTS(dtk_bumps) <= 0 )
        {
            /* no bumps by the original proposal.  finished.  */
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( dtk_parallels ) ;
            DEL_LIST( dtk_same_pass ) ;
            DEL_LIST( dtk_bumps ) ;
            return return_code ;
        }
        /*
        -- there were some data-takes bumped.  must process them.  
        */
        /*
        -- RETAIN the original return code.  on a normal return, 
        -- return this code to the calling program to indicate the 
        -- status of the original dtk_proposal.  
        */
        original_dtk_proposal_return_code = return_code ;
        break ;
    default :
        /* error.  */
        /* free allocated memory.  */
        if ( input_antenna_down_times == NULL )
            DEL_LIST( antenna_down_times ) ;
        if ( input_dtk_sat_down_times == NULL )
            DEL_LIST( dtk_sat_down_times ) ;
        if ( input_dtk_concurs == NULL )
            DEL_LIST( dtk_concurs ) ;
        if ( input_dtk_similars == NULL )
            DEL_LIST( dtk_similars ) ;
        if ( input_dtk_conflicts == NULL )
            DEL_LIST( dtk_conflicts ) ;
        if ( input_dtk_updates == NULL )
            DEL_LIST( dtk_updates ) ;
        DEL_LIST( dtk_parallels ) ;
        DEL_LIST( dtk_same_pass ) ;
        DEL_LIST( dtk_bumps ) ;
        return DTKM_ERROR_UNKNOWN_NORMAL_RETURN_CODE_FROM_DTKM_PROCESS_DTK ;
    }

    /* 
    -- free memory/initialize lists for future use and re-use with 
    -- bumped data-takes:  
    */
    DEL_LIST( dtk_parallels ) ;
    DEL_LIST( dtk_same_pass ) ;

    b_dtk_sat_down_times = create_dyn_llist() ;
    b_antenna_down_times = create_dyn_llist() ;
    b_dtk_concurs = create_dyn_llist() ;
    b_dtk_similars = create_dyn_llist() ;
    b_dtk_conflicts = create_dyn_llist() ;
    b_dtk_parallels = create_dyn_llist() ;
    b_dtk_same_pass = create_dyn_llist() ;
    b_dtk_updates = create_dyn_llist() ;
    b_dtk_bumps = create_dyn_llist() ;

    b_result_dtk =  new_table_record(APS_CDEFS(DTK)) ;
    b_dtk_proposal =  new_table_record(APS_CDEFS(DTK)) ;

    /* 
    -- Now we must process each data-take in the dtk_bump list; 
    -- the bumped data-takes.  
    */

    for ( 
            dtk_bumped = (DB_RECORD **) FIRST(dtk_bumps, dtk_bumps_ptr) ;
            NUMELTS(dtk_bumps) > 0 ;
            dtk_bumped = (DB_RECORD **) FIRST(dtk_bumps, dtk_bumps_ptr) 
        )
    {
        /*
        -- Process each record in the (possibly growing) list.  
        -- before we process it, remove it from the list (UNLINK).  
        -- later, free this data-take (dtk_bumped).  
        */
        dtk_unlinked_rec = (DB_RECORD **) UNLINK_AT_CURSOR( 
            dtk_bumps, dtk_bumps_ptr ) ;
        if ( dtk_unlinked_rec != dtk_bumped ) 
        {
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( b_dtk_sat_down_times ) ;
            DEL_LIST( b_antenna_down_times ) ;
            DEL_LIST( b_dtk_concurs ) ;
            DEL_LIST( b_dtk_similars ) ;
            DEL_LIST( b_dtk_conflicts ) ;
            DEL_LIST( b_dtk_parallels ) ;
            DEL_LIST( b_dtk_same_pass ) ;
            DEL_LIST( b_dtk_updates ) ;
            DEL_LIST( b_dtk_bumps ) ;
            free_db_record( b_result_dtk ) ;
            free_db_record( b_dtk_proposal ) ;
            DEL_LIST( dtk_bumps ) ;
            return DTKM_ERROR_UNLINKING_RECORD ;
        }
        /*
        -- copy the current record to b_dtk_proposal. 
        */
        return_code = db_copy_record( APS_CDEFS(DTK), b_dtk_proposal, 
            dtk_bumped ) ;
        if ( return_code < 0 )
        {
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( b_dtk_sat_down_times ) ;
            DEL_LIST( b_antenna_down_times ) ;
            DEL_LIST( b_dtk_concurs ) ;
            DEL_LIST( b_dtk_similars ) ;
            DEL_LIST( b_dtk_conflicts ) ;
            DEL_LIST( b_dtk_parallels ) ;
            DEL_LIST( b_dtk_same_pass ) ;
            DEL_LIST( b_dtk_updates ) ;
            DEL_LIST( b_dtk_bumps ) ;
            free_db_record( b_result_dtk ) ;
            free_db_record( b_dtk_proposal ) ;
            DEL_LIST( dtk_bumps ) ;
            return return_code ;
        }

        /*
        -- process this bumped data-take proposal; try to get it back 
        -- onto another antenna.  
        */

        /* 
        -- as we start to process the data-take, first 
        -- set the dtkstat value from BMP to the 
        -- originally proposed value:  
        */
        (void) strcpy( CAST_DTK_DTKSTAT b_dtk_proposal[DTK_DTKSTAT], 
            CAST_DTK_PROPOSED_DTKSTAT b_dtk_proposal[DTK_PROPOSED_DTKSTAT] ) ;
        
#ifdef PRINT_DIAG
        printf("%s(%d): CALLING dtkm_process_dtk() for a BUMPED DTK \n", 
                __FILE__, __LINE__ ) ;
        dtkm_print(stdout, b_dtk_proposal ) ;
#endif

        return_code = dtkm_process_dtk( APS_dbproc, 
            b_dtk_proposal, b_result_dtk, parent_dtk, 
            b_dtk_sat_down_times, b_antenna_down_times, b_dtk_concurs, 
            b_dtk_similars,  b_dtk_conflicts, b_dtk_parallels, b_dtk_same_pass, 
            b_dtk_updates, b_dtk_bumps ) ;

#ifdef PRINT_DIAG
        printf("AFTER dtkm_process_dtk() return_code = %d\n", return_code ) ;
        if ( return_code < 0 )
            printf("********  %s\n", DTKM_ERROR_MESSAGE( return_code )  ) ;
        else
        {
            switch( return_code )
            {
            case DTKM_DTK_ACCEPTED :
                printf("------- DTKM_DTK_ACCEPTED\n" ) ;
                break ;
            case DTKM_DTK_PROPOSAL_CONFLICT :
                printf("------- DTKM_DTK_PROPOSAL_CONFLICT\n" ) ;
                break ;
            case DTKM_DTK_PROPOSAL_REJ :
                printf("------- DTKM_DTK_PROPOSAL_REJ\n" ) ;
                break ;
            case DTKM_DTK_DELETED_OK :
                printf("------- DTKM_DTK_DELETED_OK\n" ) ;
                break ;
            default:
                printf("############################# UNKNOWN RETURN CODE\n" ) ;
                break ;
            }
        }
        printf("RESULT DTK:\n" ) ;
        dtkm_print(stdout, b_result_dtk ) ;
        printf("SAT DOWN TIMES:\n" ) ;
        dtkm_print_list(stdout, b_dtk_sat_down_times ) ;
        printf("ANTENNA DOWN TIMES:\n" ) ;
        dtkm_print_antenna_down_times_list(stdout, b_antenna_down_times ) ;
        printf("CONCURS:\n" ) ;
        dtkm_print_list(stdout, b_dtk_concurs ) ;
        printf("SIMILARS:\n" ) ;
        dtkm_print_list(stdout, b_dtk_similars ) ;
        printf("CONFLICTS:\n" ) ;
        dtkm_print_list(stdout, b_dtk_conflicts ) ;
        printf("PARALLELS:\n" ) ;
        dtkm_print_list(stdout, b_dtk_parallels ) ;
        printf("SAME PASS:\n" ) ;
        dtkm_print_list(stdout, b_dtk_same_pass ) ;
        printf("UPDATES:\n" ) ;
        dtkm_print_list(stdout, b_dtk_updates ) ;
        printf("BUMPS:\n" ) ;
        dtkm_print_list(stdout, b_dtk_bumps ) ;
        printf("MASTER UPDATES:\n" ) ;
        dtkm_print_list(stdout, dtk_updates ) ;
#endif

        if ( return_code < 0 )
        {
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( b_dtk_sat_down_times ) ;
            DEL_LIST( b_antenna_down_times ) ;
            DEL_LIST( b_dtk_concurs ) ;
            DEL_LIST( b_dtk_similars ) ;
            DEL_LIST( b_dtk_conflicts ) ;
            DEL_LIST( b_dtk_parallels ) ;
            DEL_LIST( b_dtk_same_pass ) ;
            DEL_LIST( b_dtk_updates ) ;
            DEL_LIST( b_dtk_bumps ) ;
            free_db_record( b_result_dtk ) ;
            free_db_record( b_dtk_proposal ) ;
            DEL_LIST( dtk_bumps ) ;
            return return_code ;
        }

        /* check for innappropriate return codes for a bumped data-take: */
        switch( return_code )
        {
        case DTKM_DTK_DELETED_OK :
        case DTKM_DTK_PROPOSAL_REJ :
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( b_dtk_sat_down_times ) ;
            DEL_LIST( b_antenna_down_times ) ;
            DEL_LIST( b_dtk_concurs ) ;
            DEL_LIST( b_dtk_similars ) ;
            DEL_LIST( b_dtk_conflicts ) ;
            DEL_LIST( b_dtk_parallels ) ;
            DEL_LIST( b_dtk_same_pass ) ;
            DEL_LIST( b_dtk_updates ) ;
            DEL_LIST( b_dtk_bumps ) ;
            free_db_record( b_result_dtk ) ;
            free_db_record( b_dtk_proposal ) ;
            DEL_LIST( dtk_bumps ) ;
            return DTKM_ERROR_BUMPED_DTK_WAS_DELETED_OR_REJECTED ;
        case DTKM_DTK_PROPOSAL_CONFLICT :
            /* 
            -- the dtk_bumped was updated from BMP to CON.  
            -- it will be in the b_dtk_updates list.  
            */
            break ;
        case DTKM_DTK_ACCEPTED_INHERITANCE :
            /* FALLTHROUGH */
        case DTKM_DTK_ACCEPTED :
            /* OK.  continue processing.  */
            break ;
        default :
            /* free allocated memory.  */
            if ( input_antenna_down_times == NULL )
                DEL_LIST( antenna_down_times ) ;
            if ( input_dtk_sat_down_times == NULL )
                DEL_LIST( dtk_sat_down_times ) ;
            if ( input_dtk_concurs == NULL )
                DEL_LIST( dtk_concurs ) ;
            if ( input_dtk_similars == NULL )
                DEL_LIST( dtk_similars ) ;
            if ( input_dtk_conflicts == NULL )
                DEL_LIST( dtk_conflicts ) ;
            if ( input_dtk_updates == NULL )
                DEL_LIST( dtk_updates ) ;
            DEL_LIST( b_dtk_sat_down_times ) ;
            DEL_LIST( b_antenna_down_times ) ;
            DEL_LIST( b_dtk_concurs ) ;
            DEL_LIST( b_dtk_similars ) ;
            DEL_LIST( b_dtk_conflicts ) ;
            DEL_LIST( b_dtk_parallels ) ;
            DEL_LIST( b_dtk_same_pass ) ;
            DEL_LIST( b_dtk_updates ) ;
            DEL_LIST( b_dtk_bumps ) ;
            free_db_record( b_result_dtk ) ;
            free_db_record( b_dtk_proposal ) ;
            DEL_LIST( dtk_bumps ) ;
            return DTKM_ERROR_UNKNOWN_NORMAL_RETURN_CODE_FROM_DTKM_PROCESS_DTK ;
        }

        /*
        -- MOVE any dtk updates to the output dtk_updates list.  
        -- this will take all of the members out of the b_dtk_updates list and 
        -- append them to dtk_updates.  
        */
        if ( NUMELTS( b_dtk_updates ) > 0 )
        {
            list_check = db_record_llist_move( dtk_updates, b_dtk_updates ) ;
            if ( list_check != dtk_updates )
            {
                /* free allocated memory.  */
                if ( input_antenna_down_times == NULL )
                    DEL_LIST( antenna_down_times ) ;
                if ( input_dtk_sat_down_times == NULL )
                    DEL_LIST( dtk_sat_down_times ) ;
                if ( input_dtk_concurs == NULL )
                    DEL_LIST( dtk_concurs ) ;
                if ( input_dtk_similars == NULL )
                    DEL_LIST( dtk_similars ) ;
                if ( input_dtk_conflicts == NULL )
                    DEL_LIST( dtk_conflicts ) ;
                if ( input_dtk_updates == NULL )
                    DEL_LIST( dtk_updates ) ;
                DEL_LIST( b_dtk_sat_down_times ) ;
                DEL_LIST( b_antenna_down_times ) ;
                DEL_LIST( b_dtk_concurs ) ;
                DEL_LIST( b_dtk_similars ) ;
                DEL_LIST( b_dtk_conflicts ) ;
                DEL_LIST( b_dtk_parallels ) ;
                DEL_LIST( b_dtk_same_pass ) ;
                DEL_LIST( b_dtk_updates ) ;
                DEL_LIST( b_dtk_bumps ) ;
                free_db_record( b_result_dtk ) ;
                free_db_record( b_dtk_proposal ) ;
                DEL_LIST( dtk_bumps ) ;
                return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
            }
        }

        /* 
        -- MOVE any bumped data-takes to the current list we are working on. 
        -- this will take all of the members out of one list and 
        -- append them to another.  
        */
        if ( NUMELTS( b_dtk_bumps ) > 0 )
        {
            list_check = db_record_llist_move( dtk_bumps, b_dtk_bumps ) ;
            if ( list_check != dtk_bumps )
            {
                /* free allocated memory.  */
                if ( input_antenna_down_times == NULL )
                    DEL_LIST( antenna_down_times ) ;
                if ( input_dtk_sat_down_times == NULL )
                    DEL_LIST( dtk_sat_down_times ) ;
                if ( input_dtk_concurs == NULL )
                    DEL_LIST( dtk_concurs ) ;
                if ( input_dtk_similars == NULL )
                    DEL_LIST( dtk_similars ) ;
                if ( input_dtk_conflicts == NULL )
                    DEL_LIST( dtk_conflicts ) ;
                if ( input_dtk_updates == NULL )
                    DEL_LIST( dtk_updates ) ;
                DEL_LIST( b_dtk_sat_down_times ) ;
                DEL_LIST( b_antenna_down_times ) ;
                DEL_LIST( b_dtk_concurs ) ;
                DEL_LIST( b_dtk_similars ) ;
                DEL_LIST( b_dtk_conflicts ) ;
                DEL_LIST( b_dtk_parallels ) ;
                DEL_LIST( b_dtk_same_pass ) ;
                DEL_LIST( b_dtk_updates ) ;
                DEL_LIST( b_dtk_bumps ) ;
                free_db_record( b_result_dtk ) ;
                free_db_record( b_dtk_proposal ) ;
                DEL_LIST( dtk_bumps ) ;
                return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
            }
        }

        /* 
        -- now we are ready to process the next data-take in 
        -- list of bumps.  
        -- prepare by deleting all of the members from the lists 
        -- for re-use by dtkm_process_dtk():
        */
        DEL_ALL( b_dtk_sat_down_times ) ;
        DEL_ALL( b_antenna_down_times ) ;
        DEL_ALL( b_dtk_concurs ) ;
        DEL_ALL( b_dtk_similars ) ;
        DEL_ALL( b_dtk_conflicts ) ;
        DEL_ALL( b_dtk_parallels ) ;
        DEL_ALL( b_dtk_same_pass ) ;
        DEL_ALL( b_dtk_updates ) ;
        DEL_ALL( b_dtk_bumps ) ;

        /* also free the dtk_bumped record.  */
        free_db_record( dtk_bumped ) ;

        /*
        -- now we are ready to handle the 
        -- next bumped dtk if any.  
        */
    }

    /*
    -- NUMELTS( dtk_bumps ) == 0 
    -- processing is at an end.  
    */

    /* free allocated memory.  */
    if ( input_antenna_down_times == NULL )
        DEL_LIST( antenna_down_times ) ;
    if ( input_dtk_sat_down_times == NULL )
        DEL_LIST( dtk_sat_down_times ) ;
    if ( input_dtk_concurs == NULL )
        DEL_LIST( dtk_concurs ) ;
    if ( input_dtk_similars == NULL )
        DEL_LIST( dtk_similars ) ;
    if ( input_dtk_conflicts == NULL )
        DEL_LIST( dtk_conflicts ) ;
    if ( input_dtk_updates == NULL )
        DEL_LIST( dtk_updates ) ;
    DEL_LIST( b_dtk_sat_down_times ) ;
    DEL_LIST( b_antenna_down_times ) ;
    DEL_LIST( b_dtk_concurs ) ;
    DEL_LIST( b_dtk_similars ) ;
    DEL_LIST( b_dtk_conflicts ) ;
    DEL_LIST( b_dtk_parallels ) ;
    DEL_LIST( b_dtk_same_pass ) ;
    DEL_LIST( b_dtk_updates ) ;
    DEL_LIST( b_dtk_bumps ) ;
    DEL_LIST( dtk_bumps ) ;

    free_db_record( b_result_dtk ) ;
    free_db_record( b_dtk_proposal ) ;

    return original_dtk_proposal_return_code  ;
    
}

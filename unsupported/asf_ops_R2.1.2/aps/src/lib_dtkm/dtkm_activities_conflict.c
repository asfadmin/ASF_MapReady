#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_activities_conflict.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_activities_conflict.c	5.2 98/01/15 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_activities_conflict.c"

#include "dtkm.h"    /* for the usual dtkm stuff.    */
#include "db_dtk.h"  /* for the dtk relation.  */
#include "db_activities.h"  /* for the activities relation.  */
#include "db_activ_conf.h"  /* for the activ_conf relation.  */

#include <string.h>         /* for strcmp strncmp strlen */



/*==============================================================================
Function:   dtkm_activity_id

Description:    determine the activity number for the data-take.  

Parameters:     
    DB_RECORD   **dtk_rec    input data-take

Returns:        
    int 

    > 0    the activity id obtained from the activities relation.  

    < 0    an error was encountered.
            DTKM_ERROR_ACTIVITY_NOT_FOUND           
            DTKM_ERROR_ACTIVITY_ILLEGAL    
            DTKM_ERROR_ACTIVITY_GT_1_RECORD

Creator:        Lawrence Stevens

Creation Date:  Mon Nov  6 12:58:38 PST 1995

Notes:      

==============================================================================*/
int dtkm_activity_id( DB_RECORD   **dtk_rec)    /* input data-take  */
{

    /* 
    -- static pointer to hold onto the db records for 
    -- use during the whole run.  
    */
    static llist        *activities_list = NULL ;
    int                 id = 0;
    DB_RECORD           **activities_rec ;
    cursor              activities_list_ptr ;

    /*
    -- if not already done so, get the entire activities relation into 
    -- the llist.  then, for each call, go through the list 
    -- instead of retrieving from the database.  
    */
    if ( activities_list == NULL )
    {

        activities_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(ACTIVITIES), "", "", APS_CDEFS(ACTIVITIES), ALL_COLS ) ;

        if ( activities_list == NULL )
            return DTKM_ERROR_DB_QUERY_FAILED ;

        if ( NUMELTS( activities_list ) == 0 )
            return DTKM_ERROR_NO_RECS_IN_ACTIVITIES_RELATION ;
    }

    /*
    -- go through each activities_rec on the list to 
    -- find a match on the input: 
    -- ACTIVITIES_OBJ       vs DTK_SAT, 
    -- ACTIVITIES_ACTY      vs DTK_ACTID [first 3 characters only], and 
    -- ACTIVITIES_TRANSID   vs DTK_TRANSID
    */
    for ( 
        activities_rec = (DB_RECORD **) FIRST(activities_list, 
            activities_list_ptr);
        activities_rec ;
        activities_rec = (DB_RECORD **) NEXT(activities_list, 
            activities_list_ptr)
        )
    {
        if (strcmp(CAST_ACTIVITIES_OBJ activities_rec[ACTIVITIES_OBJ], 
                   CAST_DTK_SAT dtk_rec[DTK_SAT] ) != 0 )
        {
            /* satellites differ:  not a match.  */
            continue ;
        }
        if (strcmp(CAST_ACTIVITIES_TRANSID activities_rec[ACTIVITIES_TRANSID], 
                   CAST_DTK_TRANSID dtk_rec[DTK_TRANSID] ) != 0     )
        {
            /* transid differs:  not a match.  */
            continue ;
        }
        if( dtkm_is_a_downlink( dtk_rec ) == TRUE )
        {
            if( strcmp(CAST_ACTIVITIES_ACTY activities_rec[ACTIVITIES_ACTY], 
                       CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) != 0 )
            {
                /* this is a downlink and ACTID values do not match.  */
                continue ;
            }
        }
        else
        {
            /* 
            -- this is not a downlink; it is an observation.  
            -- activities.acty is of the old form:  "xxy" where 
            -- xx = RE (recording) or RO (realtime observation)
            --      which match the first 2 characters of dtk.actid.  
            --  y = first character of dtk.sensor  
            -- Examples:  RES (real-time SAR) and ROS (realtime observation SAR)
            -- compare activities values with dtk values.  
            */
            if(strncmp(CAST_ACTIVITIES_ACTY activities_rec[ACTIVITIES_ACTY], 
                       CAST_DTK_ACTID dtk_rec[DTK_ACTID], 2 ) != 0 
            || strncmp((CAST_ACTIVITIES_ACTY activities_rec[ACTIVITIES_ACTY])+2,
                       CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 1 ) != 0 )
            {
                /* at least one value does not a match.  */
                continue ;
            }
        }
        /* 
        -- OK!  we have a match of activities record 
        -- vs dtk record.  
        */
        /* 
        -- check for more than one activity 
        -- id for this data-take  
        */
        if (id != 0 )
            return DTKM_ERROR_ACTIVITY_GT_1_RECORD ;

        /* save activity id from the record.  */
        id = CAST_ACTIVITIES_N_ACTY activities_rec[ACTIVITIES_N_ACTY] ;

        /* check activity id from database  */
        if (id <= 0)
            return DTKM_ERROR_ACTIVITY_ILLEGAL ;
    }

    /* check if not found.    */
    if ( id == 0 )
        return DTKM_ERROR_ACTIVITY_NOT_FOUND ;

    /* no errors.  return the activity id.  */
    return id ;
}

/*==============================================================================
Function:   activities_conflict

Description:    from two activity id's (from the activities relation), 
determine if they conflict.  

Parameters:     
    int         activity_id_1   
    int         activity_id_2   

Returns:     
    >= 0   normal.
        DTKM_ACTIVITIES_DO_NOT_CONFLICT     
        DTKM_ACTIVITIES_SHOULD_BE_COMBINED 
        DTKM_ACTIVITIES_CONFLICT          
        DTKM_ACTIVITIES_IDENTICAL        

Creator:        Lawrence Stevens

Creation Date:  Mon Nov  6 12:58:49 PST 1995

Notes:      
==============================================================================*/
int activities_conflict(
    int         activity_id_1,
    int         activity_id_2 )
{
    int             smaller_code ;
    int             larger_code ;
    int             conf_code ;

    DB_RECORD       **activ_conf_rec ;
    llist           *activ_confs = NULL ;
    cursor          activ_conf_rec_ptr ;

    /* check for identical activities  */
    if (activity_id_1 == activity_id_2)
        return DTKM_ACTIVITIES_IDENTICAL ;

    /* set up retrieve using the two activity codes  */
    smaller_code = MIN(activity_id_1, activity_id_2 ) ;
    larger_code  = MAX(activity_id_1, activity_id_2 ) ;

    sprintf(where_clause, "where %s = %d and %s = %d",
        APS_COL(ACTIV_CONF, ACTIV_CONF_N_ACTY), smaller_code,
        APS_COL(ACTIV_CONF, ACTIV_CONF_M_ACTY), larger_code ) ;

    activ_confs = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(ACTIV_CONF),
        where_clause, NULL, APS_CDEFS(ACTIV_CONF), ALL_COLS) ;

    if ( activ_confs == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    /* check for no conflict - no record found.  */
    if ( NUMELTS( activ_confs ) == 0 )
    {
        DEL_LIST( activ_confs ) ;
        return DTKM_ACTIVITIES_DO_NOT_CONFLICT ;
    }

    activ_conf_rec = (DB_RECORD **) FIRST(activ_confs, activ_conf_rec_ptr) ;

    /* save the conflict code value before freeing the memory  */
    conf_code = CAST_ACTIV_CONF_CONF_STATUS 
        activ_conf_rec[ACTIV_CONF_CONF_STATUS] ;

    /* no errors.  return the conflict code  */
    DEL_LIST( activ_confs ) ;
    return ( conf_code ) ;

}


/*==============================================================================
Function:   dtkm_activities_conflict

Description:    determine the conflict status of two data-takes. 

Parameters:     
    DB_RECORD   **proposed_dtk
    DB_RECORD   **db_dtk

Returns:     
    >= 0   normal.
        DTKS_PARALLEL
        DTKS_SHOULD_BE_COMBINED 
        DTKS_CONFLICT          
        DTKS_CONCUR
        DTKS_SIMILAR
    < 0    error.  
        DB_ILLEGAL_CONFLICT_CODE

Creator:        Lawrence Stevens

Creation Date:  Mon Nov  6 12:59:24 PST 1995

Notes:      
==============================================================================*/
int dtkm_activities_conflict(
    DB_RECORD   **proposed_dtk,     /* compare the proposal with the db dtk */
    DB_RECORD   **db_dtk ) 
{
    int         proposed_act_id ;   /* the activity id of the proposed dtk  */
    int         db_dtk_act_id ;    /* the activity id of a retrieved dtk   */
    int         conflict_status ;

        /*
        -- obtain activity id for the proposed data-take, found in
        -- the activities relation
        */
    proposed_act_id = dtkm_activity_id(proposed_dtk);
    if (proposed_act_id <= 0)
        return proposed_act_id ;

        /*
        -- to check for the conflict status, first obtain activity id for
        -- the retrieved data-take, found in the activities relation
        */
    db_dtk_act_id = dtkm_activity_id(db_dtk);
    if (db_dtk_act_id <= 0)
        return db_dtk_act_id ;


        /* 
        -- this conflict status is a check vs the activity id's from a 
        -- matrix in the database it is a first-level check.  
        -- after that, there may be detailed checking vs the dtk records.  
        */
    conflict_status = activities_conflict( proposed_act_id, db_dtk_act_id) ;
    switch(conflict_status)
    {

        case DTKM_ACTIVITIES_DO_NOT_CONFLICT :
            return DTKS_PARALLEL ;
            /*  break ;  */

        case DTKM_ACTIVITIES_SHOULD_BE_COMBINED :
            /*
            -- the activies should be combined.  but if the data-takes 
            -- have the same actid activity code and different sensor, 
            -- then they cannot be combined at all; they conflict.  
            -- 
            -- for example:
            -- actid's are equal and sensors not equal  ==> CONFLICT:  
            --   sat   actid   transid   sensor
            --   R1     RTS      F1       SR1
            --   R1     RTS      F2       SR2
            -- In this case, the actid's are the same and the conflict 
            -- status would indicate a combine.
            -- but since the sensors are different, they cannot be combined.
            -- the activities_conflict.c routine does not look at the 
            -- sensor; that happens right here.  
            -- 
            -- another example:
            -- actid's are not equal and sensors also not equal  ==> COMBINE:  
            --   sat   actid   transid   sensor
            --   J1     RTO      F2       OPS
            --   J1     RTV      F2       OVN
            -- here, the actid's are different, so that when 
            -- the activities_conflict.c routine returns a combine status, 
            -- this status is valid.  we don't even look at the sensor here. 
            */ 

            if (strcmp(CAST_DTK_ACTID proposed_dtk[DTK_ACTID],
                       CAST_DTK_ACTID       db_dtk[DTK_ACTID] ) == 0 
            &&  strcmp(CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR],
                       CAST_DTK_SENSOR       db_dtk[DTK_SENSOR] ) != 0 )
            {
                /*
                -- same activity codes:  both RTS or both RTO, etc.  
                -- but different 
                -- sensors.  SR1 v SR2, for example.  cannot combine. 
                */
                return DTKS_CONFLICT ;
            }
            else
                return DTKS_SHOULD_BE_COMBINED ;

            /*  break ;  this is commented to remove a warning.  */

        case DTKM_ACTIVITIES_CONFLICT :
            return DTKS_CONFLICT ;
            /*  break ;  this is commented to remove a warning.  */

        case DTKM_ACTIVITIES_IDENTICAL :
                /*
                -- the activies are identical.  but if the data-takes 
                -- have a different sensor mode, then they are not identical 
                -- at all; they conflict.  
                -- 
                -- for example:
                -- actid's are equal and sensor modes not equal  ==> CONFLICT:  
                --   sat   actid   transid        sensor
                --   R1     RTS      F1            SR1
                --   R1     RTS      F1            SR2
                -- In this case, the actid's are the same and the conflict 
                -- status would indicate a concur or a similar.
                --
                -- BUT since the sensor modes are different, they really do 
                -- conflict.  SR1 and SR2 are both SAR sensors, but with 
                -- different modes.  
                -- the activities_conflict.c routine does not look at the 
                -- sensor; that happens right here, in the following code.  
                -- 
                */ 

            if ( strcmp( CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR], 
                         CAST_DTK_SENSOR       db_dtk[DTK_SENSOR]) != 0 )
                return DTKS_CONFLICT ;

                /*
                -- NOW the activities are identical.  sat, sensor, 
                -- actid, transid.
                --
                -- Now check for either the data-base confirming the 
                -- prior existence, in the database, of the proposed 
                -- data-take ( DTKS_CONCUR )
                -- OR that the data-base data-take is merely similar 
                -- to the proposed data-take.  
                --
                -- checking for a match between the proposed and 
                -- retrieved dtks.  by  this we mean that they 
                -- have the same "names", i.e., either they have the 
                -- same dtkid or fadtkid values.
                --
                -- if the dtks match, the retrieved dtk is a "concurring" dtk
                -- if they don't match, the retrieved dtk is a "similar" dtk
                --
                -- NOTE:  at this point, the proposed data-take might 
                -- have neither a dtkid nor an fadtkid.  The retrieved 
                -- data-take will definitely have at least a dtkid.  
                --
                -- first compare the data-take id's, if the proposed dtk has 
                -- one. 
                */

            if ( CAST_DTK_DTKID proposed_dtk[DTK_DTKID] > 0 )
            {
#ifdef PRINT_DIAG
                PRINT_DIAG("dtkm_activities_conflict: proposed dtkid = %d\n",
                    CAST_DTK_DTKID proposed_dtk[DTK_DTKID]  ) ;
                PRINT_DIAG("dtkm_activities_conflict: db_dtk dtkid = %d\n",
                    CAST_DTK_DTKID db_dtk[DTK_DTKID]  ) ;
#endif
                /* there is a dtkid id for the proposed dtk. */
                if ( CAST_DTK_DTKID proposed_dtk[DTK_DTKID] ==
                     CAST_DTK_DTKID       db_dtk[DTK_DTKID] )
                    return DTKS_CONCUR ;
                else
                    return DTKS_SIMILAR ;
            }
            else if ( (int) strlen( CAST_DTK_FADTKID proposed_dtk[DTK_FADTKID] )
                       > 0 
                 && strncmp( CAST_DTK_FADTKID proposed_dtk[DTK_FADTKID], " ",1)
                    != 0 )
            {
#ifdef PRINT_DIAG
                PRINT_DIAG("dtkm_activities_conflict: proposed fadtkid = >%s<\n",
                    CAST_DTK_FADTKID proposed_dtk[DTK_FADTKID]  ) ;
                PRINT_DIAG("dtkm_activities_conflict: db_dtk fadtkid = >%s<\n",
                    CAST_DTK_FADTKID db_dtk[DTK_FADTKID]  ) ;
#endif
                /* there is an fadtkid id for the proposed dtk. */
                if ( 
                strcmp( CAST_DTK_FADTKID proposed_dtk[DTK_FADTKID], 
                        CAST_DTK_FADTKID       db_dtk[DTK_FADTKID]) == 0 )
                    return DTKS_CONCUR ;
                else
                    return DTKS_SIMILAR ;
            }
            else
            {
                /* the proposed data-take has neither dtkid nor fadtkid. */
                return DTKS_SIMILAR ;
            }
            /*  break ;  this is commented to remove a warning.  */
        case DTKM_ERROR_DB_QUERY_FAILED :
            return DTKM_ERROR_DB_QUERY_FAILED ;
            /*  break ;  this is commented to remove a warning.  */
        default:
            return DTKM_ERROR_ILLEGAL_CONF_STATUS_IN_ACTIV_CONF_RELATION ;
            /*  break ;  this is commented to remove a warning.  */
    }  /* end of switch   */
}

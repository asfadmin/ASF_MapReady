#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_default_values.c

External Functions Defined:
    dtkm_default_values()

File Scope Functions:
    dtkm_e1_default_values()
    dtkm_e2_default_values()
    dtkm_j1_default_values()
    dtkm_r1_default_values()
    dtkm_a1_default_values()
    dtkm_quickook_values()
    
External Variables Defined:
    


Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_default_values.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_default_values.c"

#include <string.h>       /* for strcmp, strncmp argument checks  */
#include <math.h>         /* for fabs, absolute value...          */
#include <stdio.h>        /* for fprintf etc...                   */


#include "dtkm.h"

/* FOR DATABASE TABLES        */
#include "db_activ_conf.h"     /* for activ_conf table      */
#include "db_activities.h"     /* for activities table      */
#include "db_dtk.h"            /* for dtk table             */
#include "db_dar.h"            /* for dar table             */
#include "db_satsensor.h"      /* for satsensor table         */
#include "db_station.h"        /* for station table         */

#include "timeconv.h"
#include "phase_utilities.h"   /* for check_rev_asftimes    */

/* FOR LAT/LON FIELDS        */
#include "check_lat_lon.h"     /* for check_latitude() and check_longitude() */


/*==============================================================================
Function:       dtkm_quicklook_values()

Description:    looks at darid to see try to set the 
                science_quicklook field.  if not found, 
                it is set to N.  the planner_quiclook is 
                set to N.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec  7 19:22:52 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_quicklook_values( 
    DB_RECORD   **dtk_proposal, 
    DB_RECORD   **result_dtk )
{

    llist   *dar_list = NULL ;
    DB_RECORD   **dar_rec = NULL ;
    cursor      dar_list_ptr ;

    /*  quick check for errors   */
    if (dtk_proposal == NULL)
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if (result_dtk == NULL)
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    db_copy_record ( APS_CDEFS(DTK), result_dtk, dtk_proposal ) ;

    if (CAST_DTK_PLANNER_QUICKLOOK result_dtk[DTK_PLANNER_QUICKLOOK] != 'N'
    &&  CAST_DTK_PLANNER_QUICKLOOK result_dtk[DTK_PLANNER_QUICKLOOK] != 'Y')
    {
        CAST_DTK_PLANNER_QUICKLOOK result_dtk[DTK_PLANNER_QUICKLOOK] = 'N' ;
    }

    if ( CAST_DTK_DARID result_dtk[DTK_DARID] == 0 )
    {
        /* there is no dar for this right now:  */
        CAST_DTK_SCIENCE_QUICKLOOK result_dtk[DTK_SCIENCE_QUICKLOOK] = 'N' ;
        return TRUE ;
    }

    /* there is a darid; now find the dar record.  */
    sprintf( where_clause, "where %s = %ld ", 
        APS_COL(DAR, DAR_DARID), 
        CAST_DTK_DARID result_dtk[DTK_DARID] ) ;

    dar_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(DAR), where_clause, NULL, APS_CDEFS(DAR), ALL_COLS ) ;

    if ( dar_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    if ( NUMELTS( dar_list ) == 0 )
    {
        DEL_LIST( dar_list ) ;
        return DTKM_ERROR_DAR_NOT_FOUND_FROM_DTK_DARID ;
    }

    if ( NUMELTS( dar_list ) != 1 )
    {
        DEL_LIST( dar_list ) ;
        return DTKM_ERROR_NE_1_DARS_FOUND_FROM_DTK_DARID ;
    }

    dar_rec = (DB_RECORD **) FIRST(dar_list, dar_list_ptr) ;

    /* assign the science_quicklook from the dar rec:   */
    CAST_DTK_SCIENCE_QUICKLOOK result_dtk[DTK_SCIENCE_QUICKLOOK] = 
        CAST_DAR_QUICKLOOK dar_rec[DAR_QUICKLOOK] ;

    /* clean up.  */
    DEL_LIST( dar_list ) ;

    return TRUE ;

}


/*==============================================================================
Function:   dtkm_e1_default_values

Description:  fills in E1 default values if not already there.  
1.  actid - ROBESF or RDLESF
2.  transid.  - '00'
3.  for E1, e1_pvalue = 2

Parameters:     
    DB_RECORD    **dtk_record  record to process.

Returns:    
    int
    >= 0  :
          DTKM_DEFAULT_VALUES_OK

    < 0   :  ERROR
                DTKM_ERROR_NULL_RECORD
                DTKM_ERROR_SAT_NOT_E1

Creator:    Lawrence Stevens

Creation Date:  03/29/1995

Notes:      
    SAMPLE:  dtkm_e1_default_values(dtk_record ) ;
==============================================================================*/
static int dtkm_e1_default_values( DB_RECORD **dtk_rec )
{

    /*  check for errors   */
    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E1" ) != 0  )
        return DTKM_ERROR_SAT_NOT_E1 ;

    /* 
    -- value is from the RGS interface WOS file 
    -- format, 11/11/94, page 0-6 
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 6 )
    {
        if ( strcmp( DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
                     CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0 )
        {
            /* use the same value as sensor.  */
            strcpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;
            strcat( CAST_DTK_ACTID dtk_rec[DTK_ACTID], "ESF" ) ;
        }
        else
        {
            /* Realtime Observation:   */
            strcpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    DTKM_ACTID_REALTIME_OBSERVATION_CODE ) ;
            strcat( CAST_DTK_ACTID dtk_rec[DTK_ACTID], "ESF" ) ;
        }
    }
    strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "00" ) ;

    return DTKM_DEFAULT_VALUES_OK ;
        
}


/*==============================================================================
Function:   dtkm_e2_default_values

Description:  fills in E2 default values if not already there.  
1.  actid - RTSESF
2.  transid.  - '00'
3.  for E2, e2_pvalue = 2

Parameters:     
    DB_RECORD    **dtk_record  record to process.

Returns:    
    int
    >= 0  :
          DTKM_DEFAULT_VALUES_OK

    < 0   :  ERROR
                DTKM_ERROR_NULL_RECORD
                DTKM_ERROR_SAT_NOT_E2

Creator:    Lawrence Stevens

Creation Date:  03/29/1995

Notes:      
    SAMPLE:  dtkm_e2_default_values(dtk_record ) ;
==============================================================================*/
static int dtkm_e2_default_values( DB_RECORD **dtk_rec )
{

    /*  check for errors   */
    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E2" ) != 0  )
        return DTKM_ERROR_SAT_NOT_E2 ;

    /*
    -- values are from the RGS interface WOS 
    -- format, 11/11/94, page 0-6
    -- the ESF means ESA and ASF.  
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 6 )
    {
        if ( strcmp( DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
                     CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0 )
        {
            strcpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;
            strcat( CAST_DTK_ACTID dtk_rec[DTK_ACTID], "ESF" ) ;
        }
        else
        {
            /* Realtime Observation:   */
            strcpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    DTKM_ACTID_REALTIME_OBSERVATION_CODE ) ;
            strcat( CAST_DTK_ACTID dtk_rec[DTK_ACTID], "ESF" ) ;
        }
    }
    strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "00" ) ;

    return DTKM_DEFAULT_VALUES_OK ;
        
}


/*==============================================================================
Function:   dtkm_j1_default_values

Description:  fills in several j1 default values if not already there.  
1.  the sensor in actid - character 3, from the sensor 1st character.  
2.  the agency in actid - the last 3 bytes
3.  transid.  
4.  for E1, e1_pvalue.

Parameters:     
    DB_RECORD    **dtk_record  record to process.

Returns:    
    int
    >= 0  :
          DTK_DEFAULTS_OK

    < 0   :  ERROR
                DTKM_ERROR_NULL_RECORD
                DTKM_ERROR_SAT_NOT_J1
                DTKM_ERROR_FIELD_ACTID_NOT_SET

Creator:    Lawrence Stevens

Creation Date:  03/16/1995

Notes:      
    SAMPLE:  dtkm_j1_default_values(dtk_record ) ;
==============================================================================*/
static int dtkm_j1_default_values( DB_RECORD **dtk_rec )
{

    /*  check for errors   */
    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "J1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_J1 ;

    /* 
    -- values are from the RGS interface WOS 
    -- format, 11/11/94, page 0-6 
    */

    /* 
    -- if sensor is dump, "NAS" is the default agency value, 
    -- the NAS means NASDA only.  this is the current preferred 
    -- default for NASDA J1 dump activities.
    -- NOTE: The following will NOT change the agency part if it is
    --       already filled in.
    */
    if ( strcmp ( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
            DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0 )
        if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) <= 3 )
        {
            strcpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) ;
            strcat( CAST_DTK_ACTID dtk_rec[DTK_ACTID], "NAS" ) ;
        }
        else
            strncpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
                     strlen(DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE) ) ;

    /* 
    -- if sensor is a realtime downlink, "ASF" is the default 
    -- agency value, this is the current preferred 
    -- default for NASDA J1 dump activities.
    -- NOTE: The following will NOT change the agency part if it is
    --       already filled in.
    */
    if ( strcmp ( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
            DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) == 0 )
        if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) <= 3 )
        {
            strcpy(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;
            strcat(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], "ASF" ) ;
        }
        else
            strncpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
                     strlen(DTKM_SENSOR_REALTIME_DOWNLINK_CODE) ) ;

    /* 
    -- if this field is not set here, we cannot guess 
    -- ROB, REC, DMP, or RLT here.  
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 3 )
        return DTKM_ERROR_FIELD_ACTID_NOT_SET ;

    /* 
    -- if not already set, put a value in for the agency.   
    -- These are the preferred defaults for NASDA activities: 
    -- ASF only for J1 realtime, NASDA only for J1 dump.
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 6 )
    {
        /* try to complete the ACTID field.  */
        if (strcmp(CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0 )
        {
            strcpy(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) ;
            strcpy(  (CAST_DTK_ACTID dtk_rec[DTK_ACTID])+3, "NAS" ) ;
        }
        else if (strncmp(CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                DTKM_ACTID_RECORDING_OBSERVATION_CODE,
                strlen(DTKM_ACTID_RECORDING_OBSERVATION_CODE)) == 0 )
        {
            strcpy(  (CAST_DTK_ACTID dtk_rec[DTK_ACTID])+3, "NAS" ) ;
        }
        else
            strcpy(  (CAST_DTK_ACTID dtk_rec[DTK_ACTID])+3, "ASF" ) ;
    }

    if ( dtkm_is_a_downlink( dtk_rec ) )
    {
        /* if not already set, put a value in for the transid  */
        if ( (int) strlen( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID] ) < 2 ) 
        {
            /* for downlink.  */
            strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "F1" ) ;
        }
    }

    return DTKM_DEFAULT_VALUES_OK ;
}



/*==============================================================================
Function:   dtkm_r1_default_values

Description:  fills in R1 default values if not already there.  
1.  the sensor in actid - character 3, from the sensor 1st character.  
2.  the agency in actid - the last 3 bytes
3.  transid.  
4.  for E1, e1_pvalue.

Parameters:     
    DB_RECORD    **dtk_record  record to process.

Returns:    
    int
    >= 0  :
          DTKM_DEFAULT_VALUES_OK

    < 0   :  ERROR
                DTKM_ERROR_NULL_RECORD
                DTKM_ERROR_SAT_NOT_R1
                DTKM_ERROR_FIELD_ACTID_NOT_SET

Creator:    Lawrence Stevens

Creation Date:  03/16/1995

Notes:      

     From the ASF-SIS in the WOS, the TRANS_ID codes are F3 and F4:
 
     code  description  frequency   bit rate
     ----  -----------  ---------   --------
     F3    real-time    8105 Mhz    105 Mbps
     F4    playback     8230 Mhz     85 Mbps

SAMPLE:  dtkm_r1_default_values(dtk_record ) ;
==============================================================================*/
static int dtkm_r1_default_values( DB_RECORD **dtk_rec )
{

/* the ASF means:  agency ASF only.    */
#define R1_DEFAULT_AGENCY  "ASF"

    /*  check for errors   */
    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;
    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_R1 ;

    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 3 )
        return DTKM_ERROR_FIELD_ACTID_NOT_SET ;

    /*
    -- values are from the RGS interface, WOS file from 11/11/94 
    -- paper page 0-6.  
    */

    /* 
    -- if sensor is dump or real time downlink, this makes the 
    -- actid value clear.  
    -- ALSO:  
    -- if real-time, put in transid = F3.  
    -- if playback (dump), put in transid = F4.  
    -- else put in transid = 00; must be an observation.   
    */
    if ( strcmp ( DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
                     CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0 )
    {
        /* tape dump F4 */
        strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "F4" ) ;

        /* the ASF means:  agency ASF only.    */
        if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 3 )
        {
            strcpy(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) ;
            strcat(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], R1_DEFAULT_AGENCY ) ;
        }
        else if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) == 3 )
        {
            strcat( CAST_DTK_ACTID dtk_rec[DTK_ACTID], R1_DEFAULT_AGENCY ) ;
        }
        else
        {
            /* there may be an agency but no activity code in chars 1-3.  */
            strncpy(CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
                    strlen(DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE) );
        }
    }
    else if ( strcmp ( DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
            CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0 )
    {
        /* real-time F3 */
        strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "F3" ) ;

        /* the CEF means:  agencies CSA and ASF.    */
        if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 3 )
        {
            strcpy(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;
        }
        else if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) == 3 )
        {
            /* no harm done.  actually, in a sense, do nothing.  */
            strcpy(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;
        }
        else
        {
            /* dtk.actid length > 3  */
            /* there may be an agency but no activity code in chars 1-3.  */
            strncpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
                     strlen(DTKM_SENSOR_REALTIME_DOWNLINK_CODE) ) ;
        }
    }
    else
    {
        /* 
        -- not a downlink.  therefore, this is a real-time SAR obs or 
        -- record SAR.   Just put in a "00" for dtk.transid.  
        -- can't guess about actid value for real time or recording 
        -- now.  
        */
        strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "00" ) ;
    }

    /* 
    -- if this field is not set here, we 
    -- cannot guess REC (recording)
    -- or ROB (realtime observation) here.  
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 3 )
        return DTKM_ERROR_FIELD_ACTID_NOT_SET ;

    /* if not already set, put a value in for the agency  */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 6 )
        strcpy(  (CAST_DTK_ACTID dtk_rec[DTK_ACTID])+3, R1_DEFAULT_AGENCY ) ;

    return DTKM_DEFAULT_VALUES_OK ;
}


/*==============================================================================
Function:   dtkm_a1_default_values

Description:  fills in several A1 default values if not already there.  
1.  the sensor in actid - character 3, from the sensor 1st character.  
2.  the agency in actid - the last 3 bytes
3.  transid.  

Parameters:     
    DB_RECORD    **dtk_record  record to process.

Returns:    
    int
    >= 0  :
          DTKM_DEFAULT_VALUES_OK

    < 0   :  ERROR
                DTKM_ERROR_NULL_RECORD
                DTKM_ERROR_SAT_NOT_A1
                DTKM_ERROR_FIELD_ACTID_NOT_SET

Creator:    Lawrence Stevens

Creation Date:  Thu Oct 19 18:45:10 PDT 1995

Notes:      

     From the ASF-SIS in the WOS, the TRANS_ID codes are F5, F6, and F7:
 
     code  frequency   
     ----  ---------  
     F5    8150 Mhz   
     F6    8350 Mhz   
     F7    8250 Mhz   

    SAMPLE:  dtkm_a1_default_values(dtk_record ) ;
==============================================================================*/
static int dtkm_a1_default_values( DB_RECORD **dtk_rec )
{

    /*  check for errors   */
    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "A1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_A1 ;

    /* 
    -- values are from the RGS interface WOS 
    -- format, 11/11/94, page 0-6 
    */

    /* 
    -- if sensor is dump, this makes the actid value clear.  
    -- the ASF means ASF only.  this is the current preferred 
    -- default for NASDA activities.  .  
    */
    if ( strcmp ( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
            DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0 )
    {
        if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) <= 3 )
        {
            strcpy(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) ;
            strcat(  CAST_DTK_ACTID dtk_rec[DTK_ACTID], "NAS" ) ;
        }
        else
            strncpy( CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
                     strlen(DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE) );
    }

    /* 
    -- if this field is not set here, we 
    -- cannot guess REC or ROB here.  
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 3 )
        return DTKM_ERROR_FIELD_ACTID_NOT_SET ;

    /* 
    -- if not already set, put a value in for the agency.   
    -- the NAS means NASDA only.  this is the current 
    -- default for ADEOS activities.  
    */
    if ( (int) strlen( CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) < 6 )
    {
        strcpy(  (CAST_DTK_ACTID dtk_rec[DTK_ACTID])+3, "NAS" ) ;
    }

    /* 
    -- if not already set, put a value of "00" for 
    -- non-downlinks.  
    */
    if ( (int) strlen( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID] ) < 2 ) 
    {
        if ( strcmp(DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) != 0
        &&   strcmp(DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) != 0 )
        {
            /* 
            -- dtk is not a downlink.  
            -- transid = "00" 
            */
            strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "00" ) ;
        }
        else 
        {
            /* 
            -- dtk is a downlink.  
            -- put in transid = "F5" for a try.  
            */
            strcpy( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], "F5" ) ;
        }
    }

    return DTKM_DEFAULT_VALUES_OK ;
}

/*==============================================================================
Function:   dtkm_default_values

Description:  fills in default values if not already there.  

Parameters:     
    DB_RECORD    **dtk_record  record to process.

Returns:    
    int
    >= 0  :
          DTKM_DEFAULT_VALUES_OK

    < 0   :  ERROR
                DTKM_ERROR_NULL_RECORD
                DTKM_ERROR_UNKNOWN_SAT
                DTKM_ERROR_FIELD_ACTID_NOT_SET

Creator:    Lawrence Stevens

Creation Date:  03/16/1995

Notes:      
==============================================================================*/
int dtkm_default_values( 
    DB_RECORD   **dtk_rec ,
    DB_RECORD   **result_dtk )
{

    int      return_code ;
    double  dtk_duration_days ;

    /*  check for errors   */
    if (dtk_rec == NULL)
        return DTKM_ERROR_NULL_RECORD ;

    if (result_dtk == NULL)
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

#ifdef PRINT_DIAG
    printf("%s:(%d):  input dtk_rec =\n", __FILE__, __LINE__ ) ;
    db_print_record(dtk_rec, APS_CDEFS(DTK) ) ;
#endif

    return_code = db_copy_record ( APS_CDEFS(DTK), result_dtk, dtk_rec ) ;

    if ( (int) strlen( CAST_DTK_ACTID result_dtk[DTK_ACTID] ) < 3 )
        return DTKM_ERROR_FIELD_ACTID_NOT_SET ;

    if (      strncmp( CAST_DTK_SAT result_dtk[DTK_SAT], "E1", 2) == 0 )
        return_code = dtkm_e1_default_values( result_dtk ) ;
    else if ( strncmp( CAST_DTK_SAT result_dtk[DTK_SAT], "E2", 2) == 0 ) 
        return_code = dtkm_e2_default_values( result_dtk ) ; 
    else if ( strncmp( CAST_DTK_SAT result_dtk[DTK_SAT], "J1", 2) == 0 )
        return_code = dtkm_j1_default_values( result_dtk ) ;
    else if ( strncmp( CAST_DTK_SAT result_dtk[DTK_SAT], "R1", 2) == 0 )
        return_code = dtkm_r1_default_values( result_dtk ) ;
    else if ( strncmp( CAST_DTK_SAT result_dtk[DTK_SAT], "A1", 2) == 0 )
        return_code = dtkm_a1_default_values( result_dtk ) ;
    else
        return DTKM_ERROR_UNKNOWN_SAT ;

    if (return_code < 0 )
         return return_code ;

    /*
    --  validate satellite/station_id before writing 
    --  any more data.  
    */
    return_code = dtkm_check_station_id_antenna_id(result_dtk) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    --  validate dtkstat field before writing 
    --  any more data.  this will help the case of 
    --  a dump with a status of QUE or SUB. 
    --  we want to kick out this case as an error 
    --  in the dtkstat value before mistakenly 
    --  looking into whether or not it is in the mask.
    */
    return_code = dtkm_check_dtkstat(result_dtk) ;
    if ( return_code < 0 )
        return return_code ;

    if ( !dtkm_is_a_downlink( result_dtk ) )
    {
        /* 
        -- if an observation, force a '00' value here.  
        -- there was a problem with the OPLN file processor 
        -- which kept putting in the transid from the downlink 
        -- into the recording observation record.  
        -- this kind of breaks a rule in that it clobbers 
        -- a field that might already be set, but it does 
        -- make the job of the file readers easier.  
        -- it solves a problem and could never cause an error.  
        -- besides.  what could go wrong?
        */
        strcpy( CAST_DTK_TRANSID result_dtk[DTK_TRANSID], "00" ) ;
    }


#ifdef PRINT_DIAG
    PRINT_DIAG(
        "%s(%d):  dtkm_default_values() calling dtkm_get_cvrg_points()\n",
        __FILE__, __LINE__ ) ;
#endif

    return_code = dtkm_get_cvrg_points(DB_SYBINT_USE_APS_READER_DBPROC, 
        result_dtk, result_dtk );
    if ( return_code != DTKM_GET_CVRG_POINTS_OK )
        return return_code ;

    /*
    -- set up default for dtk.sitename, if 
    -- not already set.  
    */
    if ( (int) strlen( CAST_DTK_SITENAME result_dtk[DTK_SITENAME] ) == 0 )
        return_code = dtkm_get_sitename( result_dtk, result_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    -- set up defaults for fields dtk.planner_quicklook, 
    -- dtk.science_quicklook, if not already set.
    */
    return_code = dtkm_quicklook_values( result_dtk, result_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* put in submit time as right now, if not already set.  */
    if ( tc_validate_asf_datetime(
            CAST_DTK_SUBMIT_TIME result_dtk[DTK_SUBMIT_TIME]) < 0 )
        tc_systime2asf( CAST_DTK_SUBMIT_TIME result_dtk[DTK_SUBMIT_TIME] ) ;

    /*
    -- Unused field:
    --  lookangl
    */
    CAST_DTK_LOOKANGL result_dtk[DTK_LOOKANGL] = 0 ;

    /*
    -- put in values for fa_strttime and stoptime if needed.  
    -- needs values if there are no values there AND 
    -- dtkstat = SCH AND there is a value for fa_schedule_link
    -- if these 2 things are true, then this is a FA schedule 
    -- dtk and we need to capture the two fields fa_strttime and 
    -- fa_stoptime.  
    -- In other words:  
    --      if BOTH values of fa_times are OK, then do not put in new values.
    --      if proposed dtk.dtkstat != SCH, then do not put in values.
    --      if fa_schedule_link has no value, then do not put in values.
    */
    if(   (    tc_validate_asf_datetime( CAST_DTK_FA_STRTTIME 
                                   result_dtk[DTK_FA_STRTTIME] ) != TRUE 
           ||  tc_validate_asf_datetime( CAST_DTK_FA_STOPTIME 
                                   result_dtk[DTK_FA_STOPTIME] ) != TRUE )
    &&    strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH" ) == 0 
    &&    strlen( CAST_DTK_FA_SCHEDULE_LINK result_dtk[DTK_FA_SCHEDULE_LINK] ) 
          > 0   )
    {
        /* 
        -- need to put in values for fa_strttime and stoptime  
        */
        strcpy( CAST_DTK_FA_STRTTIME result_dtk[DTK_FA_STRTTIME],
                CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME]  ) ;
        strcpy( CAST_DTK_FA_STOPTIME result_dtk[DTK_FA_STOPTIME],
                CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME]  ) ;

        /* Also, fill in fa_duration_min and asf_reduction_min  */
        return_code = tc_et_ASF_datetime_diff(
            CAST_DTK_FA_STRTTIME result_dtk[DTK_FA_STRTTIME],
            CAST_DTK_FA_STOPTIME result_dtk[DTK_FA_STOPTIME],
            &dtk_duration_days ) ;
        if( return_code != TRUE )
            return return_code ;

        /* fa_duration in minutes:   */
        CAST_DTK_FA_DURATION_MIN result_dtk[DTK_FA_DURATION_MIN] 
            = dtk_duration_days * 24.0 * 60.0 ;

        return_code = dtkm_set_asf_reduction_min( result_dtk, result_dtk ) ;
        if( return_code < 0 )
            return return_code ;

    }

    return DTKM_DEFAULT_VALUES_OK ;

}

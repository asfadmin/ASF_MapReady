#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   NASDAconversions.c

Description:    NASDA conversion routines, which are used to decode/translate
                NASDA File string values.

External Functions Defined:

File Scope Functions:
    
External Variables Defined:
    EQUIV_TABLE   NASDA_facility_id[]   contains facility id values
    EQUIV_TABLE   NASDA_dtk_stat[]      contains datatake status values
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident "@(#)NASDAconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident "@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.NASDAconversions.c"

static char asf_time_format[] = "%4d:%03d:%02d:%02d:%02d.%03d" ;

#include <ctype.h>        /* for isdigit()  */
#include <stdio.h>        /* for sscanf()   */
#include <stdlib.h>
#include <stddef.h>       /* for NULL       */
#include <string.h>

#include "timeconv.h"
#include "GENconversions.h"
#include "NASDAconversions.h"
#include "phase_utilities.h"


/* for APS basic definitions */   
#include "dapps_defs.h"    

/* for dtk relation   */
#include "db_dtk.h"

/* for phase relation */
#include "db_phase.h"

/* for syslogging     */
#include "file_utilities.h"
#include "aps_log_msg.h"

/* for permission definitions */
#include <mu_utilities.h>

/* for realtime/tapedump downlink codes */
#include "dtkm_utilities.h"

/* for debugging purposes  */
#define PRINT_DIAG 1
#undef  PRINT_DIAG

DBPROCESS   *FA_dtkf_dbproc ;

char *pointer2fa_start_yyyymmdd = &fa_start_yyyymmdd[0];
char *pointer2fa_stop_yyyymmdd = &fa_stop_yyyymmdd[0];

/*==============================================================================
Function:       NASDAc_rev_asftime_2_rsp_angle

Description:    given Nasda satellite, asftime, rev:
                determines the rsp path and rsp angle.  

Parameters:     
int NASDAc_rev_asftime_2_rsp_angle(
    char        *sat,            input NASDA satellite
    char        *asftime,        input time.  
    int         rev,             input rev number
    int         *rsp,            output rsp 
    double      *rsp_angle )     output angle

Returns:        int
    >= 0:  No error:  
        NASDA_REV_ASFTIME_2_RSP_ANGLE_OK

    <  0:  Error:  
        NASDA_BAD_ASFTIME 
        NASDA_ASFTIME_NOT_WITHIN_ANY_PHASE 
        NASDA_NOT_A_NASDA_SATELLITE 
        NASDA_REV_NUMBER_NOT_IN_PHASE 
        NASDA_ERROR_IN_PHASE_START_TIME 
        NASDA_INPUT_ASFTIME_NOT_WITHIN_INPUT_REV 
        and other codes returned by called routines.

Creator:        Lawrence Stevens

Creation Date:  Mon Jun 12 12:53:06 PDT 1995

Notes:      

RSP refers to Request System for Planning.  
In the Request System for Planning, a satelite position is 
identified by RSP path (or just by RSP) and RSP angle.  

RSP path refers to one of the revs that repeats.  For example, 
for J1, there is a 659 rev repeat cycle.  That means that there 
are 659 separate "generic" revs.  The satellite is always flying 
or experiencing one of them.  Each of these generic revs is identified 
by a number [1, 659].  They are numbered such that as the rev number 
increments by one, so does the RSP path, until the path eventually will 
increment from 659 to 1.  

The RSP angle, [0.0, 360.0], refers to a position within the RSP 
path, using an angle from 0 degrees to 360 degrees to identify a 
point within the circle of the designated orbit.  

REFERENCE:  JERS-1 Operation Interface Specification [HE-89033]
November 7, 1991 revision-3, from NASDA EOS.
This document, in Appendix 12, contains the complete description, 
including equations, describing the RSP.  The RSP path is the 
same as the GRS path, described in Appendix 11.  Equation A11.23, 
is used, which computes path number given ascending node of orbit 
of interest.  Here, the path number for the first rev in the phase 
is computed.  Since the RSP number increments with each rev, 
and it moves from 659 to 0, we use a modulus 659 calculation, from 
the difference in rev number from the first rev in the phase.  

==============================================================================*/

int NASDAc_rev_asftime_2_rsp_angle(
    char        *sat,           /* input NASDA satellite            */
    char        *asftime,       /* input time.                      */
    int         rev,            /* input rev number                 */
    int         *rsp,           /* output rsp                       */
    double      *rsp_angle )    /* output angle                     */
{

    int         return_code ;
    DB_RECORD   **phase_rec ;

    double      et_input ;
    double      et_end_of_rev ;
    double      time_for_one_rev_days ;

    int         rev_number_for_asftime ;

    double      et_phase_start ;
    double      et_start_of_rev ;
    double      et_rev_start_to_input_time ;
    double      fraction ;

    int         int_rsp_angle_100 ;


    /* initialize the output values to unusable.  */
    *rsp = -1 ;
    *rsp_angle = -1.0  ;

    /* check the inputs.  */
    if (!tc_asf2et(asftime, &et_input) )
        return NASDA_BAD_ASFTIME ;

    /* 
    -- obtain the phase relation record for the input time.  
    */
    return_code = asftime_2_phase(sat, asftime, &phase_rec ) ;
    if ( return_code < 0 )
        return return_code ;

    if ( return_code != PHASE_INPUT_TIME_WITHIN_A_PHASE )
        return NASDA_ASFTIME_NOT_WITHIN_ANY_PHASE ;

    /* 
    -- now check for this phase being a NASDA 
    -- phase - contains RSP data 
    */
    if ( CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] <= 0 )
        return NASDA_NOT_A_NASDA_SATELLITE ;

    /* check input rev number to see if it is in this phase.   */
    return_code = phase_rev_in_phase( phase_rec, rev ) ;
    if (!return_code)
        return NASDA_REV_NUMBER_NOT_IN_PHASE ;

    /********************************************************************/
    /*                                                                  */
    /*  Step 1:                                                         */
    /*  compute Ephemeris time for phase start                          */
    /*                                                                  */
    /********************************************************************/
    return_code = tc_asf2et(
        CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], &et_phase_start) ;
    if (!return_code)
        return NASDA_ERROR_IN_PHASE_START_TIME ;

    /********************************************************************/
    /*                                                                  */
    /*  Step 2:                                                         */
    /*  compute what rev the input asftime is in                        */
    /*                                                                  */
    /********************************************************************/
    return_code = phase_asftime2rev( phase_rec, asftime, 
        &rev_number_for_asftime ) ;
    if ( return_code < 0 )
        return return_code ;

    /* verify this against input rev to help error reporting:  */
    if ( rev != rev_number_for_asftime )
        return NASDA_INPUT_ASFTIME_NOT_WITHIN_INPUT_REV ;

    /********************************************************************/
    /*                                                                  */
    /*  Step 3:                                                         */
    /*  compute start time of the input rev.                            */
    /*                                                                  */
    /********************************************************************/
    return_code = phase_rev2et( phase_rec, rev, 
        &et_start_of_rev, &et_end_of_rev ) ;
    if ( return_code < 0 )
        return return_code ;

    /********************************************************************/
    /*                                                                  */
    /*  Step 4:                                                         */
    /*  compute the rsp angle to .01 degree accuracy:                   */
    /*  compute the time from start of rev to input time;               */
    /*  convert this to a fraction of a rev;                            */
    /*  convert the fraction to [0, 360] degrees with 2 decimal         */
    /*  places                                                          */
    /*                                                                  */
    /********************************************************************/
    et_rev_start_to_input_time = et_input - et_start_of_rev ;

    time_for_one_rev_days = 
        (double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS]
        / (double) CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    fraction = et_rev_start_to_input_time / time_for_one_rev_days ;
    *rsp_angle = fraction * 360.0 ;
    /* 
    -- want to round off to nearest .01 degrees.  therefore, 
    -- multiply the calculation by 100, then add about 1/2, then 
    -- convert to integer, truncating in the process.  then divide by 
    -- 100 to convert back to degrees and return that to the calling
    -- routine.  
    */
    int_rsp_angle_100 = 100.0 * (*rsp_angle) + 0.49999999  ;

    *rsp_angle = (double) int_rsp_angle_100 / 100.0 ;

    /********************************************************************/
    /*                                                                  */
    /*  Step 6:                                                         */
    /*  compute RSP path from input rev number.                         */
    /*                                                                  */
    /********************************************************************/
    /* rsp is an argument to the calling subroutine:  int *rsp   */
    return_code = NASDAc_phase_rev2rsp(phase_rec, rev, rsp ) ;
    if ( return_code < 0 )
        return return_code ;

    return NASDA_REV_ASFTIME_2_RSP_ANGLE_OK ;

}

/*==============================================================================
Function:       NASDAc_phase_first_rsp_rev

Description:    compute the RSP path of the first rev in a phase.  
                used in other routines, mainly, this is a private 
                kind of routine.  
Parameters:     
int NASDAc_phase_first_rsp_rev(
    DB_RECORD   **phase_rec,     phase record which has the data to use. 
    int         *rev1,           output rev number of first rev in phase.
    int         *rsp1 )          output rsp of first rev in phase.       

Returns:        
    >= 0:  No error:
            NASDA_PHASE_RSP1_OK 
    <  0:  Error:
            NASDA_INPUT_PHASE_REC_IS_NULL 
            NASDA_NOT_A_NASDA_PHASE_RECORD
            other errors as returned from called routines.  

Creator:        Lawrence Stevens

Creation Date:  Tue Jun 13 13:06:42 PDT 1995

Notes:      
==============================================================================*/

int NASDAc_phase_first_rsp_rev(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
    int         *rev1,          /* output rev number of first rev in phase. */
    int         *rsp1 )         /* output rsp of first rev in phase.        */
{

    int         return_code ;

    int         first_rev_in_phase ;
    int         rsp_first_rev ;

    double      delta_longitude_rsp_0 ;
    double      delta_longitude_per_rsp_path ; ;

    /* initialize the output value to unusable.  */
    *rsp1 = -1 ;
    *rev1 = -1 ;

    /* check the input.  */
    if ( phase_rec == NULL )
        return NASDA_INPUT_PHASE_REC_IS_NULL ;

    /* 
    -- now check for this phase being a NASDA 
    -- phase - contains RSP data 
    */
    if ( CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] <= 0 )
        return NASDA_NOT_A_NASDA_PHASE_RECORD ;

    /********************************************************************/
    /*                                                                  */
    /*  get the first rev in the phase                                  */
    /*                                                                  */
    /********************************************************************/
    return_code = phase_first_rev(phase_rec, &first_rev_in_phase ) ;
    if ( return_code < 0 )
        return return_code ;
        
    /********************************************************************/
    /*                                                                  */
    /*  Compute RSP Step 1:                                             */
    /*  compute delta longitude between rsp 0 and first rev in phase.   */
    /*                                                                  */
    /********************************************************************/
    delta_longitude_rsp_0 = CAST_PHASE_PHASE_LON phase_rec[PHASE_PHASE_LON] 
        - CAST_PHASE_RSP_0_LON phase_rec[PHASE_RSP_0_LON] ;

    /********************************************************************/
    /*                                                                  */
    /*  Compute RSP Step 2:                                             */
    /*  compute delta longitude per path.                               */
    /*  it is negative; this is how NASDA works it.                     */
    /*                                                                  */
    /********************************************************************/
    delta_longitude_per_rsp_path = -1.0 * (double) 360.0 
        / CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    /********************************************************************/
    /*                                                                  */
    /*  Compute RSP Step 3:                                             */
    /*  compute number of paths in delta longitude from Step 2;         */
    /*  that number is the RSP path number for the first rev in phase.  */
    /*                                                                  */
    /********************************************************************/

    /* 
    -- using addition of 1/2 and implicit default trucation to 
    -- get a net result of roundoff to the nearest integer.  
    -- normally, the division result will be extremely close to 
    -- an integer, anyway.  
    */
    rsp_first_rev = 
        0.499999 + ( delta_longitude_rsp_0 / delta_longitude_per_rsp_path) ;

    /* rectify this rsp to [1, PHASE_CYCLE_REVS]   */
    while ( rsp_first_rev < 1 )
        rsp_first_rev += CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    while ( rsp_first_rev > CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] )
        rsp_first_rev -= CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    /* assign output values:   */
    *rev1 = first_rev_in_phase ;
    *rsp1 = rsp_first_rev ;

    return NASDA_PHASE_RSP1_OK  ;

}

/*==============================================================================
Function:       NASDAc_phase_rsp2firstrev

Description:    given a Nasda phase record and RSP path, compute the 
                first rev in the phase with this RSP.  this routine 
                is used by other routines for more particular results.
                this may seem kind of specific, but it will be helpful. 
Parameters:     
int NASDAc_phase_rsp2firstrev(
    DB_RECORD   **phase_rec,  phase record which has the data to use. 
    int         input_rsp,    input rsp                              
    int         *first_rev ) output - first rev in phase with rsp. 

Returns:        
    >= 0 :  No error:
        NASDA_PHASE_RSP2FIRSTREV_OK ;

    <  0 :  Error:
        NASDA_INPUT_PHASE_REC_IS_NULL 
        NASDA_NOT_A_NASDA_PHASE_RECORD
        NASDA_BAD_RSP_PATH 
        NASDA_RSP_NOT_FOUND_CHECK_PHASE_RELATION 

Creator:        Lawrence Stevens

Creation Date:  Tue Jun 13 12:57:26 PDT 1995

Notes:      
RSP refers to Request System for Planning.  
In the Request System for Planning, a satelite position is 
identified by RSP path (or just by RSP) and RSP angle.  

RSP path refers to one of the revs that repeats.  For example, 
for J1, there is a 659 rev repeat cycle.  That means that there 
are 659 separate "generic" revs.  The satellite is always flying 
or experiencing one of them.  Each of these generic revs is identified 
by a number [1, 659].  They are numbered such that as the rev number 
increments by one, so does the RSP path, until the path eventually will 
increment from 659 to 1.  

The RSP angle, [0.0, 360.0], refers to a position within the RSP 
path, using an angle from 0 degrees to 360 degrees to identify a 
point within the circle of the designated orbit.  

REFERENCE:  JERS-1 Operation Interface Specification [HE-89033]
November 7, 1991 revision-3, from NASDA EOS.
This document, in Appendix 12, contains the complete description, 
including equations, describing the RSP.  The RSP path is the 
same as the GRS path, described in Appendix 11.  Equation A11.23, 
is used, which computes path number given ascending node of orbit 
of interest.  Here, the path number for the first rev in the phase 
is computed.  Since the RSP number increments with each rev, 
and it moves from 659 to 0, we use a modulus 659 calculation, from 
the difference in rev number from the first rev in the phase.  

==============================================================================*/

int NASDAc_phase_rsp2firstrev(
    DB_RECORD   **phase_rec, /* phase record which has the data to use. */
    int         input_rsp,   /* input rsp                               */
    int         *first_rev )/* output - first rev in phase with rsp.   */
{

    int         return_code ;

    int         first_rev_in_phase ;
    int         rsp_first_rev ;
    int         rev ;
    int         rsp_for_rev ;
    int         end_rev ;

    /* initialize the output value to unusable in 
    -- case of an early return.  
    */
    *first_rev = -1 ;

    /* check the input.  */
    if ( phase_rec == NULL )
        return NASDA_INPUT_PHASE_REC_IS_NULL ;

    /* 
    -- now check for this phase being a NASDA 
    -- phase - contains RSP data 
    */
    if ( CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] <= 0 )
        return NASDA_NOT_A_NASDA_PHASE_RECORD ;

    /* check input RSP number to see if it is legal.      */
    if (!NASDAc_phase_validate_rsp(phase_rec, input_rsp ) ) 
        return NASDA_BAD_RSP_PATH ;

    /* get first rev in phase and its corresponding rsp path.   */
    return_code = NASDAc_phase_first_rsp_rev( phase_rec, &first_rev_in_phase, 
        &rsp_first_rev ) ;
    if( return_code < 0 )
        return return_code ;

    /* 
    -- set up a rev bracket starting at first_rev_in_phase 
    -- with a length of one repeat cycle.  remember the -1.   
    -- also, make sure that the rev is in the phase.  
    */
    end_rev = first_rev_in_phase 
        + CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] - 1 ;

    if ( end_rev > CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] )
        end_rev = CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] ;

    /* 
    -- the input RSP path should be encountered somewhere in any 
    -- complete repeat cycle.  we look in the first full cycle 
    -- of revs in this phase.  if the desired rsp path is not 
    -- found, there is a problem with the phase relation 
    -- data.  
    */
    for (rev = first_rev_in_phase; rev <= end_rev; rev++ )
    {
        /* compute the RSP for the rev:  */
        rsp_for_rev = rsp_first_rev 
            + ( rev - first_rev_in_phase ) 
                * CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] ;

        /* rectify the RSP to [1,PHASE_CYCLE_REVS]  */
        rsp_for_rev = 
            rsp_for_rev % CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;
        if ( rsp_for_rev == 0 )
            rsp_for_rev = CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

        /* compare to input_rsp  */
        if ( rsp_for_rev == input_rsp ) 
        {
            /* FOUND  */
            *first_rev = rev ;
            return NASDA_PHASE_RSP2FIRSTREV_OK ;
        }
    }

    return NASDA_RSP_NOT_FOUND_CHECK_PHASE_RELATION ;

}

/*==============================================================================
Function:       NASDAc_phase_validate_rsp

Description:    given a phase record, checks input rsp vs valid RSP
                values 

Parameters:     
int NASDAc_phase_validate_rsp(
    DB_RECORD   **phase_rec,     phase record which has the data to use. 
    int        rsp )

Returns:        int
    TRUE    OK
    FALSE   not OK
    

Creator:        Lawrence Stevens

Creation Date:  Wed Jun 14 14:37:01 PDT 1995

Notes:      
==============================================================================*/
int NASDAc_phase_validate_rsp( 
     DB_RECORD   **phase_rec,    /* phase record which has the data to use.  */
     int        rsp )
{
    /* check to see if this is a NASDA phase.  */
    if ( CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] <= 0 )
        return FALSE ;

    if ( rsp < 0
    ||   rsp > CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] )
        return FALSE ;

    return TRUE ;
}


/*==============================================================================
Function:       NASDAc_phase_rev2rsp

Description:    given a Nasda phase record, convert rev to RSP path.

Parameters:     
    DB_RECORD   **phase_rec,     phase record which has the data to use. 
    int         rev,             input rev number                      
    int         *rsp )           output rsp                           

Returns:        int
    >= 0 : NASDA_PHASE_REV2RSP_OK  ;

    < 0  : Error:
        INPUT_PHASE_REC_IS_NULL
        NOT_A_NASDA_PHASE_RECORD
        REV_NUMBER_NOT_IN_PHASE

Creator:        Lawrence Stevens

Creation Date:  Tue Jun 13 09:44:55 PDT 1995

Notes:      
RSP refers to Request System for Planning.  
In the Request System for Planning, a satelite position is 
identified by RSP path (or just by RSP) and RSP angle.  

RSP path refers to one of the revs that repeats.  For example, 
for J1, there is a 659 rev repeat cycle.  That means that there 
are 659 separate "generic" revs.  The satellite is always flying 
or experiencing one of them.  Each of these generic revs is identified 
by a number [1, 659].  They are numbered such that as the rev number 
increments by one, so does the RSP path, until the path eventually will 
increment from 659 to 1.  

The RSP angle, [0.0, 360.0], refers to a position within the RSP 
path, using an angle from 0 degrees to 360 degrees to identify a 
point within the circle of the designated orbit.  

REFERENCE:  JERS-1 Operation Interface Specification [HE-89033]
November 7, 1991 revision-3, from NASDA EOS.
This document, in Appendix 12, contains the complete description, 
including equations, describing the RSP.  The RSP path is the 
same as the GRS path, described in Appendix 11.  Equation A11.23, 
is used, which computes path number given ascending node of orbit 
of interest.  Here, the path number for the first rev in the phase 
is computed.  Since the RSP number increments with each rev, 
and it moves from 659 to 0, we use a modulus 659 calculation, from 
the difference in rev number from the first rev in the phase.  

==============================================================================*/

int NASDAc_phase_rev2rsp(
    DB_RECORD   **phase_rec,    /* phase record which has the data to use. */
    int         rev,            /* input rev number                        */
    int         *rsp )          /* output rsp                              */
{

    int         return_code ;

    int         first_rev_in_phase ;
    int         rsp_first_rev ;
    int         revs_since_phase_start ;

    /* initialize the output value to unusable.  */
    *rsp = -1 ;

    /* check the input.  */
    if ( phase_rec == NULL )
        return NASDA_INPUT_PHASE_REC_IS_NULL ;

    /* 
    -- now check for this phase being a NASDA 
    -- phase - contains RSP data 
    */
    if ( CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] <= 0 )
        return NASDA_NOT_A_NASDA_PHASE_RECORD ;

    /* get first rev number and RSP path of first rev   */
    return_code = NASDAc_phase_first_rsp_rev( phase_rec, 
        &first_rev_in_phase, &rsp_first_rev ) ;
    if ( return_code < 0 )
        return return_code ;

    /* check input rev number to see if it is in this phase.   */
    if ( rev < first_rev_in_phase 
    ||   rev > CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] ) 
        return NASDA_REV_NUMBER_NOT_IN_PHASE ;

    /********************************************************************/
    /*                                                                  */
    /*  Compute the RSP path for  the input rev number.                 */
    /*                                                                  */
    /*  With each successive rev, the RSP path increases by             */
    /*  PHASE_PHASE_DAYS.                                               */
    /*                                                                  */
    /*  so the RSP path, going from the first rev in the phase          */
    /*  to the input rev, increases by:                                 */
    /*                                                                  */
    /*  ( revs_since_phase_start ) times the PHASE_CYCLE_DAYS.          */
    /*                                                                  */
    /*  we use this info to compute RSP path for the input rev.         */
    /*                                                                  */
    /********************************************************************/
    revs_since_phase_start = rev - first_rev_in_phase ;
    *rsp = rsp_first_rev 
        + ( revs_since_phase_start 
            * CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] )  ;

    /* 
    -- rectify the RSP path, *rsp, 
    -- to within [1, PHASE_CYCLE_REVS]    
    -- use modulus function based on number of revs in the repeat
    -- cycle.  
    */
    *rsp %= CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;
    if ( *rsp == 0 )
        *rsp = CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    return NASDA_PHASE_REV2RSP_OK  ;

}

/*==============================================================================
Function:       NASDAc_asftime_rsp_angle_2_rev_time()

Description:    given Nasda satellite, asftime, rsp, angle:  
                determine rev number and exact asftime.

Parameters:     
int NASDAc_asftime_rsp_angle_2_rev_time(
    char        *sat,            input NASDA satellite                  
    char        *asftime,        input time.                        
    int         rsp,             input rsp                      
    double      rsp_angle,       input angle                    
    int         search_flag,     SEARCH_FORWARD or SEARCH_BACKWARD
    int         *rev,            output rev number              
    char        *asftime_out  )  output asftime             

Returns:        int
    >= 0:  No error:
        NASDA_ASFTIME_RSP_ANGLE_2_REV_TIME_OK

    < 0 :  Errors:
        BAD_ASFTIME 
        BAD_RSP_ANGLE 
        ASFTIME_NOT_WITHIN_ANY_PHASE 
        NOT_A_NASDA_SATELLITE 
        BAD_RSP_PATH 
        ERROR_IN_PHASE_START_TIME_OR_ASFTIME 
        ERROR_IN_COMPUTED_TIME 
        ERROR_IN_PHASE_START_TIME 
        REV_NOT_FOUND_FOR_RSP 
        BAD_VALUE_FOR_SEARCH_FLAG 


Creator:        Lawrence Stevens

Creation Date:  Thu Jun  8 18:44:27 PDT 1995

Notes:      

RSP refers to Request System for Planning.  
In the Request System for Planning, a satelite position is 
identified by RSP path (or just by RSP) and RSP angle.  

RSP path refers to one of the revs that repeats.  For example, 
for J1, there is a 659 rev repeat cycle.  That means that there 
are 659 separate "generic" revs.  The satellite is always flying 
or experiencing one of them.  Each of these generic revs is identified 
by a number [1, 659].  They are numbered such that as the rev number 
increments by one, so does the RSP path, until the path eventually will 
increment from 659 to 1.  

The RSP angle, [0.0, 360.0], refers to a position within the RSP 
path, using an angle from 0 degrees to 360 degrees to identify a 
point within the circle of the designated orbit.  

REFERENCE:  JERS-1 Operation Interface Specification [HE-89033]
November 7, 1991 revision-3, from NASDA EOS.
This document, in Appendix 12, contains the complete description, 
including equations, describing the RSP.  The RSP path is the 
same as the GRS path, described in Appendix 11.  Equation A11.23, 
is used, which computes path number given ascending node of orbit 
of interest.  Here, the path number for the first rev in the phase 
is computed.  Since the RSP number increments with each rev, 
and it moves from 659 to 0, we use a modulus 659 calculation, from 
the difference in rev number from the first rev in the phase.  

==============================================================================*/

int NASDAc_asftime_rsp_angle_2_rev_time(
    char        *sat,           /* input NASDA satellite                */
    char        *asftime,       /* input time.                          */
    int         rsp,            /* input rsp                            */
    double      rsp_angle,      /* input angle                          */
    int         search_flag,    /* SEARCH_FORWARD or SEARCH_BACKWARD    */
    int         *rev,           /* output rev number                    */
    char        *asftime_out  ) /* output asftime                       */
{

    int         return_code ;
    DB_RECORD   **phase_rec ;

    double      et_input ;
    double      time_for_one_rev_days ;

    int         first_rev_in_phase ;
    int         rev_number_for_asftime ;
    int         revs_per_day ;
    int         rev_bracket_start, rev_bracket_end ;
    int         j, rsp_for_rev_j ;


    double      delta_time_to_rev_days ;
    double      time_duration_days ;
    double      et_phase_start ;
    double      et_rsp_angle ;


    /* initialize the output values  */
    *rev = 0 ;
    strcpy(asftime_out, "" ) ;

    /* check the inputs.  */
    if (!tc_asf2et(asftime, &et_input) )
        return NASDA_BAD_ASFTIME ;

    if ( rsp_angle < 0.0 || rsp_angle > 360.0 )
        return NASDA_BAD_RSP_ANGLE ;

    /* 
    -- obtain the phase relation record for the input time.  
    */
    return_code = asftime_2_phase(sat, asftime, &phase_rec ) ;
    if ( return_code < 0 )
        return return_code ;
    if ( return_code != PHASE_INPUT_TIME_WITHIN_A_PHASE )
    {
        free_db_record(phase_rec) ;
        return NASDA_ASFTIME_NOT_WITHIN_ANY_PHASE ;
    }

    /* 
    -- now check for this phase being a NASDA 
    -- phase - contains RSP data 
    */
    if ( CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] <= 0 )
    {
        free_db_record(phase_rec) ;
        return NASDA_NOT_A_NASDA_SATELLITE ;
    }

    /* validate input RSP path.   */
    if (!NASDAc_phase_validate_rsp(phase_rec, rsp ) )
    {
        free_db_record(phase_rec) ;
        return NASDA_BAD_RSP_PATH ;
    }

    /***********************************************************************/
    /*                                                                     */
    /* Step 1:                                                             */
    /* Determine the rev number for the input asftime.                     */
    /*                                                                     */
    /***********************************************************************/
    return_code = phase_asftime2rev(phase_rec, asftime, 
        &rev_number_for_asftime ) ;
    if (return_code < 0)
    {
        free_db_record(phase_rec) ;
        return return_code  ;
    }

    /***********************************************************************/
    /*                                                                     */
    /* Step 2:                                                             */
    /* Determine the rev bracket within which to check                     */
    /* RSP.  This rev bracket is about one day wide.                       */
    /*                                                                     */
    /***********************************************************************/

    time_for_one_rev_days 
        = (double) CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS]
          /        CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

    revs_per_day = 1 + (int) ( 1.0 / time_for_one_rev_days ) ;
    

    if ( search_flag == SEARCH_FORWARD )
    {
        rev_bracket_start = rev_number_for_asftime ;
        rev_bracket_end = rev_bracket_start + revs_per_day ;
    }
    else if ( search_flag == SEARCH_BACKWARD )
    {
        rev_bracket_end = rev_number_for_asftime ;
        rev_bracket_start = rev_bracket_end - revs_per_day ;
    }
    else
    {
        free_db_record(phase_rec) ;
        return NASDA_BAD_VALUE_FOR_SEARCH_FLAG ;
    }

    /* 
    -- for safety, check and limit the rev bracket to 
    -- within the current phase:  
    */
    return_code = phase_first_rev(phase_rec, &first_rev_in_phase ) ;
    if ( rev_bracket_start < first_rev_in_phase )
        rev_bracket_start = first_rev_in_phase ;

    if ( rev_bracket_end > CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] )
        rev_bracket_end = CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] ;

    /***********************************************************************/
    /*                                                                     */
    /* Step 3:                                                             */
    /* Get rev for input RSP                                               */
    /* See Note in header describing RSP.                                  */
    /* The RSP path for each rev in the bracket will be                    */
    /* computed.  When one of these computed RSP matches                   */
    /* the input RSP, bingo.  The desired rev number is                    */
    /* found.                                                              */
    /*                                                                     */
    /***********************************************************************/

    *rev = 0 ;
    for (j = rev_bracket_start; j <= rev_bracket_end; j++ ) 
    {
        return_code =  NASDAc_phase_rev2rsp( phase_rec, j, &rsp_for_rev_j ) ;    
        if ( return_code < 0 )
        {
            free_db_record(phase_rec) ;
            return return_code ;
        }

        /* 
        -- rsp_for_rev_j is now computed; 
        -- if ( rsp_for_rev_j == input_rsp):  bingo.    
        */
        if ( rsp_for_rev_j == rsp )
        {
            /* capture the rev number for output  */
            *rev = j ;
            break ;
        }
    }

    if ( *rev == 0 )
    {
        free_db_record(phase_rec) ;
        return NASDA_REV_NOT_FOUND_FOR_RSP ;
    }
    /* OK; the rev number for output was found.  */

    /***********************************************************************/
    /*                                                                     */
    /* Step 4:                                                             */
    /* Get the time for the rsp_angle within the rev.                      */
    /* We convert the input RSP angle to a time duration which             */
    /* is a fraction of the time for one rev, then add that                */
    /* time duration to the start time of the desired rev.  This           */
    /* yields the output time.  This output time is within                 */
    /* the desired rev and is the time when the satellite is               */
    /* located at the input RSP angle within the orbit.  the               */
    /* start of the rev would be 0.0 degrees, half way through             */
    /* would be 180.0 degrees etc.                                         */
    /*                                                                     */
    /***********************************************************************/

    /* ephemeris time at start of the phase  */
    if (!tc_asf2et( CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], 
                    &et_phase_start ) )
        return NASDA_ERROR_IN_PHASE_START_TIME ;

    /* delta time to start of the rev we just found  */
    delta_time_to_rev_days = 
        (*rev - first_rev_in_phase) * time_for_one_rev_days  ; 

    /* fraction of the time for one rev, indicated by rsp_angle  */
    time_duration_days = ( rsp_angle/360.0 ) * time_for_one_rev_days ;

    /* add them up to get the actual time indicated by the rsp_angle   */
    et_rsp_angle = 
        et_phase_start + delta_time_to_rev_days + time_duration_days ;

    /* 
    -- this time is what we must return to the calling 
    -- program, in asf format.
    */
    free_db_record(phase_rec) ;
    if (!tc_et2asf(et_rsp_angle, asftime_out ) )
        return NASDA_ERROR_IN_COMPUTED_TIME ;

    return NASDA_ASFTIME_RSP_ANGLE_2_REV_TIME_OK ;

}

/*==============================================================================
Function:       NASDAc_asfdate_rsp_2_rev

Description:    given a day and an rsp, find the rev number it refers to.  
                the routine will first search forward in time and then 
                backward in time, from the start of the day indicated  
                in the input asftime.  only the year and day of the
                input asf time are looked at.  This function is useful
                when reading a data-take type of Nasda file.  

Parameters:     
int NASDAc_asfdate_rsp_2_rev(
    char        *sat,            input NASDA satellite      
    char        *asfdate,        input time.            
    int         rsp,             input rsp          
    int         *rev )           output rev number 

Returns:        
    >= 0:  No error:
        NASDA_ASFDATE_RSP_2_REV_OK
    <  0:  Error:
        NASDA_BAD_ASFTIME 
        other errors as diagnosed by called routines.  

Creator:        Lawrence Stevens

Creation Date:  Thu Jun 15 18:31:44 PDT 1995

Notes:      
==============================================================================*/

int NASDAc_asfdate_rsp_2_rev(
    char        *sat,           /* input NASDA satellite                */
    char        *asfdate,       /* input time.  the date part (yyyy:ddd) is all that is used.  */
    int         rsp,            /* input rsp                            */
    int         *rev )          /* output rev number                    */
{

    int         return_code ;

    char        asftime[22] = "yyyy:ddd:00:00:00.000" ;
    char        asftime_out[22] ;

    double      et_input ;


    /* initialize the output values  */
    *rev = 0 ;

    /*
    -- use ony the year and day from the input.  
    */
    strncpy( asftime, asfdate, 8 ) ;
    if (!tc_asf2et(asftime, &et_input) )
        return NASDA_BAD_ASFTIME ;

    /* 
    -- search forward first, then backwards in time from the 
    -- end of the input date.  this is because the date may 
    -- straddle the start of a phase.  
    */
    return_code = NASDAc_asftime_rsp_angle_2_rev_time( 
        sat, asftime, rsp, 0.01, SEARCH_FORWARD, rev, asftime_out ) ;

    if ( return_code == NASDA_ASFTIME_RSP_ANGLE_2_REV_TIME_OK )
        return NASDA_ASFDATE_RSP_2_REV_OK ;

    /* search backwards in time from the end of the day now.  */
    /* set up the end of the day:  */
    strcpy( asftime+9, "23:59:59.999" ) ;
    return_code = NASDAc_asftime_rsp_angle_2_rev_time( 
        sat, asftime, rsp, 0.01, SEARCH_BACKWARD, rev, asftime_out ) ;

    if ( return_code == NASDA_ASFTIME_RSP_ANGLE_2_REV_TIME_OK )
        return NASDA_ASFDATE_RSP_2_REV_OK ;
    else
        /*  error:    */
        return return_code ;
}   



/*==============================================================================
Function:       NASDAc_update_fadtkid()

Description:    the '000000' in the result string is replaced by the source 
                string.  

Creator:        Lawrence Stevens

Creation Date:  Thu May  9 19:17:13 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int NASDAc_update_fadtkid(
    void        *unused_pointer,
    char        *source_string,   /* like "123456"                */
    char        *result_string)   /* like "123456/123/000000"     */
{

    /* check the input source:  */
    if ( strlen(source_string) != 6 )
        return FALSE ;

    /* check the input result_string:  */
    if ( strlen(result_string) != 17 )
        return FALSE ;

    if ( strncmp(result_string+11, "000000", 6) )
        return FALSE ;

    /* at this point, we believe.  */
    strcpy(result_string+11, source_string ) ;

    return TRUE ;

}

/*==============================================================================
Function:       list of EQUIVALENCE TABLES.

Description:    the tables are used by the driver to help translate fields.
                see  the VALUE_DEFS below.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 15:26:58 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
/* EQUIVALENCE TABLES    */

EQUIV_TABLE   NASDA_REQA_sensor[]= 
{   {"S", "SAR"},
    {"O", "OPS"},
    {"V", "VNR"},
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_REQA_actid[]= 
/*external  APS internal value */
{   {"S",   DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* recording activity */
    {"O",   DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* recording activity */
    {"V",   DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* recording activity */
    {"D",   DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE},    /* tapedump activity */
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_REQA_station_id[]= 
{   
    {"FAIS", "ASF"},        /* ASF ground station,      datatake OK  */
    {"HEOC", "XXX"},        /* Hatoyama ground station, datatake N/A */
    {"    ", "   "},        /* dtk_fillin_FA_response() will fill this in */
    {NULL,   NULL}
};

/* NOTE
-- The REQR does NOT include sensor information, so we arbitrarily assign 
-- tapedump downlink code DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE to the sensor.
-- This allows us to proceed to conflict analysis.  The sensor field will be
-- correctly populated when the OPL1 file is ingested
        EQUIV_TABLE   NASDA_REQR_sensor[]=  not needed
        EQUIV_TABLE   NASDA_REQR_actid[]=   not needed
*/

EQUIV_TABLE   NASDA_REQR_transid[]=
{   {"X1",  "F5"},
    {"X2",  "F6"},
    {"X3",  "F7"},
    {NULL, NULL}
};
 
EQUIV_TABLE   NASDA_sat[]= 
{   {"ERS1",    "J1"},
    {"ADEOS",   "A1"},
    {NULL, NULL}
};


EQUIV_TABLE   NASDA_dtk_stat[]= 
{   
    {"OPLN    ",    "SCH"}, 
    {"OPL1",        "SCH"}, 
    {"REQM    ",    "REQ"}, 
    {"REQR",        "PLN"},
    {NULL,    NULL}
};

EQUIV_TABLE   NASDA_activity_type[]= 
{   
    {"OPLN    ",    MU_OPLN },
    {"OPL1",        MU_OPL1 }, 
    {"REQM    ",    MU_REQM_msge }, 
    {"REQR",        MU_REQR_STGS },
    {"REQA    ",    MU_REQA },
    {NULL,    NULL}
};


/*
-- NOTE: Use DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE in the case of DUMP activity.
--       Use DTKM_SENSOR_REALTIME_DOWNLINK_CODE actid for REALTIME activity (new)
--
-- Also, for REALTIME, we do not fill in the agency part of the actid, 
-- because this will be filled in later by dtkm_default_values().
*/
EQUIV_TABLE   NASDA_opln_actid[]= 
{   
/*  external  APS internal value */
    {"HEOCM", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "NAS"}, 
    {"FAISM", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE "ASF"}, 
    {"    M", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {"HEOCR", DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {"FAISR", DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {"    R", DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_actid_2_sensor[]= 
{   
/*external  APS internal value */
    {"R",   DTKM_SENSOR_REALTIME_DOWNLINK_CODE},  /* The recording acquisition */
    {"M",   DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE},  /* The dump acquisitions  */ 
    {NULL,  NULL}
};


/*
-- NOTE:
-- For DUMP observation, the 'Recording' activity id given below. 
-- For REALTIME observation, there used to be no change from acquisition 
-- activity id. Now, however, we maintain REALTIME observation information;
-- that is why we use the 'Realtime Observation' activity id.
*/
EQUIV_TABLE   NASDA_obs_actid[]= 
{   
/*external  APS internal value */
    {"R",   DTKM_ACTID_REALTIME_OBSERVATION_CODE},  /* Realtime Observation */
    {"M",   DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* Recording activity   */
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_plan_flags[]= 
{   
    {"P", "SKIP"},    /* P = in the past; skip the record.                 */
    {"F", "READ"},    /* F = in the future; read and process the record.   */
    {NULL, NULL}
};

EQUIV_TABLE  NASDA_station_flags[]=
{
    {"HEOC", "SKIP"},       /* Hatoyama station ADEOS,  datatake N/A */
    {"ASF ", "READ"},       /* ASF ground station ADEOS,datatake OK  */
    {"WFF ", "SKIP"},       /* McMurdo station ADEOS,   datatake N/A */
    {NULL,    NULL}
};


/*
-- the following assumes that all observation records are written
*/
EQUIV_TABLE   NASDA_mode_flags[]= 
{   
    {"R", "RITE"},    /* R = Realtime, acquisition record is written (new)  */
    {"M", "RITE"},    /* M = Dump, the acquisition record is written        */
    {NULL, NULL}
} ;
/*
-- the following assumes that all observation records are written
*/
EQUIV_TABLE    ADEOS_mode_flags[]=
{
    {"REAL",    "RITE"},
    {"MDR ",    "RITE"},
    {"LMDR",    "RITE"},
    {NULL,      NULL}
} ;

EQUIV_TABLE   ADEOS_acq_actid[]= 
{   
    /* 
    -- model real-time dtks with DTKM_SENSOR_REALTIME_DOWNLINK_CODE record  (new)
    -- for the downlink - a SCH status, and then the observations
    -- inherit this SCH status.
    -- We no longer give them a status of "INV" to make them "invisible" 
    -- to the APS.
    */
/* external  APS internal value */
    {"REAL", DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {"MDR ", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {"LMDR", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {NULL, NULL}
};

EQUIV_TABLE   ADEOS_actid_2_sensor[]= 
{   

    /* 
    -- model real-time dtks with DTKM_SENSOR_REALTIME_DOWNLINK_CODE record  (new)
    -- for the downlink - a SCH status, and then the observations
    -- inherit this SCH status.
    -- We no longer give them a status of "INV" to make them "invisible" 
    -- to the APS.
    */
/* external  APS internal value */
    {"REAL", DTKM_SENSOR_REALTIME_DOWNLINK_CODE}, 
    {"MDR ", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {"LMDR", DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE}, 
    {NULL, NULL}
};

/*
-- For DUMP observation, the 'Recording' activity id given below. 
-- For REALTIME observation, there used to be no change from acquisition 
-- activity id. Now, however, we maintain REALTIME observation information;
-- that is why we use the 'Realtime Observation' activity id.
*/
EQUIV_TABLE   ADEOS_obs_actid[]= 
{   
/* external  APS internal value */
    {"REAL", DTKM_ACTID_REALTIME_OBSERVATION_CODE}, /* Realtime Observation */
    {"MDR ", DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* Recording activity  */
    {"LMDR", DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* Recording activity  */
    {NULL, NULL}
};

EQUIV_TABLE   ADEOS_sensor[]=
{
    {"NSCAT ",  "NSC"}, 
    {"TOMS  ", "TOM"}, 
    {"OCTS  ", "OCT"}, 
    {"POLDER", "POL"}, 
    {"ILAS  ", "LAS"}, 
    {"IMG   ", "IMG"}, 
    {"TEDA  ", "EDA"}, 
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_condition2dtkstat[]= 
{   
    {"NORM", "REP"}, /* normal condition, but REP is tentative.            */ 
                     /* final determination made by dtk_status_by_blanking */
    {"    ", "REP"}, /* omission, but assign a tentative REP. See above.   */
    {"TIME", "REJ"},    /* illegal observing period     */ 
    {"GRS ", "REJ"},    /* illegal GRS number           */ 
    {"PRY ", "REJ"},    /* illegal priority level       */ 
    {"SENS", "REJ"},    /* illegal sensor type          */ 
    {"INF ", "REJ"},    /* illegal sensor condition     */ 
    {"RSP ", "REJ"},    /* illegal RSP data             */ 
    {"DUPL", "REJ"},    /* exist duplicate GRS number   */ 
    {"COVR", "REJ"},    /* out of receiving range   */ 
    {"VNIR", "REJ"},    /* exist MDR operation for VNIR*/ 
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_condition2notes[]= 
{   
    {"NORM", ""}, /* normal condition, but PLN is tentative.            */ 
                     /* final determination made by dtk_status_by_blanking */
    {"    ", ""}, /* omission, but assign a tentative PLN. See above.   */
    {"TIME", "TIME ERROR in time bracket"},    
    {"GRS ", "GRS ERROR GRS"},    
    {"PRY ", "PRY ERROR illegal priority level"},  
    {"SENS", "SENS ERROR illegal sensor type"},   
    {"INF ", "INF ERROR illegal sensor condition"},
    {"RSP ", "RSP ERROR illegal RSP data"},    
    {"DUPL", "DUPL ERROR duplicate GRS number"},
    {"COVR", "COVR ERROR out of receiving range"},
    {"VNIR", "VNIR ERROR MDR operation for VNIR"},
    {NULL, NULL}
};

EQUIV_TABLE   NASDA_station_id[]= 
{   
    {"FAIS", "ASF"},        /* ASF ground station,      datatake OK  */
    {"HEOC", "XXX"},        /* Hatoyama ground station, datatake N/A */
    {"ASF ", "ASF"},        /* ASF ground station ADEOS,datatake OK  */
    {"WFF ", "MCM"},        /* McMurdo station ADEOS,   datatake OK  */
    {NULL,   NULL}
};

EQUIV_TABLE   NASDA_valid_downlink_stations[]= 
{   
    {"FAIS", "ASF"},        /* ASF ground station,      datatake OK  */
                            /* 
                            -- All other ground stations are invalid 
                            -- for downlink purposes.
                            */
    {NULL,   NULL}
};

EQUIV_TABLE   NASDA_sensor[]= 
{   
    {"SAR", "SAR"},       
    {"OPS", "OPS"},       
    {"OVN", "VNR"},        /* Visual Near Infrared    */
    {NULL,  NULL}
};

EQUIV_TABLE     NASDA_REQM_recording_actid[]=
{   
/* external  APS internal value */
    {"SAR", DTKM_ACTID_RECORDING_OBSERVATION_CODE},       
    {"OPS", DTKM_ACTID_RECORDING_OBSERVATION_CODE},       
    {"OVN", DTKM_ACTID_RECORDING_OBSERVATION_CODE}, /* Visual Near Infrared */
    {NULL,  NULL}
};



/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_reqr[] =  

Description:    description of REQR data records.  

Parameters:     

Returns:        

Creator:       Miguel Siu 

Creation Date:  Thu Sep  7 15:14:56 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
VALUE_DEFS NASDA_valuedefs_reqr[] =  
{
    /* 
    -- HEADER FIELDS:  
    -- EXAMPLE of REQR header:  
    --           10        20        30        40        50        60        70
    --  01234567890123456789012345678901234567890123456789012345678901234567890
    --  |    |    |    |    |    |    |    |    |    |    |    |    |    |    
    --
    --  REQR000087 ADEOS  HMMO ASF  19950406 03:51:57   70   886 19960424 19960
    --  521 19950217 V44                                        
    */
    /*  0 reqr */
    {FILE_HEADER,   REPORT_CONTROL,
        4,  6,  gen_string2str,         NULL,   (int) fa_file_id},
    {FILE_HEADER,   REPORT_HEADER,
        0,  10, gen_string2malloc_str,
                                        NULL,   (int) &fa_filename} ,
    {FILE_HEADER,   REPORT_HEADER,
        23, 4,  gen_string2malloc_str,  NULL,   (int) &fa_file_dest}, 
    {FILE_HEADER,   REPORT_HEADER,
        28, 8,  NASDAc_yyyymmdd2asftime,NULL,   (int) fa_creation_date} ,
    /*
    -- fa_record_size is informational, not used by any file utilities
    */
    /*  4 reqr */
    {FILE_HEADER,   REPORT_HEADER,
        46, 4,  gen_string2int,         NULL,   (int) &fa_record_size},
    {FILE_HEADER,   REPORT_HEADER,
        51, 5,  gen_string2int,         NULL,   (int) &fa_number_of_records},
    {FILE_HEADER,   REPORT_HEADER,
        57, 8,  gen_string2str,         NULL,   (int) &fa_start_yyyymmdd},
    {FILE_HEADER,   REPORT_HEADER,
        66, 8,  gen_string2str,         NULL,   (int) &fa_stop_yyyymmdd},

    /*  8 reqr */
    {FILE_HEADER,   REPORT_RECORD,
        0,  4,  table_lookupFA2APS,     (void *)NASDA_dtk_stat,
                                                DTK_DTKSTAT},
    {FILE_HEADER,   REPORT_CONTROL,
        0,  4,  table_lookupFA2APS,     (void *)NASDA_activity_type,
                                                (int) fa_activity_type},
    {FILE_HEADER,   REPORT_RECORD,
        11, 6,  table_lookupFA2APS,     (void *)NASDA_sat,
                                                DTK_SAT} ,
    /* 
    -- The following global variable fa_sat_id is needed by various
    -- conversions routines.
    */
    /* 11 reqr */
    {FILE_HEADER,   REPORT_CONTROL,
        11, 6,  table_lookupFA2APS,     (void *)NASDA_sat,  
                                                (int) fa_sat_id} ,
    {FILE_HEADER,   REPORT_RECORD,
        23, 3,  gen_string2str,         NULL,   DTK_STATION_ID},

    /* 
    -- DEFAULT FIELD   
    */
    /* 13 reqr */
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_darid,   NULL, DTK_DARID},
    /* The following is used to get permissions in multi-user environment */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_default_ALL_stations,   NULL,
                                (int)fa_station_id},
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,                NULL,
                                (int) &fa_trigger_dtk_list_handling_for_STGS},
    /* 16 reqr */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,                NULL,
                                (int) &fa_trigger_always_create_response_file},
    /*
    -- File sequential number information is now carried in ??
    --
    --     < VALUE_DEFS entry for DTK_NOTES omitted here >
    --
    -- However, insert an identifier 'RSP. =' since RSP information 
    -- for start_time, stop_time is still carried in DTK_NOTES.
    */
    /* 17 reqr */
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_notes,   NULL, DTK_NOTES},


    /*  
    --  REQR RECORD DATA FIELDS:  
    --
    -- EXAMPLE of REQR record (70 chars):  
    --           10        20        30        40        50        60        70
    --  01234567890123456789012345678901234567890123456789012345678901234567890
    --  |    |    |    |    |    |    |    |    |    |    |    |    |    |    
    -- 
    --  19960424 00:58:13 19960424 01:08:10    51  80.52    51 116.00 X3 REAL
    --  19960424 00:59:13 19960424 00:59:55    51  84.08    51  86.61 X1 MDR3
    --
    */
    /* 18 reqr */
    {FILE_RECORD,   REPORT_RECORD,
        0,  17, NASDAc_yyyymmdd_hhmmss2rev,     
                                            NULL,       DTK_REV},
    {FILE_RECORD,   REPORT_RECORD,
        0,  17, NASDAc_yyyymmdd_hhmmss2asf,     
                                            NULL,       DTK_STRTTIME},
    {FILE_RECORD,   REPORT_RECORD,
        18, 17, NASDAc_yyyymmdd_hhmmss2asf,
                                            NULL,       DTK_STOPTIME},
    /* 21 reqr */
    {FILE_RECORD,   REPORT_RECORD,
        36, 25, gen_string2strcat,          NULL,       DTK_NOTES},
    {FILE_RECORD,   REPORT_RECORD,
        62, 2,  table_lookupFA2APS,         NASDA_REQR_transid, DTK_TRANSID},
    {FILE_RECORD,   REPORT_RECORD,
        65, 4,  gen_default_DMP,            NULL,       DTK_SENSOR},
    {FILE_RECORD,   REPORT_RECORD,
        65, 4,  gen_default_DMP,            NULL,       DTK_ACTID},

    {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           


/*==============================================================================
Function:       FA_FILEDEF NASDA_filedef_reqr

Description:    describes the REQR file for the file processor driver.  

Parameters:     

Returns:        

Creator:        Miguel Siu 

Creation Date:  Thu Sep  7 15:31:18 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_reqr = 
{
    /* 
    -- header composed of "header" 128 
    */
    128,                    /* header length      */
    70,                     /* data record length */
    NASDA_valuedefs_reqr    /* pointer to the file field descriptions       */
} ;


/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_reqa[] =  

Description:    description of REQA data records.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 17 15:39:31 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
VALUE_DEFS NASDA_valuedefs_reqa[] =  
{
    /* NOTE for REQA:  */
    /************************************************************************ 
     * check for NASDA - blanked out values indicating unplanned request.   * 
     * Sept-1994 LJS:  I checked my notes and found a note from a meeting:  * 
     *      if unplanned, "concerned fields are left blank"                 * 
     * the indication was that there might be nothing wrong with the        * 
     * request format (NORM), and that the request could be unplanned       * 
     * for other reasons                                                    * 
     ************************************************************************/

    /* 
    -- HEADER FIELDS:  
    -- EXAMPLE of REQA header:  
    --  0123456789012345678901234567890123456789012345678901234567890
    --
    --  REQA    ERS1HMMOFAIS1994101312:25:04N008200038
    */

    /*  0 reqa */
    {FILE_HEADER,   REPORT_HEADER,
        0,   8, gen_string2malloc_str,  NULL,   (int) &fa_filename} ,
    /*  1 reqa */
    {FILE_HEADER,   REPORT_CONTROL,
        0,  8,  table_lookupFA2APS,         (void *)NASDA_activity_type,
                                                        (int) fa_activity_type},
    {FILE_HEADER,   REPORT_HEADER,
        16, 4,  gen_string2malloc_str,  NULL,   (int) &fa_file_dest}, 
    {FILE_HEADER,   REPORT_HEADER,
        20, 16, NASDAc_headertime2asftime,NULL, (int) fa_creation_date} ,
    /*
    -- fa_record_size is informational, not used by any file utilities
    */
    /*  4 reqa */
    {FILE_HEADER,   REPORT_HEADER,
        37, 4,  gen_string2int,         NULL,   (int) &fa_record_size},
    {FILE_HEADER,   REPORT_HEADER,
        41, 5,  gen_string2int,         NULL,   (int) &fa_number_of_records},
    {FILE_HEADER,   REPORT_RECORD,
        8,  4,  table_lookupFA2APS,     (void *)NASDA_sat,
                                                DTK_SAT} ,
    /* 
    -- DEFAULT FIELD   
    */
    /*  7 reqa */
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_darid,   NULL, DTK_DARID},

    /*
    -- gen_set_trigger populates triggers with a TRUE value
    */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,        NULL,
                                    (int) &fa_trigger_dtk_status_by_blanking},
    /*  9 reqa */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,        NULL,
                                    (int) &fa_trigger_dtk_fillin_FA_response},
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,        NULL,
                                    (int) &fa_trigger_dtk_create_FA_downlinks},

    /* The following is used to get permissions in multi-user environment */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_default_ASF_station,    NULL,
                                    (int)fa_station_id},

    /*  
    --  REQA RECORD DATA FIELDS:  
    --
    -- EXAMPLE of REQA record (82 chars):  
01234567890123456789012345678901234567890123456789012345678901234567890123456789

S1234501NORM1994103119941211533261.96533270.00199412091994120920:05:5820:14:12FAIS
    --
    */
    /* 
    -- The following is just a check of DTKM_FADTKID flag, needs no destination
    */
    /* 11 reqa */
    {FILE_RECORD,   REPORT_CONTROL,
        0,  8,  NASDAc_check_fadtkid,       NULL,       NULL},
    {FILE_RECORD,   REPORT_RECORD,
        0,  8,  gen_string2str,             NULL,       DTK_FADTKID},
    /* 13 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        0,  1,  table_lookupFA2APS,         (void *)NASDA_REQA_sensor,
                                                        DTK_SENSOR},
    {FILE_RECORD,   REPORT_RECORD,
        0,  1,  table_lookupFA2APS,         (void *)NASDA_REQA_actid,
                                                        DTK_ACTID},
    /* 15 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        1,  5,  gen_string2int,             NULL,       DTK_REV},
    /* 16 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        6,  2,  gen_string2tinyint,         NULL,       DTK_DTKID},
    /* 
    -- The following is a temporary determination, which could be superseded
    -- by the function dtk_status_by_blanking()
    */
    /* 17 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        8,  4,  table_lookupFA2APS,         (void *)NASDA_condition2dtkstat,
                                                        DTK_DTKSTAT},
    /* 18 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        8,  4,  table_lookupFA2APS,         (void *)NASDA_condition2notes,
                                                        DTK_NOTES},
    {FILE_RECORD,   REPORT_RECORD,
        28, 26, NASDAc_reqa_rsp_date2strttime,
                                            NULL,       DTK_STRTTIME},
    /* 20 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        28, 26, NASDAc_reqa_rsp_date2stoptime,
                                            NULL,       DTK_STOPTIME},
/* 
--      < VALUE_DEFS entry for DTK_REV omitted here >
*/
    /* 21 reqa */
    {FILE_RECORD,   REPORT_CONTROL,
        28, 26, NASDAc_reqa_rsp_date2trig,  NULL,       
                                    (int) &fa_trigger_skip_default_values},
    /* 22 reqa */
    {FILE_RECORD,   REPORT_RECORD,
        78, 4,  table_lookupFA2APS,         (void *)NASDA_REQA_station_id,
                                                        DTK_STATION_ID},

    {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           


/*==============================================================================
Function:       FA_FILEDEF NASDA_filedef_reqa

Description:    describes the REQA file for the file processor driver.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 17 14:52:06 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_reqa = 
{
    /* 
    -- header composed of "header" 128 
    */
    128,                    /* header length      */
    82,                     /* data record length */
    NASDA_valuedefs_reqa    /* pointer to the file field descriptions       */
} ;


/*==============================================================================
Function:       VALUE_DEFS NASDA_subrecord_valuedefs_opln[]

Description:    this is a field description of the OPLN subrecord, 
                in this case an observation record, which defines the 
                fields and the translator routines.  In some cases, 
                this is a table lookup and the EQUIV_TABLE for it is 
                listed.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 15:27:40 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
/* DEFINITIONS FOR OPLN */
VALUE_DEFS NASDA_subrecord_valuedefs_opln[] =  
{
    /*  OBSERVATION RECORD DATA FIELDS:  */
    /*  the format of the operation times are the same as the link times. */
    /* 
    -- NOTE:  format is:  YYYYMMDDhh:mm:sshh:mm:ss     
    --        it gives date, start time, end time.  
    -- EXAMPLE of OPLN subrecord (observation record):
    -- 01234567890123456789012345678901234567890123456789
    --
    -- 1994101603:51:2403:55:29127127.14127142.43OVN0979010
    --
    */
    /*  0 opln */
    {FILE_RECORD,   REPORT_RECORD,
        0,  24, NASDAc_link_time2strttime,
                            NULL,               DTK_STRTTIME},
    {FILE_RECORD,   REPORT_RECORD,
        0,  24, NASDAc_link_time2stoptime,
                            NULL,               DTK_STOPTIME},
    {FILE_RECORD,   REPORT_RECORD,
        0,  24, NASDAc_link_time2rev,
                            NULL,               DTK_REV},
    /*  3 opln */
    {FILE_RECORD,   REPORT_RECORD,
        42, 3,  table_lookupFA2APS,     
                            NASDA_sensor,       DTK_SENSOR},

    {FILE_RECORD,   REPORT_RECORD,
        45, 6,  NASDAc_update_fadtkid,  NULL,   DTK_FADTKID },

    /*
    -- observation (sensing) dtks inherit their DTK_ACTID from the
    -- acquisition (downlink)dtk via global variable fa_acquisition_mode
    -- in the routine NASDAc_inherit_actid
    */
    /*  5 opln */
    {FILE_RECORD,   REPORT_RECORD,
        1,  1,  NASDAc_inherit_actid,   NULL,   DTK_ACTID},

    /* 
    -- The following declarations do NOT need a source string,
    -- but we pass one anyway to satisfy the action of FILE_RECORD
    */

    /*
    -- recording dtks inherit their DTK_FA_SCHEDULE_LINK from acquisition dtk
    -- via global variable fa_schedule_link
    */
    /*  6 opln */
    {FILE_RECORD,   REPORT_RECORD,
        1,  1,  NASDAc_inherit_fa_sch_link,
                                NULL,           DTK_FA_SCHEDULE_LINK},
    /*
    -- THE INSTANCE OF ZERO RECORDS IS THE CONTROL THAT STOPS THE RECURSION
    */
    /*  7 opln */
    {FILE_RECORD,   REPORT_CONTROL,
        1,  1,  NASDAc_default_subrecords,
                                NULL,           (int) &fa_number_of_subrecords},

    {0, 0, -1, -1, NULL, NULL, NULL} 
} ;


/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_opln

Description:    describes the data records of the OPLN, in this case 
                the acquisition records.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 15:30:41 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.

EXAMPLE of OPLN acquisition record followed by its observation records:  
01234567890123456789012345678901234567890123456789012345678901234567890123456789

FF21994101419:41:11 1994101419:49:32 5650977031994101419:41:0719:49:21MHEOCM 3
1994101403:50:5803:55:03125142.12125157.41OVN0977030H                           
1994101406:57:1606:59:21213119.65213127.45OVN0977040H                           
1994101405:21:0705:23:12169119.65169127.45SAR097703  AA 00

01234567890123456789012345678901234567890123456789012345678901234567890123456789
==============================================================================*/
VALUE_DEFS NASDA_valuedefs_opln[] =  
{
    /* 
    -- HEADER FIELDS:  
    -- EXAMPLE of OPLN header:
    -- 01234567890123456789012345678901234567890123456789
    --
    -- OPLN    ERS1HMMOFAIS1994101307:00:17Y008000021
    */

    /*  0 opln */
    {FILE_HEADER,   REPORT_HEADER,
        0,   8, gen_string2malloc_str,  NULL,   (int) &fa_filename} ,
    {FILE_HEADER,   REPORT_HEADER,
        16, 4,  gen_string2malloc_str,  NULL,   (int) &fa_file_dest}, 
    {FILE_HEADER,   REPORT_HEADER,
        20, 16, NASDAc_headertime2asftime,
                                        NULL,   (int) fa_creation_date} ,
    /*
    -- fa_record_size is informational, not used by any file utilities
    */
    /*  3 opln */
    {FILE_HEADER,   REPORT_HEADER,
        37, 4,  gen_string2int,         NULL,   (int) &fa_record_size},
    {FILE_HEADER,   REPORT_HEADER,
        41, 5,  gen_string2int,         NULL,   (int) &fa_number_of_records},

    /* 
    -- The following global variable fa_sat_id is needed by various
    -- conversions routines.
    */
    /*  5 opln */
    {FILE_HEADER,   REPORT_CONTROL,
        8,  4,  table_lookupFA2APS,     (void *)NASDA_sat,  
                                                (int) fa_sat_id} ,
    {FILE_HEADER,   REPORT_RECORD,
        8,  4,  table_lookupFA2APS,     (void *)NASDA_sat,  
                                                DTK_SAT} ,
    /*  7 opln */
    {FILE_HEADER,   REPORT_CONTROL,
        0,  8,  table_lookupFA2APS,     (void *)NASDA_activity_type,
                                                (int) fa_activity_type},
    {FILE_HEADER,   REPORT_RECORD,
        0,  8,  table_lookupFA2APS,     (void *)NASDA_dtk_stat,
                                                DTK_DTKSTAT},
    {FILE_HEADER,   REPORT_RECORD,
        16, 4,  table_lookupFA2APS,     (void *)NASDA_valid_downlink_stations,
                                                DTK_STATION_ID},

    /* The following is used to get permissions in multi-user environment */
    /* 10 opln */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_default_ASF_station,
                                        NULL,   (int)fa_station_id},
    /* 
    -- DEFAULT FIELD
    -- fa_subrecord_size is informational, not used by any file utilities
    */
    /* 11 opln */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, NASDAc_default_observation_size,                
                                        NULL,   (int) &fa_subrecord_size},
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_darid,   NULL,   DTK_DARID},

    /* 
    -- DATA RECORD FIELDS  (ACQUISITION + OBSERVATION):  
    --
    -- NOTE:  the opln data records have a single "Acquisition record"
    -- followed by one or more "Observation records".  
    -- the "Acquisition record" pertains to downlink events (acquistions)
    -- while the "Observation record" pertains to the satellite 
    -- getting image data from the ground by observing it.  
    -- thus a Real Time data-take is one Acquisition record, the downlink 
    -- info, followed by a single Observation record, the sensing info.  
    -- and a Tape Dump data-take is one Acquisition record (the tape dump)
    -- followed by one or more observations (the sensor recording).
    */

    /*  
    --  ACQUISITION RECORD DATA FIELDS:  
    -- EXAMPLE of OPLN acquisition record:
    --
012345678901234567890123456789012345678901234567890123456789012345678901234567

FF21994101606:42:44 1994101606:52:42 2150979031994101606:42:3906:50:53MHEOCM 3
    --
    */
    /* 13 opln */
    {FILE_RECORD,   REPORT_RECORD,
        40, 6,  gen_string2str,         NULL,       DTK_FADTKID },

    {FILE_RECORD,   REPORT_RECORD,
        -1, -1, gen_string2strcat,      "/",        DTK_FADTKID },

    {FILE_RECORD,   REPORT_RECORD,
        37, 3,  gen_string2strcat,      NULL,       DTK_FADTKID },

    /* 
    -- the '000000' is important.  for a TAPEDUMP, it stays at '000000', 
    -- for a REAL TIME, the '000000' is updated by NASDAc_update_fadtkid()
    -- from the observation (sub)record field.  
    */
    /* 16 opln */
    {FILE_RECORD,   REPORT_RECORD,
        -1, -1, gen_string2strcat,      "/000000",  DTK_FADTKID },

    /* this is the number of observation records (subrecords) to come:  */
    /* 17 opln */
    {FILE_RECORD,   REPORT_CONTROL,
        76, 2,  gen_string2int,     NULL,       (int) &fa_number_of_subrecords},

    /* 
    -- if the value of this field is not 'F', then SKIP the 
    -- record AND ALL OF ITS OBSERVATION RECORDS as well.  
    -- the value can be 'F' (future) or 'P' (past). 
    -- note that the SKIP action uses the fa_number_of_subrecords obtained above
    */
    /* 18 opln */
    {FILE_RECORD,   REPORT_CONTROL,
        0,  1,  table_lookupFA2APS, (void *)NASDA_plan_flags,
                                                (int)fa_processing_flag},
    {FILE_RECORD,   REPORT_RECORD,
        1,  2,  gen_string2str,             NULL,       DTK_TRANSID},

    /*
    -- NOTE: we no longer need AOS_TIME/LOS_TIME. Any antenna-specific
    -- procedures are now handled during conflict analysis of each 
    -- individual datatake proposal, and during creation of the WOS file
    -- which contains the instructions for the antenna.
    --
                < 2 VALUE_DEFS entry has been ommitted here >
    */
    /* 20 opln */
    {FILE_RECORD,   REPORT_RECORD,
        40, 6,  gen_string2str,             NULL,       DTK_FA_SCHEDULE_LINK},
    {FILE_RECORD,   REPORT_CONTROL,
        40, 6,  gen_string2str,             NULL,       (int)fa_schedule_link},
    /* 
    -- NOTE:  the format for the 24-character time is:  
    --        YYYYMMDDhh:mm:sshh:mm:ss     
    --        it gives date, start time, end time.  
    */
    /* 22 opln */
    {FILE_RECORD,   REPORT_RECORD,
        46, 24, NASDAc_link_time2strttime,  NULL,       DTK_STRTTIME},
    /* 23 opln */
    {FILE_RECORD,   REPORT_RECORD,
        46, 24, NASDAc_link_time2stoptime,  NULL,       DTK_STOPTIME},

    {FILE_RECORD,   REPORT_RECORD,
        46, 24, NASDAc_link_time2rev,       NULL,       DTK_REV},

    /*
    -- NOTE: we no longer look at the 'requesting station' value in the
    -- data records.  Datatakes will be processed regardless of who 
    -- requested them.  This mimics the behaviour of the old OPLN
    -- ingestion in R1Aprime delivery.
    --
                < a VALUE_DEFS entry has been ommitted here >
    */

    /* 25 opln */
    {FILE_RECORD,   REPORT_RECORD,
        75, 1,  table_lookupFA2APS,     (void *)NASDA_actid_2_sensor,
                                                DTK_SENSOR},

    /* 
    -- the following field is the Requesting Agency + Acquisition Mode, 
    -- with values  of 'FAIS'(ASF station) 'HEOC','    ' (HEOC station)
    -- and 'R' (realtime) or 'M' (MDR tape dump).
    */
    /* 26 opln */
    {FILE_RECORD,   REPORT_RECORD,
        71, 5,  table_lookupFA2APS,     (void *)NASDA_opln_actid,
                                                DTK_ACTID},
    /*
    -- the fa_record_write_flag will toggle the 'write' action 
    -- of fa_ascii_record_ingestion for acquisition record. 
    */
    {FILE_RECORD,   REPORT_CONTROL,
        75, 1,  table_lookupFA2APS,     (void *)NASDA_mode_flags,
                                                (int)fa_record_write_flag},
    /*
    -- the fa_acquisition_mode will be used by NASDAc_inherit_actid()
    */
    {FILE_RECORD,   REPORT_CONTROL,
        75, 1,  table_lookupFA2APS,     (void *)NASDA_obs_actid,
                                                (int) fa_acquisition_mode},

    /* 29 opln */
    {FILE_SUBRECORD,    REPORT_CONTROL,
        -1, -1, NULL,                   (void *)&NASDA_filedef_subrecord_opln,                                                  NULL },

    {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           



/*==============================================================================
Function:       NASDA_filedef_subrecord_opln

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Fri Aug  4 14:53:22 PDT 1995

Notes:      
==============================================================================*/
FA_FILEDEF NASDA_filedef_subrecord_opln =
{
    0,
    80,
    NASDA_subrecord_valuedefs_opln
} ;

/*==============================================================================
Function:       FA_FILEDEF NASDA_filedef_opln

Description:    describes the OPLN file for the file processor driver.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 15:32:04 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_opln = 
{
    /* 
    -- header composed of "header" 128 
    -- + "file descriptor" 278
    */
    406,                    /* header length      */
    80,                     /* data record length */
    NASDA_valuedefs_opln    /* pointer to the file field descriptions       */
} ;

/*==============================================================================
Function:       VALUE_DEFS NASDA_subrecord_valuedefs_opl1[]

Description:    this is a field description of the OPL1 subrecord, 
                in this case an observation record, which defines the 
                fields and the translator routines.  In some cases, 
                this is a table lookup and the EQUIV_TABLE for it is 
                listed.  

Parameters:     

Returns:        

Creator:        Miguel Siu 

Creation Date:  Thu Oct 12 13:45:13 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
/* DEFINITIONS FOR OPL1 */
VALUE_DEFS NASDA_subrecord_valuedefs_opl1[] =  
{
    /*  OBSERVATION RECORD DATA FIELDS:  */
    /* 
    -- EXAMPLE of OPLN subrecord (observation record):
         10        20        30        40        50        60        70
01234567890123456789012345678901234567890123456789012345678901234567890123456789
     |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
OCTS   OBS NOCT0055001 OCTS00004867    41  69.72    41 249.72  1556  1556 1996052 1 00:32:44 19960521 01:23:12 19960521 00:34:05 19960521 00:44:23 SOCT0055002 *********** *********** ********* ** ***********
    -- 
    -- Reference:   ADEOS file descriptions, page 23, Table 4.  
    --              OPL1 Data Record (observation)
    */
    /*  0 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        0,  6,  table_lookupFA2APS,     (void *)ADEOS_sensor,       DTK_SENSOR},

    /*
    -- observation (sensing) dtks inherit their DTK_ACTID from the
    -- acquisition (downlink)dtk via global variable fa_acquisition_mode
    -- in the routine NASDAc_inherit_actid
    */
    {FILE_RECORD,   REPORT_RECORD,
        1,  1,  NASDAc_inherit_actid,   NULL,                       DTK_ACTID},
    /* 
    -- We no longer use Orbit Total Number corresponding to the start time
    -- to get DTK_REV.  See note for DTK_STRTTIME.
    --
    -- We DO use the Start of Mission Data in Acquisition segment, and derive
    -- our DTK_REV from there.
    */
    /* 2 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        110,17, NASDAc_yyyymmdd_hhmmss2rev,         NULL, DTK_REV},


    /* NOTE: 
    -- we are now using the Start of Mission Data in Acquisition Segment field.
    -- Lee Poulson, who is familiar with the ADEOS satellite, says that the
    -- field refers to the observation time for the sensor being downlinked.
    --
    -- Previously, we were using a field that was reflects the larger view of
    -- the sensor's operation, but may not necessarily be the best to describe
    -- what is being downlinked during the acquisition
    */
    /*  3 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        110,17, NASDAc_yyyymmdd_hhmmss2asf,
                                        NULL,               DTK_STRTTIME},
    /*
    -- see comments for DTK_STRTTIME, above
    */
    /*  4 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        128,17, NASDAc_yyyymmdd_hhmmss2asf,
                                        NULL,               DTK_STOPTIME},
    /*  5 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        146,    11, gen_string2str,     NULL,               DTK_FADTKID},

    /* 
    -- for real-time observations, there is no need to set dtkstat to "INV"
    --            < a VALUE_DEFS entry is omitted here >
    */

    /* 
    -- The following declarations do NOT need offset+length arguments,
    -- but we pass dummy values to satisfy the action of FILE_RECORD
    */

    /*
    -- THE INSTANCE OF ZERO RECORDS IS THE CONTROL THAT STOPS THE RECURSION
    */
    /*  6 opl1 */
    {FILE_RECORD,   REPORT_CONTROL,
        0,  1,  NASDAc_default_subrecords,
                                NULL,           (int) &fa_number_of_subrecords},
    {0, 0, -1, -1, NULL, NULL, NULL} 
} ;


/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_opl1

Description:    describes the data records of the OPLN, in this case 
                the acquisition records.  

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Oct 12 13:45:13 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.

EXAMPLE of OPL1 acquisition record followed by its observation records:  
==============================================================================*/
VALUE_DEFS NASDA_valuedefs_opl1[] =  
{
    /* 
    -- HEADER FIELDS:  
    -- EXAMPLE of OPL1 header:
         10        20        30        40        50        60        70
01234567890123456789012345678901234567890123456789012345678901234567890123456789
     |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
OPL1001305 ADEOS  HMMO ASF  19960516 02:38:11 0000  2090 19960521 19960528 199601 12 V02
    */

    /* The following instruction gets the OPL1 descriptor (read a new record)
    -- It does not require an offset or width; we provide one for consistency.
    -- It will populate the array fa_subrecord_control[] which in turn is used
    -- to populate the global variable fa_number_of_subrecords !!
    -- Also, as a by-product, it will provide the fa_number_of_records.
    */
    /*  0 opl1 */
    {FILE_DESCRIPTOR,   REPORT_CONTROL,
        0,   1, NULL,                   NULL,   NULL } ,
    {FILE_HEADER,   REPORT_HEADER,
        0,  10, gen_string2malloc_str,  NULL,   (int) &fa_filename} ,
    {FILE_HEADER,   REPORT_RECORD,
        0,  4,  table_lookupFA2APS,     (void *)NASDA_dtk_stat,
                                                DTK_DTKSTAT},
    /*  3 opl1 */
    {FILE_HEADER,   REPORT_CONTROL,
        0,  4,  table_lookupFA2APS,     (void *)NASDA_activity_type,
                                                (int) fa_activity_type},
    /* 
    -- The following global variable fa_sat_id is needed by various
    -- conversions routines.
    */
    {FILE_HEADER,   REPORT_CONTROL,
        11, 5,  table_lookupFA2APS,     (void *)NASDA_sat,  
                                                (int) fa_sat_id} ,
    {FILE_HEADER,   REPORT_RECORD,
        11, 5,  table_lookupFA2APS,     (void *)NASDA_sat,  
                                                DTK_SAT} ,
    /*  6 opl1 */
    {FILE_HEADER,   REPORT_HEADER,
        23, 4,  gen_string2malloc_str,  NULL,   (int) &fa_file_dest}, 
    {FILE_HEADER,   REPORT_HEADER,
        28, 17, NASDAc_yyyymmdd_hhmmss2asf,NULL,(int) fa_creation_date} ,
    /*
    --        < VALUE_DEFS entry for fa_number_of_records omitted here >
    */


    /* 
    -- DEFAULT FIELD   
    */
    /*  8 opl1 */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,        NULL,
                                        (int) &fa_trigger_dtk_delete_range},
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_darid,   NULL,   DTK_DARID},

    /* The following is used to get permissions in multi-user environment */
    /* 10 opl1 */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_default_ASF_station,    NULL,
                                        (int)fa_station_id},


    /* 
    -- DATA RECORD FIELDS  (ACQUISITION + OBSERVATION):  
    --
    -- NOTE:  the opl1 data records have a single "Acquisition record"
    -- followed by one or more "Observation records".  
    -- the "Acquisition record" pertains to downlink events (acquistions)
    -- while the "Observation record" pertains to the satellite 
    -- getting image data from the ground by observing it.  
    -- thus a Real Time data-take is one Acquisition record, the downlink 
    -- info, followed by a single Observation record, the sensing info.  
    -- and a Tape Dump data-take is one Acquisition record (the tape dump)
    -- followed by one or more observations (the sensor recording).
    */

    /*  
    --  ACQUISITION RECORD DATA FIELDS:  
    -- EXAMPLE of OPL1 acquisition record:
         10        20        30        40        50        60        70
01234567890123456789012345678901234567890123456789012345678901234567890123456789
     |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
   41  82.26    41 118.98 NDTX0055001 19960521 00:36:15 19960521 00:46:32 X3 REAL 3 ASF
    */

    /*
    -- DTK_FA_SCHEDULE_LINK will be: Operation Plan Number / Downlnk Mode
    -- like this:  "NDTX0055003/REAL"   or "NDTX0055003/LMDR"
    -- since sometimes, as here, the same plan number is used for a realtime 
    -- and a tape dump on the same rev.  
    */
    /* 11 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        26, 11, gen_string2str,             NULL,       DTK_FA_SCHEDULE_LINK},
    /*
    Well, not needed anymore. This situation is 
    {FILE_RECORD,   REPORT_RECORD,
        -1, -1, gen_string2strcat,          "/",        DTK_FA_SCHEDULE_LINK},
    {FILE_RECORD,   REPORT_RECORD,
        77, 4,  gen_string2strcat,          NULL,       DTK_FA_SCHEDULE_LINK},
    */

    /* 
    -- DTK_FADTKID will be:   Operation Plan Number / Downlnk Mode
    -- like this:  "NDTX0055003/REAL"   or "NDTX0055003/LMDR"
    -- since sometimes, as here, the same plan number is used for a realtime 
    -- and a tape dump on the same rev.  
    */
    {FILE_RECORD,   REPORT_RECORD,
        26, 11, gen_string2str,             NULL,       DTK_FADTKID},
    /* 13 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        -1, -1, gen_string2strcat,          "/",        DTK_FADTKID},
    {FILE_RECORD,   REPORT_RECORD,
        77, 4,  gen_string2strcat,          NULL,       DTK_FADTKID},

    {FILE_RECORD,   REPORT_RECORD,
        38, 17, NASDAc_yyyymmdd_hhmmss2rev, NULL,       DTK_REV},
    /* 16 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        38, 17, NASDAc_yyyymmdd_hhmmss2asf, NULL,       DTK_STRTTIME},
    {FILE_RECORD,   REPORT_RECORD,
        56, 17, NASDAc_yyyymmdd_hhmmss2asf, NULL,       DTK_STOPTIME},
    /* 18 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        74, 2,  table_lookupFA2APS,         NASDA_REQR_transid,
                                                        DTK_TRANSID},
    {FILE_RECORD,   REPORT_RECORD,
        77, 4,  table_lookupFA2APS,     (void *)ADEOS_actid_2_sensor,
                                                DTK_SENSOR},
    /* 20 opl1 */
    {FILE_RECORD,   REPORT_RECORD,
        77, 4,  table_lookupFA2APS,     (void *)ADEOS_acq_actid,
                                                DTK_ACTID},
    /*
    -- the fa_record_write_flag will toggle the 'write' action 
    -- of fa_ascii_record_ingestion for acquisition record. 
    */
    {FILE_RECORD,   REPORT_CONTROL,
        77, 4,  table_lookupFA2APS,     (void *)ADEOS_mode_flags,
                                                (int)fa_record_write_flag},
    /*
    -- the fa_acquisition_mode will be used by NASDAc_inherit_actid()
    */
    /* 22 opl1 */
    {FILE_RECORD,   REPORT_CONTROL,
        77, 4,  table_lookupFA2APS,     (void *)ADEOS_obs_actid,
                                                (int)fa_acquisition_mode},
    {FILE_RECORD,   REPORT_RECORD,
        87, 4,  table_lookupFA2APS,     (void *)NASDA_station_id,   
                                                DTK_STATION_ID},
    /* 
    -- if the value of this field is 'HEOC', then SKIP the 
    -- record AND ALL OF ITS OBSERVATION RECORDS as well.  
    -- the value needs to be ASF which indicates ASF groundstation
    -- or WFF which indicates Wallops Flight Facility
    --
    -- note that the SKIP action uses the fa_number_of_subrecords obtained above
    -- note that this also supercedes previous determination
    -- of fa_processing_flag
    */
    /* 24 opl1 */
    {FILE_RECORD,   REPORT_CONTROL,
        87, 4,  table_lookupFA2APS,     (void *)NASDA_station_flags,    
                                                (int)fa_processing_flag},

    /* 
    -- this is the number of observation records (subrecords) to come,  
    -- get this number from global variable fa_subrecord_control:
    */
    {FILE_RECORD,   REPORT_CONTROL,
        0,  1,  NASDAc_query_subrecord, NULL,   (int) &fa_number_of_subrecords},

    /* 26 opl1 */
    {FILE_SUBRECORD,REPORT_CONTROL,
        -1, -1, NULL,                   (void *)&NASDA_filedef_subrecord_opl1,                                                  NULL },

    /*
    -- fa_record_size is informational, not used by any file utilities
    --
    --      < VALUE_DEFS entry for fa_record_size omitted here >
    */

    /* 
    -- fa_subrecord_size is informational, not used by any file utilities
    --
    --      < VALUE_DEFS entry for fa_subrecord_size omitted here >
    */

    /*
    -- The 'receiving' station id controls whether this record is processed.
    --
    --      < VALUE_DEFS entry for fa_processing_flag omitted here >
    */

    /* 27 opl1 */
    {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           



/*==============================================================================
Function:       NASDA_filedef_subrecord_opl1

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Oct 12 13:45:13 PDT 1995

Notes:      
==============================================================================*/
FA_FILEDEF NASDA_filedef_subrecord_opl1 =
{
    0,
    206,
    NASDA_subrecord_valuedefs_opl1
} ;

/*==============================================================================
Function:       FA_FILEDEF NASDA_filedef_opl1

Description:    describes the OPLN file for the file processor driver.  

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Oct 12 13:45:13 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_opl1 = 
{
    /* 
    -- header composed of "header" 128 
    -- + "file descriptor" 278
    */
    128,                    /* header length      */
    92,                     /* data record length */
    NASDA_valuedefs_opl1    /* pointer to the file field descriptions       */
} ;

/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_reqm

Description:    describes the data records of the REQM, in this case 
                the tape dump records.  

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 17:05:37 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
VALUE_DEFS NASDA_valuedefs_reqm[] =  
{
    /* 
    -- HEADER FIELDS:  
    --
    -- EXAMPLE of REQM header:
    --
    -- first part of header:
    -- 01234567890123456789012345678901234567890123456789
    -- 
    -- REQM    ERS1HMMOFAIS1994101312:35:35YVAR 00000
    -- 
    -- continuing here at offset 128 with number of tape dumps, recsize:
    -- 890123456789012345
    -- 
    --  182 126 264  46
    */
    /*  0 reqm */
    {FILE_HEADER,   REPORT_HEADER,
        0,  8,  gen_string2str,             NULL,(int) &fa_filename} ,
    {FILE_HEADER,   REPORT_HEADER,
        16, 4,  gen_string2str,             NULL,(int) &fa_file_dest} ,
    {FILE_HEADER,   REPORT_HEADER,
        20, 16, NASDAc_headertime2asftime,  NULL,(int) fa_creation_date} ,
    /* 
    -- NOTE:  the tape dump records are all up front, followed by 
    -- observation records which are processed later.
    -- The number of observation records is stored in fa_number_of_subrecords.
    */
    /*  3 reqm */
    {FILE_HEADER,   REPORT_CONTROL,
        128, 4,  gen_string2int,            NULL,(int) &fa_number_of_records},

    /*  4 reqm */
    /*  The following global variable retains its value during entire run */
    {FILE_HEADER,   REPORT_CONTROL,
        128, 4,  gen_string2int,            NULL,(int) 
                                                &fa_number_of_primary_recs},
    /*
    -- fa_record_size is informational, not used by any file utilities
    */
    {FILE_HEADER,   REPORT_CONTROL,
        132, 4, NASDAc_default_reqm_size,   NULL,(int) &fa_record_size},

    /*  6 reqm */
    /*  The following global variable retains its value during entire run */
    {FILE_HEADER,   REPORT_CONTROL,
        136, 4, gen_string2int,             NULL,(int) 
                                                 &fa_number_of_secondary_recs},
    /*
    -- fa_number_of_subrecords refers to the number of secondary records
    -- (observation plan records) located in the latter part of the REQM
    */
    {FILE_HEADER,   REPORT_CONTROL,
        136, 4, gen_string2int,             NULL,(int)&fa_number_of_subrecords},
    /* 
    -- fa_subrecord_size is informational, not used by any file utilities
    */
    /*  8 reqm */
    {FILE_HEADER,   REPORT_CONTROL,
        140, 4, gen_string2int,             NULL,(int)&fa_subrecord_size},

    {FILE_HEADER,   REPORT_RECORD,
        8,  4,  table_lookupFA2APS, (void *)NASDA_sat,  DTK_SAT} ,
    {FILE_HEADER,   REPORT_CONTROL,
        8,  4,  table_lookupFA2APS, (void *)NASDA_sat,  (int) fa_sat_id} ,
    {FILE_HEADER,   REPORT_RECORD,
        0,  8,  table_lookupFA2APS, (void *)NASDA_dtk_stat, 
                                                        DTK_DTKSTAT},
    {FILE_HEADER,   REPORT_CONTROL,
        0,  8,  table_lookupFA2APS, (void *)NASDA_activity_type,    
                                                        (int) fa_activity_type},
    /* 13 reqm */
    {FILE_HEADER,   REPORT_RECORD,
        16, 4,  table_lookupFA2APS, (void *)NASDA_station_id,   
                                                        DTK_STATION_ID}, 

    /* 
    -- DEFAULT FIELD   
    -- both actid and sensor are set to DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
    -- actid is set at a later time
    */
    /* 14 reqm */
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, gen_default_DMP,            NULL,       DTK_SENSOR},
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_darid,       NULL,       DTK_DARID},
    /* 16 reqm */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_set_trigger,            NULL,
                    (int) &fa_trigger_reqm_secondary_record_ingestion},
    /* The following is used to get permissions in multi-user environment */
    {FA_DEFAULT,    REPORT_CONTROL,
        -1, -1, gen_default_ASF_station,NULL,       (int)fa_station_id},

    /*
    -- insert "RSP. =" into notes, for identication of RSP information
    */
    /* 18 reqm */
    {FA_DEFAULT,    REPORT_RECORD,
        -1, -1, NASDAc_default_notes,       NULL,       DTK_NOTES},



    /* 
    -- DATA RECORD FIELDS:  
    */
    /*
    -- EXAMPLE of an REQM tape dump record:
    --
    -- 0123456789012345678901234567890123456789012345678901234567890123456789
    -- 
    -- 10350919941211  8 91.34  8114.69F2 3SAR103512SAR103513SAR103514
    --
    -- NOTE:  The following instructions refer to the processing of 
    -- reproduction plan records in the REQM.  They are followed by
    -- observation plan records, which are processed later.
    -- 
    -- Observation records will be processed using the instructions in
    -- the VALUE_DEFS NASDA_valuedefs_secondary_reqm[] array.
    */

    /*  REPRODUCTION PLAN RECORD DATA FIELDS:  */

    /*
    -- The following is a default value for REPRODUCTION PLAN records,
    -- which are handled by the FILE_RECORD instructions in these VALUE_DEFS
    */
    /* 19 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        -1, -1, gen_default_DMP,            NULL,       DTK_ACTID},
    /* 20 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        0,  6,  gen_string2str,             NULL,       DTK_FADTKID},
    {FILE_RECORD,   REPORT_RECORD,
        0,  6,  gen_string2str,             NULL,       DTK_FA_SCHEDULE_LINK},
    /*
    -- The following instruction populates the global variable fa_schedule_link
    -- which is used to help populate the global array fa_sch_link_control[].
    -- fa_sch_link_control is used by NASDAc_search_inherit_link to
    -- recover this reproduction record's  fa_schedule_link value for 
    -- its corresponding observation records at a later time.
    */
    /* 22 reqm */
    {FILE_RECORD,   REPORT_CONTROL,
        0,  6,  gen_string2str,             NULL,(int)&fa_schedule_link},
    {FILE_RECORD,   REPORT_CONTROL,
        6,  8,  gen_string2str,             NULL,(int)&fa_start_yyyymmdd},
    {FILE_RECORD,   REPORT_RECORD,
        6,  26, NASDAc_date_rsp2strttime,   NULL,       DTK_STRTTIME},
    {FILE_RECORD,   REPORT_RECORD,
        6,  26, NASDAc_date_rsp2stoptime,   NULL,       DTK_STOPTIME},
    /* 26 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        6,  26, NASDAc_date_rsp2rev,        NULL,       DTK_REV},
    {FILE_RECORD,   REPORT_RECORD,
        14, 18, gen_string2strcat,          NULL,       DTK_NOTES},
    {FILE_RECORD,   REPORT_RECORD,
        32, 2,  gen_string2str,             NULL,       DTK_TRANSID},
    /*
    -- The following instruction does not have a destination indicated.
    -- What we are doing is populating the global array fa_sch_link_control[]
    -- which is used in the handling of the REQM recording records.
    -- The destination (fa_sch_link_control) is implied.
    --
    -- This 'populating' action is carried out for each reproduction plan record
    --
    -- NOTE:the global variable fa_schedule_link is also used in this
    --      populating action. See preceding notes for more information.
    -- NOTE:the global variable fa_number_of_subrecords is used to create
    --      the global array fa_sch_link_control[] before populating it.
    */
    /* 29 reqm */
    {FILE_RECORD,   REPORT_CONTROL,
        34, 92, NASDAc_populate_sch_link,   NULL,       NULL},

    {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           


/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_secondary_reqm
                FA_FILEDEF NASDA_filedef_secondary_reqm
Description:    describes the secondary data records of the REQM, 
                in this case the observation records.  

Creator:        Miguel Siu

Creation Date:  Wed Mar 20 15:03:13 PST 1996

Notes:      This VALUE_DEFS array is not referenced in the 
            FA_FILENAME NASDA_files array, as is expected.
            The reqm file does not meet our file model of [header + records] or
            [header + concatenated record/subrecord], so we have to resort
            to processing the extra records using the instructions below.

Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
VALUE_DEFS NASDA_valuedefs_secondary_reqm[] =  
{
    /* 
    -- SECONDARY DATA RECORD FIELDS:  
    */
    /*
    -- EXAMPLE of an REQM observation record:
    --
    -- 0123456789012345678901234567890123456789012345678901234567890123456789
    -- 
    -- SAR099503HEOC19941101 99172.08 99202.35  AA 00
    */

    /*  OBSERVATION PLAN RECORD DATA FIELDS:  */
    /*  0 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        0,  3,  table_lookupFA2APS,         NASDA_sensor,       
                                                        DTK_SENSOR},
    {FILE_RECORD,   REPORT_RECORD,
        0,  3,  table_lookupFA2APS,         NASDA_REQM_recording_actid,
                                                        DTK_ACTID},
    /*
    -- Please note that the fa_dktid is not the same value as fa_schedule_link,
    -- because the fa_dtkid needs to be UNIQUE for each record in the file.
    -- fa_dtkid cannot be inherited from the reproduction record, unlike the
    -- fa_schedule_link which is inherited from the reproduction record.
    */
    /*  2 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        0,  9,  gen_string2str,             NULL,       DTK_FADTKID},
    /*
    -- The following instruction searches the global fa_sch_link_control[]
    -- array and returns the value for DTK_FA_SCHEDULE_LINK.
    -- This value corresponds to a previous reproduction plan in the REQM.
    */
    /*  3 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        0,  9,  NASDAc_search_inherit_link, NULL,       DTK_FA_SCHEDULE_LINK},
    {FILE_RECORD,   REPORT_CONTROL,
        13, 8,  gen_string2str,             NULL,(int)&fa_start_yyyymmdd},
    {FILE_RECORD,   REPORT_RECORD,
        13, 26, NASDAc_date_rsp2strttime,   NULL,       DTK_STRTTIME},
    /*  6 reqm */
    {FILE_RECORD,   REPORT_RECORD,
        13, 26, NASDAc_date_rsp2stoptime,   NULL,       DTK_STOPTIME},
    {FILE_RECORD,   REPORT_RECORD,
        13, 26, NASDAc_date_rsp2rev,        NULL,       DTK_REV},
    {FILE_RECORD,   REPORT_RECORD,
        21, 18, gen_string2strcat,          NULL,       DTK_NOTES},
    {0, 0, -1, -1, NULL, NULL, NULL} 
};                                                           

FA_FILEDEF NASDA_filedef_secondary_reqm =
{
    /*
    -- This file definition is a duplicate of the NASDA_filedef_reqm definition,
    -- but it contains the NASDA_valuedefs_secondary_reqm.
    */
    144,                    /* header length     */
    46,                     /* observation plan record composed of 46 bytes  */
    NASDA_valuedefs_secondary_reqm  /* pointer to the SECONDARY descriptions */
} ;


/*==============================================================================
Function:       FA_FILEDEF NASDA_filedef_reqm

Description:    describes the REQM file for the file processor driver.

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 17:07:10 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_reqm =
{
    /*
    -- header composed of "header" 128
    -- + "descriptor" 16
    */
    144,                    /* header length     */
    126,                    /* data record composed of 126 bytes  */
    NASDA_valuedefs_reqm    /* pointer to the descriptions       */
} ;

/*==============================================================================
Function:       VALUE_DEFS NASDA_valuedefs_msge

Description:    describes the data records of the MSGE, which is a response
                to the processing of the REQM file.

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 09:32:40 PST 1995

Notes:      These valuedefs use the array msge_default_strings[]
            which is included below.
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
char *msge_default_strings[] = 
{   "\
MSGE FILE FROM FAIS.  EVALUATION OF J-ERS-1 REQM TAPE DUMP REQUESTS\n\
TODAY'S DATE/TIME:    ",
    "\n\
THERE ARE TAPE DUMP REQUESTS WHICH CANNOT BE FULFILLED\n\
DUE TO OPERATIONAL OR MAINTENANCE CONFLICTS OR DUE TO REQM FILE ERRORS\n\n\
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -",
    "\
REQM FILE HEADER INFO:       \n\
File name:              REQM \n\
Project:                ERS1 \n\
Sending GS:             HMMO \n\
Receiving GS:           FAIS",
    "\
File date:              ", 
    "\
File time:              ",
    "\
File descriptor existence flag:  Y\n\
Record length:          VAR\n\
No. of data records:    0",
    "\n\
REQM FILE DESCRIPTOR INFO:",
    "\
No. of MDR reproduction plans          = ",
    "\
Record length of MDR reproduction plan = ",
    "\
No. of MDR observation plans           = ",
    "\
Record length of MDR observation plan  = ",
    "\n\
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\n\
THE FOLLOWING TAPE DUMP REQUESTS COULD NOT BE ACCEPTED:\n\n\
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -",

/*
-- First string to be used by FILE_RECORD type needs to employ leading \n 
*/
    "\n\n\
REQM DUMP WHICH CANNOT BE DONE:",
    "\
Acquisition Plan id:               ",
    "\
Reproduction date:                 ",
    "\
RSP at beginning of reproduction:  ",
    "\
RSP at the end of reproduction:    ",
    "\
MDR DUMP AT ASF IS IMPOSSIBLE DUE TO CONFLICT\n\n\
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -",

/*
-- First string to be used by FILE_TRAILER type needs to employ leading \n 
*/
    "\n\n\
END OF MSGE FILE FOR EVALUATION OF REQM REQUESTS"
} ;

VALUE_DEFS NASDA_valuedefs_msge[] =
{
/*
-- NOTE: The file creator will now take a value of 0 in the 'length' field
-- to mean 'use the strlen of string as the length.'
-- This means that we no longer have to 'count characters' to try to determine
-- the length of a string.
-- The 'offset' field's function remains intact; we are still responsible
-- for placing strings judiciously, so that they not overwrite each other.
*/
  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_DATE]},
  {NON_DTKATTR, FILE_HEADER,    90, 25, write_current_datetime,
        NULL,   NULL},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,
        NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_MSG]},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_INFO]},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_FDATE]},
  {NON_DTKATTR, FILE_HEADER,    24, 8,  write_date_mask_yyyymmdd,
        NULL,   (int) &fa_creation_date},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_FTIME]},
  {NON_DTKATTR, FILE_HEADER,    24, 8,  write_time_mask,
        NULL,   (int) &fa_creation_date},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_INFO2]},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_DESCR]},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_REP_NO]},
  {NON_DTKATTR, FILE_HEADER,    41, 3,  write_inttoblankascii,
        NULL,   (int) &fa_number_of_primary_recs},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_REP_LEN]},
   /*
   -- fa_record_size is informational, not used by any file utilities
   */
  {NON_DTKATTR, FILE_HEADER,    41, 3,  write_inttoblankascii,
        NULL,   (int) &fa_record_size},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_OBS_NO]},
  {NON_DTKATTR, FILE_HEADER,    41, 3,  write_inttoblankascii,
        NULL,   (int) &fa_number_of_secondary_recs},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_OBS_LEN]},
  /* 
  -- fa_subrecord_size is informational, not used by any file utilities
  */
  {NON_DTKATTR, FILE_HEADER,    41, 3,  write_inttoblankascii,
        NULL,   (int) &fa_subrecord_size},
  {DTK_NEWLINE, FILE_HEADER,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_HEADER,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[HEADER_LIST]},


  {DTK_DEFAULT, FILE_RECORD,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[RECORD_TITLE]},
  {DTK_NEWLINE, FILE_RECORD,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_RECORD,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[RECORD_PLAN_ID]},
  {DTK_RECORD,  FILE_RECORD,    35, 0,  NULL,
        NULL,   DTK_FADTKID},
  {DTK_NEWLINE, FILE_RECORD,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_RECORD,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[RECORD_REPDATE]},
  {DTK_DEFAULT, FILE_RECORD,    35, 0,  NULL,
        NULL,   (int) &pointer2fa_start_yyyymmdd},
  {DTK_NEWLINE, FILE_RECORD,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_RECORD,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[RECORD_RSPINIT]},
  {DTK_RECORD,  FILE_RECORD,    35, 9,  write_begin_rsp,
        NULL,   DTK_NOTES},
  {DTK_NEWLINE, FILE_RECORD,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_RECORD,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[RECORD_RSPSTOP]},
  {DTK_RECORD,  FILE_RECORD,    35, 9,  write_end_rsp,
        NULL,   DTK_NOTES},
  {DTK_NEWLINE, FILE_RECORD,    0,  0,  NULL,   NULL,   NULL},

  {DTK_DEFAULT, FILE_RECORD,    0,  0,  NULL,
        NULL,   (int) &msge_default_strings[RECORD_TRAILER]},

  {DTK_DEFAULT, FILE_TRAILER,   0,  0,  NULL,
        NULL,   (int) &msge_default_strings[MSGE_TRAILER]},



  {0,   0,  0,  0,  NULL,   NULL,   NULL}
} ;

/*==============================================================================
Function:      FA_FILEDEF NASDA_filedef_msge 

Description:   describes the MSGE file which is created as a result of
                the file processor driver's ingestion of REQM

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Fri Sep 29 09:00:20 PDT 1995

Notes:      
Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_msge =
{
    2000,                   /* header length is approximate     */
    640,                    /* record length is approximate     */
    NASDA_valuedefs_msge    /* pointer to the descriptions      */
} ;

/*==============================================================================
Function:      FA_FILEDEF NASDA_valuedefs_stgs 

Description:   describes the STGS file which is created as a result of
                the file processor driver's ingestion of REQR

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 10:44:35 PST 1995

Notes:      These valuedefs use the string array stgs_default_strings[]
            which is included below.
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
char *stgs_default_strings[] = { "ADEOS " , "ASF " , "HMMO" , "  47" , "19950601", "V01" , "ALL" , "A" , "\n", "***** ******" };

VALUE_DEFS NASDA_valuedefs_stgs[] =
{

{  DTK_RECORD   ,   FILE_HEADER  ,   0   ,   10   ,  stgs_filename         ,
   NULL         ,   NULL                           },

{  DTK_DEFAULT  ,   FILE_HEADER  ,   11  ,    6   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[AADEOS]  },

{  DTK_DEFAULT  ,   FILE_HEADER  ,   18  ,    4   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[ASF]    },

{  DTK_DEFAULT  ,   FILE_HEADER  ,   23  ,    4   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[HEOC]   },

{  DTK_RECORD   ,   FILE_HEADER  ,   28  ,   17   ,  file_creation_date    ,
   NULL         ,   NULL                           },    /* YYYYMMDD hh:mm:ss  */
  
{  DTK_DEFAULT  ,   FILE_HEADER   ,  46  ,    4   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[RECLEN] }, 

{  NON_DTKATTR  ,   FILE_HEADER   ,  51  ,    5   , inttoblankascii             ,
   NULL         ,   (int) &fa_number_of_records       },

{  DTK_DEFAULT  ,   FILE_HEADER   ,  57  ,    8   ,  NULL                   ,
   NULL         ,   (int) &pointer2fa_start_yyyymmdd                },

{  DTK_DEFAULT   ,   FILE_HEADER  ,  66  ,    8   ,  NULL                   ,
   NULL         ,   (int) &pointer2fa_stop_yyyymmdd                 },
 
{  DTK_DEFAULT  ,   FILE_HEADER   ,  75  ,    8   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[FFVDATE]},

{  DTK_DEFAULT  ,   FILE_HEADER   ,  84  ,    3   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[FFVNUM] },

{  DTK_DEFAULT  ,   FILE_HEADER   , 127  ,    1   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[NEWLIN] },

/*   
-- stgs data records 
-- NOTE: do not use following instruction until the time->rev->RSP conversion
--       functions have been modified to work with ADEOS satellite
--
--      < VALUE_DEFS entry for DTK_REV omitted here >
*/

{  DTK_RECORD   ,   FILE_RECORD   ,   0  ,    8   ,  c2yyyymmdd            ,
   NULL         ,   DTK_STRTTIME                   },

{  DTK_RECORD   ,   FILE_RECORD   ,   9  ,    5   ,  temp_revecho          ,  
   NULL         ,   DTK_NOTES                      },
/*
-- NOTE: the following notation is used (ie:ALL A ***** ******) to communicate
-- that the full range of a dtk has been errored/rejected (start to stop time).
--
-- Our conflict analysis software accepts or rejects/errors a datatake in its
-- entirety;  we currently do NOT support partial rejection of a datatake, and
-- cannot therefore report available/unavailable portions of a datatake.
--
-- Because of our current functionality as detailed above,
-- and by agreement with NASDA, we will report either 
-- a rejection of all datatake proposals for a rev 
--          (Station_status = 'ALL' for the STGS path reply record)
-- or an acceptance of all datatake proposals for a rev 
--          (non-existence of the STGS path reply record)
--
*/
{  DTK_DEFAULT  ,   FILE_RECORD   ,  15  ,    3   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[ALL]    },  

{  DTK_DEFAULT  ,   FILE_RECORD   ,  19  ,    1   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[A]      },   

{  DTK_DEFAULT  ,   FILE_RECORD   ,  21  ,   12   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[ASTERISK_RSP]      },   

{  DTK_DEFAULT  ,   FILE_RECORD   ,  34  ,   12   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[ASTERISK_RSP]      },   

/*
-- NOTE: the following are not used.  The RSP data passed in the DTK_NOTES
-- field corresponds to the start and stop time of the dtk in question, and
-- therefore denotes an UNAVAILABLE time range if the dtk is errored/rejected.
--
-- AVAILABLE portions of a rejected/errored datatake CANNOT be reported since
-- this information is not currently available from our conflict analysis
-- mechanism.
-- (see above for more info)
--
--      < VALUE_DEFS entry for DTK_NOTES omitted here >
*/

{  DTK_DEFAULT  ,   FILE_RECORD   ,  46  ,    1   ,  NULL                  ,
   NULL         ,   (int) &stgs_default_strings[NEWLIN] },
 
{           0   ,             0   ,   0  ,    0   ,  NULL                  ,
   NULL         ,          NULL                    }

};


/*==============================================================================
Function:      FA_FILEDEF NASDA_filedef_stgs 

Description:   describes the STGS file which is created as a result of
                the file processor driver's ingestion of REQR

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Fri Sep 29 09:00:20 PDT 1995

Notes:      
Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_stgs =
{
    128,                    /* header length     */
    47,                     /* data record composed of 126 bytes */
    NASDA_valuedefs_stgs    /* pointer to the descriptions       */
} ;

/*==============================================================================
Function:      FA_FILEDEF NASDA_valuedefs_reqw 

Description:   describes the REQW file, the weekly request file to NASDA

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 10:44:35 PST 1995

Notes:      These valuedefs use the array reqw_default_strings[]
            which is included below.
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
char *reqw_default_strings[] = { "REQW" , "ERS1" , "HMMO" , "N" , "0038" };
VALUE_DEFS NASDA_valuedefs_reqw[] =
{
/*
-- reqw header record definitions
*/
  {  DTK_DEFAULT  ,  FILE_HEADER    ,   0  ,    4   ,  NULL                  ,
   NULL         ,  (int) &reqw_default_strings[REQW]    },
 
  {  DTK_DEFAULT  ,  FILE_HEADER    ,   8  ,    4   ,  NULL                  ,
   NULL         ,  (int) &reqw_default_strings[ERS1W]   },
 
  {  DTK_DEFAULT_STRING, FILE_HEADER,  12  ,    4   ,  NULL                  ,
   NULL         ,  (int)"FAIS"                          },
 
  {  DTK_DEFAULT  ,  FILE_HEADER    ,  16  ,    4   ,  NULL                  ,
   NULL         ,  (int) &reqw_default_strings[HMMOW]   },
 
  {  DTK_FUNCTION   ,   FILE_HEADER  ,   20  ,   16   ,  file_creation_date_ns ,
   NULL         ,   NULL                                },    
   /* YYYYMMDDhh:mm:ss  */
 
  {  DTK_DEFAULT  ,   FILE_HEADER  ,   36  ,    1   ,  NULL                  ,
   NULL         ,   (int) &reqw_default_strings[NW]     },

  {  DTK_DEFAULT  ,   FILE_HEADER  ,   37  ,    4   ,  NULL                  ,
   NULL         ,   (int) &reqw_default_strings[RECSIZW]},
 
  {  NON_DTKATTR  , FILE_HEADER    ,   41  ,    5   ,  inttoascii            ,
   NULL         ,   (int) &fa_number_of_records            },
 
/*  
-- reqw data records  definitions
*/
 
  {  DTK_RECORD   ,   FILE_RECORD  ,    0  ,    1   ,  NULL                  ,
   NULL         ,   DTK_SENSOR                          },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,    1  ,    5   ,  inttoascii            ,
   NULL         ,   DTK_REV                             },
 
  {  NON_DTKATTR   ,   FILE_RECORD  ,   6  ,    2   ,  inttoascii            ,
   NULL         ,   (int) &fa_dtkid                     },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,    8  ,    8   ,  c2yyyymmdd            ,
   NULL         ,   DTK_STRTTIME                        },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   16  ,    18   ,  calcrsp              ,
   NULL         ,   DTK_REV                             },

  {  DTK_RECORD   ,   FILE_RECORD  ,   34  ,    3   ,  sensorfld             ,
   NULL         ,   DTK_SENSOR                          },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   37  ,    1   ,  opsgainmode           ,
   NULL         ,   DTK_SENSOR                          },
 
  {           0   ,             0   ,   0  ,    0   ,  NULL                  ,
   NULL         ,          NULL                    }
} ;


/*==============================================================================
Function:      FA_FILEDEF NASDA_filedef_reqw 

Description:   describes the REQW file, the weekly request file to NASDA

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 10:44:35 PST 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_reqw =
{
    128,                    /* header length     */
    38,                     /* data record composed of 38 bytes */
    NASDA_valuedefs_reqw    /* pointer to the descriptions       */
} ;

/*==============================================================================
Function:      FA_FILEDEF NASDA_valuedefs_reqq 

Description:   describes the REQQ file, the quarterly request file to NASDA

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 10:44:35 PST 1995

Notes:      These valuedefs use the array reqq_default_strings[]
            which is included below.
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
char *reqq_default_strings[] = { "REQQ" , "ERS1" , "HMMO" , "N" , "0061" };

VALUE_DEFS NASDA_valuedefs_reqq[] =
{
/*  
-- reqq header record definitions   
*/
  {  NON_DTKATTR    , FILE_HEADER  ,   41  ,    5   ,  reqq_set_trigger ,
   NULL         ,   (int) &fa_trigger_REQQ_add_grs_records },
 
  {  DTK_DEFAULT_STRING  ,  FILE_HEADER    ,   0  ,    8   ,  NULL      ,
   NULL         ,  (int)"REQQ    "    },
 
  {  DTK_DEFAULT  ,  FILE_HEADER    ,   8  ,    4   ,  NULL             ,
   NULL         ,  (int) &reqq_default_strings[ERS1]    },
 
  {  DTK_DEFAULT_STRING ,  FILE_HEADER,12  ,    4   ,  NULL             ,
   NULL         ,  (int)"FAIS"                       },
 
  {  DTK_DEFAULT  ,  FILE_HEADER    ,  16  ,    4   ,  NULL             ,
   NULL         ,  (int) &reqq_default_strings[HMMO]    },
 
  {  DTK_FUNCTION,   FILE_HEADER  ,   20  ,   16   ,  file_creation_date_ns ,
   NULL         ,   NULL                     },    /* YYYYMMDDhh:mm:ss  */
 
  {  DTK_DEFAULT  ,   FILE_HEADER  ,   36  ,    1   ,  NULL                  ,
   NULL         ,   (int) &reqq_default_strings[N]      },

  {  DTK_DEFAULT  ,   FILE_HEADER  ,   37  ,    4   ,  NULL                  ,
   NULL         ,   (int) &reqq_default_strings[RECSIZ] },
 
  {  NON_DTKATTR    , FILE_HEADER  ,   41  ,    5   ,  inttoascii            ,
   NULL         ,   (int) &fa_number_of_records            },
 
/*  
-- reqq data records definitions  
*/
  {  DTK_RECORD   ,   FILE_RECORD  ,    0  ,    1   ,  NULL                  ,
   NULL         ,   DTK_SENSOR                          },

  {  DTK_RECORD   ,   FILE_RECORD  ,    1  ,    5   ,  inttoascii            ,
   NULL         ,   DTK_REV                             },
 
  {  NON_DTKATTR  ,   FILE_RECORD  ,    6  ,    2   ,  inttoascii            ,
   NULL         ,   (int)&fa_dtkid                      },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   10  ,    8   ,  c2yyyymmdd            ,
   NULL         ,   DTK_STRTTIME                        },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   18  ,    8   ,  c2yyyymmdd            ,
   NULL         ,   DTK_STOPTIME                        },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   32  ,    3   ,  sensorfld             ,
   NULL         ,   DTK_SENSOR                          },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   35  ,   18   ,  calcrsp              ,
   NULL         ,   DTK_REV                             },
 
  {  DTK_RECORD   ,   FILE_RECORD  ,   54  ,    1   ,  opsgainmode           ,
   NULL         ,   DTK_SENSOR                      },
 
  {           0   ,             0   ,   0  ,    0   ,  NULL                  ,
   NULL         ,          NULL                    }
 
};

/*==============================================================================
Function:      FA_FILEDEF NASDA_filedef_reqq 

Description:   describes the REQQ file, the quarterly request file to NASDA

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Fri Sep 29 09:00:20 PDT 1995

Notes:      
Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILEDEF NASDA_filedef_reqq =
{
    128,                    /* header length     */
    61,                     /* data record composed of 61 bytes */
    NASDA_valuedefs_reqq    /* pointer to the descriptions       */
} ;

/*==============================================================================
Function:      FA_FILENAME NASDA_files 

Description:    This is the 'master' list of all NASDA files.
                It contains the 4-character code which defines each file.
                For files coming into the APS system, it also carries
                a 'response-file' definition, and its 4-character code.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 10:44:35 PST 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
FA_FILENAME NASDA_files[] =
{
/*
-- INCOMING FILES
*/
    {"NASDA",   "OPLN", &NASDA_filedef_opln,    NULL,                 NULL},
    {"NASDA",   "REQA", &NASDA_filedef_reqa,    NULL,                 NULL},
    {"NASDA",   "REQM", &NASDA_filedef_reqm,    &NASDA_filedef_msge, "MSGE"},
    {"NASDA",   "REQR", &NASDA_filedef_reqr,    &NASDA_filedef_stgs, "STGS"},
    {"NASDA",   "OPL1", &NASDA_filedef_opl1,    NULL,                 NULL},
/*
-- OUTGOING FILES
*/
    {"NASDA",   "REQQ", &NASDA_filedef_reqq,    NULL,                 NULL},
    {"NASDA",   "REQW", &NASDA_filedef_reqw,    NULL,                 NULL},
    {NULL,  NULL,   NULL,   NULL,   NULL}
} ;

/*==============================================================================
Function:       NASDAc_query_subrecord

Description:    get fa_number_of_subrecords from fa_subrecord_control[]

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Thu Oct 12 14:12:28 PDT 1995

Notes:  After utilizing a member of fa_subrecord_control, set it to zero
==============================================================================*/
int NASDAc_query_subrecord(
    void    *unused_pointer,
    char    *unused_pointer2,
    int     *unused_number_of_subrecords ) 
{
    int i ;

    for (i=0; fa_subrecord_control[i]==0; i++) ;
    fa_number_of_subrecords = fa_subrecord_control[i] ;
    fa_subrecord_control[i] = 0 ;

    return (TRUE) ;
}

/*==============================================================================
Function:       NASDAc_default_observation_size

Description:    returns the size of an opln observation record.

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Thu Jul 13 15:41:46 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_default_observation_size(
    void    *unused_pointer,
    char    *unused_character_pointer,
    int     *opln_observation_recsize ) 
{
    *opln_observation_recsize = 80 ;
    return (TRUE) ;
}

/*==============================================================================
Function:       NASDAc_default_reqm_size

Description:    default value for REQM record size.  the number is not
                in the file itself.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 17:20:46 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_default_reqm_size(
    void    *unused_pointer,
    char    *unused_character_pointer,
    int     *reqm_recsize ) 
{
    *reqm_recsize = 126 ;
    return (TRUE) ;
}

/*==============================================================================
Function:       NASDAc_default_darid

Description:    default value for darid (set it to zero)

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Sep  5 16:13:07 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_default_darid(
    void    *unused_pointer,
    char    *unused_character_pointer,
    int     *darid ) 
{
    *darid = 0 ;
    return (TRUE) ;
}

/*==============================================================================
Function:       NASDAc_default_notes

Description:    insert default note 'RSP. =' which indicates that RSP
                information is stored in this field.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec  6 15:42:52 PST 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_default_notes(
    void    *unused_pointer,
    char    *unused_character_pointer,
    char    *insert_here ) 
{
    strcpy(insert_here, "RSP. =" ) ;
    return (TRUE) ;
}


/*==============================================================================
Function:       NASDAc_date_time2asftime

Description:    Converts one of the NASDA date time formats to ASF time.


Returns:        
    TRUE     if ok.  
    FALSE    if any error.  

Creator:        Lawrence Stevens

Creation Date:  Tue Jul 11 18:00:04 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/

int NASDAc_date_time2asftime(
    void        *unused_pointer,
    char        *YY_MM_DD_hh_mm_ss,  /* YY-MM-DD hh:mm:ss       */
    char        *asftime )           /* yyyy:ddd:hh:mm:ss.sss   */
{

    int     YY, MM, DD, hh, mm, ss ; /* decoded fields from the input string. */

    int     year, doy ;  /* fields to make up asftime      */

    int     return_code ;

    return_code = sscanf(YY_MM_DD_hh_mm_ss, "%d-%d-%d %d:%d:%d",
        &YY, &MM, &DD, &hh, &mm, &ss) ;

    /*
    -- make use of the return code from sscanf, which gives a count
    -- of the successfully decoded fields.  
    */
    if (return_code != 6)
        return FALSE ;

    /* 
    -- grossly checkout the month, day, hour, minutes, and seconds 
    -- values from the input string.  
    */
    if ((MM < 1 || MM > 12)
    || (DD < 1  || DD > 31)
    || (hh < 0 || hh > 23)
    || (mm < 0  || mm > 59)
    || (ss < 0  || ss > 59))
        return FALSE ;
 
    /* 
    -- checkout the year, the four-digit 
    -- year; allow for past the year 2000:  */
    if ( YY < 90 )
        year = 2000 + YY ;
    else 
        year = 1900 + YY ;

    /*
    -- convert month and day to day of year (doy)
    -- input : yyyy mm dd   output: ddd
    */
    if (!tc_cal2doy(year, MM, DD, &doy))
        return FALSE  ;
 
    /* write out the asf format time */
    sprintf(asftime, asf_time_format,
        year, doy, hh, mm, ss, 0) ;
 
    return(tc_validate_asf_datetime(asftime) != TRUE ? FALSE : TRUE) ;
}   


/*==============================================================================
Function:       NASDAc_yyyymmdd2asftime

Description:    converts yyyymmdd to ASF 21-character format time.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 16:06:18 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_yyyymmdd2asftime(
    void        *unused_pointer,
    char        *yyyymmss,      /* YY-MM-DD hh:mm:ss       */
    char        *asftime )      /* yyyy:ddd:hh:mm:ss.sss   */
{
    return ( tc_yyyymmdd2asf( yyyymmss, asftime )  ) ;
}

/*==============================================================================
Function:      NASDAc_yyyymmdd_hhmmss2asf 

Description:    converts yyyymmdd_hhmmss to ASF 21-character format time.  

Parameters:     

Returns:        

Creator:        Miguel Siu 

Creation Date:  Fri Sep  8 18:40:35 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_yyyymmdd_hhmmss2asf(
    void        *unused_pointer,
    char        *yms,           /* YYYYMMDD hh:mm:ss       */
    char        *asftime )      /* yyyy:ddd:hh:mm:ss.sss   */
{
    char datestring[] = "YYYYMMDD";
    char timestring[] = "hh:mm:ss";

    strncpy (datestring, yms,   8);
    strncpy (timestring, yms+9, 8);

    return ( tc_yyyymmdd_hhmmss2asf( datestring, timestring, asftime )  ) ;
}

/*==============================================================================
Function:       NASDAc_link_time2strttime

Description:    accept the 24-character link times field; 
                extract the start time.

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Wed Jul 12 17:09:27 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_link_time2strttime(
    void        *unused_pointer,
    char        *YYYYMMDDhh_mm_sshh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
    char        *strttime )                /* yyyy:ddd:hh:mm:ss.sss     */
{

    char    YY_MM_DD_hh_mm_ss[] = "YY-MM-DD hh:mm:ss" ;

    /*
    -- reformat the start time from the input 
    -- string into the format expected for 
    -- NASDAc_date_time2asftime()
    -- this format is:  YY-MM-DD hh:mm:ss       
    -- then just call NASDAc_date_time2asftime() to 
    -- do the real work.
    */

    /* YY */
    strncpy(YY_MM_DD_hh_mm_ss,    YYYYMMDDhh_mm_sshh_mm_ss+2,  2 ) ; 

    /* MM */
    strncpy(YY_MM_DD_hh_mm_ss+3,  YYYYMMDDhh_mm_sshh_mm_ss+4,  2 ) ; 

    /* DD */
    strncpy(YY_MM_DD_hh_mm_ss+6,  YYYYMMDDhh_mm_sshh_mm_ss+6,  2 ) ; 

    /* hh */
    strncpy(YY_MM_DD_hh_mm_ss+9,  YYYYMMDDhh_mm_sshh_mm_ss+8,  2 ) ; 

    /* mm */
    strncpy(YY_MM_DD_hh_mm_ss+12, YYYYMMDDhh_mm_sshh_mm_ss+11, 2 ) ; 

    /* ss */
    strncpy(YY_MM_DD_hh_mm_ss+15, YYYYMMDDhh_mm_sshh_mm_ss+14, 2 ) ; 

    return  (NASDAc_date_time2asftime( NULL, YY_MM_DD_hh_mm_ss, strttime)) ;

}

/*==============================================================================
Function:       NASDAc_link_time2stoptime

Description:    extract the stop time from the link info and convert to 
                asftime.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Wed Jul 12 17:29:15 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_link_time2stoptime(
    void    *unused_pointer,
    char    *YYYYMMDDhh_mm_sshh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
    char    *stoptime )                /* yyyy:ddd:hh:mm:ss.sss     */
{

    char    start_hh[]          = "hh";
    char    stop_hh[]           = "hh";
    char    YY_MM_DD_noon[]     = "YY-MM-DD 12:00:00" ;
    char    YY_MM_DD_hh_mm_ss[] = "YY-MM-DD 00:00:00" ;
    char    asftime_noon[]      = "yyyy:ddd:hh:mm:ss.sss" ;     

    /*
    -- reformat the stop time from the input string into the format 
    __ expected for NASDAc_date_time2asftime()
    -- this format is:  YY-MM-DD hh:mm:ss       
    --
    -- Next, we must find out if the stoptime string goes over midnight.
    -- If so, we must increment the date string to reflect this.
    -- Here's how: If the stop hours are less than the start hours, we know
    --      that we have gone over midnight.  A datatake NEVER goes over
    --      15 minutes, much less a full day, so we can use this test.
    --
    --      When we add a day, we take the start day at 12:00 noon and add
    --      a day using tc_asf_add_ndays.  We then append the stoptime to
    --      the new date string.
    -- 
    -- Finally, we just call NASDAc_date_time2asftime() to 
    -- do the real work.
    */

    /* compare hh */
    strncpy(start_hh, YYYYMMDDhh_mm_sshh_mm_ss+8,  2 ) ; 
    strncpy(stop_hh,  YYYYMMDDhh_mm_sshh_mm_ss+16, 2 ) ; 

    if ( strcmp(start_hh, stop_hh) > 0 ) 
    {
        /* 
        -- stop_hh occurs before start_hh in the ASCII character set
        -- therefore, stop_hh represents a lower hour value than start_hh 
        */
        strncpy(YY_MM_DD_noon,    YYYYMMDDhh_mm_sshh_mm_ss+2,  2 ) ; 
        strncpy(YY_MM_DD_noon+3,  YYYYMMDDhh_mm_sshh_mm_ss+4,  2 ) ; 
        strncpy(YY_MM_DD_noon+6,  YYYYMMDDhh_mm_sshh_mm_ss+6,  2 ) ; 

        if (!NASDAc_date_time2asftime( NULL, YY_MM_DD_noon, asftime_noon))
            return (FALSE) ;
        if (!tc_asf_add_ndays( asftime_noon, (double) 1.0, stoptime) ) 
            return (FALSE) ;
    }
    else
    {
        /* YY */
        strncpy(YY_MM_DD_hh_mm_ss,    YYYYMMDDhh_mm_sshh_mm_ss+2,  2 ) ; 

        /* MM */
        strncpy(YY_MM_DD_hh_mm_ss+3,  YYYYMMDDhh_mm_sshh_mm_ss+4,  2 ) ; 

        /* DD */
        strncpy(YY_MM_DD_hh_mm_ss+6,  YYYYMMDDhh_mm_sshh_mm_ss+6,  2 ) ; 

        if (!NASDAc_date_time2asftime( NULL, YY_MM_DD_hh_mm_ss, stoptime))
            return (FALSE) ;
    }

    /* hh */
    strncpy(stoptime+9,  YYYYMMDDhh_mm_sshh_mm_ss+16,  2 ) ; 

    /* mm */
    strncpy(stoptime+12, YYYYMMDDhh_mm_sshh_mm_ss+19, 2 ) ; 

    /* ss */
    strncpy(stoptime+15, YYYYMMDDhh_mm_sshh_mm_ss+22, 2 ) ; 


    return(tc_validate_asf_datetime(stoptime) != TRUE ? FALSE : TRUE) ;
}

/*==============================================================================
Function:       NASDAc_link_time2rev

Description:    compute the rev number corresponding to the NASDA link time.
                just take the start time and call utility routine to 
                compute the rev number.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Wed Jul 12 18:35:05 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_link_time2rev(
    void    *unused_pointer,
    char    *YYYYMMDDhh_mm_sshh_mm_ss, /* YYYYMMDDhh:mm:sshh:mm:ss  */
    int     *rev ) 
{
    int     return_code ;
    char    asftime[22] ;

    return_code = NASDAc_link_time2strttime( NULL, 
        YYYYMMDDhh_mm_sshh_mm_ss, asftime ) ;
    if (!return_code)
        return FALSE ;

    return_code = asftime2rev(fa_sat_id, asftime, rev ) ;
    if (return_code < 0)
        return FALSE ;

    return TRUE ;

}

/*==============================================================================
Function:       NASDAc_headertime2asftime

Description:    Convert the header time (yyyymmddhh:mm:ss) to asftime.

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 17 11:13:03 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_headertime2asftime(
    void        *unused_pointer,
    char        *YYYYMMDDhh_mm_ss, /* YYYYMMDDhh:mm:ss          */
    char        *asftime )         /* yyyy:ddd:hh:mm:ss.sss     */
{

    /* NASDAc_link_time2strttime() only looks at the first 16 chars.  */
    return (NASDAc_link_time2strttime( unused_pointer, 
        YYYYMMDDhh_mm_ss, asftime )  ) ;

}

/*==============================================================================
Function:       NASDAc_check_fadtkid

Description:    check the validity of fa_dtkid string being returned by the
                flight agency. 

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Mar 27 10:08:50 PST 1996

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_check_fadtkid(
    void    *unused_pointer,
    char    *fa_dtkid_string,
    char    *unused_pointer2 )
{
    char    local_dummy_arg [2] = " " ;

    if (!table_lookupFA2APS(NASDA_REQA_sensor, fa_dtkid_string,
        local_dummy_arg) 
    ||  !gen_field_is_numeric(fa_dtkid_string+1, 7) )
    {
            aps_log_msg(file_util_progname, APS_ERROR, 
"NASDAc_check_fadtkid():\
fa_dtkid may have been changed by flight agency, has invalid format:\n",
                DO_SYSLOG, DO_PRINT);
        return (FALSE) ;
    }
    else
        return (TRUE) ;
}

/*==============================================================================
Function:       NASDAc_reqa_rsp_date2strttime

Description:    converts the REQA fields of rsp, date to strttime for the 
                observation. 

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 17 14:55:16 PDT 1995

Notes:      If the input string is blank, do not try to get a strttime.
            Just return a blank field, since this will be checked by the
            routine dtk_status_by_blanking() 
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_reqa_rsp_date2strttime(
    void    *unused_pointer,
    char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD   */
    char    *strttime )         /* yyyy:ddd:hh:mm:ss.sss        */
{
    char    date_rsp[27] ;
    char    blanks[]="                                                      ";

    /*
    -- If the input string is blank, do not try to get a strttime. 
    -- See Notes above
    */
    if (strncmp(blanks, rsp_date, strlen(rsp_date)) == 0 )
    {
        strncpy(strttime, blanks, strlen(strttime) ) ;
        return (TRUE) ;
    }

    /* 
    -- Need to take the input rsp_date, NNNXXX.XXNNNXXX.XXYYYYMMDD, 
    -- and switch to date_rsp:          YYYYMMDDNNNXXX.XXNNNXXX.XX
    */
    strncpy(date_rsp, rsp_date+18, 8 ) ;
    strncpy(date_rsp+8, rsp_date, 18 ) ;
    date_rsp[26] = '\0' ;

    return ( NASDAc_date_rsp2strttime( unused_pointer, date_rsp, strttime)  ) ;

}

/*==============================================================================
Function:       NASDAc_reqa_rsp_date2stoptime

Description:    converts the REQA fields of rsp, date to stoptime for the 
                observation. 

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 17 14:55:16 PDT 1995

Notes:      If the input string is blank, do not try to get a strttime.
            Just return a blank field, since this will be checked by the
            routine dtk_status_by_blanking() 
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_reqa_rsp_date2stoptime(
    void    *unused_pointer,
    char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD   */
    char    *stoptime )         /* yyyy:ddd:hh:mm:ss.sss        */
{
    char    date_rsp[27] ;
    char    blanks[]="                                                      ";

    /*
    -- If the input string is blank, do not try to get a stoptime. 
    -- See Notes above
    */
    if (strncmp(blanks, rsp_date, strlen(rsp_date)) == 0 )
    {
        strncpy(stoptime, blanks, strlen(stoptime) ) ;
        return (TRUE) ;
    }

    /* 
    -- Need to take the input rsp_date, NNNXXX.XXNNNXXX.XXYYYYMMDD, 
    -- and switch to date_rsp:          YYYYMMDDNNNXXX.XXNNNXXX.XX
    */
    strncpy(date_rsp, rsp_date+18, 8 ) ;
    strncpy(date_rsp+8, rsp_date, 18 ) ;
    date_rsp[26] = '\0' ;

    return ( NASDAc_date_rsp2stoptime( unused_pointer, date_rsp, stoptime)  ) ;

}

/*==============================================================================
Function:       NASDAc_reqa_rsp_date2rev

Description:    converts the REQA fields of rsp, date to rev for the 
                observation. 

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 17 14:55:16 PDT 1995

Notes:      If the input string is blank, do not try to get a strttime.
            Just return a blank field (zero), since this will be checked by the
            routine dtk_status_by_blanking() 
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_reqa_rsp_date2rev(
    void    *unused_pointer,
    char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD   */
    int     *rev )    
{
    char    date_rsp[27] ;
    char    blanks[]="                                                      ";

    /*
    -- If the input string is blank, do not try to get a rev. See Notes above
    */
    if (strncmp(blanks, rsp_date, strlen(rsp_date)) == 0 )
    {
        *rev = 0 ;
        return (TRUE) ;
    }

    /* 
    -- Need to take the input rsp_date, NNNXXX.XXNNNXXX.XXYYYYMMDD, 
    -- and switch to date_rsp:          YYYYMMDDNNNXXX.XXNNNXXX.XX
    */
    strncpy(date_rsp, rsp_date+18, 8 ) ;
    strncpy(date_rsp+8, rsp_date, 18 ) ;
    date_rsp[26] = '\0' ;

    return ( NASDAc_date_rsp2rev( unused_pointer, date_rsp, rev)  ) ;

}


/*==============================================================================
Function:       NASDAc_reqa_rsp_date2trig

Description:    converts the REQA fields of rsp, date to a trigger for the
                dtk_defaults routine.
                
                Only datatakes which contain information necessary for 
                the dtk_defaults routine may make use of it.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Tue Oct 31 12:01:32 PST 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_reqa_rsp_date2trig(
    void    *unused_pointer,
    char    *rsp_date,          /* NNNXXX.XXNNNXXX.XXYYYYMMDD   */
    int     *trigger_for_skip )    
{
    char    blanks[]="                                                      ";

    /*
    -- If ANY of the input string is blank, we will not need to call the
    -- routine dtk_default_values(). 
    */
    if (strncmp(blanks, rsp_date,   9) == 0 
    ||  strncmp(blanks, rsp_date+9, 9) == 0
    ||  strncmp(blanks, rsp_date+18,8) == 0 )
    {
        *trigger_for_skip = 1 ;
    }
    else
        *trigger_for_skip = 0 ;

    return (TRUE) ;
}

/*==============================================================================
Function:       NASDAc_yyyymmdd_hhmmss2rev 

Description:    converts REQR start time to rev 

Parameters:     

Returns:        TRUE conversion completed correctly
                FALSE error occurred.

Creator:        Lawrence Stevens

Creation Date:  Mon Sep 11 16:50:32 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_yyyymmdd_hhmmss2rev(
    void        *unused_pointer,
    char        *yms,           /* YYYYMMDD hh:mm:ss       */
    int         *rev )      /* yyyy:ddd:hh:mm:ss.sss   */
{
    char datestring[] = "YYYYMMDD";
    char timestring[] = "hh:mm:ss";
    char asftime[22] ;

    strncpy (datestring, yms,   8);
    strncpy (timestring, yms+9, 8);

    if (!tc_yyyymmdd_hhmmss2asf( datestring, timestring, asftime ))
        return (FALSE) ;

    return ( asftime2rev(fa_sat_id, asftime, rev)  ) ;
}

/*==============================================================================
Function:       NASDAc_inherit_actid

Description:    this routine populates the activity id field, using
                the value of global variable fa_acquisition_mode.

                The value of fa_acquisition_mode is determined by the
                acquisition record; fa_acquisition_mode can then be used
                to determine an activity id value for the observation record.

                In this manner, the observation record 'inherits' its 
                activity id value from the acquisition record.
                

Parameters:     

Returns:       the output actid value will be a 3-character string.   

Creator:        Miguel Siu

Creation Date:  Tue Mar 18 10:22:58 PST 1997

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_inherit_actid(
    void        *unused_pointer ,
    char        *unused_string ,
    char        *actid ) 
{

    strcpy(actid, fa_acquisition_mode) ;

    return TRUE ;

}


/*==============================================================================
Function:       NASDAc_date_rsp2strttime

Description:    from date and rsp info, compute strttime.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 17:26:59 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_date_rsp2strttime(
    void    *unused_pointer,
    char    *date_rsp,          /* YYYYMMDDNNNXXX.XXNNNXXX.XX        */
    char    *strttime )         /* yyyy:ddd:hh:mm:ss.sss    */
{
    int     return_code ;

    char    yyyymmdd[9] ;
    int     start_rsp ;
    double  start_rsp_angle ;
    char    asftime[22] ;
    char    scan_string[] = "NNN XXX.XX" ;
    int     rev ;           /* not used after computed.     */

    /* 
    -- setup the local variables yyyymmdd, start_rsp, start_rsp_angle
    -- from date_rsp.  The format is yyyymmddNNNxxx.xx
    -- where yyyymmdd is date, NNN is rsp, xxx.xx is rsp_angle.
    */
    strncpy(yyyymmdd, date_rsp, 8 ) ;
    yyyymmdd[8] = '\0' ;

    strncpy(scan_string,    date_rsp + 8,  3) ;
    strncpy(scan_string + 4,date_rsp + 11, 6) ;
    return_code = sscanf(scan_string, "%d %lf", &start_rsp, &start_rsp_angle ) ;
    /* 
    -- make use of the return code, which is the number of 
    -- successfully decoded fields.  
    */
    if ( return_code != 2 )
        return FALSE ;

    return_code = NASDAc_yyyymmdd2asftime( unused_pointer, yyyymmdd, asftime ) ;
    if ( !return_code )
        return FALSE ;

    return_code = NASDAc_asftime_rsp_angle_2_rev_time( "J1", asftime, 
        start_rsp, start_rsp_angle, SEARCH_FORWARD, 
        &rev, strttime ) ;

    if (return_code < 0)
    {
        /* 
        -- set the time to the end of the day and then 
        -- search backwards for the right rev.  
        -- perhaps the start of the day was before the 
        -- start of a phase and that is why we didn't get 
        -- a good return code.  
        */
        strcpy(asftime+9, "23:59:59.999" ) ;

        return_code = NASDAc_asftime_rsp_angle_2_rev_time( "J1", 
            asftime, start_rsp, start_rsp_angle, SEARCH_BACKWARD, 
            &rev, strttime ) ;
        /* the second try is the last try:   */
        if (return_code < 0)
            return FALSE ;
    }

    return TRUE ;

}

/*==============================================================================
Function:       NASDAc_date_rsp2stoptime

Description:    from date and rsp info, compute strttime.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 18:48:55 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_date_rsp2stoptime(
    void    *unused_pointer,
    char    *date_rsp,          /* YYYYMMDDNNNXXX.XXNNNXXX.XX        */
    char    *stoptime )         /* yyyy:ddd:hh:mm:ss.sss    */
{
    int     return_code ;

    char    yyyymmdd[9] ;
    int     stop_rsp ;
    double  stop_rsp_angle ;
    char    asftime[22] ;
    char    asftime_plus1[22] ;
    char    scan_string[] = "NNN XXX.XX" ;
    int     rev ;           /* not used after computed.     */

    /* 
    -- setup the local variables yyyymmdd, stop_rsp, stop_rsp_angle
    -- from date_rsp.  The format is yyyymmddNNNxxx.xx
    -- where yyyymmdd is date, NNN is rsp, xxx.xx is rsp_angle.
    */
    strncpy(yyyymmdd, date_rsp, 8 ) ;
    yyyymmdd[8] = '\0' ;

    strncpy(scan_string,    date_rsp + 17,  3) ;
    strncpy(scan_string + 4,date_rsp + 20,  6) ;
    return_code = sscanf(scan_string, "%d %lf", &stop_rsp, &stop_rsp_angle ) ;
    /* 
    -- make use of the return code, which is the number of 
    -- successfully decoded fields.  
    */
    if ( return_code != 2 )
        return FALSE ;

    return_code = NASDAc_yyyymmdd2asftime( unused_pointer, yyyymmdd, asftime ) ;
    if ( !return_code )
        return FALSE ;

    return_code = NASDAc_asftime_rsp_angle_2_rev_time( "J1", asftime, 
        stop_rsp, stop_rsp_angle, SEARCH_FORWARD, 
        &rev, stoptime ) ;

    if (return_code < 0)
    {
        /* 
        -- FIRST TRY
        -- the attempt to get the time did not work.  
        */
        /* 
        -- set the time to the end of the day and then 
        -- search backwards for the right rev.  
        -- perhaps the start of the day was before the 
        -- start of a phase and that is why we didn't get 
        -- a good return code.  
        */
        strcpy(asftime+9, "23:59:59.999" ) ;

        return_code = NASDAc_asftime_rsp_angle_2_rev_time( "J1", 
            asftime, stop_rsp, stop_rsp_angle, SEARCH_BACKWARD, 
            &rev, stoptime ) ;
        if (return_code < 0)
        {
            /* 
            -- SECOND TRY
            -- the second try did not work, either.  
            -- one last attempt to get the stop time.  
            -- perhaps the stop time is into the next day.
            -- in other words, perhaps the start time is one 
            -- day and the stop time is the next day.  
            -- so do a SEARCH_FORWARD using the input date + 1.
            */
            strcpy(asftime+9, "00:00:00.000") ;
            return_code = tc_asf_add_ndays( asftime, (double) 1.0, 
                asftime_plus1) ;
            if(!return_code)
                return FALSE ;

            /* this is our LAST CHANCE:  */
            return_code = NASDAc_asftime_rsp_angle_2_rev_time( "J1", 
                asftime_plus1, stop_rsp, stop_rsp_angle, SEARCH_FORWARD, 
                &rev, stoptime ) ;
            if (return_code < 0)
                return FALSE ;
        }
    }

    return TRUE ;

}

/*==============================================================================
Function:       NASDAc_date_rsp2rev

Description:    from date and rsp info, compute rev.  

Parameters:     

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Jul 14 18:48:43 PDT 1995

Notes:      
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
int NASDAc_date_rsp2rev(
    void    *unused_pointer,
    char    *date_rsp,          /* YYYYMMDDNNNXXX.XXNNNXXX.XX        */
    int     *rev )         
{
    int     return_code ;

    char    yyyymmdd[9] ;
    int     start_rsp ;
    double  start_rsp_angle ;
    char    asftime[22] ;
    char    scan_string[] = "NNN XXX.XX" ;

    /* 
    -- setup the local variables yyyymmdd, start_rsp, start_rsp_angle
    -- from date_rsp.  The format is yyyymmddNNNxxx.xx
    -- where yyyymmdd is date, NNN is rsp, xxx.xx is rsp_angle.
    */
    strncpy(yyyymmdd, date_rsp, 8 ) ;
    yyyymmdd[8] = '\0' ;

    strncpy(scan_string,    date_rsp +  8,  3) ;
    strncpy(scan_string + 4,date_rsp + 11,  6) ;
    return_code = sscanf(scan_string, "%d %lf", &start_rsp, &start_rsp_angle ) ;
    /* 
    -- make use of the return code, which is the number of 
    -- successfully decoded fields.  
    */
    if ( return_code != 2 )
        return FALSE ;

    return_code = NASDAc_yyyymmdd2asftime( unused_pointer, yyyymmdd, asftime ) ;
    if ( !return_code )
        return FALSE ;

    /* rev is int*, a parameter to this routine...  */
    return_code = NASDAc_asfdate_rsp_2_rev( "J1", asftime, start_rsp, 
        rev ) ;

    if (return_code < 0)
        return FALSE ;

    return TRUE ;

}

/*==============================================================================
Function:       NASDAc_default_subrecords(

Description:    returns NASDA default subrecords (equals zero)

Parameters:     NO input required

Returns:        
Type          Name              Definition
int                             TRUE = success. ESA default subrecords returned.

Creator:        Miguel Siu

Creation Date:  Wed Jul 12 11:32:48 PDT 1995

Notes:      
==============================================================================*/
int NASDAc_default_subrecords(
    void        *unused_pointer, 
    char        *unused_pointr2,
    int         *subrecords)
{
    *subrecords = 0  ;
    return(TRUE)  ;
}

/*==============================================================================
Function:       NASDAc_inherit_fa_sch_link

Description:    uses global fa_schedule_link to populate a dtk DB_RECORD field

Parameters:     

Returns:        Always returns TRUE

Creator:        Miguel Siu

Creation Date:  Mon Mar 18 16:23:07 PST 1996

Notes:      
==============================================================================*/
int NASDAc_inherit_fa_sch_link(
    void        *unused_pointer, 
    char        *unused_pointer2,
    char        *schedule_link )        
{
    strncpy(schedule_link, fa_schedule_link, strlen(schedule_link)) ;
    return(TRUE)  ;
}

/*==============================================================================
Function:       NASDAc_populate_sch_link

Description:    uses global fa_schedule_link plus the sensor-segment
                information provided in the REQM reproduction record
                to populate the fa_sch_link_control[] array

Parameters:     

Returns:        TRUE    array was correctly populated.
                FALSE   an error has occurred, array is incomplete.

Creator:        Miguel Siu

Creation Date:  Wed Mar 20 17:24:03 PST 1996

Notes:      
==============================================================================*/
int NASDAc_populate_sch_link(
    void        *unused_pointer, 
    char        *sensor_segment_string,
    char        *unused_destination_pointer )        
{
    static int  create_array = 1;
    int     i, j ;
    int     no_o_segments ;
    char    two_bytes[3] = "xx" ;


    /*
    -- Create the fa_sch_link_control[] array, once
    -- (There are 10 possible segments for each reproduction plan record)
    -- NOTE that the array is initialized with NULLs (calloc usage)
    */
    if (create_array)
    {
        fa_sch_link_control = 
            (FA_KEY_STRUCT *) calloc((fa_number_of_subrecords*10)+1,
                                        sizeof(FA_KEY_STRUCT)  ) ;

        if (!fa_sch_link_control)
        {
            /* error, stop processing the file */
            aps_log_msg(file_util_progname, APS_ERROR, 
"NASDAc_populate_sch_link():\
ERROR allocating array for REQM observation record processing\n",
                DO_SYSLOG, DO_PRINT);

            return (FALSE) ;
        }
        fa_sch_link_control[0].sensor_segment[0] = NULL ;
        create_array = 0 ;
    }

    strncpy(two_bytes, sensor_segment_string, 2) ;
    if (!gen_field_is_numeric(two_bytes, 2) )
        return (FALSE) ;
    no_o_segments = atoi(two_bytes) ;

    /*
    -- Advance fa_sch_link_control[] array to its last element,
    -- before starting the populating action.
    */
    for ( i = 0 ; fa_sch_link_control[i].sensor_segment[0] != NULL; i ++ ) ;

    if ( no_o_segments == 0)
    {
        /* 
        -- The sensor_segment_string is missing needed information,
        -- so keep on reading segments until we encounter a blank.
        */
        j = 0 ;
        while ( sensor_segment_string[ 2+(j*9) ] != ' ')
        {
            strncpy(fa_sch_link_control[i].sensor_segment,
                        sensor_segment_string+2+(j*9), 9 ) ; 
            strncpy(fa_sch_link_control[i++].planning_ID,
                        fa_schedule_link, 6 ) ;
            j++ ;
        }
    }
    else
    {
        for ( j = 0 ; j < no_o_segments; j ++ )
        {
            strncpy(fa_sch_link_control[i].sensor_segment,
                        sensor_segment_string+2+(j*9), 9 ) ; 
            strncpy(fa_sch_link_control[i++].planning_ID,
                        fa_schedule_link, 6 ) ;
        }
    }

    fa_sch_link_control[i+1].sensor_segment[0] = NULL ;

    return(TRUE)  ;
}

/*==============================================================================
Function:       NASDAc_search_inherit_link

Description:    search through the global array fa_sch_link_control[]
                and find the value which corresponds to the sensor_segment_key 
                provided below.  The schedule_link value found is then used 
                to populate a dtk DB_RECORD field

Parameters:     

Returns:        Always returns TRUE

Creator:        Miguel Siu

Creation Date:  Mon Mar 18 16:23:07 PST 1996

Notes:      The global array fa_sch_link_control is used here.
==============================================================================*/
int NASDAc_search_inherit_link(
    void        *unused_pointer, 
    char        *key,
    char        *schedule_link )        
{
    int     i ;
    int     found_match;

    found_match = 0 ;

    for ( i = 0; fa_sch_link_control[i].sensor_segment[0] != NULL ; i++ )
    {
        if (strncmp(key, fa_sch_link_control[i].sensor_segment, strlen(key)) == 0)
        {
            strncpy(schedule_link, fa_sch_link_control[i].planning_ID, 
                sizeof(fa_sch_link_control[i].planning_ID)); 
            found_match = 1 ;
            break ;
        }
    }

    if (found_match)
        return (TRUE)  ;
    else
        return (FALSE) ;
}

/*==============================================================================
Function:       MSGE functions

int write_current_datetime      (char *unused_ptr,  char *buf,  int length)
int write_date_mask_yyyymmdd    (char *asftime, char *buf, int unused_length)
int write_time_mask             (char *asftime,     char *buf,  int length)
int write_begin_rsp             (char *rsp_string,  char *buf,  int length)
int write_end_rsp               (char *rsp_string,  char *buf,  int length)
int write_inttoblankascii       (int *integer,      char *buf,  int length )

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 11:47:25 PST 1995

Notes:      
==============================================================================*/
/*
-- MSGE functions.
-- Please note that all of these functions append a null byte at the
-- end of the output string.  This is useful for the MSGE's 
-- "append data value after its label" model.
*/
int write_current_datetime (char *unused_ptr, char *buf, int unused_length)
{
    return (tc_systime2asf(buf) ) ;
}

/*
-- The following not only writes the date mask from an asftime string,
-- it also converts it to yyyy/mm/dd format
*/
int write_date_mask_yyyymmdd(char *asftime, char *buf, int unused_length)
{
    char local_yyyymmdd[] = "yyyymmdd" ;

    if (!tc_asf2yyyymmdd(asftime,local_yyyymmdd) ) return(FALSE) ;

    strncpy(buf,    local_yyyymmdd,     4) ;
    strncpy(buf+4,  "/",                1) ;
    strncpy(buf+5,  local_yyyymmdd+4,   2) ;
    strncpy(buf+7,  "/",                1) ;
    strncpy(buf+8,  local_yyyymmdd+6,   2) ;

    buf[10] = '\0' ;
    return (TRUE) ;
}
 

int write_time_mask(char *asftime, char *buf, int length)
{
    strncpy(buf, asftime+9, length) ;
    buf[length] = '\0' ;
    return (TRUE) ;
}

/*
-- The following routine MUST assume XXXYYY.YY for rsp_string
-- in order to parse the data correctly
*/
int write_begin_rsp(char *rsp_string, char *buf, int length)
{
    if (length < 9) return (FALSE) ; 

    strncpy(buf,    rsp_string+6,   3) ;
    strncpy(buf+3,  "  ",           2) ;
    strncpy(buf+5,  rsp_string+9,   length-3) ;
    strcat(buf, rsp_string+9) ;
    buf[length+2] = '\0' ;
    return (TRUE) ;
}


/*
-- The following routine MUST assume XXXYYY.YY for rsp_string
-- in order to parse the data correctly
*/
int write_end_rsp(char *rsp_string, char *buf, int length)
{
    if (length < 9) return (FALSE) ; 

    strncpy(buf,    rsp_string+15,  3) ;
    strncpy(buf+3,  "  ",           2) ;
    strncpy(buf+5,  rsp_string+18,  length-3) ;
    buf[length+2] = '\0' ;
    return (TRUE) ;
}
 

int write_inttoblankascii( int *integer , char *buf , int length )
{
    char s[11];
    int  i;

    i = *integer;
    itoa(i,s);

    for( i=0 ; i<(int)( length - strlen(s) ); i++ )
        buf[i] = ' ';
    strncpy(&buf[(int)(length - strlen(s))] , s , strlen(s));
    buf[length] = '\0' ;

    return(TRUE);
}

/*==============================================================================
Function:       

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 11:53:49 PST 1995

Notes:      
==============================================================================*/
/*   stgs functions   */

int stgs_filename( char *unusedptr , char *buf , int unused_length )
{
strncpy(buf,"STGS",4);
/*  printf("notes:%s  len:%d \n",dtk_notes, strlen(dtk_notes)); */
/*  if(strlen(dtk_notes) != 0)                                  */
/*      strncpy(&buf[4],dtk_notes,6);                           */

strncpy(&buf[4], fa_file_id, 6) ;
return(TRUE);
}

int file_creation_date( char *unusedptr , char *buf , int length)
{
char curr_time[22] , yyyymmdd[9]="yyyymmdd" , epoch[18]="yyyymmdd hh:mm:ss";
tc_systime2asf( curr_time);
/*  printf("curr_time:%s\n", curr_time);  */
tc_asf2yyyymmdd(curr_time , yyyymmdd);
strncpy(epoch,yyyymmdd,8);
strncpy(&epoch[8]," ",1);
strncpy(&epoch[9],&curr_time[9],8);
strncpy(buf,epoch,length);
return(TRUE);
}

int inttoascii( int *integer , char *buf , int length )
{
char s[11];
int  i;
i = *integer;
itoa(i,s);
/*  printf("i: %d  s: %s  strlen: %d\n", i,s,strlen(s));  */
for( i=0 ; i<(int)( length - strlen(s) ); i++ )
buf[i] = '0';
strncpy(&buf[(int)(length - strlen(s))] , s , strlen(s));
return(TRUE);
}

int inttoblankascii( int *integer , char *buf , int length )
{
    char s[11];
    int  i;

    i = *integer;
    itoa(i,s);
    /*  printf("i: %d  s: %s  strlen: %d\n", i,s,strlen(s));  */

    for( i=0 ; i<(int)( length - strlen(s) ); i++ )
        buf[i] = ' ';
    strncpy(&buf[(int)(length - strlen(s))] , s , strlen(s));

    return(TRUE);
}

int c2yyyymmdd(char *stxxtime , char *buf , int length)
{
char yyyymmdd[9]="yyyymmdd";
/*  printf("stxxtime:%s  len:%d  \n", stxxtime , strlen(stxxtime) );  */
tc_asf2yyyymmdd(stxxtime,yyyymmdd);
strncpy(buf,yyyymmdd,length);
return(TRUE);
}

#ifdef MIGUEL_COMMENT_OUT
/* 
-- Routine beginendrsp() is no longer used.  By agreement with NASDA,
-- we no longer have to report 'partial availability of a path',
-- hence we no longer need the RSP start/stop that is conveyed by beginendrsp
*/
int beginendrsp(char *dtk_notes, char *buf , int length)
{
char bersp[] = "PPPPP AAA.AA PPPPP AAA.AA";
/*  printf("dtk_notes:%s dtk_notes length:%d\n",dtk_notes,strlen(dtk_notes));  */
strncpy(bersp,&dtk_notes[6],12);
strncpy(&bersp[13],&dtk_notes[19],12);
/*  printf("bersp:%s\n",bersp);  */
strncpy(buf,bersp,length);

return(TRUE);
}
#endif

/*
-- the following is a temporary routine that 'echoes' the rev which is 
-- indicated in the RSP path information.
-- NO CONVERSIONS are taking place here.
*/
int temp_revecho(char *dtk_notes, char *buf , int length)
{
char bersp[] = "PPPPP";

strncpy(bersp,&dtk_notes[6],5);
strncpy(buf,bersp,length);

return(TRUE);
}

/*==============================================================================
Function:       reqq/reqw functions

int reqq_set_trigger        (int *trigger,  char *unusedptr,int unused_length)
int file_creation_date_ns   (char *unusedptr,   char *buf,  int length)
int sensorfld               (char *sensor,      char *buf,  int length)
int opsgainmode             (char *sensor,      char *buf,  int length)
int calcrsp                 (int *dtk_rev,      char *buf,  int length)

Description:    

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Wed Dec 20 11:54:10 PST 1995

Notes:      
==============================================================================*/
int reqq_set_trigger    (int *trigger,  char *unusedptr, int unused_length)
{
    *trigger = 1 ;
    return (TRUE) ;
}

int file_creation_date_ns( char *unusedptr , char *buf , int length)
{
    char curr_time[22] ; 
    char yyyymmdd[9]="yyyymmdd" ; 
    char epoch[17]="yyyymmddhh:mm:ss" ;

    tc_systime2asf( curr_time);
    /*  printf("curr_time:%s\n", curr_time);  */
    tc_asf2yyyymmdd(curr_time , yyyymmdd);
    /*  printf("yyyymmdd:%s\n",yyyymmdd);  */

    strncpy(epoch,yyyymmdd,8);
    strncpy(&epoch[8],&curr_time[9],8);
    strncpy(buf,epoch,length);

    return(TRUE);
}
 
int sensorfld(char *sensor , char *buf , int length)
{
    if( strcmp(sensor,"VNR") == 0 )
        strncpy(buf,"OVN",length);
    else
        strncpy(buf,sensor,length);

    return(TRUE);
}

int opsgainmode( char *sensor , char *buf , int length )
{
    /*  printf("opsgainmode sensor:%s\n",sensor);  */
    if( strcmp(sensor,"SAR") == 0 )
        return(TRUE);
    else
        strncpy(buf,"N",length);

    return(TRUE);
}

int calcrsp( int *dtk_rev , char *buf , int length )
{
 
    extern int j1rt2rsp_(void *dbproc, int *dtk_rev  , 
                        char *strttime , char *stoptime ,
                        int  *rsp    , double *angl1 , double *angl2  ,
                        int  *ret_code );
    int    rsp   , ret_code ;
    double angl1 , angl2 ;
    char bersp[19];
 
    j1rt2rsp_(&FA_dtkf_dbproc , dtk_rev , 
            fa_file_start_time , fa_file_stop_time , 
            &rsp , &angl1 , &angl2 , 
            &ret_code);
 
    if( ret_code )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
            "error computing rsp path and angles from dtk attributes\n", 
            DO_SYSLOG, DO_PRINT);

        sprintf(file_util_msg, 
            "ret_code:%d , rev: %d , strttime: %s , stoptime: %s \n",
            ret_code , *dtk_rev , fa_file_start_time , fa_file_stop_time );
        aps_log_msg(file_util_progname, APS_ERROR, file_util_msg, 
            DO_SYSLOG, DO_PRINT);

        return (FALSE);
    }
 
    /*  
    -- printf("calcrsp--- rsp:%d angl1:%6.2f angl2:%6.2f\n",rsp,angl1,angl2);  
    */
    sprintf( bersp    , "%3d%6.2f" , rsp , angl1);
    sprintf(&bersp[9] , "%3d%6.2f" , rsp , angl2);
    strncpy(buf,bersp,length);

    return(TRUE);
}


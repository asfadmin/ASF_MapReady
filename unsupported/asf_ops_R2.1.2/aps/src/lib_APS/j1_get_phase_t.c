#undef PRINT_DIAG
#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	syb_j1_get_phase_t.c

Description:	contains syb_j1_get_phase_t()

External Functions Defined:
syb_j1_get_phase_t()

Note:		

	This file was edited using 4-space tabs.  

==============================================================================*/
#pragma ident	"@(#)j1_get_phase_t.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.j1_get_phase_t.c"


#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>

#include <stdlib.h>

#include "timeconv.h"				/* for time conversions.  				*/
#include "phase_utilities.h"		/* for the phase utility and relation.  */

 

/*==============================================================================
Function:       j1_get_phase_t()

Description:    GIVEN ephemeris time, gets J1 phase data.  

Parameters:     
 
   Input Parameters:
   Name         Type    Definition
   dbproc	*DBPROCESS  pointer to area with Sybase info.  
   ETIME	*REAL*8  TIME (EPHEMERIS JULIAN DAYS)
   Output Parameters:
   PHASE_NAME   *CH*1    PHASE NAME, I.E., A, B, C...
   PHASE_START	*CH*21   ASFTIME:  yyyy:ddd:hh:mm:ss.sss for start of phase.
   PHASE_LON 	*real*8  longitude of first ascending node.  
   PHASE_DAYS 	*int	number of days in the orbit.  
   PHASE_ORBITS	*int	number of revs in the phase.  
   LAST_REV   	*int	the last rev number in the phase
   CYCLE_DAYS 	*int	number of days in the ground rack repeat cycle.  
   CYCLE_REVS	*int	number of revs in the ground rack repeat cycle.
   ORB_A    	*real*8	semimajor axis of orbit
   ORB_E   	*real*8	eccentricity of orbit
   ORB_I  	*real*8	inclination of orbit
   ORB_ARG_PERI *real*8	argument of periapsis of orbit.   
   RSP_0_LON	*real*8 longitude of ascending RSP=0 node.  
   N_ROWS		*int	number of rows in the RSP scheme.  
   MIN_ROW		*int	min row number in the RSP scheme.  
   MAX_ROW		*int	max row number in the RSP scheme.  
   IER          *INT     CONDITION CODE:
                        = -1     ERROR IN INPUT TIME.  
                        =  0     NORMAL CONDITION.
                        =  1     THE TIME WAS BEFORE ALL NODES.
 				the time was before all phase time periods 
 				for this satellite. 
                        =  2     THE TIME WAS BETWEEN 2 NODES SEPARATED BY A GAP
                                IN TIME LARGER THAN ONE NODAL PERIOD.
 				the time was after the first phase start and 
 				before the last phase end yet it was not 
 				within any phase time period for this satellite.
 				Therefore, the input time was between two 
 				phases; in a gap between 2 phases.  
                        =  3     THE TIME WAS AFTER ALL NODES.
 				the time was after all phase time periods 
 				for this satellite. 
                        =  4     NO RECORDS WERE FOUND IN THE PHASE RELATION
                                FOR THIS SATELLITE.  Or any other serious 
								problem.  

Returns:         void

Creator:        Lawrence Stevens

Creation Date:  Wed Sep 27 11:31:00 PDT 1995

Notes:		
	This file was edited using 4-space tabs.  
==============================================================================*/
void j1_get_phase_t(
	DBPROCESS 	*dbproc, 
	double		*ETIME,
	char 		*PHASE_NAME,
	char 		*PHASE_START, 
	double 		*PHASE_LON  ,
	int 		*PHASE_DAYS ,
	int 		*PHASE_ORBITS,
	int 		*LAST_REV   ,
	int 		*CYCLE_DAYS ,
	int 		*CYCLE_REVS ,
	double 		*ORB_A      ,
	double 		*ORB_E      ,
	double 		*ORB_I      ,
	double 		*ORB_ARG_PERI,
	double		*RSP_0_LON,    /* longitude of ascending RSP=0 node.*/
	int			*N_ROWS,       /* number of rows in the RSP scheme. */
	int			*MIN_ROW,      /* min row number in the RSP scheme.	*/
	int			*MAX_ROW,      /* max row number in the RSP scheme.	*/
	int		*IER	)
{

DB_RECORD   **phase_rec = NULL ;

RETCODE     return_code;

char		asftime[22];

#ifdef PRINT_DIAG
printf("j1_get_phase_t: starting: ETIME = %.9f,\n", *ETIME);
printf("\tdbproc = %x\n", dbproc);
#endif

/* initialize the output data parameters by clearing them. 	*/
*PHASE_NAME = '\0';
*PHASE_START = '\0';
*PHASE_LON = -2000;
*PHASE_DAYS = -10;
*PHASE_ORBITS = -10;
*LAST_REV = -1;
*CYCLE_DAYS = -10;
*CYCLE_REVS = -10;
*ORB_A = -1.0;
*ORB_E = -1.0;
*ORB_I = -1.0;
*ORB_ARG_PERI = -1.0;
*RSP_0_LON	= -500.0 ;
*N_ROWS		= -1 ;
*MIN_ROW	= -1 ;
*MAX_ROW	= -1 ;
*IER = -111;

/* check *ETIME  */
if(!tc_et2asf(*ETIME, asftime) )
{
    /* error in input time  */
    *IER = -1;
    return;
}
 
#ifdef PRINT_DIAG
printf(" asftime = %s\n", asftime);
#endif
 
return_code = asftime_2_phase( "J1", asftime, &phase_rec )  ;
switch( return_code )
{
case PHASE_INPUT_TIME_WITHIN_A_PHASE :
    *IER = 0 ;
    break ;
case PHASE_INPUT_TIME_BEFORE_ALL_PHASES :
    *IER = 1 ;
    break ;
case PHASE_INPUT_TIME_BETWEEN_PHASES :
    *IER = 2 ;
    break ;
case PHASE_INPUT_TIME_AFTER_ALL_PHASES :
    *IER = 3 ;
    break ;
default :
    /* there was some serious problem with the data. */
    printf("%s(%d):  j1_get_phase_t(): %s\n",
        __FILE__, __LINE__, PHASE_ERROR_MESSAGE( return_code ) ) ;
    printf("SAT = J1, ETIME = %.9f,\n", *ETIME);
    printf(" asftime = %s\n", asftime);
    *IER = 4 ;
    /* clean up memory.  */
    free_db_record( phase_rec ) ;
    return ;
    break ;
}
 
/* 
-- note that the desired record might not have been found and 
-- that data will be set from the phase record dispite the 
-- non-zero error code.  
*/

/*
-- this record holds the data that we want;  assign
-- the output values.
*/
*PHASE_NAME      = CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME] ;
*(PHASE_NAME+1)  = '\0' ;
strcpy(PHASE_START, CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START] ) ;
*PHASE_LON      = CAST_PHASE_PHASE_LON phase_rec[PHASE_PHASE_LON] ;
*PHASE_DAYS     = CAST_PHASE_PHASE_DAYS phase_rec[PHASE_PHASE_DAYS] ;
*PHASE_ORBITS   = CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] ;
*LAST_REV       = CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] ;
*CYCLE_DAYS     = CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] ;
*CYCLE_REVS     = CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;
*ORB_A          = CAST_PHASE_ORB_A phase_rec[PHASE_ORB_A] ;
*ORB_E          = CAST_PHASE_ORB_E phase_rec[PHASE_ORB_E] ;
*ORB_I          = CAST_PHASE_ORB_I phase_rec[PHASE_ORB_I] ;
*ORB_ARG_PERI   = CAST_PHASE_ORB_ARG_PERI phase_rec[PHASE_ORB_ARG_PERI] ;
*RSP_0_LON      = CAST_PHASE_RSP_0_LON phase_rec[PHASE_RSP_0_LON] ;
*N_ROWS         = CAST_PHASE_N_ROWS phase_rec[PHASE_N_ROWS] ;
*MIN_ROW		= CAST_PHASE_MIN_ROW phase_rec[PHASE_MIN_ROW] ;
*MAX_ROW		= CAST_PHASE_MAX_ROW phase_rec[PHASE_MAX_ROW] ;
 
/* clean up memory.  */
free_db_record( phase_rec ) ;
 
return;
 
}

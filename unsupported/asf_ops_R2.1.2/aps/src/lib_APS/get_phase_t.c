#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	syb_get_phase_t.c

Description:	given a satellite and a time, get phase info.  

External Functions Defined:
get_phase_t()
	
Notes:

==============================================================================*/
#pragma ident	"@(#)get_phase_t.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.get_phase_t.c"



/*==============================================================================
Function:       get_phase_t()

Description:    given a satellite and a time, get phase info.

Parameters:     
   Functions called:
   Input Parameters:
   Name         Type    Definition
   dbproc	*DBPROCESS  pointer to area with Sybase info.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   ETIME	*REAL*8  TIME (EPHEMERIS JULIAN DAYS)
   Output Parameters:
   PHASE_NAME   *CH*1    (String) PHASE NAME, I.E., A, B, C...
   PHASE_START	*CH*21   (String) ASFTIME:  yyyy:ddd:hh:mm:ss.sss for start of phase.
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
   antarctic_mode *CH*1 (String) value is Y or N.  
   IER          *INT     CONDITION CODE:
                        =-1     THE INPUT ETIME WAS ILLEGAL.
                        = 0     NORMAL CONDITION.
                        = 1     THE TIME WAS BEFORE ALL NODES.
 				the time was before all phase time periods 
 				for this satellite. 
 				NOTE:  
 				in this case, the values returned will be for the first phase 
 				that begins AFTER the input ETIME.  
                        = 2     THE TIME WAS BETWEEN 2 NODES SEPARATED BY A GAP
                                IN TIME LARGER THAN ONE NODAL PERIOD.
 				the time was after the first phase start and 
 				before the last phase end yet it was not 
 				within any phase time period for this satellite.
 				Therefore, the input time was between two 
 				phases; in a gap between 2 phases.  
 				NOTE:  
 				in this case, the values returned will be for the first phase 
 				that begins AFTER the input ETIME.  
                        = 3     THE TIME WAS AFTER ALL NODES.
 				the time was after all phase time periods 
 				for this satellite. 
                        = 4     NO RECORDS WERE FOUND IN THE PHASE RELATION
                                FOR THIS SATELLITE.
                        = 12    error in code.  this is in case there is a 
 								self-inconsistency in the logic of testing 
 								the various flags. 

Creator:        Lawrence Stevens

Creation Date:  Fri Sep 22 19:03:30 PDT 1995

Notes:		
==============================================================================*/

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

#include "timeconv.h"
#include "phase_utilities.h"
 
void get_phase_t(
	DBPROCESS 	*dbproc, 
	char 		*SAT, 
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
	char		*antarctic_mode,
	int			*IER	)
{
DB_RECORD       **phase_rec = NULL ;

RETCODE         return_code;

char		asftime[22];

#ifdef PRINT_DIAG
printf("get_phase_t: starting: SAT = %2.2s, ETIME = %.9f,\n", SAT, *ETIME) ;
#endif

/* initialize the output data parameters by clearing them. 	*/
*PHASE_NAME   = '\0';
*PHASE_START  = '\0';
*PHASE_LON    = -2000;
*PHASE_DAYS   = -10;
*PHASE_ORBITS = -10;
*LAST_REV     = -1;
*CYCLE_DAYS   = -10;
*CYCLE_REVS   = -10;
*ORB_A        = -1.0;
*ORB_E        = -1.0;
*ORB_I        = -1.0;
*ORB_ARG_PERI = -1.0;
*antarctic_mode = '\0';
*IER          = -111;

if(!tc_et2asf(*ETIME, asftime) )
{
	/* error in input time	*/
	*IER = -1;
	return;
}

#ifdef PRINT_DIAG
printf(" asftime = %s\n", asftime); 
#endif

return_code = asftime_2_phase( SAT, asftime, &phase_rec )  ; 
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
	printf("%s(%d):  get_phase_t(): %s\n",
		__FILE__, __LINE__, PHASE_ERROR_MESSAGE( return_code ) ) ;
	printf("SAT = %2.2s, ETIME = %.9f,\n", SAT, *ETIME);
	printf(" asftime = %s\n", asftime); 
	*IER = 4 ;
	return ;
	break ;
}


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
*antarctic_mode = CAST_PHASE_ANTARCTIC_MODE phase_rec[PHASE_ANTARCTIC_MODE] ;
*(antarctic_mode+1)  = '\0' ;

/* clean up memory.  */
free_db_record( phase_rec ) ;

return;

}

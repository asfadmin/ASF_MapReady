#undef PRINT_DIAG
#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	syb_sscvrev2time.c

Description:	contains sscvrev2time()

External Functions Defined:
sscvrev2time()
	
Notes:
	This file written with a 4-character tab setting. 

==============================================================================*/
#pragma ident	"@(#)sscvrev2time.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.sscvrev2time.c"


#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>

#include <stdlib.h>
#include "timeconv.h"
#include "phase_utilities.h"    /* for phase routines and phase definitions. */
#include "aps_db_table.h"       /* for table names.     */
 

/*==============================================================================
Function:       sscvrev2time()

Description:    Given a sat and a rev number, gets the start time 
				and end time of the rev.  Note special cases 
				described below for when the rev does not belong to 
				any phase.  the times returned are then the times 
				of the next and previous phase boundaries.  
				The purpose of this routine is to help, in specific 
				site coverage (compute data-take opportunities) 
				to determine a valid time bracket from a rev bracket
				even if one of the revs is in a phase gap.  

Parameters:     
   Input Parameters:
   Name         Type    Definition
   dbproc	*DBPROCESS  pointer to area with Sybase info.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   REV		*INT  		rev number

   Output Parameters:

   ET1		*real*8		time of start of REV.  If REV is not within any phase, 
						it has no useable value.  

   ASFT1	*CH*21  	ASFTIME:  yyyy:ddd:hh:mm:ss.sss for start of phase.
 			the asf time format for ET1.  
 			if the input REV was not within a phase, then this 
 			time is moved forward from REV to the start of the 
 			next phase, if any.  See values for IER.

   ET2		*real*8		time of end of REV.  If REV is not within any phase, 
                    	it has no useable value.  

   ASFT2	*CH*21  	ASFTIME:  yyyy:ddd:hh:mm:ss.sss for start of phase.
 			the asf time format for ET2.  
 			if the input REV was not within a phase, then this 
 			time is moved backward from REV to the end of the 
 			previous phase, if any.  See values for IER.

   IER          *INT     CONDITION CODE:
                        = -1    bad phase_start ASF time.  
                        =  0     NORMAL CONDITION.
                        =  1     THE REV WAS BEFORE ALL PHASES
 				the rev was before all phase time periods 
 				for this satellite. 
                        =  2     THE REV WAS BETWEEN 2 NODES SEPARATED BY A GAP
                                 IN TIME LARGER THAN ONE NODAL PERIOD.
 				the rev was after the first phase start and 
 				before the last phase end yet it was not 
 				within any phase time period for this satellite.
 				Therefore, the input rev was between two 
 				phases; in a gap between 2 phases.  
                        = 3     THE REV WAS AFTER ALL NODES.
 				the rev was after all phase time periods 
 				for this satellite. 
                        = 4     NO RECORDS WERE FOUND IN THE PHASE RELATION
                                FOR THIS SATELLITE.

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Wed Sep 27 14:01:02 PDT 1995

Notes:		
	This file written with a 4-character tab setting. 
==============================================================================*/
void sscvrev2time(
	DBPROCESS 	*dbproc, 
	char 		*SAT, 
	int			*REV,
	double		*ET1,
	char 		*ASFT1,
	double		*ET2,
	char 		*ASFT2,
	int			*IER	)
{

DB_RECORD       **phase_rec = NULL ;

RETCODE         return_code;

char		asftime[22];

int 		first_rev ; /* first rev number in the phase.  */
double 		et1 ;       /* ephemeris time of the start of the phase.  */
double 		tnode ;     /* time for a single rev.          */

#ifdef PRINT_DIAG
printf("sscvrev2time: starting: SAT = %2.2s, REV = %d,\n", SAT, *REV);
#endif

/* initialize the output data parameters by clearing them. 	*/
*ET1 = -0.1;
strcpy(ASFT1, "no time 1 computed");
*ET2 = -0.1;
strcpy(ASFT2, "no time 2 computed");
*IER = -1;

/* get the phase record with the input rev number.  */
return_code = rev_2_phase( SAT, *REV, &phase_rec )  ;
switch( return_code )
{
case PHASE_INPUT_REV_WITHIN_A_PHASE :
    *IER = 0 ;
    break ;
case PHASE_INPUT_REV_BEFORE_ALL_PHASES :
    *IER = 1 ;
	free_db_record( phase_rec ) ;
	return ;
    break ;
case PHASE_INPUT_REV_BETWEEN_PHASES :
    *IER = 2 ;
	free_db_record( phase_rec ) ;
	return ;
    break ;
case PHASE_INPUT_REV_AFTER_ALL_PHASES :
    *IER = 3 ;
	free_db_record( phase_rec ) ;
	return ;
    break ;
default :
    /* there was some serious problem with the data. */
    printf("%s(%d):  get_phase_r(): %s\n",
        __FILE__, __LINE__, PHASE_ERROR_MESSAGE( return_code ) ) ;
    printf("SAT = %2.2s, REV = %d,\n", SAT, *REV);
    *IER = 4 ;
	if ( phase_rec != NULL )
		free_db_record( phase_rec ) ;
    return ;
    break ;
}

#ifdef PRINT_DIAG
printf("sscvrev2time: retrieved:\n" ) ;
db_print_record(phase_rec, APS_CDEFS(PHASE) ) ;
#endif

first_rev =   CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV]  
			- CAST_PHASE_PHASE_ORBITS   phase_rec[PHASE_PHASE_ORBITS] + 1 ;

#ifdef PRINT_DIAG
printf("first_rev = %d\n", first_rev) ;
#endif

/* 
-- compute the time for one nodal period in 
-- double precision.				
*/
tnode = CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] ;
tnode = tnode / CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;

#ifdef PRINT_DIAG
printf ("tnode = %f\n", tnode) ;
#endif

/* get ephemeris time for phase start */
if ( !tc_asf2et(CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START], &et1) )
{
	free_db_record( phase_rec ) ;
	return  ;
}

/* set the time of the start of the rev.  */
*ET1 = et1 + ( *REV - first_rev ) * tnode ;

/* set the time of the end of the rev.  */
*ET2 = *ET1 + tnode ;

/* subtract .99 of a millisecond so that ET2
belongs to the current rev number only.  */
*ET2 = *ET2 - 0.99/24/3600/1000 ;

/*  get the corresponding ASF format times.  */
if ( !tc_et2asf(*ET1, ASFT1) )
{
	free_db_record( phase_rec ) ;
	return ;
}
if ( !tc_et2asf(*ET2, ASFT2) )
{
	free_db_record( phase_rec ) ;
	return ;
}

*IER = 0 ;
free_db_record( phase_rec ) ;
return ;

}

#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	syb_get_phase.c

Description:	gets the phase data for a SAT, PHASE_NAME.  

External Functions Defined:
get_phase()
	
Notes:
calls phase_get_list(), which checks the phase relation...

==============================================================================*/
#pragma ident	"@(#)get_phase.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.get_phase.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>

#include <stdlib.h>

#include "timeconv.h"

#include "phase_utilities.h"


/*==============================================================================
Function:       get_phase()

Description:    gets the phase data for a SAT, PHASE_NAME


Creator:        Lawrence Stevens

Creation Date:  Fri Sep 22 13:29:18 PDT 1995

Notes:		
*  Purpose:     GIVEN A SAT AND phase name, gets phase data. 
*  NOTE:  	this routine was converted to C from Fortran.  All of the 
*		parameters are pointers in this C version.
==============================================================================*/

/*
--  Input Parameters:
--  Name         Type    Definition
--  SAT          CH*2    SATELLITE NAME:  E1, J1, OR RS
--  PHASE_NAME   CH*1    PHASE NAME, I.E., A, B, C...
--
--  Output Parameters:
--  PHASE_START	CH*21   ASFTIME:  yyyy:ddd:hh:mm:ss.sss for start of phase.
--  PHASE_LON 	real*8  longitude of first ascending node.  
--  PHASE_DAYS 	int	number of days in the orbit.  
--  PHASE_ORBITS	int	number of revs in the phase.  
--  LAST_REV   	int	the last rev number in the phase
--  CYCLE_DAYS 	int	number of days in the ground rack repeat cycle.  
--  CYCLE_REVS	int	number of revs in the ground rack repeat cycle.
--  ORB_A    	real*8	semimajor axis of orbit
--  ORB_E   	real*8	eccentricity of orbit
--  ORB_I  	real*8	inclination of orbit
--  ORB_ARG_PERI real*8	argument of periapsis of orbit.   
--
*/

void get_phase(
	DBPROCESS 	*dbproc, 
	char 		*SAT, 
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
	double 		*ORB_ARG_PERI  )
{

DB_RECORD       **phase_rec ;
DB_RECORD       **phase_rec_found ;
llist           *phase_list = NULL ;
cursor          phase_list_ptr ;


DBCHAR		sat[3];
DBCHAR		phase_name[2];  
DBCHAR		phase_start[22];
DBFLT8		phase_lon;
DBINT           phase_days;
DBINT           phase_orbits;
DBINT           last_rev;
DBINT           cycle_days;
DBINT           cycle_revs;
DBFLT8		orb_a;
DBFLT8		orb_e;
DBFLT8		orb_i;
DBFLT8		orb_arg_peri;

RETCODE         return_code;

char prev_sat[3], prev_phase_name[2];

char		asftime[22];
int 		prev_last_rev, first_rev, found, rcode, ier;
double		prev_et2, et1, et2, tnode;

#ifdef PRINT_DIAG
printf("get_phase: starting: SAT = %s, PHASE_NAME = %s\n", SAT, PHASE_NAME);
#endif

/* initialize the output data parameters by clearing them.     */
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

/* 
-- get all of the phase records in a list; 
-- note that phase_get_list checks the list against 
-- overlaps.  
*/
/*
-- get the phase recs for the given satellite;
-- pass the address of the phase_list so that its value
-- can be set right.  phase_get_list will allocate a list
-- of DB_RECORDS.
*/
return_code = phase_get_list( SAT, &phase_list ) ;
if ( return_code != PHASE_GET_LIST_OK ) 
{
	printf("get_phase.c:  %s\n", PHASE_ERROR_MESSAGE(return_code) ) ;
	printf("              SAT = %s, PHASE_NAME = %s\n", SAT, PHASE_NAME); 
	printf("              terminating this run.\n");
    return_code = system("banner ERROR");
    exit(1);

}
/*
-- find the desired phase record
*/
found = 0;
for(phase_rec = (DB_RECORD **) FIRST(phase_list, phase_list_ptr) ;
	phase_rec != NULL ;
	phase_rec = (DB_RECORD **) NEXT(phase_list, phase_list_ptr)  )
{
	if( CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME] == *PHASE_NAME )
	{
		/* this record holds the data that we want.	*/
		found = 1;
		break ;
	}
}
 
if(found == 0)
{
	printf("get_phase.c:  error; phase not found in phase relation. \n");
	printf("              SAT = %s, PHASE_NAME = %s\n", SAT, PHASE_NAME); 
	printf("              terminating this run.\n");
	rcode = system("banner ERROR");
	exit(1);
}

/* 
-- this record holds the data that we want;  assign 
-- the output values. 
*/ 
strcpy(PHASE_START, CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START] ) ;
*PHASE_LON 		= CAST_PHASE_PHASE_LON phase_rec[PHASE_PHASE_LON] ;
*PHASE_DAYS 	= CAST_PHASE_PHASE_DAYS phase_rec[PHASE_PHASE_DAYS] ;
*PHASE_ORBITS 	= CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] ;
*LAST_REV 		= CAST_PHASE_LAST_REV phase_rec[PHASE_LAST_REV] ;
*CYCLE_DAYS 	= CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] ;
*CYCLE_REVS 	= CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;
*ORB_A 			= CAST_PHASE_ORB_A phase_rec[PHASE_ORB_A] ;
*ORB_E 			= CAST_PHASE_ORB_E phase_rec[PHASE_ORB_E] ;
*ORB_I 			= CAST_PHASE_ORB_I phase_rec[PHASE_ORB_I] ;
*ORB_ARG_PERI 	= CAST_PHASE_ORB_ARG_PERI phase_rec[PHASE_ORB_ARG_PERI] ;

/* clean up memory.  */
DEL_LIST( phase_list ) ;

return;

}

#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	syb_get_phase_r.c

Description:	given a rev number and satellite, get phase info.  

External Functions Defined:
get_phase_r()
	
File Scope Functions:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)get_phase_r.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.get_phase_r.c"



/*==============================================================================
Function:       get_phase_r()

Description:    GIVEN A SAT AND a rev number, gets phase data.

Returns:
   IER          *INT     CONDITION CODE:
                        = 0     NORMAL CONDITION.
                        = 1     THE REV WAS BEFORE ALL NODES.
 				the rev was before all phase time periods 
 				for this satellite. 
                        = 2     THE REV WAS BETWEEN 2 PHASES
 				the rev was after the first phase start and 
 				before the last phase end yet it was not 
 				within any phase time period for this satellite.
 				Therefore, the input rev was between two 
 				phases; in a gap between 2 phases.  
                        = 3     THE TIME WAS AFTER ALL NODES.
 				the rev was after all phase time periods 
 				for this satellite. 
                        = 4     NO RECORDS WERE FOUND IN THE PHASE RELATION
                                FOR THIS SATELLITE.

Creator:        Lawrence Stevens

Creation Date:  Fri Sep 22 17:53:50 PDT 1995

Notes:		
==============================================================================*/

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

#include "phase_utilities.h"
 
void get_phase_r(
	DBPROCESS 	*dbproc, 
	char 		*SAT, 
	int			*REV,
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
	int			*IER	)
{

DB_RECORD       **phase_rec = NULL ;

RETCODE         return_code;

#ifdef PRINT_DIAG
printf("get_phase_r: starting: SAT = %2.2s, REV = %d,\n", SAT, *REV);
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
*IER = -111;

return_code = rev_2_phase( SAT, *REV, &phase_rec )  ;
switch( return_code ) 
{
case PHASE_INPUT_REV_WITHIN_A_PHASE :
	*IER = 0 ;
	break ;
case PHASE_INPUT_REV_BEFORE_ALL_PHASES :
	*IER = 1 ;
	break ;
case PHASE_INPUT_REV_BETWEEN_PHASES :
	*IER = 2 ;
	break ;
case PHASE_INPUT_REV_AFTER_ALL_PHASES :
	*IER = 3 ;
	break ;
default :
	/* there was some serious problem with the data. */
	printf("%s(%d):  get_phase_r(): %s\n", 
		__FILE__, __LINE__, PHASE_ERROR_MESSAGE( return_code ) ) ; 
	printf("SAT = %2.2s, REV = %d,\n", SAT, *REV);
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
 
/* clean up memory.  */
free_db_record( phase_rec ) ;

return ;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_phase.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)f77_db_phase.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_phase.c"
#include <db_sybint.h>

#include "db_phase.h"

struct f77_phase_rec {
	DBFLT8	phase_lon ;
	DBFLT8	orb_a ;
	DBFLT8	orb_e ;
	DBFLT8	orb_i ;
	DBFLT8	orb_arg_peri ;
	DBFLT8	rsp_0_lon ;
	DBFLT8	EW_tol_km ;
	DBFLT8	NS_tol_min ;
	DBINT	phase_days ;
	DBINT	phase_orbits ;
	DBINT	last_rev ;
	DBINT	cycle_days ;
	DBINT	cycle_revs ;
	DBINT	n_rows ;
	DBINT	min_row ;
	DBINT	max_row ;
	DBCHAR	phase_name ;
	char	sat[3] ;
	char	phase_start[22] ;
} f77_phase_rec_ ;


/*
	To access a phase record from fortran enter the following
	includes in the fortran code

      INCLUDE 'F77_db_extern.h'
      INCLUDE 'F77_db_phase.h'

    The db_extern contains declarations for the following

       db_get_records
       db_ftn_first_record
       db_ftn_next_record

    while the db_phase contains the declarations for
    the column names in the phase table.  The names
    are exactly the same as in the database table
*/


void load_f77_phase_rec(DB_RECORD **phase_rec) 
{
	f77_phase_rec_.phase_lon =	
		CAST_PHASE_PHASE_LON 	phase_rec[PHASE_PHASE_LON] ;

	f77_phase_rec_.phase_days =	
		CAST_PHASE_PHASE_DAYS	phase_rec[PHASE_PHASE_DAYS] ;

	f77_phase_rec_.phase_orbits =
		CAST_PHASE_PHASE_ORBITS	phase_rec[PHASE_PHASE_ORBITS] ;

	f77_phase_rec_.last_rev =	
		CAST_PHASE_LAST_REV		phase_rec[PHASE_LAST_REV] ;

	f77_phase_rec_.cycle_days =	
		CAST_PHASE_CYCLE_DAYS 	phase_rec[PHASE_CYCLE_DAYS] ;

	f77_phase_rec_.cycle_revs =	
		CAST_PHASE_CYCLE_REVS 	phase_rec[PHASE_CYCLE_REVS] ;

	f77_phase_rec_.orb_a = 		
		CAST_PHASE_ORB_A 		phase_rec[PHASE_ORB_A] ;

	f77_phase_rec_.orb_e = 		
		CAST_PHASE_ORB_E 		phase_rec[PHASE_ORB_E] ;

	f77_phase_rec_.orb_i = 		
		CAST_PHASE_ORB_I 		phase_rec[PHASE_ORB_I] ;

	f77_phase_rec_.orb_arg_peri =
		CAST_PHASE_ORB_ARG_PERI phase_rec[PHASE_ORB_ARG_PERI] ;

	f77_phase_rec_.rsp_0_lon = 	
		CAST_PHASE_RSP_0_LON 	phase_rec[PHASE_RSP_0_LON] ;

	f77_phase_rec_.n_rows =		
		CAST_PHASE_N_ROWS 		phase_rec[PHASE_N_ROWS] ;

	f77_phase_rec_.min_row = 	
		CAST_PHASE_MIN_ROW 		phase_rec[PHASE_MIN_ROW] ;

	f77_phase_rec_.max_row = 	
		CAST_PHASE_MAX_ROW 		phase_rec[PHASE_MAX_ROW] ;

	f77_phase_rec_.EW_tol_km = 	
		CAST_PHASE_EW_TOL_KM 	phase_rec[PHASE_EW_TOL_KM] ;

	f77_phase_rec_.NS_tol_min = 	
		CAST_PHASE_NS_TOL_MIN 	phase_rec[PHASE_NS_TOL_MIN] ;

	f77_phase_rec_.phase_name =	
		CAST_PHASE_PHASE_NAME 	phase_rec[PHASE_PHASE_NAME] ;

	strcpy(f77_phase_rec_.sat, 
		CAST_PHASE_SAT phase_rec[PHASE_SAT]) ;

	strcpy(f77_phase_rec_.phase_start, 
		CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START]) ;
} 

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_satsensor.c

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
#pragma ident	"@(#)f77_db_satsensor.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_satsensor.c"
#include <db_sybint.h>
#include "db_satsensor.h"

/*
-- the order in the fields of this structure 
-- must correspond to 
-- the COMMON block f77_satsensor_rec in f77_db_satsensor.inc
-- note that in the Sun world, fortran symbos acquire 
-- an extra '_' at the end of the name.  
-- so that this declaration: f77_satsensor_rec_
-- has the same name as the fortran f77_satsensor_rec
-- in f77_db_satsensor.inc
*/
struct f77_satsensor_rec {
	DBFLT8	beamradiusd;
	DBFLT8	lookangled;
	DBINT opermode; 
	char chopermode[16] ;	
	char sat[3] ;
	char sensor[4] ;
} f77_satsensor_rec_;


/* To access a satsensor record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_satsensor.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_satsensor contains the declarations for
   the column names in the satsensor table. The names
   are exactly the same as in the database table
*/


void load_f77_satsensor_rec(DB_RECORD **satsensor_rec) 
{
	f77_satsensor_rec_.opermode =	
		CAST_SATSENSOR_OPERMODE       satsensor_rec[SATSENSOR_OPERMODE] ;

	f77_satsensor_rec_.beamradiusd =
		CAST_SATSENSOR_BEAMRADIUSD    satsensor_rec[SATSENSOR_BEAMRADIUSD] ;

	f77_satsensor_rec_.lookangled =	
		CAST_SATSENSOR_LOOKANGLED     satsensor_rec[SATSENSOR_LOOKANGLED] ;

	strcpy (f77_satsensor_rec_.sat,
		CAST_SATSENSOR_SAT            satsensor_rec[SATSENSOR_SAT]) ;

	strcpy (f77_satsensor_rec_.sensor,
		CAST_SATSENSOR_SENSOR         satsensor_rec[SATSENSOR_SENSOR]) ;
	
	strcpy(f77_satsensor_rec_.chopermode,	
		CAST_SATSENSOR_CHOPERMODE     satsensor_rec[SATSENSOR_CHOPERMODE]) ;
}

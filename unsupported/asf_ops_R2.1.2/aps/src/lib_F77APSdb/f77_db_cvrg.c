#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_cvrg.c

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
#pragma ident	"@(#)f77_db_cvrg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_cvrg.c"

#include <db_sybint.h>

#include "db_cvrg.h"

struct f77_cvrg_rec {
	DBFLT8	mjdate;
	DBFLT8	sublat;
	DBFLT8	sublon;
	DBFLT8	nrlat;
        DBFLT8	nrlon;
        DBFLT8	farlat;
	DBFLT8  farlon;
        DBFLT8  satalt;
        DBFLT8  sunang;

        DBINT marker ; 
	DBINT rev ;	
        DBTINYINT opermode ;
        
        DBCHAR	sunn ;
	DBCHAR  crossflag ;
        DBCHAR  ascdsc ; 
        
        char sat [3] ;
        char sensor [4] ;
        char masks [5] ;
        
} f77_cvrg_rec_;


/* To access a cvrg record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_cvrg.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_cvrg contains the declarations for
   the column names in the cvrg table. The names
   are exactly the same as in the database table
*/


void load_f77_cvrg_rec(DB_RECORD **cvrg_rec) 
{
	f77_cvrg_rec_.marker =	
                CAST_CVRG_MARKER        cvrg_rec[CVRG_MARKER] ;

	f77_cvrg_rec_.mjdate =	
		CAST_CVRG_MJDATE        cvrg_rec[CVRG_MJDATE] ;
        
        f77_cvrg_rec_.rev =
		CAST_CVRG_REV	        cvrg_rec[CVRG_REV] ;

	f77_cvrg_rec_.sublat =	
                CAST_CVRG_SUBLAT        cvrg_rec[CVRG_SUBLAT] ;

	f77_cvrg_rec_.sublon =	
                CAST_CVRG_SUBLON        cvrg_rec[CVRG_SUBLON] ;

	f77_cvrg_rec_.nrlat =	
		CAST_CVRG_NRLAT         cvrg_rec[CVRG_NRLAT] ;

	f77_cvrg_rec_.nrlon = 		
		CAST_CVRG_NRLON	        cvrg_rec[CVRG_NRLON] ;

	f77_cvrg_rec_.farlat = 		
		CAST_CVRG_FARLAT        cvrg_rec[CVRG_FARLAT] ;

	f77_cvrg_rec_.satalt = 		
                CAST_CVRG_SATALT        cvrg_rec[CVRG_SATALT] ;

	f77_cvrg_rec_.sunang =
                CAST_CVRG_SUNANG        cvrg_rec[CVRG_SUNANG] ;

	f77_cvrg_rec_.sunn = 	
                CAST_CVRG_SUN 	        cvrg_rec[CVRG_SUN] ;

	f77_cvrg_rec_.opermode =
                CAST_CVRG_OPERMODE      cvrg_rec[CVRG_OPERMODE] ;

	f77_cvrg_rec_.crossflag = 	
				CAST_CVRG_CROSSFLAG     cvrg_rec[CVRG_CROSSFLAG] ;

	f77_cvrg_rec_.ascdsc = 	
                CAST_CVRG_ASCDSC        cvrg_rec[CVRG_ASCDSC] ;

        strcpy (f77_cvrg_rec_.sat,
                CAST_CVRG_SAT           cvrg_rec[CVRG_SAT]) ;

        strcpy (f77_cvrg_rec_.sensor,
                CAST_CVRG_SENSOR        cvrg_rec[CVRG_SENSOR]) ;

        strcpy (f77_cvrg_rec_.masks,
                CAST_CVRG_MASKS         cvrg_rec[CVRG_MASKS]) ;
}





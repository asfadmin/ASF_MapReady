#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_sscvrg.c

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
#pragma ident	"@(#)f77_db_sscvrg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_sscvrg.c"

#include <db_sybint.h>

#include "db_sscvrg.h"

struct f77_sscvrg_rec {
	DBFLT8  strtet;
        DBFLT8  stopet; 
        DBFLT8	strtlat;
	DBFLT8	stoplat;
	DBFLT8	nrlat1;
	DBFLT8  nrlon1;
        DBFLT8	farlat1;
        DBFLT8	farlon1;
	DBFLT8  nrlat2;
	DBFLT8  nrlon2;
	DBFLT8  farlat2;
        DBFLT8  farlon2;

        DBINT  darid;	
	DBINT  rev;
	
        DBCHAR  ascdsc;
	
        char sitename [33] ;
        char sat [3] ;
        char sensor [4] ;
        char strttime [22] ;
        char stoptime [22] ;
       
      
} f77_sscvrg_rec_;


/* To access a sscvrg record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_sscvrg.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_sscvrg contains the declarations for
   the column names in the sscvrg table. The names
   are exactly the same as in the database table
*/


void load_f77_sscvrg_rec(DB_RECORD **sscvrg_rec) 
{

	f77_sscvrg_rec_.darid =	
                CAST_SSCVRG_DARID           sscvrg_rec[SSCVRG_DARID] ;

	f77_sscvrg_rec_.rev =	
		CAST_SSCVRG_REV             sscvrg_rec[SSCVRG_REV] ;

	f77_sscvrg_rec_.strtet =
		CAST_SSCVRG_STRTET	    sscvrg_rec[SSCVRG_STRTET] ;

	f77_sscvrg_rec_.stopet =	
		CAST_SSCVRG_STOPET 	    sscvrg_rec[SSCVRG_STOPET] ;

	f77_sscvrg_rec_.strtlat =	
		CAST_SSCVRG_STRTLAT 	    sscvrg_rec[SSCVRG_STRTLAT] ;

	f77_sscvrg_rec_.stoplat =	
		CAST_SSCVRG_STOPLAT 	    sscvrg_rec[SSCVRG_STOPLAT] ;

        f77_sscvrg_rec_.nrlat1 = 		
                CAST_SSCVRG_NRLAT1	    sscvrg_rec[SSCVRG_NRLAT1] ;

	f77_sscvrg_rec_.nrlon1 = 		
		CAST_SSCVRG_NRLON1	    sscvrg_rec[SSCVRG_NRLON1] ;

	f77_sscvrg_rec_.farlat1 = 		
                CAST_SSCVRG_FARLAT1         sscvrg_rec[SSCVRG_FARLAT1] ;

	f77_sscvrg_rec_.farlon1 =
		CAST_SSCVRG_FARLON1         sscvrg_rec[SSCVRG_FARLON1] ;

	f77_sscvrg_rec_.nrlat2 = 	
		CAST_SSCVRG_NRLAT2 	    sscvrg_rec[SSCVRG_NRLAT2] ;

	f77_sscvrg_rec_.nrlon2 =		
		CAST_SSCVRG_NRLON2 	    sscvrg_rec[SSCVRG_NRLON2] ;

	f77_sscvrg_rec_.farlat2 = 	
                CAST_SSCVRG_FARLAT2 	    sscvrg_rec[SSCVRG_FARLAT2] ;

	f77_sscvrg_rec_.farlon2= 	
                CAST_SSCVRG_FARLON2	    sscvrg_rec[SSCVRG_FARLON2] ;

        f77_sscvrg_rec_.ascdsc =
                CAST_SSCVRG_ASCDSC          sscvrg_rec[SSCVRG_ASCDSC] ;

        strcpy (f77_sscvrg_rec_.sitename,
                CAST_SSCVRG_SITENAME        sscvrg_rec[SSCVRG_SITENAME]) ;

        strcpy (f77_sscvrg_rec_.sat,
                CAST_SSCVRG_SAT             sscvrg_rec[SSCVRG_SAT]) ;
        
        strcpy (f77_sscvrg_rec_.sensor, 
                CAST_SSCVRG_SENSOR          sscvrg_rec[SSCVRG_SENSOR]) ;

        strcpy (f77_sscvrg_rec_.strttime,
                CAST_SSCVRG_STRTTIME        sscvrg_rec[SSCVRG_STRTTIME]) ;

        strcpy (f77_sscvrg_rec_.stoptime,
                CAST_SSCVRG_STOPTIME        sscvrg_rec[SSCVRG_STOPTIME]) ;


}






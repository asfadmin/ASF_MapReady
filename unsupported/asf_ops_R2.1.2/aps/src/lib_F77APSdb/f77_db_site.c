#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_site.c

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
#pragma ident	"@(#)f77_db_site.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_site.c"

#include <db_sybint.h>

#include "db_site.h"

struct f77_site_rec {
	DBFLT8	radius;
	DBFLT8	nwlat;
	DBFLT8	nwlon;
	DBFLT8	nelat ;
        DBFLT8	nelon;
        DBFLT8	selat;
	DBFLT8  selon;
	DBFLT8  swlat;
	DBFLT8  swlon;

        DBCHAR	shape;

        char sitename [33] ;
        char comments [50] ;

} f77_site_rec_;


/* To access a site record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_site.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_site contains the declarations for
   the column names in the site table. The names
   are exactly the same as in the database table
*/


void load_f77_site_rec(DB_RECORD **site_rec) 
{

	f77_site_rec_.radius =
		CAST_SITE_RADIUS	    site_rec[SITE_RADIUS] ;

	f77_site_rec_.nwlat =	
		CAST_SITE_NWLAT 	    site_rec[SITE_NWLAT] ;

	f77_site_rec_.nwlon =	
		CAST_SITE_NWLON 	    site_rec[SITE_NWLON] ;

	f77_site_rec_.nelat =	
		CAST_SITE_NELAT 	    site_rec[SITE_NELAT] ;

        f77_site_rec_.nelon = 		
                CAST_SITE_NELON	            site_rec[SITE_NELON] ;

	f77_site_rec_.selat = 		
		CAST_SITE_SELAT	            site_rec[SITE_SELAT] ;

	f77_site_rec_.selon = 		
                CAST_SITE_SELON             site_rec[SITE_SELON] ;

	f77_site_rec_.swlat =
		CAST_SITE_SWLAT             site_rec[SITE_SWLAT] ;

	f77_site_rec_.swlon = 	
		CAST_SITE_SWLON             site_rec[SITE_SWLON] ;

        f77_site_rec_.shape = 
                CAST_SITE_SHAPE             site_rec[SITE_SHAPE] ;

        strcpy (f77_site_rec_.sitename,
                CAST_SITE_SITENAME          site_rec[SITE_SITENAME]) ;
   
        strcpy (f77_site_rec_.comments,
                CAST_SITE_COMMENTS          site_rec[SITE_COMMENTS]) ;
}







#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_dar.c

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
#pragma ident	"@(#)f77_db_dar.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_dar.c"

#include <db_sybint.h>

#include "db_dar.h"

struct f77_dar_rec {
	DBFLT8	radius;
	DBFLT8	nwlat;
	DBFLT8	nwlon;
	DBFLT8	nelon;
	DBFLT8	selat;
	DBFLT8  nelat;   	
	DBFLT8  selon;
	DBFLT8  swlat;
	DBFLT8  swlon;
	DBINT   darid;
	DBINT   prvdarid;
	DBINT   nobs; 
	DBINT   rev;
	DBCHAR  shape;
	DBCHAR  ascdsc;
	char  fobs[16] ;
	char userid [16] ;
	char reqtime [22] ;
	char reqstat [3] ;
	char prvreqstat [4] ;
	char sat [3] ;
	char sensor [4] ;
	char strttime [22] ;
	char endtime [22] ;
	char sitename [33] ;
	char usercmnt [256] ;
	char plnrcmnt [256] ;
} f77_dar_rec_;


/* To access a dar record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_dar.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_dar contains the declarations for
   the column names in the dar table. The names
   are exactly the same as in the database table
*/


void load_f77_dar_rec(DB_RECORD **dar_rec) 
{
	f77_dar_rec_.darid =	
                CAST_DAR_DARID      dar_rec[DAR_DARID] ;

	f77_dar_rec_.prvdarid =	
		CAST_DAR_PRVDARID   dar_rec[DAR_PRVDARID] ;

	f77_dar_rec_.radius =
		CAST_DAR_RADIUS	    dar_rec[DAR_RADIUS] ;

	f77_dar_rec_.nwlat =	
		CAST_DAR_NWLAT 	    dar_rec[DAR_NWLAT] ;

	f77_dar_rec_.nwlon =	
		CAST_DAR_NWLON 	    dar_rec[DAR_NWLON] ;

	f77_dar_rec_.nelat =	
		CAST_DAR_NELAT 	    dar_rec[DAR_NELAT] ;

        f77_dar_rec_.nelon = 		
                CAST_DAR_NELON	    dar_rec[DAR_NELON] ;

	f77_dar_rec_.selat = 		
		CAST_DAR_SELAT	    dar_rec[DAR_SELAT] ;

	f77_dar_rec_.selon = 		
                CAST_DAR_SELON      dar_rec[DAR_SELON] ;

	f77_dar_rec_.swlat =
		CAST_DAR_SWLAT      dar_rec[DAR_SWLAT] ;

	f77_dar_rec_.swlon = 	
		CAST_DAR_SWLON 	    dar_rec[DAR_SWLON] ;

	f77_dar_rec_.nobs =		
		CAST_DAR_NOBS 	    dar_rec[DAR_NOBS] ;


 	strcpy (f77_dar_rec_.fobs, 
		CAST_DAR_FOBS     dar_rec[DAR_FOBS]) ;


	f77_dar_rec_.rev = 	
                CAST_DAR_REV	    dar_rec[DAR_REV] ;

        f77_dar_rec_.shape =
                CAST_DAR_SHAPE      dar_rec[DAR_SHAPE] ;

        f77_dar_rec_.ascdsc =
                CAST_DAR_ASCDSC     dar_rec[DAR_ASCDSC] ;

        strcpy (f77_dar_rec_.userid,
                CAST_DAR_USERID     dar_rec[DAR_USERID]) ;

        strcpy (f77_dar_rec_.reqtime,
                CAST_DAR_REQTIME    dar_rec[DAR_REQTIME]) ;

        strcpy (f77_dar_rec_.reqstat,
                CAST_DAR_REQSTAT    dar_rec[DAR_REQSTAT]) ;

        strcpy (f77_dar_rec_.prvreqstat,
                CAST_DAR_PRVREQSTAT   dar_rec[DAR_PRVREQSTAT]) ;

        strcpy (f77_dar_rec_.sat,
                CAST_DAR_SAT        dar_rec[DAR_SAT]) ;
        
        strcpy (f77_dar_rec_.sensor, 
                CAST_DAR_SENSOR     dar_rec[DAR_SENSOR]) ;

        strcpy (f77_dar_rec_.strttime,
                CAST_DAR_STRTTIME    dar_rec[DAR_STRTTIME]) ;

        strcpy (f77_dar_rec_.endtime,
                CAST_DAR_ENDTIME    dar_rec[DAR_ENDTIME]) ;

        strcpy (f77_dar_rec_.sitename,
                CAST_DAR_SITENAME   dar_rec[DAR_SITENAME]) ;
   
        strcpy (f77_dar_rec_.usercmnt,
                CAST_DAR_USERCMNT    dar_rec[DAR_USERCMNT]) ;
 
        strcpy (f77_dar_rec_.plnrcmnt,
                CAST_DAR_PLNRCMNT   dar_rec[DAR_PLNRCMNT]) ;

}


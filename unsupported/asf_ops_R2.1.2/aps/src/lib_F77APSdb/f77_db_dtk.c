#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_dtk.c

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
#pragma ident	"@(#)f77_db_dtk.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_dtk.c"

#include <db_sybint.h>

#include "db_dtk.h"

       
 struct f77_dtk_rec {
       DBFLT8  strtlat ;
       DBFLT8  stoplat ; 
       DBFLT8  nrlat1 ;
       DBFLT8  nrlon1 ;
       DBFLT8  farlat1 ;
       DBFLT8  farlon1 ;
       DBFLT8  nrlat2 ; 
       DBFLT8  nrlon2 ;
       DBFLT8  farlat2 ;
       DBFLT8  farlon2 ;
       DBFLT8  lookangl ;

       DBINT rev ;
       DBINT darid ;
       
       DBTINYINT dtkid ;
       DBCHAR  ascdsc ;

       char sat [3];
       char sensor [4];
       char fadtkid [21];
       char actid [7]; 
       char strttime [22];
       char endtime [22];
       char dtkstat [4];
       char transid [3];
       char sitename [33];
       char notes [41];
       char dtkdate [22];
 
}f77_dtk_rec_;


/*
       To access a dtk record from fortran enter the following
       includes in the fortran code

       INCLUDE 'F77_db_extern.h'   
       INCLUDE 'F77_db_dtk.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_dtk contains the declarations for
   the column names in the dtk table. The names
   are exactly the same as in the database table
*/


void load_f77_dtk_rec(DB_RECORD **dtk_rec) 
{
	f77_dtk_rec_.rev =	
                CAST_DTK_REV                    dtk_rec[DTK_REV] ;

	f77_dtk_rec_.dtkid =	
		CAST_DTK_DTKID                  dtk_rec[DTK_DTKID] ;

	f77_dtk_rec_.darid =
		CAST_DTK_DARID                  dtk_rec[DTK_DARID] ;

	f77_dtk_rec_.strtlat =	
                CAST_DTK_STRTLAT                dtk_rec[DTK_STRTLAT] ;

	f77_dtk_rec_.stoplat =	
                CAST_DTK_STOPLAT                dtk_rec[DTK_STOPLAT] ;

	f77_dtk_rec_.nrlat1 =	
		CAST_DTK_NRLAT1	                dtk_rec[DTK_NRLAT1] ;

        f77_dtk_rec_.nrlon1 = 		
                CAST_DTK_NRLON1                 dtk_rec[DTK_NRLON1] ;

	f77_dtk_rec_.farlat1 = 		
                CAST_DTK_FARLAT1                dtk_rec[DTK_FARLAT1] ;

	f77_dtk_rec_.farlon1 = 	
                CAST_DTK_FARLON1                dtk_rec[DTK_FARLON1] ;

	f77_dtk_rec_.nrlat2 =
		CAST_DTK_NRLAT2                 dtk_rec[DTK_NRLAT2] ;

	f77_dtk_rec_.nrlon2 = 	
		CAST_DTK_NRLON2	                dtk_rec[DTK_NRLON2] ;

	f77_dtk_rec_.farlat2 =		
		CAST_DTK_FARLAT2                dtk_rec[DTK_FARLAT2] ;

	f77_dtk_rec_.farlon2 = 	
                CAST_DTK_FARLON2                dtk_rec[DTK_FARLON2] ;

	f77_dtk_rec_.lookangl = 	
                CAST_DTK_LOOKANGL               dtk_rec[DTK_LOOKANGL] ;
       
        f77_dtk_rec_.ascdsc =
                CAST_DTK_ASCDSC                 dtk_rec[DTK_ASCDSC] ;

        strcpy (f77_dtk_rec_.sat,
                CAST_DTK_SAT                    dtk_rec[DTK_SAT]) ;

        strcpy (f77_dtk_rec_.sensor,
                CAST_DTK_SENSOR                 dtk_rec[DTK_SENSOR]) ;

        strcpy (f77_dtk_rec_.fadtkid,
                CAST_DTK_FADTKID                dtk_rec[DTK_FADTKID]) ;

        strcpy (f77_dtk_rec_.actid,
                CAST_DTK_ACTID                  dtk_rec[DTK_ACTID]) ;

        strcpy (f77_dtk_rec_.strttime,
                CAST_DTK_STRTTIME               dtk_rec[DTK_STRTTIME]) ;

        strcpy (f77_dtk_rec_.endtime,
                CAST_DTK_STOPTIME               dtk_rec[DTK_STOPTIME]) ;

        strcpy (f77_dtk_rec_.dtkstat,
                CAST_DTK_DTKSTAT                dtk_rec[DTK_DTKSTAT]) ;

        strcpy (f77_dtk_rec_.transid,
                CAST_DTK_TRANSID                dtk_rec[DTK_TRANSID]) ;

        strcpy (f77_dtk_rec_.sitename,
                CAST_DTK_SITENAME               dtk_rec[DTK_SITENAME]) ;

        strcpy (f77_dtk_rec_.notes,
                CAST_DTK_NOTES                  dtk_rec[DTK_NOTES]) ;

        strcpy (f77_dtk_rec_.dtkdate,
                CAST_DTK_DTKDATE                dtk_rec[DTK_DTKDATE]) ;

}         




#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_seg.c

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
#pragma ident	"@(#)f77_db_seg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_seg.c"

#include <db_sybint.h>

#include "db_seg.h"

       
 struct f77_seg_rec {
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

       DBINT darid ;
       DBINT rev ;
       
       DBTINYINT segid ;

       DBCHAR  ascdsc ;

       char sat [3];
       char sensor [4];
       char dtkid [16];
       char strttime [22];
       char stoptime [22];
       char segstat [4];
       char segdate [10];
 
}f77_seg_rec_;


/*
       To access a seg record from fortran enter the following
       includes in the fortran code

       INCLUDE 'F77_db_extern.h'   
       INCLUDE 'F77_db_seg.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_seg contains the declarations for
   the column names in the seg table. The names
   are exactly the same as in the database table
*/


void load_f77_seg_rec(DB_RECORD **seg_rec) 
{
	f77_seg_rec_.darid =	
                CAST_SEG_DARID       seg_rec[SEG_DARID] ;

	f77_seg_rec_.rev =	
		CAST_SEG_REV         seg_rec[SEG_REV] ;

	f77_seg_rec_.segid =
		CAST_SEG_SEGID       seg_rec[SEG_SEGID] ;

	f77_seg_rec_.strtlat =	
                CAST_SEG_STRTLAT     seg_rec[SEG_STRTLAT] ;

	f77_seg_rec_.stoplat =	
                CAST_SEG_STOPLAT     seg_rec[SEG_STOPLAT] ;

	f77_seg_rec_.nrlat1 =	
		CAST_SEG_NRLAT1	     seg_rec[SEG_NRLAT1] ;

        f77_seg_rec_.nrlon1 = 		
                CAST_SEG_NRLON1      seg_rec[SEG_NRLON1] ;

	f77_seg_rec_.farlat1 = 		
                CAST_SEG_FARLAT1     seg_rec[SEG_FARLAT1] ;

	f77_seg_rec_.farlon1 = 	
                CAST_SEG_FARLON1     seg_rec[SEG_FARLON1] ;

	f77_seg_rec_.nrlat2 =
		CAST_SEG_NRLAT2      seg_rec[SEG_NRLAT2] ;

	f77_seg_rec_.nrlon2 = 	
		CAST_SEG_NRLON2	     seg_rec[SEG_NRLON2] ;

	f77_seg_rec_.farlat2 =		
		CAST_SEG_FARLAT2     seg_rec[SEG_FARLAT2] ;

	f77_seg_rec_.farlon2 = 	
                CAST_SEG_FARLON2     seg_rec[SEG_FARLON2] ;

	f77_seg_rec_.lookangl = 	
                CAST_SEG_LOOKANGL    seg_rec[SEG_LOOKANGL] ;
       
        f77_seg_rec_.ascdsc =
                CAST_SEG_ASCDSC      seg_rec[SEG_ASCDSC] ;

        strcpy (f77_seg_rec_.sat,
                CAST_SEG_SAT         seg_rec[SEG_SAT]) ;

        strcpy (f77_seg_rec_.sensor,
                CAST_SEG_SENSOR      seg_rec[SEG_SENSOR]) ;

        strcpy (f77_seg_rec_.dtkid,
                CAST_SEG_DTKID       seg_rec[SEG_DTKID]) ;

        strcpy (f77_seg_rec_.strttime,
                CAST_SEG_STRTTIME    seg_rec[SEG_STRTTIME]) ;

        strcpy (f77_seg_rec_.stoptime,
                CAST_SEG_STOPTIME    seg_rec[SEG_STOPTIME]) ;

        strcpy (f77_seg_rec_.segstat,
                CAST_SEG_SEGSTAT     seg_rec[SEG_SEGSTAT]) ;

        strcpy (f77_seg_rec_.segdate,
                CAST_SEG_SEGDATE     seg_rec[SEG_SEGDATE]) ;

}         




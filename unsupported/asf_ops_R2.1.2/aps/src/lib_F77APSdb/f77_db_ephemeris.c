#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_ephemeris.c

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
#pragma ident	"@(#)f77_db_ephemeris.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_ephemeris.c"

#include <db_sybint.h>

#include "db_ephemeris.h"

struct f77_ephemeris_rec {
	DBFLT8	epoch;
        DBINT  startrev;
	DBINT  endrev;	
        DBCHAR	phase_name;
        char filename [100] ;
        char starttime [22] ;
        char endtime [22] ;
        char sat [3] ;
        char sv_filename [100] ;
        char sv_type [10] ;
        char sv_rev [5] ;
        char sv_time [21] ;
        char sv_r_x [11] ;
        char sv_r_y [11] ; 
        char sv_r_z [11] ;
        char sv_v_x [11] ;
        char sv_v_y [11] ;
        char sv_v_z [11] ;

} f77_ephemeris_rec_;


/* To access a ephemeris record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_ephemeris.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_ephemeris contains the declarations for
   the column names in the ephemeris table. The names
   are exactly the same as in the database table
*/


void load_f77_ephemeris_rec(DB_RECORD **ephemeris_rec) 
{
	f77_ephemeris_rec_.startrev =	
                CAST_EPHEMERIS_STARTREV         ephemeris_rec[EPHEMERIS_STARTREV];

        f77_ephemeris_rec_.endrev =	
                CAST_EPHEMERIS_ENDREV           ephemeris_rec[EPHEMERIS_ENDREV] ;

	f77_ephemeris_rec_.epoch =
		CAST_EPHEMERIS_EPOCH 	        ephemeris_rec[EPHEMERIS_EPOCH] ;

	f77_ephemeris_rec_.phase_name =	
		CAST_EPHEMERIS_PHASE_NAME       ephemeris_rec[EPHEMERIS_PHASE_NAME] ;

        strcpy (f77_ephemeris_rec_.filename,
                CAST_EPHEMERIS_FILENAME         ephemeris_rec[EPHEMERIS_FILENAME]) ;

        strcpy (f77_ephemeris_rec_.starttime,
                CAST_EPHEMERIS_STARTTIME        ephemeris_rec[EPHEMERIS_STARTTIME]) ;

        strcpy (f77_ephemeris_rec_.endtime,
                CAST_EPHEMERIS_ENDTIME          ephemeris_rec[EPHEMERIS_ENDTIME]) ;

        strcpy (f77_ephemeris_rec_.sat, 
                CAST_EPHEMERIS_SAT              ephemeris_rec[EPHEMERIS_SAT]) ;

        strcpy (f77_ephemeris_rec_.sv_filename,
                CAST_EPHEMERIS_SV_FILENAME      ephemeris_rec[EPHEMERIS_SV_FILENAME]) ;
        
        strcpy (f77_ephemeris_rec_.sv_type,   
                CAST_EPHEMERIS_SV_TYPE          ephemeris_rec[EPHEMERIS_SV_TYPE]) ;

        strcpy (f77_ephemeris_rec_.sv_rev,
                CAST_EPHEMERIS_SV_REV           ephemeris_rec[EPHEMERIS_SV_REV]) ;

        strcpy (f77_ephemeris_rec_.sv_time,
                CAST_EPHEMERIS_SV_TIME          ephemeris_rec[EPHEMERIS_SV_TIME]) ;

        strcpy (f77_ephemeris_rec_.sv_r_x,
                CAST_EPHEMERIS_SV_R_X           ephemeris_rec[EPHEMERIS_SV_R_X]) ;

        strcpy (f77_ephemeris_rec_.sv_r_y,
                CAST_EPHEMERIS_SV_R_Y          ephemeris_rec[EPHEMERIS_SV_R_Y]) ;

        strcpy (f77_ephemeris_rec_.sv_r_z,
                CAST_EPHEMERIS_SV_R_Z           ephemeris_rec[EPHEMERIS_SV_R_Z]) ;

        strcpy (f77_ephemeris_rec_.sv_v_x,
                CAST_EPHEMERIS_SV_V_X           ephemeris_rec[EPHEMERIS_SV_V_X]) ;

        strcpy (f77_ephemeris_rec_.sv_v_y,
                CAST_EPHEMERIS_SV_V_Y           ephemeris_rec[EPHEMERIS_SV_V_Y]) ;
       
        strcpy (f77_ephemeris_rec_.sv_v_z,
                CAST_EPHEMERIS_SV_V_Z           ephemeris_rec[EPHEMERIS_SV_V_Z]) ;  

}





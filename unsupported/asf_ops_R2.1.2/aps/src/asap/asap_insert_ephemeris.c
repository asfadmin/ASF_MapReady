#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		asap_insert_ephmeris.c

Description:	contains asap_insert_ephmeris() which inserts an 
				ephemeris record.  

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)asap_insert_ephemeris.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/asap/SCCS/s.asap_insert_ephemeris.c"

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */

#include "db_ephemeris.h"       /* for the ephemeris relation.              */


/*==============================================================================

Function:       asap_insert_ephmeris

Description:    checks to see if the ephemeris record already exists in 
				the database, then inserts it.

Creator:    Lawrence Stevens

Creation Date:  Mon Sep 18 16:47:09 PDT 1995

Notes:

==============================================================================*/

int asap_insert_ephemeris(
	DBPROCESS   *APS_dbproc,    /* Sybase db process          			*/
	char		*filename,		/* ephemeris file name.   				*/
	char		*starttime,		/* start time for ephemeris file.   	*/
	char		*endtime,		/* end time for ephemeris file.   		*/
	int			*startrev,		/* start rev number for ephemeris file. */
	int			*endrev,		/* end rev number for ephemeris file.   */
	char		*sat,			/* satellite 							*/
	double		*epoch, 		/* JD start time/epoch for ephemeris file.*/
	char		*phase_name,	
	char		*sv_filename,	/* file which holds nominal SV for asc node.*/
	char		*sv_type,		/* NOMINAL  */
	char		*sv_rev,		/* rev for state vector of asc. node. */
	char		*sv_time,		/* ASF time for state vector of asc. node. */
	char		*sv_r_x,		/* Rx for state vector of asc. node. */
	char		*sv_r_y,		/* Ry for state vector of asc. node. */
	char		*sv_r_z,		/* Rz for state vector of asc. node. */
	char		*sv_v_x,		/* Vx for state vector of asc. node. */
	char		*sv_v_y,		/* Vy for state vector of asc. node. */
	char		*sv_v_z  )  	/* Vz for state vector of asc. node. */
{

	int			nrecs_inserted ;
	DB_RECORD   **ephemeris_rec = NULL ;

#ifdef DEBUG
	printf("%s(%d):  arguments:  \n", __FILE__, __LINE__ ) ;
	printf("%s(%d):  filename=%s, \n", __FILE__, __LINE__, filename ) ;
	printf("%s(%d):  starttime=%s, \n", __FILE__, __LINE__, starttime ) ;
	printf("%s(%d):  endtime=%s, \n", __FILE__, __LINE__, endtime ) ;
	printf("%s(%d):  startrev=%d, \n", __FILE__, __LINE__, *startrev ) ;
	printf("%s(%d):  endrev=%d, \n", __FILE__, __LINE__, *endrev ) ;
	printf("%s(%d):  sat=%s, \n", __FILE__, __LINE__, sat ) ;
	printf("%s(%d):  epoch=%lf, \n", __FILE__, __LINE__, *epoch ) ;
	printf("%s(%d):  phase_name=%s, \n", __FILE__, __LINE__, phase_name ) ;
	printf("%s(%d):  sv_filename=%s, \n", __FILE__, __LINE__, sv_filename ) ;
	printf("%s(%d):  sv_type=%s, \n", __FILE__, __LINE__, sv_type ) ;
	printf("%s(%d):  sv_rev=%s, \n", __FILE__, __LINE__, sv_rev ) ;
	printf("%s(%d):  sv_time=%s, \n", __FILE__, __LINE__, sv_time ) ;
	printf("%s(%d):  sv_r_x=%s, \n", __FILE__, __LINE__, sv_r_x ) ;
	printf("%s(%d):  sv_r_y=%s, \n", __FILE__, __LINE__, sv_r_y ) ;
	printf("%s(%d):  sv_r_z=%s, \n", __FILE__, __LINE__, sv_r_z ) ;
	printf("%s(%d):  sv_v_x=%s, \n", __FILE__, __LINE__, sv_v_x ) ;
	printf("%s(%d):  sv_v_y=%s, \n", __FILE__, __LINE__, sv_v_y ) ;
	printf("%s(%d):  sv_v_z=%s, \n", __FILE__, __LINE__, sv_v_z ) ;
#endif
    /*
    -- Want to append an ephemeris relation DB_RECORD.  First create one,
    -- then copy values into its fields.  Then append it.
    */
	/* 
	-- this creates the memory as well. 
	-- remember to clean this up:  
	*/
    ephemeris_rec =  new_table_record(APS_CDEFS(EPHEMERIS)) ;

	strcpy( CAST_EPHEMERIS_FILENAME ephemeris_rec[EPHEMERIS_FILENAME], 
		filename ) ;
	strcpy(CAST_EPHEMERIS_STARTTIME ephemeris_rec[EPHEMERIS_STARTTIME], 
		starttime ) ;
	strcpy(CAST_EPHEMERIS_ENDTIME ephemeris_rec[EPHEMERIS_ENDTIME], 
		endtime ) ;
	CAST_EPHEMERIS_STARTREV ephemeris_rec[EPHEMERIS_STARTREV] = *startrev ;
	CAST_EPHEMERIS_ENDREV ephemeris_rec[EPHEMERIS_ENDREV] = *endrev ;
	strcpy(CAST_EPHEMERIS_SAT ephemeris_rec[EPHEMERIS_SAT], sat ) ;
	CAST_EPHEMERIS_EPOCH ephemeris_rec[EPHEMERIS_EPOCH] = *epoch ;
	CAST_EPHEMERIS_PHASE_NAME ephemeris_rec[EPHEMERIS_PHASE_NAME] 
		= *phase_name ;
	strcpy(CAST_EPHEMERIS_SV_FILENAME ephemeris_rec[EPHEMERIS_SV_FILENAME],
		sv_filename ) ;
	strcpy(CAST_EPHEMERIS_SV_TYPE ephemeris_rec[EPHEMERIS_SV_TYPE],
		sv_type ) ;
	strcpy(CAST_EPHEMERIS_SV_REV ephemeris_rec[EPHEMERIS_SV_REV],
		sv_rev ) ;
	strcpy(CAST_EPHEMERIS_SV_TIME ephemeris_rec[EPHEMERIS_SV_TIME],
		sv_time ) ;
	strcpy(CAST_EPHEMERIS_SV_R_X ephemeris_rec[EPHEMERIS_SV_R_X],
		sv_r_x ) ;
	strcpy(CAST_EPHEMERIS_SV_R_Y ephemeris_rec[EPHEMERIS_SV_R_Y],
		sv_r_y ) ;
	strcpy(CAST_EPHEMERIS_SV_R_Z ephemeris_rec[EPHEMERIS_SV_R_Z],
		sv_r_z ) ;
	strcpy(CAST_EPHEMERIS_SV_V_X ephemeris_rec[EPHEMERIS_SV_V_X],
		sv_v_x ) ;
	strcpy(CAST_EPHEMERIS_SV_V_Y ephemeris_rec[EPHEMERIS_SV_V_Y],
		sv_v_y ) ;
	strcpy(CAST_EPHEMERIS_SV_V_Z ephemeris_rec[EPHEMERIS_SV_V_Z],
		sv_v_z ) ;

#ifdef DEBUG
	printf("%s(%d):  ephemeris_rec to append = \n", __FILE__, __LINE__ ) ;
	db_print_record( ephemeris_rec, APS_CDEFS(EPHEMERIS) ) ;
#endif

	/* remember to clean this list up:  */
	nrecs_inserted = db_insert_single_record(APS_dbproc,
			ephemeris_rec, APS_TABLE(EPHEMERIS), APS_CDEFS(EPHEMERIS) ) ;

	/* clean up everything:     */
	free_db_record( ephemeris_rec ) ;

	if ( nrecs_inserted != 1 )
		return 1 ;   /* error;  not inserted.  */

	return 0 ;    /* OK  */
}

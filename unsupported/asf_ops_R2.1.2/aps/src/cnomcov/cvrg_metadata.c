#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cvrg_metadata.c

Description:	holds cvrg_metadata() 

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)cvrg_metadata.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cnomcov/SCCS/s.cvrg_metadata.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <libgen.h>		/* for the basename function.  	*/
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_cvrgmdat.h"    /* for the cvrg relation.               */


/*==============================================================================
Function:       cvrg_metadata()

Description:    Write the db record to the cvrgmdat relation which 
				has the run flags and values for a create_nominal_coverage 
				run.  

Parameters:     
   Input Parameters:
   Name         Type    Definition
   APS_dbproc	*DBPROCESS 	pointer to a previously established 
 			Sybase process.  
   NOWTIME      *CH*21  ASF time identifying the time of the run.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   SENSOR       *CH*3    SENSOR NAME, I.E. SAR, OPS, ETC.  
 			use sensor = "*" if the sensor is not specified.
   T1		*ch*21	t1 and t2 delimit the time bracket.  The times
   T2		*ch*21	are NOT included in the time bracket.  
   MASKID	*CH*3   run mask:  ASF or GBL; 
   repflag	*CH*1   replication flag:  Y or N.  Y to use the 
 			ground track repeat cycle to extend  the 
 			input ephemeris file past its end time.  
   filename	*CH*100  name of the ephemeris file.   
 
   Output Parameters:
   marker	*int	this is a serial number identifying the run.  
 			it also appears in each cvrg record for 
 			traceability and deleteability.  

Returns:  	void

Creator:    Lawrence Stevens

Creation Date:  Sun Oct  1 22:38:44 PDT 1995

Notes:		
	This routine is called from Fortran

	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
void cvrg_metadata(
	DBPROCESS 	*APS_dbproc, 
	char 		*NOWTIME, 
	char 		*SAT, 
	char 		*SENSOR, 
	char		*T1,
	char		*T2,
	char 		*MASKID, 
	char 		*repflag,
	char 		*filename,
	int		*marker  )
{

char		nowtime[22];
char		sat[3];
char		sensor[4];
char		t1[22];
char		t2[22];
char		maskid[4];
char		*file_basename;		/* basename of input filename				*/
char		fname[100];			/* file name to go into the cvrgmdat rec. 	*/

DB_RECORD	**cvrgmdat_rec ;
llist		*cvrgmdat_list = NULL ;
cursor		cvrgmdat_list_ptr ;

DBINT		max_marker;
int			n_cvrgmdat;
int			cvrgmdat_found;
int			rcode, system();
RETCODE         return_code;

/* for safety,  copy the Fortran strings locally and supply 
   the string termination.  */

strncpy(nowtime, NOWTIME, 21);
nowtime[21] = '\0';

strncpy(sat, SAT, 2);
sat[2] = '\0';

strncpy(sensor, SENSOR, 3);
sensor[3] = '\0';

strncpy(maskid, MASKID, 3);
maskid[3] = '\0';

strncpy(t1, T1, 21);
t1[21] = '\0';

strncpy(t2, T2, 21);
t2[21] = '\0';

printf("cvrg_metadata:  APS_dbproc = %x, nowtime = %s,\n", APS_dbproc, nowtime );
printf("                sat = %s, sensor = %s,\n", sat, sensor);
printf("                t1 = %s, t2 = %s\n", t1, t2);
printf("                maskid = %s, repflag = %1.1s\n", maskid, repflag);
printf("                filename = \n>%s<\n", filename);

/* 
-- obtain the maximum of the marker value from the cvrgmdat 
-- relation by retrieving them all, sorted by marker.  then get the 
-- last of the record.  there is no max function at the moment. 
-- this is fast, anyway, though.  and not run very often, either.  
*/
sprintf(where_clause, "where %s = '%2.2s' and %s = '%3.3s' ", 
	APS_COL(CVRGMDAT, CVRGMDAT_SAT),    sat, 
	APS_COL(CVRGMDAT, CVRGMDAT_SENSOR), sensor );
printf("cvrg_metadata:  where_clause = \n>%s<\n", where_clause);

sprintf(orderby_cols, " %s ", APS_COL(CVRGMDAT, CVRGMDAT_MARKER) ) ;
printf("cvrg_metadata:  orderby_cols = >%s<\n", orderby_cols);

cvrgmdat_list = db_get_records(APS_dbproc, APS_TABLE(CVRGMDAT),
	where_clause, orderby_cols, APS_CDEFS(CVRGMDAT), ALL_COLS) ;
if (cvrgmdat_list == NULL)
{
	printf("cvrg_metadata:  cvrgmdat query failed.\n");
	printf("cvrg_metadata:  orderby_cols = >%s<\n", orderby_cols);
	printf("cvrg_metadata:  orderby_cols = >%s<\n", orderby_cols);
	printf("              terminating the run.\n");
	rcode = system("banner ERROR");
	exit (1);
}

if (NUMELTS(cvrgmdat_list) <= 0 )
	max_marker = 0 ;
else
{
	cvrgmdat_rec = (DB_RECORD **) LAST(cvrgmdat_list, cvrgmdat_list_ptr) ;
	max_marker = CAST_CVRGMDAT_MARKER cvrgmdat_rec[CVRGMDAT_MARKER] ;
}
/* all done getting the max_marker.  */
DEL_LIST( cvrgmdat_list ) ;

file_basename = basename(filename);
if(strlen(file_basename) < 3)
	strcpy(fname, filename);
else
	strcpy(fname, file_basename);

*marker = max_marker + 1;

/* prepare a DB_RECORD to insert.  start by allocating storage:  */
cvrgmdat_rec =  new_table_record(APS_CDEFS(CVRGMDAT) ) ;

/* now set values into the new record.  */
CAST_CVRGMDAT_MARKER cvrgmdat_rec[CVRGMDAT_MARKER] = *marker ;
strcpy( CAST_CVRGMDAT_SAT cvrgmdat_rec[CVRGMDAT_SAT], sat ) ;
strcpy( CAST_CVRGMDAT_SENSOR cvrgmdat_rec[CVRGMDAT_SENSOR], sensor ) ;
strcpy( CAST_CVRGMDAT_START_TIME cvrgmdat_rec[CVRGMDAT_START_TIME], t1 ) ;
strcpy( CAST_CVRGMDAT_STOP_TIME cvrgmdat_rec[CVRGMDAT_STOP_TIME], t2 ) ;
strcpy( CAST_CVRGMDAT_GEN_TIME cvrgmdat_rec[CVRGMDAT_GEN_TIME], nowtime ) ;
strcpy( CAST_CVRGMDAT_RUNMASK cvrgmdat_rec[CVRGMDAT_RUNMASK], maskid ) ;
CAST_CVRGMDAT_REPFLAG cvrgmdat_rec[CVRGMDAT_REPFLAG] = *repflag ;
strcpy( CAST_CVRGMDAT_VERSION cvrgmdat_rec[CVRGMDAT_VERSION], "2.00" ) ;
strcpy( CAST_CVRGMDAT_EPHEMERIS cvrgmdat_rec[CVRGMDAT_EPHEMERIS], fname ) ;

printf("\ncvrg_metadata:  new record = \n" ) ;
db_print_record(cvrgmdat_rec, APS_CDEFS(CVRGMDAT) ) ;

n_cvrgmdat = db_insert_single_record(APS_dbproc,
	cvrgmdat_rec, APS_TABLE(CVRGMDAT), APS_CDEFS(CVRGMDAT)) ;
if ( n_cvrgmdat != 1 )
{
	printf("cvrg_metadata:  cvrgmdat insert rec failed.\n");
	printf("cvrg_metadata:  new record was supposed to be: \n" ) ;
	db_print_record(cvrgmdat_rec, APS_CDEFS(CVRGMDAT) ) ;
	printf("              terminating the run.\n");
	rcode = system("banner ERROR");
	/* free the memory allocated.  */
	free_db_record( cvrgmdat_rec ) ;
	exit (1);
}
else
	printf ("cvrg_metadata:  %d  row inserted in cvrgmdat. \n", n_cvrgmdat) ;

/* free the memory allocated.  */
free_db_record( cvrgmdat_rec ) ;

printf("cvrg_metadata:  returning:  marker = %d\n", *marker);

}

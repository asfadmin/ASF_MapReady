#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		delete_cov.c

Description:	has routine delete_cov()

External Functions Defined:
				delete_cov()
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  

==============================================================================*/
#pragma ident	"@(#)delete_cov.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cnomcov/SCCS/s.delete_cov.c"



/*==============================================================================
Function:       delete_cov()

Description:    GIVEN A SAT, sensor, rev bracket and time bracket, 
		this routine deletes coverage static data, which is data 
		from the cvrg relation.  
 		the cvrg relation contains sensor swath location data 

Parameters:
  Input Parameters:
  Name         Type    Definition
  APS_dbproc	*DBPROCESS 	pointer to a previously established 
			Sybase process.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   SENSOR       *CH*3    SENSOR NAME, I.E. SAR, OPS, ETC.  
 			use sensor = "*" if the sensor is not specified.
   rev1		*int	rev1 and rev2 delimit the rev bracket.  The revs 
   rev2         *int	rev1 and rev2 are included in the bracket.  
   t1		*double	t1 and t2 delimit the time bracket.  The times
   t2		*double	are NOT included in the time bracket.  
   STATIONID	*CH*3    station mask:  ASF or McMurdo; 
 
   Output Parameters:
   n_cvrg	*int	the  number of records deleted from the cvrg
 			relation.  

Creator:        Lawrence Stevens

Creation Date:  Sun Oct  1 21:38:20 PDT 1995

Notes:		
	This file was written with a 4-character tab setting.  
 	That this routine is called from Fortran
==============================================================================*/
#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>

#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */

#include "db_cvrg.h"   		/* for the cvrg relation.              */
 
void delete_cov(
	DBPROCESS 	*APS_dbproc, 
	char 		*SAT, 
	char 		*SENSOR, 
	int			*rev1,
	int			*rev2,
	double		*t1,
	double		*t2,
	char 		*STATIONID, 
	DBINT 		*n_cvrg      )
{

DBCHAR		sat[3];
DBCHAR		sensor[4];
DBCHAR		stationid[4];

RETCODE         return_code;

char 	buf2[200];	/* for SQL statements.			*/

/* initialize output record counts in case of an early return. */
*n_cvrg = 0;

/* for safety,  copy the Fortran strings locally and supply 
   the string termination.  */

strncpy(sat, SAT, 2);
sat[2] = '\0';

strncpy(sensor, SENSOR, 3);
sensor[3] = '\0';

printf("delete_cov:  STATIONID = %3.3s\n", STATIONID);
strncpy(stationid, STATIONID, 3);
stationid[3] = '\0';
printf("delete_cov:  stationid = %s\n", stationid);

/* 
-- if stationid == * then make stationid equal to a 
-- null string.  which means to delete from all stations.  
*/
if (!strncmp(stationid,"*", 1))
	strcpy(stationid,"");
printf("delete_cov:  stationid = %s\n", stationid);

printf("delete_cov:  APS_dbproc = %x, sat = %s, sensor = %s,\n",
	APS_dbproc, sat, sensor);
printf("             rev1 = %d, rev2 = %d, \n", *rev1, *rev2);
printf("             t1 = %20.9f, t2 = %20.9f\n", *t1, *t2);
printf("             stationid = %s\n", stationid);

sprintf(where_clause, "where %s = '%2.2s' and %s = '%3.3s' and \
	%s >= %d and %s <= %d and \
	%s > %20.9f and %s < %20.9f", 
	APS_COL(CVRG, CVRG_SAT), sat,    APS_COL(CVRG, CVRG_SENSOR), sensor, 
	APS_COL(CVRG, CVRG_REV), *rev1,  APS_COL(CVRG, CVRG_REV), *rev2, 
	APS_COL(CVRG, CVRG_MJDATE), *t1, APS_COL(CVRG, CVRG_MJDATE), *t2 );

printf("delete_cov:  where_clause = >%s<\n", where_clause);

*n_cvrg = db_delete_records( APS_dbproc, APS_TABLE(CVRG),  where_clause ) ;
printf ("delete_cov:  %d  rows deleted from cvrg. \n", *n_cvrg);

printf("delete_cov:  returning:  n_cvrg = %d \n", *n_cvrg );

}

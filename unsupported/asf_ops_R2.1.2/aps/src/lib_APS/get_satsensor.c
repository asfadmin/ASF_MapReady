#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Function:       get_satsensor()

Description:    obtains sensor beam pointing data from the satsensor 
				relation.  
				GIVEN A SAT AND SENSOR code, gets satsensor data.
Parameters:

   Input Parameters:
   *APS_dbproc    DBPROCESS  process structure address for DBLIB.
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   SENSOR   	*CH*3    SENSOR NAME, I.E., SAR, OPS, SR1...

   Output Parameters:
   MODE		*int	satellite orientation mode.  0 = yaw steering
 						     1 = center pointing
   CMODE	*ch*15	satellite orientation mode.  'YAW STEERING   '
 						  or 'CENTER POINTING'
   BRAD		*real*8	sensor beam radius, degrees.
   LOOK		*real*8	look angle, degrees, from the downward vertical.  
 			> 0 means right looking sensor, < 0 means a 
 			left looking sensor.  
   ERRNO	*int	error number.  0 = OK.
 
Creator:        Lawrence Stevens

Creation Date:  Tue Sep 26 15:55:38 PDT 1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)get_satsensor.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.get_satsensor.c"


#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

#include "db_sybint.h"    /* for APS sybase interface routines.  includes
							dapps_list.h  */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_satsensor.h"   /* for satsensor table                  */

 
void get_satsensor(
	DBPROCESS   *APS_dbproc,
	char        *SAT,
	char        *SENSOR,
	int         *MODE,
	char		*CMODE,
	double		*BRAD,
	double		*LOOK ,
	int			*ERRNO	)
{

char	sat[3];
char	sensor[4];
int		nrecs;

RETCODE	return_code;

DB_RECORD       **satsensor_rec ;
llist           *satsensor_list = NULL ;
cursor          satsensor_list_ptr ;


/*initialize parameters in case of an early return:  */
*MODE = -1;
strcpy(CMODE, "query failed");
*BRAD = -1.0;
*LOOK = 0.0;

*ERRNO = 0;

/* for safety,  copy the Fortran strings locally and supply
   the string termination.  */
 
strncpy(sat, SAT, 2);
sat[2] = '\0';
 
strncpy(sensor, SENSOR, 3);
sensor[3] = '\0';
 
printf("get_satsensor:  APS_dbproc = %x, sat = %s, sensor = %s\n", 
	APS_dbproc, sat, sensor);

sprintf(where_clause, "where %s = '%2.2s' and %s = '%3.3s'", 
	APS_COL(SATSENSOR, SATSENSOR_SAT), sat,
	APS_COL(SATSENSOR, SATSENSOR_SENSOR), sensor ) ;

satsensor_list = db_get_records( APS_dbproc, APS_TABLE(SATSENSOR),
	where_clause, NULL, APS_CDEFS(SATSENSOR), ALL_COLS) ;
if ( satsensor_list == NULL)
{
	printf("DB query failed.\n" ) ;
	*ERRNO = -1;
	return ;
}
if ( NUMELTS( satsensor_list ) == 0 )
{
	printf("get_satsensor:  no records found in satsensor relation for\n");
	printf("                sat = >%s< sensor = >%s<\n", sat, sensor);
	printf("                there must be one record.\n"); 
	printf("		where_clause used:  \n>%s<\n", where_clause);

	/* clean up.  */
	DEL_LIST( satsensor_list ) ;
	*ERRNO = 1;
	return;
}
if ( NUMELTS( satsensor_list ) != 1 )
{
	nrecs = NUMELTS( satsensor_list ) ;
	printf("get_satsensor:  %d records found in satsensor relation for\n", 
		nrecs ) ;
	printf("        sat = >%s< sensor = >%s<\n", sat, sensor);
	printf("        there must be one record.\n"); 
	printf("		where_clause used:  \n>%s<\n", where_clause);

	/* clean up.  */
	DEL_LIST( satsensor_list ) ;
	*ERRNO = 1;
	return;
}

satsensor_rec = FIRST( satsensor_list, satsensor_list_ptr ) ;

*MODE = CAST_SATSENSOR_OPERMODE satsensor_rec[SATSENSOR_OPERMODE] ;
strcpy(CMODE, CAST_SATSENSOR_CHOPERMODE satsensor_rec[SATSENSOR_CHOPERMODE] );
*BRAD = CAST_SATSENSOR_BEAMRADIUSD satsensor_rec[SATSENSOR_BEAMRADIUSD] ;
*LOOK = CAST_SATSENSOR_LOOKANGLED satsensor_rec[SATSENSOR_LOOKANGLED] ;

/* clean up.  */
DEL_LIST( satsensor_list ) ;
*ERRNO = 0;

return;

}

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#define PRINT_DIAG
#undef PRINT_DIAG

/********************************************************************
*  Name:        insert_sscvrg
*  Module Type: SUBROUTINE      Language: C
*  Purpose:     GIVEN A SAT, sensor, time, and sensor swath info, etc,
*		and other data, insert a sscvrg record 
*		into the data base 
*
*	note that this routine can be called from Fortran; all the arguments
*	are pointers.  
*
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  APS_dbproc	*DBPROCESS 	pointer to a previously established 
*			Sybase process.  
*  darid	*int	this is a serial number identifying the dar.  
*			which this record observes.  
*  sitename     *CH*33  SITE NAME of area being observed.  this would not 
*			be a dar; it might be a hypothetical site.  in this 
*			case, the darid = 0;
*  sat          *CH*2    SATELLITE NAME:  E1, J1, OR RS
*  sensor       *CH*3    SENSOR NAME, I.E. SAR, OPS, ETC.  
*			use sensor = "*" if the sensor is not specified.
*  rev		*int	rev number of the data
*  strttime	*char[22] start time of the observation.  
*  stoptime	*char[22] stop time of the observation.  
*  strtet	*double	start time in real julian days. 
*  stopet	*double	stop time in real julian days. 
*  strtlat	*double	subsat latitude at time strtet.
*  stoplat	*double	subsat latitude at time stopet.
*  nrlat1	*double	swath near point latitude and longitude at time mjdate.
*  nrlon1	*double	
*  farlat1	*double	swath far point latitude and longitude at time mjdate.
*  farlon1	*double	
*  nrlat2	*double	swath near point latitude and longitude at time mjdate.
*  nrlon2	*double	
*  farlat2	*double	swath far point latitude and longitude at time mjdate.
*  farlon2	*double	
*  ascdsc	*char1	A if ascending, D if descending.  The satellite is 
*			considered ascending if the progress of its 
*			sub-satellite point is northward.  
*  Output Parameters:
*  nrecs	*int	the number of records appended.  normally 1.
*
****************************************************************************/
#pragma ident	"@(#)insert_sscvrg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.insert_sscvrg.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
 
#include "db_sscvrg.h"       /* for the sscvrg relation.              */
  
void insert_sscvrg(
	DBPROCESS 	*APS_dbproc, 
	int			*darid,
	char 		*sitename, 
	char 		*sat, 
	char 		*sensor, 
	int			*rev,
	char		*strttime,
	char		*stoptime,
	double		*strtet,
	double		*stopet,
	double		*strtlat,
	double		*stoplat,
	double 		*nrlat1,
	double 		*nrlon1,
	double 		*farlat1,
	double 		*farlon1,
	double 		*nrlat2,
	double 		*nrlon2,
	double 		*farlat2,
	double 		*farlon2,
	char		*ascdsc,

	int			*nrecs	)
{

DB_RECORD   **sscvrg_rec = NULL ;
llist       *sscvrg_list = NULL ;

int			rcode ;

*nrecs = 0;

#ifdef PRINT_DIAG
printf("insert_sscvrg:  APS_dbproc = %x, darid = %d,\n", APS_dbproc, *darid);
printf("              sitename = %.32s\n", sitename);
printf("              sat = %.2s, sensor = %.3s, rev = %d,\n",sat,sensor, *rev);
printf("              strttime = %.21s, stoptime = %.21s\n", strttime,stoptime);
printf("              strtet = %.9f, stopet = %.9f \n", *strtet, *stopet); 
printf("              strtlat = %.3f, stoplat = %.3f \n", *strtlat, *stoplat); 
printf("              nrlat1/nrlon1 = %7.3f/%8.3f,\n", *nrlat1, *nrlon1); 
printf("              farlat1/farlon1 = %7.3f/%8.3f,\n", *farlat1, *farlon1); 
printf("              nrlat2/nrlon2 = %7.3f/%8.3f,\n", *nrlat2, *nrlon2); 
printf("              farlat2/farlon2 = %7.3f/%8.3f,\n", *farlat2, *farlon2); 
printf("              ascdsc = %.1s\n", ascdsc); 
#endif 

/*
	-- Want to append an sscvrg relation DB_RECORD.  First create one,
	-- then copy values into its fields.  Then append it.
*/

/*
-- create a new DB_RECORD, with memory.  
-- remember to free all of the data.  
*/

sscvrg_rec =  new_table_record(APS_CDEFS(SSCVRG)) ;
CAST_SSCVRG_DARID sscvrg_rec[SSCVRG_DARID] = *darid ;
strcpy( CAST_SSCVRG_SITENAME sscvrg_rec[SSCVRG_SITENAME], sitename ) ;
strcpy( CAST_SSCVRG_SAT sscvrg_rec[SSCVRG_SAT], sat ) ;
strcpy( CAST_SSCVRG_SENSOR sscvrg_rec[SSCVRG_SENSOR], sensor ) ;
CAST_SSCVRG_REV sscvrg_rec[SSCVRG_REV] = *rev ;
strcpy( CAST_SSCVRG_STRTTIME sscvrg_rec[SSCVRG_STRTTIME], strttime ) ;
strcpy( CAST_SSCVRG_STOPTIME sscvrg_rec[SSCVRG_STOPTIME], stoptime ) ;
CAST_SSCVRG_STRTET sscvrg_rec[SSCVRG_STRTET] = *strtet ;
CAST_SSCVRG_STOPET sscvrg_rec[SSCVRG_STOPET] = *stopet ;
CAST_SSCVRG_STRTLAT sscvrg_rec[SSCVRG_STRTLAT] = *strtlat ;
CAST_SSCVRG_STOPLAT sscvrg_rec[SSCVRG_STOPLAT] = *stoplat ;
CAST_SSCVRG_NRLAT1 sscvrg_rec[SSCVRG_NRLAT1] = *nrlat1 ;
CAST_SSCVRG_NRLON1 sscvrg_rec[SSCVRG_NRLON1] = *nrlon1 ;
CAST_SSCVRG_FARLAT1 sscvrg_rec[SSCVRG_FARLAT1] = *farlat1 ;
CAST_SSCVRG_FARLON1 sscvrg_rec[SSCVRG_FARLON1] = *farlon1 ;
CAST_SSCVRG_NRLAT2 sscvrg_rec[SSCVRG_NRLAT2] = *nrlat2 ;
CAST_SSCVRG_NRLON2 sscvrg_rec[SSCVRG_NRLON2] = *nrlon2 ;
CAST_SSCVRG_FARLAT2 sscvrg_rec[SSCVRG_FARLAT2] = *farlat2 ;
CAST_SSCVRG_FARLON2 sscvrg_rec[SSCVRG_FARLON2] = *farlon2 ;
CAST_SSCVRG_ASCDSC sscvrg_rec[SSCVRG_ASCDSC] = *ascdsc ;

printf("%s(%d):  inserting sscvrg record = \n\t%s %s %ld  %s   %s  %c\n", 
	__FILE__, __LINE__, 
	CAST_SSCVRG_SAT sscvrg_rec[SSCVRG_SAT], 
	CAST_SSCVRG_SENSOR sscvrg_rec[SSCVRG_SENSOR], 
	CAST_SSCVRG_REV sscvrg_rec[SSCVRG_REV], 
	CAST_SSCVRG_STRTTIME sscvrg_rec[SSCVRG_STRTTIME], 
	CAST_SSCVRG_STOPTIME sscvrg_rec[SSCVRG_STOPTIME],
	CAST_SSCVRG_ASCDSC sscvrg_rec[SSCVRG_ASCDSC] ) ;

*nrecs = db_insert_single_record(APS_dbproc,
	sscvrg_rec, APS_TABLE(SSCVRG), APS_CDEFS(SSCVRG) ) ;

/* now clean up everything:     */
free_db_record( sscvrg_rec ) ;
 
if(*nrecs != 1)
{
	printf("\n\ninsert_sscvrg:  INSERT FAILED for sscvrg relation.\n");
	printf("              check to see that the relation exists.\n");
	printf("              Also check to see that there is only  \n");
	printf("              one run for this satellite going at a time.\n");
	printf("              satellite = %s\n", sat);
	printf("              Now terminating this run.\n");
	rcode = system("banner ERROR");
	exit(1);
}

#ifdef PRINT_DIAG
printf("insert_sscvrg:  returning:   nrecs = %d\n", *nrecs);
#endif

return ;

}

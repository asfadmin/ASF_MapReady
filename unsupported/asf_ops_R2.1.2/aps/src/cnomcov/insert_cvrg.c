#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		insert_cvrg.c

Description:	holds the routine insert_cvrg()

External Functions Defined:
				insert_cvrg()
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)insert_cvrg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cnomcov/SCCS/s.insert_cvrg.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
#include <stdlib.h>

/* FOR SYBASE INTERFACES  */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_cvrg.h"        /* for the cvrg relation.              */


/*==============================================================================
Function:       insert_cvrg()

Description:    GIVEN A SAT, sensor, time, and sensor swath info, etc,
				and other coverage data, insert a cvrg record 
				into the data base 

Parameters:     
   Input Parameters:
   Name         Type    Definition
   APS_dbproc	*DBPROCESS 	pointer to a previously established 
 			Sybase process.  
   marker	*int	this is a serial number identifying the run.  
   sat          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   sensor       *CH*3    SENSOR NAME, I.E. SAR, OPS, ETC.  
 			use sensor = "*" if the sensor is not specified.
   mjdate	*real*8	time of the data, real Julian days ephemeris time.
   rev		*int	rev number of the data
   sublat	*double	subsat latitude and longitude at time mjdate.
   sublon	*double	
   nrlat	*double	swath near point latitude and longitude at time mjdate.
   nrlon	*double	
   farlat	*double	swath far point latitude and longitude at time mjdate.
   farlon	*double	
   satalt	*double	altitude of the satellite above the surface of the
 			earth
   sunflag		*char1	S if the satellite is sunlit; - if not.  
   sunang	*double	solar zenith angle at subsat point. [0.0 - 180.0]
   station_id*char3	ASF if the satellite is in the ASF station mask.
                    MCM if the satellite is in the McMurdo station mask.
                    --- if the satellite is in neither station mask.
   masks	*char4	not surrently used. 
   opermode	*int	not used
   crossflag	*char1	N if ascending node, D if descending node.  
   ascdsc	*char1	A if ascending, D if descending.  The satellite is 
 			considered ascending if the progress of its 
 			sub-satellite point is northward.  
   Output Parameters:
   nrecs	*int	the number of records appended.  normally 1.
 			the run must terminate if the insert fails.

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Mon Oct  2 10:35:56 PDT 1995

Notes:		
 	This routine is called from Fortran
	It is called for every minute of a time bracket for a coverage run, 
	so we want it to be fast.  

	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
void insert_cvrg(
	DBPROCESS 	*APS_dbproc, 
	int			*marker,
	char 		*sat, 
	char 		*sensor, 
	double		*mjdate,
	int			*rev,
	double		*sublat,
	double 		*sublon,
	double 		*nrlat,
	double 		*nrlon,
	double 		*farlat,
	double 		*farlon,
	double		*satalt,
	char		*sunflag,
	double		*sunang,
	char		*station_id,
	char		*masks,
	int			*opermode,
	char		*crossflag,
	char		*ascdsc,
	int			*nrecs	)
{

RETCODE         return_code;
DB_RECORD       **cvrg_rec ;
int				rcode, system();

*nrecs = 0;

#ifdef DEBUG
printf("insert_cvrg:  APS_dbproc = %x, marker = %d,\n", APS_dbproc, *marker);
printf("              sat = %s, sensor = %s, mjdate = %.9f, rev = %d,\n", 
	sat, sensor, *mjdate, *rev);
printf("              sublat/sublon = %7.3f/%8.3f,\n", *sublat, *sublon); 
printf("              nrlat/nrlon = %7.3f/%8.3f,\n", *nrlat, *nrlon); 
printf("              farlat/farlon = %7.3f/%8.3f,\n", *farlat, *farlon); 
printf("              satalt = %.3f, sunflag = %1.1s, sunang = %f,\n", 
	*satalt, sunflag, *sunang); 
printf("              station_id = %3.3s, masks = %4.4s, opermode = %d,\n",
	station_id, masks, *opermode ); 
printf("              crossflag = %1.1s, ascdsc = %1.1s\n", crossflag, ascdsc); 
#endif

/* create a DB_RECORD, put in the values, then call db_insert_single_record.  */
cvrg_rec =  new_table_record(APS_CDEFS(CVRG)) ;

CAST_CVRG_MARKER cvrg_rec[CVRG_MARKER] =  *marker ;
strcpy( CAST_CVRG_SAT cvrg_rec[CVRG_SAT],    sat ) ;
strcpy( CAST_CVRG_SENSOR cvrg_rec[CVRG_SENSOR], sensor ) ; 
CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] =  *mjdate ; 
CAST_CVRG_REV cvrg_rec[CVRG_REV] =     *rev ; 
CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] =  *sublat ; 
CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] =  *sublon ; 
CAST_CVRG_NRLAT cvrg_rec[CVRG_NRLAT] =   *nrlat ; 
CAST_CVRG_NRLON cvrg_rec[CVRG_NRLON] =   *nrlon ;
CAST_CVRG_FARLAT cvrg_rec[CVRG_FARLAT] =  *farlat ;
CAST_CVRG_FARLON cvrg_rec[CVRG_FARLON] =  *farlon ;
CAST_CVRG_SATALT cvrg_rec[CVRG_SATALT] =  *satalt ;
CAST_CVRG_SUN cvrg_rec[CVRG_SUN] =     *sunflag ;
CAST_CVRG_SUNANG cvrg_rec[CVRG_SUNANG] =  *sunang ;
strcpy( CAST_CVRG_STATION_ID cvrg_rec[CVRG_STATION_ID], station_id ) ;
strcpy( CAST_CVRG_MASKS cvrg_rec[CVRG_MASKS],      masks ) ;
CAST_CVRG_OPERMODE cvrg_rec[CVRG_OPERMODE] = *opermode ;
CAST_CVRG_CROSSFLAG cvrg_rec[CVRG_CROSSFLAG] = *crossflag ;
CAST_CVRG_ASCDSC cvrg_rec[CVRG_ASCDSC] =    *ascdsc ;

#ifdef DEBUG
printf("\ninsert_cvrg:  record to insert = \n" ) ;
db_print_record(cvrg_rec, APS_CDEFS(CVRG) ) ;
#endif

*nrecs = db_insert_single_record(APS_dbproc,
	cvrg_rec, APS_TABLE(CVRG), APS_CDEFS(CVRG) ) ;

/* free the DB_RECORD we created */
free_db_record(cvrg_rec) ;

if( *nrecs != 1 )
{
	printf ("\n\ninsert_cvrg:  nrecs inserted = %d.  should be 1 \n", *nrecs);
	printf("insert_cvrg:  INSERT FAILED for cvrg relation.\n");
	printf("              check to see that the relation exists.\n");
	printf("              Also check to see that there is only  \n");
	printf("              one run for this satellite going at a time.\n");
	printf("              satellite = %s\n", sat);
	printf("              Now terminating this run.\n");
	rcode = system("banner ERROR");
	exit(1);
}

#ifdef DEBUG
printf("insert_cvrg:  returning:   nrecs = %d\n", *nrecs);
#endif

return ;

}

#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	get_cvg.c

Description:	for an input sat, sensor, time, it computes sensor coverage 
				points, using interpolation from the cvrg relation.  

External Functions Defined:	
get_cvg()
	
File Scope Functions:
ret_cvgpts()

	
External Variables Defined:
	
File Scope Variables:
	
Notes:
*
* Written by Craig K. Fujimoto
* Ported to  unix/Sybase by Larry Stevens
*

==============================================================================*/
#pragma ident	"@(#)get_cvg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.get_cvg.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
 
#include <stdlib.h>

#include "timeconv.h"
#include "dapps_defs.h"

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
 
#include "db_cvrg.h"       /* for the cvrg relation.              */

/*  these are Fortran routines; we have to add a '_' to the end of their
    names when calling them.  					*/
extern	ll2xyz_(double*, double*);
extern	xyz2ll_(double*, double*);
extern	gctrvf_(double*, double*, double*, double*);


RETCODE	return_code;	/* return code for Sybase calls.	*/


/*==============================================================================
Function:       get_cvg

Description:    given an ASF format time, determine the coverage information

Creator:        Lawrence Stevens

Creation Date:  Thu Sep 21 17:35:24 PDT 1995

Notes:		
==============================================================================*/

int get_cvg(
	DBPROCESS	*APS_dbproc,	/* Sybase process id.  	*/
	char 		*sat,		/* input satellite.  	*/
	char		*sen,		/* input sensor     	*/
	int 		rev,		/* input rev number		*/
	char 		*asft,		/* input asf time for desired cvrg points.  */
	float 		*sublat,	/* sublat point from cvrg relation, interpolated. */
	float		*nrlat,		/* nrlat point from cvrg relation, interpolated. */
	float		*nrlon,		/* nrlon point from cvrg relation, interpolated. */
	float		*frlat,		/* frlat point from cvrg relation, interpolated. */
	float		*frlon,		/* frlon point from cvrg relation, interpolated. */
	char 		*ascdsc   )	/* ascdsc point, interpolated, could be S if match*/
{
	extern int cvrg_allowed(char *sat, char *sensor ) ;

	double et,time[2];
	double subl[2][2],nrl[2][2],frl[2][2];
	double sxyz[2][3],nxyz[2][3],fxyz[2][3];
	double spt[3],npt[3],fpt[3];
	double newsl[2],newnl[2],newfl[2];

	double frac;
	char ad[2];
	int return_code;

	#ifdef PRINT_DIAG
	   printf("get_cvg sat=%s\n", sat);
	   printf("get_cvg sen=%s\n", sen);
	   printf("get_cvg rev=%d\n", rev);	
	#endif 

	/*
	-- check to see if nominal coverage is allowed for this sat/sensor.  
	-- if not, then return "non-values" for the fields.  
	*/

	return_code = cvrg_allowed( sat, sen ) ; 
	if (return_code == 1 )
	{
		/* 
		-- coverage is NOT allowed.  set values 
		-- AND RETURN.   
		*/
		*sublat = 0 ;
		*nrlat = 0 ;
		*nrlon = 0 ;
		*frlat = 0 ;
		*frlon = 0 ;
		strcpy(ascdsc, "-" ) ;
		return 0;
	}
	if (return_code != 0) 
		 return return_code;
	/* 
	-- coverage IS allowed.  set values 
	*/

	/* Convert the asf format time to ephemeris time */
	return_code = tc_asf2et(asft,&et);

	/* Get the adjacent coverage points */
	return_code = ret_cvgpts(APS_dbproc,
		sat,sen,rev,et,&time,&subl,&nrl,&frl,&ad);
	if (return_code != 0) 
		 return return_code;

	/*
	-- if the adjacent coverage points retrieved are the same 
	-- point, then just copy the points to the appropriate 
	-- place.  
	-- the criterion is the time delta is < .9 milliseconds.  
	*/

	/* the times are in day units.  */
	if ( (time[1] - time[0]) < (.9 / 24.0 / 60.0 / 60.0 / 1000.0) )
	{
		/* 
		-- Write the lats/lons of the first cvrg rec 
		-- to the output variables 
		*/
		*sublat = subl[0][0] ;
		*nrlat  = nrl[0][0] ;
		*nrlon  = nrl[0][1] ;
		*frlat  = frl[0][0] ;
		*frlon  = frl[0][1] ;
	}
	else
	{
		/* Convert the lats/lons of the first cvrg rec to xyz points */
		/* Fortran calls	*/
		ll2xyz_(subl[0],sxyz[0]);
		ll2xyz_(nrl[0],nxyz[0]);
		ll2xyz_(frl[0],fxyz[0]);

		/* Convert the lats/lons of the second cvrg rec to xyz points */
		/* Fortran calls	*/
		ll2xyz_(subl[1],sxyz[1]);
		ll2xyz_(nrl[1],nxyz[1]);
		ll2xyz_(frl[1],fxyz[1]);

		/* Calc the fraction of time */
		frac = (et - time[0]) / (time[1] - time[0]);
				
		/* Determine the xyz coords of the new point inbetween */
		/* use gctrvf, a Fortran routine...	*/
		/* Subsat point */
		gctrvf_(spt,sxyz[0],sxyz[1],&frac);
		/* Near point */
		gctrvf_(npt,nxyz[0],nxyz[1],&frac);
		/* Far point */
		gctrvf_(fpt,fxyz[0],fxyz[1],&frac);

		/* Convert the xyz coords back to lats/lons */
		/* Fortran routines	*/
		xyz2ll_(spt,newsl);
		xyz2ll_(npt,newnl);
		xyz2ll_(fpt,newfl);

		/* Set the output variables */
		*sublat = newsl[0];

		*nrlat = newnl[0];
		*nrlon = newnl[1];

		*frlat = newfl[0];
		*frlon = newfl[1];

	}

	strcpy(ascdsc,ad);

	return 0;

}


/*****************************************************************
* Name: ret_cvgpts
*
* Purpose
*	return the cvrg points before and after the input ephemeris time.
*
* Input
*	sat		c*	satellite
*	sen		c*	sensor
*	rev		i	rev
*	et		d	ephemeris time to find cvrg for
*
* Output
*	time		d	ephemeris time of cvrg recs found
*	subl,nrl,frl	d	l/l points of cvrg recs
*	ad		c*	ascending/descending flag
*
* Internal
*	cov		struct	coverage structure
*	onemin		d	one minute
*	return_code		i	return status
*	rcount,errno	i	Ingres return status
*	erev		i	rev number from e structure - fix
*
*****************************************************************/
int ret_cvgpts(
	DBPROCESS	*APS_dbproc,	/* sybase process  		*/
	char 		*sat,		/* input satellite 		*/
	char 		*sen,		/* input sensor 		*/
	int 		rev,		/* input rev number 	*/
	double 		et,			/* input time 			*/

	/* 
	-- what follows are 2 element arrays of data
	-- the first element of each array comes from 
	-- the cvrg record just before the input time; the 
	-- second element comes from the first cvrg record 
	-- after the input time.  
	-- NOTE:  they could be the SAME point if there is a 
	-- cvrg record within .9 milliseconds of the input 
	-- time.  
	*/
	double 		*time,	/* times of the cvrg points  						*/
	double 		*subl,	/* subsat lat&lon of the 2 cvrg points:  4 values 	*/
	double		*nrl,	/* near lat&lon of the 2 cvrg points.  4 values.  	*/
	double		*frl,	/* far lat&lon of the 2 cvrg points.  4 values.  	*/

	char 		*ad   )		/* Ascending / Decending flag:  A/D  */
{

llist      *cvrg_list = NULL ;
cursor     cvrg_list_ptr ;
DB_RECORD  **cvrg_rec ;	/* rec from cvrg relation.  */


char 	buf[500];	/* buffer used mainly for Sybase commands. */
struct {
     DBFLT8 time;
     DBFLT8 sublat,sublon;
     DBFLT8 nrlat,nrlon;
     DBFLT8 frlat,frlon;
     DBCHAR ascdsc[2];
} cov[2];

DBFLT8 cvrg_time;
DBFLT8 sublat,sublon;
DBFLT8 nrlat,nrlon;
DBFLT8 frlat,frlon;
DBCHAR ascdsc1;
DBCHAR ascdsc2;

double onemin;
double one_tinytime ;

int return_code;
int rcount,errno;

#ifdef PRINT_DIAG
char	asftime[ASF_TIME_STR_LENGTH+1] ;
#endif
/*  
## range of cv is #cvrg
 */

#ifdef PRINT_DIAG
printf("ret_cvgpts sat=%s\n",sat);
printf("ret_cvgpts sen=%s\n",sen);
printf("ret_cvgpts rev=%d\n",rev);
#endif 

/* this is 1.5 minutes.  try to get a cvrg point within this time:  */
onemin = 1.0 / 24.0 / 60.0 * 1.5;

/* 
-- this is something used to be sure to pick up an 
-- equality in double precision when we retrieve.  
-- I had a query which should have picked up a record 
-- with the same time and didn't - maybe because of 
-- double precision roundoff or something.  
-- make this smaller than our resolution ( .001 seconds)
-- make it .9 milliseconds in units of days:  
-- if the equality is picked up, then this routine would
-- return 2 coverage recs identical.  
*/
one_tinytime = .9 / 24.0 / 60.0 / 60.0 / 1000.0 ;

sprintf(where_clause, "where %s = '%.2s' and %s = '%.3s' and \
	%s <= %d and %s >= %d and  \
	%s <= %.10f and %s >= %.10f ",
	APS_COL(CVRG, CVRG_SAT), sat,       APS_COL(CVRG, CVRG_SENSOR), sen, 
	APS_COL(CVRG, CVRG_REV), rev,       APS_COL(CVRG, CVRG_REV), (int)(rev-1), 
	APS_COL(CVRG, CVRG_MJDATE), (double)(et + one_tinytime), 
	APS_COL(CVRG, CVRG_MJDATE), (double)(et - onemin ) ) ;

sprintf(orderby_cols, "%s, %s ", 
	APS_COL(CVRG, CVRG_REV) ,
	APS_COL(CVRG, CVRG_MJDATE) ) ;

#ifdef PRINT_DIAG
printf("get_cvg:  where_clause = \n%s\n", where_clause);
printf("get_cvg:  orderby_cols = \n%s\n", orderby_cols);
#endif 

cvrg_list = db_get_records(APS_dbproc, APS_TABLE(CVRG),
	where_clause, orderby_cols, APS_CDEFS(CVRG), ALL_COLS) ;
if (cvrg_list == NULL)
{
#ifdef PRINT_DIAG
	printf("DB query failed.\n" ) ;
#endif
	return -1 ;
}

rcount = NUMELTS( cvrg_list ) ;
if (rcount == 0 ) 
{
#ifdef PRINT_DIAG
	printf("get_cvg.c:  Error: No CVRG records found. \n");
#endif 
	DEL_LIST( cvrg_list ) ;
    return 1;
}

/* 
-- the query just got recs before the desired time.  
-- get the LAST record.  
*/
cvrg_rec = (DB_RECORD **) LAST(cvrg_list, cvrg_list_ptr) ;

/* assign the first set ( cov[0]. ) of data elements:  */

cov[0].time = CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] ;
cov[0].sublat = CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] ; 
cov[0].sublon =  CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] ;
cov[0].nrlat =  CAST_CVRG_NRLAT cvrg_rec[CVRG_NRLAT] ;
cov[0].nrlon =  CAST_CVRG_NRLON cvrg_rec[CVRG_NRLON] ;
cov[0].frlat =  CAST_CVRG_FARLAT cvrg_rec[CVRG_FARLAT] ;
cov[0].frlon =  CAST_CVRG_FARLON cvrg_rec[CVRG_FARLON] ;
ascdsc1 = CAST_CVRG_ASCDSC cvrg_rec[CVRG_ASCDSC] ;

#ifdef PRINT_DIAG
   printf("FIRST POINT:\n" ) ;
   printf("et  : %20.10f\n",et);
   tc_et2asf( et, asftime) ;
   printf("asft  : %s\n",asftime) ;
   printf("cov[0].time : %20.10f\n",cov[0].time);
   tc_et2asf( cov[0].time, asftime) ;
   printf("asft  : %s\n",asftime) ;
   printf("sublat  : %f\n",cov[0].sublat) ;
   printf("ascdsc  : %c\n",ascdsc1) ;
   printf("dif1 : %12.10f\n",et-cov[0].time);
   printf("one.five minutes : %12.10f\n\n",onemin);
#endif 

/* finished with the list and records.  */
DEL_LIST( cvrg_list ) ;

/* Find the coverage record just AFTER the input et time */

sprintf(where_clause, "where %s = '%.2s' and %s = '%.3s' and \
	%s >= %d and %s <= %d and  \
	%s >= %.10f and %s <= %.10f ",
	APS_COL(CVRG, CVRG_SAT), sat,       APS_COL(CVRG, CVRG_SENSOR), sen, 
	APS_COL(CVRG, CVRG_REV), rev,       APS_COL(CVRG, CVRG_REV), (int)(rev+1), 
	APS_COL(CVRG, CVRG_MJDATE), (double)(et - one_tinytime ), 
	APS_COL(CVRG, CVRG_MJDATE), (double)(et + onemin ) ) ;

sprintf(orderby_cols, "%s, %s ", 
	APS_COL(CVRG, CVRG_REV) ,
	APS_COL(CVRG, CVRG_MJDATE) ) ;

#ifdef PRINT_DIAG
printf("get_cvg:  where_clause = \n%s\n", where_clause);
printf("get_cvg:  orderby_cols = \n%s\n", orderby_cols);
#endif 

cvrg_list = db_get_records(APS_dbproc, APS_TABLE(CVRG),
	where_clause, orderby_cols, APS_CDEFS(CVRG), ALL_COLS) ;
if (cvrg_list == NULL)
{
#ifdef PRINT_DIAG
	printf("DB query failed.\n" ) ;
#endif
	return -1 ;
}

rcount = NUMELTS( cvrg_list ) ;
if (rcount == 0 ) 
{
#ifdef PRINT_DIAG
	printf("get_cvg.c:  Error: No CVRG records found. \n");
#endif 
	DEL_LIST( cvrg_list ) ;
    return 1;
}

/* 
-- the query just got recs AFTER the desired time.  
-- get the FIRST record.  
*/
cvrg_rec = (DB_RECORD **) FIRST(cvrg_list, cvrg_list_ptr) ;

/* assign the second set ( cov[1] ) of data elements:  */

cov[1].time = CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] ;
cov[1].sublat = CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] ; 
cov[1].sublon =  CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] ;
cov[1].nrlat =  CAST_CVRG_NRLAT cvrg_rec[CVRG_NRLAT] ;
cov[1].nrlon =  CAST_CVRG_NRLON cvrg_rec[CVRG_NRLON] ;
cov[1].frlat =  CAST_CVRG_FARLAT cvrg_rec[CVRG_FARLAT] ;
cov[1].frlon =  CAST_CVRG_FARLON cvrg_rec[CVRG_FARLON] ;
ascdsc2 = CAST_CVRG_ASCDSC cvrg_rec[CVRG_ASCDSC] ;

/* free the memory:  */
DEL_LIST( cvrg_list ) ;

#ifdef PRINT_DIAG
   printf("SECOND POINT:\n" ) ;
   printf("et  : %20.10f\n",et);
   tc_et2asf( et, asftime) ;
   printf("asft  : %s\n",asftime) ;
   printf("cov[1].time : %20.10f\n",cov[1].time);
   tc_et2asf( cov[1].time, asftime) ;
   printf("asft  : %s\n",asftime) ;
   printf("ascdsc : %c\n",ascdsc2) ;
   printf("sublat : %f\n",cov[1].sublat) ;
   printf("dif2 : %12.10f\n",cov[1].time-et);
   printf("one : %12.10f\n\n",onemin);
#endif 

/* Assign the values to the correct variables */
*(time+0) = cov[0].time;
*(time+1) = cov[1].time;

*(subl+0) = cov[0].sublat;
*(subl+1) = cov[0].sublon;
*(subl+2) = cov[1].sublat;
*(subl+3) = cov[1].sublon;

*(nrl+0) = cov[0].nrlat;
*(nrl+1) = cov[0].nrlon;
*(nrl+2) = cov[1].nrlat;
*(nrl+3) = cov[1].nrlon;

*(frl+0) = cov[0].frlat;
*(frl+1) = cov[0].frlon;
*(frl+2) = cov[1].frlat;
*(frl+3) = cov[1].frlon;

/* the value from cvrg of cvrg.ascdsc can be A ascending, D descending, */
/* or S stationary (between ascending and descending) 					*/
/* but the status of the the desired point between them must be A or D.	*/
/* since one of the two coverage points must be A or D, we choose it    */
/* to describe the condition between it and the stationary point:  		*/

if( ascdsc1 == 'A' | ascdsc1 == 'D' )
{
	*ad = ascdsc1 ;
}
else
{
	*ad = ascdsc2 ;
}

/*  the return ad is a string; terminate with a null:  */

*(ad+1) = '\0' ;

return 0;

}

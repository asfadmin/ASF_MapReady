#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*============================================================================
Filename:
 
Description:
 
External Functions Defined:
   
File Scope Functions:
   
External Variables Defined:
   
File Scope Variables:
 
Notes:
 
==============================================================================
*/
#pragma ident   "@(#)get_ncvg.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.get_ncvg.c"

/*****************************************************************
* Name: get_ncvg.qc
*
* $Logfile:   ACS003:[BLD.MPS.SCHD.SRC]GET_NCVG.QCV  $
*
* Purpose
*	retrieve a group of coverage points from the database
*
* Written by Lawrence Stevens, after Craig K. Fujimoto
*
* Functions
*	get_ncvg
*	ret_ncvgpts
*
* $Date$ $Revision$ $Author$
*
*****************************************************************/
#include <stdio.h>

#include "db_sybint.h"
#include "db_sybextern.h"

#include "aps_db_table.h"
#include "db_cvrg.h"
#include "aps_extern.h"

#include "mps.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include "timeconv.h"

extern ll2xyz_ (double *nr, double *nxyz) ;
extern xyz2ll_ (double *pxyz, double *pll) ;
extern gctrvf_ (double *q, double *p1, double *p2, double *f) ;

/*****************************************************************
*
* Name: get_ncvg
*
* Purpose
*	given an ephemeris time, determine the coverage information
*
* Input
*	scov		c12	name of a temp table like cvrg
*	sat		c*	satellite id
*	sensor		c*	sensor id
*	rev		i	rev number
*	asft1		c	ASF time for start of time period.  
*	asft2		c	ASF time for end of time period.  
*	npts		i	number of points of coverage, including the 
*				start time, evenly spaced intermediate times,
*				and the end time.  
* Output
*	nrlats, nrlons	f	arrays for near lats and lons
*	frlats, frlons	f	arrays for far lats and lons
* Internal
*	et1, et2	d	ephemeris times of start and stop times.
*	subl, nrl, frl	d	l/l points for cvrg recs
*	sxyz, nxyz, fxyz	d	xyz points for cvrg recs
*	spt, npt, fpt	d	xyz for new cvrg point
*	newnl, newfl	d	l/l points for new cvrg point
*      	frac		d	fraction of time from first to target
*	stat		i	return status
*
*****************************************************************/
int get_ncvg(
	char *sat, char *sen, int rev, 
	char *asft1, char *asft2,  int npts, 
	float *nrlats, float *nrlons, 
	float *frlats, float *frlons)
{

	double et1, et2, et[60], time[2], mjdate1, mjdate2 ;
	double nrl[2][2], frl[2][2] ;
	double sxyz[2][3], nxyz[2][3], fxyz[2][3] ;
	double npt[3], fpt[3] ;
	double newnl[2], newfl[2] ;

	double frac ;
	char ad[2], ans[81];
	int stat, j ;

	llist *cvrgs ;
	cursor ptr ;

#ifdef DEBUG
	printf("get_ncvg sat=%s\n", sat) ;
	printf("get_ncvg sen=%s\n", sen) ;
	printf("get_ncvg rev=%d\n", rev) ;	
#endif

	/* Convert the asf format time to ephemeris time */
	stat = tc_asf2et(asft1, &et1) ;
	stat = tc_asf2et(asft2, &et2) ;

	/* compute all the times.  */
	et[1] = et1 ;
	for ( j = 2 ; j < npts ; ++j )
	{
		et[j] = et1 + (j - 1) * ( et2 - et1) / (npts - 1) ;
	}
	et[npts] = et2 ;

	/*
	-- compute the cvrg retrieve time bracket as the given 
	-- times expanded by 2 minutes to be sure to retrieve all 
	-- relevant records. 
	*/

	mjdate1 = et1 - ( 2.1/60.0)/24.0 ;
	mjdate2 = et2 + ( 2.1/60.0)/24.0 ;

#ifdef EMBEDDED_QUEL
/* fill the temp relation with the relevant coverage.  */
## delete scov
## append to scov (cvrg.all) where  cvrg.#sat = sat 
##				and cvrg.#sensor = sen
##				and cvrg.#rev >= rev - 1
##				and cvrg.#rev <= rev + 1
##				and cvrg.#mjdate > mjdate1 
##				and cvrg.#mjdate < mjdate2
#endif

	sprintf(where_clause, 
		"where %s = '%s' and %s = '%s' \n\
		and %s >= %d - 1 \n\
		and %s <= %d + 1 \n\
		and %s > %f \n\
		and %s < %f",
			APS_COL(CVRG, CVRG_SAT), sat, APS_COL(CVRG, CVRG_SENSOR), sen, 
			APS_COL(CVRG, CVRG_REV), rev,
			APS_COL(CVRG, CVRG_REV), rev,
			APS_COL(CVRG, CVRG_MJDATE), mjdate1, 
			APS_COL(CVRG, CVRG_MJDATE), mjdate2) ;

	cvrgs = db_get_records(APS_dbproc, APS_TABLE(CVRG), 
		where_clause, NULL, APS_CDEFS(CVRG), ALL_COLS) ;

	for (j=1 ; j <= npts ; ++j)
	{
		/* Get the adjacent coverage points */
		stat = ret_ncvgpts(cvrgs, sat, sen, rev, 
			et[j], &time, &nrl, &frl) ;

		if (stat != 0) 
			return stat;

		/* Convert the lats/lons of the first cvrg rec to xyz points */
		ll2xyz_(nrl[0], nxyz[0]);
		ll2xyz_(frl[0], fxyz[0]);

		/* Convert the lats/lons of the second cvrg rec to xyz points */
		ll2xyz_(nrl[1], nxyz[1]);
		ll2xyz_(frl[1], fxyz[1]);

		/* Calc the fraction of time */
		frac = (et[j] - time[0]) / (time[1] - time[0]);
            
		/* Determine the xyz coords of the new point inbetween */
		/* Near point */
		gctrvf_(npt, nxyz[0], nxyz[1], &frac);
		/* Far point */
		gctrvf_(fpt, fxyz[0], fxyz[1], &frac);

		/* Convert the xyz coords back to lats/lons */
		xyz2ll_(npt, newnl);
		xyz2ll_(fpt, newfl);

		/* Set the output variables */
		*(nrlats+j) = newnl[0];	
		*(nrlons+j) = newnl[1];

		*(frlats+j) = newfl[0];
		*(frlons+j) = newfl[1];

   	}	/* end of the for loop.  */
	return 0;
}


/*****************************************************************
* Name: ret_ncvgpts
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
*	subl, nrl, frl	d	l/l points of cvrg recs
*	ad		c*	ascending/descending flag
*
* Internal
*	cov		struct	coverage structure
*	onemin		d	one minute
*	stat		i	return status
*	rcount,  errno	i	Ingres return status
*	erev		i	rev number from e structure - fix
*
*****************************************************************/
int ret_ncvgpts(llist *cvrgs, char *sat, char *sen, int rev, 
	double et, double *time, double *nrl, double *frl)
{
	struct {
		double time ;
		double sublat, sublon ;
		double nrlat, nrlon ;
		double frlat, frlon ;
		char ascdsc[2] ;
	} cov[2] ;

	DB_RECORD **cvrg_rec ;
	cursor ptr ;

	char *cvrg_sat ;
	char *cvrg_sensor ;

	double onemin ;
	int stat ;

#ifdef DEBUG
   printf("ret_ncvgpts sat=%s\n", sat) ;
   printf("ret_ncvgpts sen=%s\n", sen) ;
   printf("ret_ncvgpts rev=%d\n\n", rev) ;	
#endif

   onemin = 1.0 / 24.0 / 60.0 * 1.5 ;

/* Find the coverage record just before the input et time */
#ifdef EMBEDDED_QUEL
## retrieve (cov[0].time   = cv.#mjdate,
##           cov[0].sublat = cv.#sublat, cov[0].sublon = cv.#sublon,
##           cov[0].nrlat  = cv.#nrlat,  cov[0].nrlon  = cv.#nrlon,
##           cov[0].frlat  = cv.#farlat, cov[0].frlon  = cv.#farlon,
##           cov[0].ascdsc = cv.#ascdsc)
##   where cv.#sat = sat and cv.#sensor = sen
##     and ( cv.#rev = rev OR cv.#rev = rev - 1 )
##     and cv.#mjdate <= et  
##     and cv.#mjdate >= et  - 0.01
## {
## }
## inquire_equel(errno=errorno, rcount=rowcount)
#endif

	cvrg_rec = FIRST(cvrgs, ptr) ;
	while (cvrg_rec)
	{

		cvrg_sat = (char *) CAST_CVRG_SAT cvrg_rec[CVRG_SAT] ; 
		cvrg_sensor = (char *) CAST_CVRG_SENSOR cvrg_rec[CVRG_SENSOR] ; 

		if ((strcmp(cvrg_sat, sat) == 0)
		&& (strcmp(cvrg_sensor, sen) == 0)
		&& (CAST_CVRG_REV cvrg_rec[CVRG_REV] == rev || CAST_CVRG_REV cvrg_rec[CVRG_REV] - 1)
		&& (CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] <= et)
		&& (CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] >= et - 0.01))
			break ;

		cvrg_rec = NEXT(cvrgs, ptr) ;
	}
	if (!cvrg_rec)  /* no match found */
	{
		printf("No CVRG records found\n") ;
		return 1 ;
	}
	
	cov[0].time = CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] ;
	cov[0].sublat = CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] ;
	cov[0].sublon = CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] ;

	cov[0].nrlat = CAST_CVRG_NRLAT cvrg_rec[CVRG_NRLAT] ;
	cov[0].nrlon = CAST_CVRG_NRLON cvrg_rec[CVRG_NRLON] ;

	cov[0].frlat = CAST_CVRG_FARLAT cvrg_rec[CVRG_FARLAT] ;
	cov[0].frlon = CAST_CVRG_FARLON cvrg_rec[CVRG_FARLON] ;

#ifdef DEBUG
   printf("et  : %20.10f\n", et) ;
   printf("cov1 : %20.10f\n", cov[0].time) ;
   printf("dif1 : %12.10f\n", et-cov[0].time) ;
   printf("one : %12.10f\n\n", onemin) ;	
#endif


/* Find the coverage record just after the input et time */

#ifdef EMBEDDED_QUEL
## retrieve (cov[1].time = cv.#mjdate,
##           cov[1].sublat = cv.#sublat, cov[1].sublon = cv.#sublon,
##           cov[1].nrlat  = cv.#nrlat,  cov[1].nrlon  = cv.#nrlon,
##           cov[1].frlat  = cv.#farlat, cov[1].frlon  = cv.#farlon,
##           cov[1].ascdsc = cv.#ascdsc)
##   where cv.#sat = sat and cv.#sensor = sen
##     and ( cv.#rev = rev OR cv.#rev = rev + 1 )
##     and cv.#mjdate > et  
##     and cv.#mjdate < et + 0.01
## {
##	endretrieve
## }
## inquire_equel(errno=errorno, rcount=rowcount)
#endif

	cvrg_rec = FIRST(cvrgs, ptr) ;
	while (cvrg_rec)
	{
		cvrg_sat = (char *) CAST_CVRG_SAT cvrg_rec[CVRG_SAT] ; 
		cvrg_sensor = (char *) CAST_CVRG_SENSOR cvrg_rec[CVRG_SENSOR] ; 

		if ((strcmp(cvrg_sat, sat) == 0)
		&& (strcmp(cvrg_sensor, sen) == 0)
		&& (CAST_CVRG_REV cvrg_rec[CVRG_REV] == rev || CAST_CVRG_REV cvrg_rec[CVRG_REV] + 1)
		&& (CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] > et)
		&& (CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] < et + 0.01))
			break ;

		cvrg_rec = NEXT(cvrgs, ptr) ;
	}

	if (!cvrg_rec)  /* no match found */
	{
		printf("No CVRG records found\n") ;
		return -1 ;
	}
	
	cov[1].time = CAST_CVRG_MJDATE cvrg_rec[CVRG_MJDATE] ;
	cov[1].sublat = CAST_CVRG_SUBLAT cvrg_rec[CVRG_SUBLAT] ;
	cov[1].sublon = CAST_CVRG_SUBLON cvrg_rec[CVRG_SUBLON] ;

	cov[1].nrlat = CAST_CVRG_NRLAT cvrg_rec[CVRG_NRLAT] ;
	cov[1].nrlon = CAST_CVRG_NRLON cvrg_rec[CVRG_NRLON] ;

	cov[1].frlat = CAST_CVRG_FARLAT cvrg_rec[CVRG_FARLAT] ;
	cov[1].frlon = CAST_CVRG_FARLON cvrg_rec[CVRG_FARLON] ;

#ifdef DEBUG
   printf("et  : %20.10f\n", et) ;
   printf("cov2 : %20.10f\n", cov[1].time) ;
   printf("dif2 : %12.10f\n", cov[1].time-et) ;
   printf("one : %12.10f\n\n", onemin) ;
#endif

	/* Assign the values to the correct variables */
	*(time+0) = cov[0].time ;
	*(time+1) = cov[1].time ;

	*(nrl+0) = cov[0].nrlat ;
	*(nrl+1) = cov[0].nrlon ;
	*(nrl+2) = cov[1].nrlat ;
	*(nrl+3) = cov[1].nrlon ;

	*(frl+0) = cov[0].frlat ;
	*(frl+1) = cov[0].frlon ;
	*(frl+2) = cov[1].frlat ;
	*(frl+3) = cov[1].frlon ;

	return 0 ;
}

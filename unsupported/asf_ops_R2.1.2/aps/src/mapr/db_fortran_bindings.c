#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		db_fortran_bindings.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)db_fortran_bindings.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.db_fortran_bindings.c"

#include <stdio.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#include <dapps_list.h>
#include <db_sybint.h>
#include <db_cvrg.h>
#include <db_dar.h>
#include <db_dtk.h>
#include <db_seg.h>
#include <db_satsensor.h>
#include <db_site.h>
#include <db_sscvrg.h>


void
get_satsensor_rec(
	DB_RECORD 	**rec,
	char 	sat[2],
	char 	sensor[3],
	int	*opermode,
	char 	chopermode[15],
        float	*beamradiusd,
        float	*lookangled)
{
        if (! rec)
                return;
        strncpy(sat, CAST_SATSENSOR_SAT          rec[SATSENSOR_SAT], 2);
        strncpy(sensor, CAST_SATSENSOR_SENSOR    rec[SATSENSOR_SENSOR], 3);
        *opermode     = CAST_SATSENSOR_OPERMODE  rec[SATSENSOR_OPERMODE];
        strncpy(chopermode, CAST_SATSENSOR_CHOPERMODE rec[SATSENSOR_CHOPERMODE], 15);
        *beamradiusd  = CAST_SATSENSOR_BEAMRADIUSD   rec[SATSENSOR_BEAMRADIUSD];
        *lookangled   = CAST_SATSENSOR_LOOKANGLED   rec[SATSENSOR_LOOKANGLED];
}

void
get_dtk_rec(
	DB_RECORD 	**rec,
	char 	sat[2],
	char 	sensor[3],
	int	*revs,
	int	*dtkids,
        float	nrlat[2],
        float	nrlon[2],
        float	farlat[2],
        float	farlon[2],
        char    starttime[22],
        char    stoptime[22],
	char	*ascdsc
	)
{
	if (! rec)
		return;

	*revs	 	= CAST_DTK_REV 	   rec[DTK_REV];
	*dtkids	 	= CAST_DTK_DTKID   rec[DTK_DTKID];
	strncpy(sat, rec[DTK_SAT], 2);
	strncpy(sensor, rec[DTK_SENSOR], 3);
	nrlat[0] 	= CAST_DTK_NRLAT1   rec[DTK_NRLAT1];
	nrlat[1] 	= CAST_DTK_NRLAT2   rec[DTK_NRLAT2];
	nrlon[0] 	= CAST_DTK_NRLON1   rec[DTK_NRLON1];
	nrlon[1] 	= CAST_DTK_NRLON2   rec[DTK_NRLON2];
	farlat[0] 	= CAST_DTK_FARLAT1   rec[DTK_FARLAT1];
	farlat[1] 	= CAST_DTK_FARLAT2   rec[DTK_FARLAT2];
	farlon[0] 	= CAST_DTK_FARLON1   rec[DTK_FARLON1];
	farlon[1] 	= CAST_DTK_FARLON2   rec[DTK_FARLON2];
	strncpy(starttime, rec[DTK_STRTTIME], 21);
	strncpy(stoptime, rec[DTK_STOPTIME], 21);
	ascdsc[0] 	= CAST_DTK_ASCDSC   rec[DTK_ASCDSC];
}


void
get_cvrg_rec(
	DB_RECORD	**rec,
	char 	sat[2],
	char 	sensor[3],
	int	*revs,
	double	*mjdate,
        float  *sublat,
        float  *sublon,
        float  *nrlat,
        float  *nrlon,
        float  *farlat,
        float  *farlon,
	char	*ascdsc
	)
{
	if (!rec)
		return;

	*revs	 	= CAST_CVRG_REV		rec[CVRG_REV];
	*mjdate	 	= CAST_CVRG_MJDATE	rec[CVRG_MJDATE];
	strncpy(sat, rec[CVRG_SAT], 2);
	strncpy(sensor, rec[CVRG_SENSOR], 3);
	*sublat		= CAST_CVRG_SUBLAT   rec[CVRG_SUBLAT];
	*sublon		= CAST_CVRG_SUBLON   rec[CVRG_SUBLON];
	*nrlat 		= CAST_CVRG_NRLAT   rec[CVRG_NRLAT];
	*nrlon 		= CAST_CVRG_NRLON   rec[CVRG_NRLON];
	*farlat 	= CAST_CVRG_FARLAT   rec[CVRG_FARLAT];
	*farlon 	= CAST_CVRG_FARLON   rec[CVRG_FARLON];
	ascdsc[0] 	= CAST_CVRG_ASCDSC   rec[CVRG_ASCDSC];
}


void
get_site_rec(
	DB_RECORD	**rec,
	char 	shape[1],
	float	*radius,
        float  *nwlat,
        float  *nwlon,
        float  *nelat,
        float  *nelon,
        float  *selat,
        float  *selon,
        float  *swlat,
        float  *swlon
	)
{
	if (!rec)
		return;

	shape[0] 	= CAST_SITE_SHAPE    rec[SITE_SHAPE];
	*radius	 	= CAST_SITE_RADIUS rec[SITE_RADIUS];
	*nwlat 	= CAST_SITE_NWLAT   rec[SITE_NWLAT];
	*nwlon 	= CAST_SITE_NWLON   rec[SITE_NWLON];
	*nelat 	= CAST_SITE_NWLAT   rec[SITE_NELAT];
	*nelon 	= CAST_SITE_NWLON   rec[SITE_NELON];
	*selat 	= CAST_SITE_NWLAT   rec[SITE_SELAT];
	*selon 	= CAST_SITE_NWLON   rec[SITE_SELON];
	*swlat 	= CAST_SITE_NWLAT   rec[SITE_SWLAT];
	*swlon 	= CAST_SITE_NWLON   rec[SITE_SWLON];
}


void
get_dar_rec(
	DB_RECORD	**rec,
	char 	shape[1],
	float	*radius,
        float  *nwlat,
        float  *nwlon,
        float  *nelat,
        float  *nelon,
        float  *selat,
        float  *selon,
        float  *swlat,
        float  *swlon
	)
{
	if (!rec)
		return;

	shape[0] 	= CAST_DAR_SHAPE    rec[DAR_SHAPE];
	*radius	 	= CAST_DAR_RADIUS rec[DAR_RADIUS];
	*nwlat 	= CAST_DAR_NWLAT   rec[DAR_NWLAT];
	*nwlon 	= CAST_DAR_NWLON   rec[DAR_NWLON];
	*nelat 	= CAST_DAR_NWLAT   rec[DAR_NELAT];
	*nelon 	= CAST_DAR_NWLON   rec[DAR_NELON];
	*selat 	= CAST_DAR_NWLAT   rec[DAR_SELAT];
	*selon 	= CAST_DAR_NWLON   rec[DAR_SELON];
	*swlat 	= CAST_DAR_NWLAT   rec[DAR_SWLAT];
	*swlon 	= CAST_DAR_NWLON   rec[DAR_SWLON];
}

void
get_seg_rec(
	DB_RECORD	**rec,
	char 	*sat,
	char 	*sensor,
	int	*rev,
	int	*darid,
	int	*segid,
	char	*starttime,
	char	*stoptime,
        float  nrlat[2],
        float  nrlon[2],
        float  farlat[2],
        float  farlon[2],
	char	*ascdsc
	)
{
	if (!rec)
		return;

        *rev            = CAST_SEG_REV     rec[SEG_REV];
        *darid          = CAST_SEG_DARID   rec[SEG_DARID];
        *segid          = CAST_SEG_SEGID   rec[SEG_SEGID];
        strncpy(sat, rec[SEG_SAT], 2);
        strncpy(sensor, rec[SEG_SENSOR], 3);
        nrlat[0]        = CAST_SEG_NRLAT1   rec[SEG_NRLAT1];
        nrlat[1]        = CAST_SEG_NRLAT2   rec[SEG_NRLAT2];
        nrlon[0]        = CAST_SEG_NRLON1   rec[SEG_NRLON1];
        nrlon[1]        = CAST_SEG_NRLON2   rec[SEG_NRLON2];
        farlat[0]       = CAST_SEG_FARLAT1   rec[SEG_FARLAT1];
        farlat[1]       = CAST_SEG_FARLAT2   rec[SEG_FARLAT2];
        farlon[0]       = CAST_SEG_FARLON1   rec[SEG_FARLON1];
        farlon[1]       = CAST_SEG_FARLON2   rec[SEG_FARLON2];
        ascdsc[0]       = CAST_SEG_ASCDSC    rec[SEG_ASCDSC];
        strncpy(starttime, rec[SEG_STRTTIME], 21);
        strncpy(stoptime, rec[SEG_STOPTIME], 21);

}


void
get_sscvrg_rec(
	DB_RECORD	**rec,
	char* 	sat,
	char* 	sensor,
	int	*rev,
	char* 	starttime,
	char* 	stoptime,
	double	*strtet,
	double	*stopet,
	float	*strtlat,
	float	*stoplat,
        float  nrlat[2],
        float  nrlon[2],
        float  farlat[2],
        float  farlon[2],
	char	*ascdsc
	)
{
	if (!rec)
		return;

	strncpy(sat, rec[SSCVRG_SAT], 2);
	strncpy(sensor, rec[SSCVRG_SENSOR], 3);
	strncpy(starttime, rec[SSCVRG_STRTTIME], 21);
	strncpy(stoptime, rec[SSCVRG_STOPTIME], 21);
        *rev            = CAST_SSCVRG_REV      rec[SSCVRG_REV];
        *strtet         = CAST_SSCVRG_STRTET   rec[SSCVRG_STRTET];
        *stopet         = CAST_SSCVRG_STOPET   rec[SSCVRG_STOPET];
        *strtlat        = CAST_SSCVRG_STRTLAT   rec[SSCVRG_STRTLAT];
        *stoplat        = CAST_SSCVRG_STOPLAT   rec[SSCVRG_STOPLAT];
        nrlat[0]        = CAST_SSCVRG_NRLAT1   rec[SSCVRG_NRLAT1];
        nrlat[1]        = CAST_SSCVRG_NRLAT2   rec[SSCVRG_NRLAT2];
        nrlon[0]        = CAST_SSCVRG_NRLON1   rec[SSCVRG_NRLON1];
        nrlon[1]        = CAST_SSCVRG_NRLON2   rec[SSCVRG_NRLON2];
        farlat[0]       = CAST_SSCVRG_FARLAT1   rec[SSCVRG_FARLAT1];
        farlat[1]       = CAST_SSCVRG_FARLAT2   rec[SSCVRG_FARLAT2];
        farlon[0]       = CAST_SSCVRG_FARLON1   rec[SSCVRG_FARLON1];
        farlon[1]       = CAST_SSCVRG_FARLON2   rec[SSCVRG_FARLON2];
        ascdsc[0]       = CAST_SSCVRG_ASCDSC    rec[SSCVRG_ASCDSC];

}

get_no_elements(list, number_elements)
struct 	llist *list;
int	*number_elements;
{
	if (list)
		*number_elements = NUMELTS(list);
	else
		*number_elements = 0;
}

#include "asf.h"
#include <unistd.h>

#include "ddr.h"
#include "geolocate.h"
#include "asf_meta.h"
#include "minimize.h"
#include "offset.h"

typedef struct {
	double lat,lon;
} latlon;

#define N_LOCPTS 100
typedef struct {
	char *ceos;
	struct DDR *ddr;
	meta_parameters *meta;
	double offT;
	double offX;
	int nPts;
	double x[N_LOCPTS],y[N_LOCPTS];
	double elev[N_LOCPTS];
	latlon geo[N_LOCPTS];
	latlon est[N_LOCPTS];
} imgLocRec;

const double deg2rad=3.14159265358979/180.0;

double diffLatLon(latlon a,latlon b)
{
	return ((a.lat-b.lat)*(a.lat-b.lat)+
	
	cos(a.lat*deg2rad)*(a.lon-b.lon)*(a.lon-b.lon));
}

double errorVsLoc(imgLocRec *loc)
{
	double totalErr=0;
	int i;
	for (i=0;i<loc->nPts;i++)
		totalErr+=diffLatLon(loc->geo[i],loc->est[i]);
	return totalErr;
}

double errorVsTimeAz(double t,imgLocRec *loc)
{
	int i;
	double saved_timeOffset=loc->meta->geo->timeShift;
	double saved_slant=loc->meta->geo->slantShift;
	loc->meta->geo->timeShift=t;
	loc->meta->geo->slantShift=loc->offX;
	for (i=0;i<loc->nPts;i++)
	{
		int y=(int)loc->y[i],x=(int)loc->x[i];
		meta_get_orig((void *)loc->ddr,y,x,&y,&x);
		meta_get_latLon(loc->meta,y,x,loc->elev[i],
				&loc->est[i].lat,&loc->est[i].lon);
	}
	loc->meta->geo->timeShift=saved_timeOffset;
	loc->meta->geo->slantShift=saved_slant;
	return errorVsLoc(loc);
}

double errorVsTimeRng(double x,imgLocRec *loc)
{
	int i;
	double saved_timeOffset=loc->meta->geo->timeShift;
	double saved_slant=loc->meta->geo->slantShift;
	loc->meta->geo->timeShift=loc->offT;
	loc->meta->geo->slantShift=x;
	for (i=0;i<loc->nPts;i++)
	{
		int y=(int)loc->y[i],x=(int)loc->x[i];
		meta_get_orig((void *)loc->ddr,y,x,&y,&x);
		meta_get_latLon(loc->meta,y,x,loc->elev[i],
				&loc->est[i].lat,&loc->est[i].lon);
	}
	loc->meta->geo->timeShift=saved_timeOffset;
	loc->meta->geo->slantShift=saved_slant;
	return errorVsLoc(loc);
}

void printPosError(imgLocRec *loc)
{
	double er=6356754.9000;/*Earth radius, at poles. (m)*/
	int i;
	for (i=0;i<loc->nPts;i++)
	{
		double lat=deg2rad*loc->geo[i].lat;
		double dLat=deg2rad*(loc->est[i].lat-loc->geo[i].lat),
			dLon=deg2rad*(loc->est[i].lon-loc->geo[i].lon);
	
		printf("Positional Error: %f m north, %f m east\n",
			dLat*er,
			dLon*er*cos(lat));
	}
}
/*****************************************************
Offset:
	External entry points.  These pass around
a (void *) so our imgLocRec never sees the outside
world.
*/

void *init_offset(char *ceos,meta_parameters *meta,struct DDR *ddr)
{
	imgLocRec *loc=(imgLocRec *)MALLOC(sizeof(imgLocRec));
	loc->meta=meta;
	loc->ceos=ceos;
	loc->ddr=ddr;
	loc->nPts=0;
	return (void *)loc;
}

void add_offset(void *l,double x,double y,
	double lat,double lon,double elev)
{
	imgLocRec *loc=(imgLocRec *)l;
	loc->x[loc->nPts]=x;
	loc->y[loc->nPts]=y;
	loc->elev[loc->nPts]=elev;
	loc->geo[loc->nPts].lat=lat;
	loc->geo[loc->nPts].lon=lon;
	loc->nPts++;
}

void refine_offset(void *l,double *out_t,double *out_x)
{
	int i;
	imgLocRec *loc=(imgLocRec *)l;
	
	loc->offT=0.0,loc->offX=0.0;
	errorVsTimeAz(loc->offT,loc);
	errorVsTimeRng(loc->offX,loc);
	printPosError(loc);
	
#ifndef CHECK_OFFSET
	for (i=0;i<20;i++)
	{
		loc->offT = minimize((minFunc)errorVsTimeAz,(void *)loc,loc->offT,0.0001);
		loc->offX = minimize((minFunc)errorVsTimeRng,(void *)loc,loc->offX,1.0);
		printf("Offsets: Time=%f s; Slant Range=%f m\n",loc->offT,loc->offX);
	}
	
	errorVsTimeAz(loc->offT,loc);
	errorVsTimeRng(loc->offX,loc);
	printPosError(loc);
#endif
	*out_t=loc->offT;
	*out_x=loc->offX;
}

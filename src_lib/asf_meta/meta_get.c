/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   These are the external entry points
for general routines.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independance.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"

#ifndef SQR
# define SQR(x) ((x)*(x))
#endif

/*General Calls:*/
/* Convert a line, sample pair to a time, slant-range pair.
These only use the geo_parameters structure, and only work 
for SR and GR images.  They apply the time and slant range 
correction fudge factors. Returns seconds and meters.
*/
double meta_get_time(meta_parameters *sar,double yLine, double xSample)
{
	if (sar->geo->type=='S'||
		sar->geo->type=='G')/*Slant or ground range-- easy.*/
		return yLine*sar->geo->azPixTime+sar->geo->timeShift;
	else if (sar->geo->type=='P')
	{
		double time,slant;
		meta_get_timeSlantDop(sar,yLine,xSample,&time,&slant,NULL);
		return time+sar->geo->timeShift;
	}
	else /*Unknown projection type.*/
	{
		printf("Error!  Unknown projection type '%c' passed to meta_get_slant!\n",
			sar->geo->type);
		exit(1);
	}
	return 0.0;/*<- for whining compilers.*/
}
double meta_get_slant(meta_parameters *sar,double yLine, double xSample)
{
	if (sar->geo->type=='S')/*Slant range is easy.*/
		return sar->geo->slantFirst+xSample*sar->geo->xPix+sar->geo->slantShift;
	else if (sar->geo->type=='G')/*Ground range is tougher.*/
	{/*We figure out the ground angle, phi, for this pixel, then
	use that and the law of cosines to find the slant range.*/
		double ht=sar->ifm->ht,er=sar->ifm->er;
		double minPhi=acos((SQR(ht)+SQR(er)-SQR(sar->geo->slantFirst))/
			(2.0*ht*er));
		double phi=minPhi+xSample*(sar->geo->xPix/sar->ifm->er);
		double slantRng=sqrt(SQR(ht)+SQR(er)-2.0*ht*er*cos(phi));
		return slantRng+sar->geo->slantShift;
	} 
	else if (sar->geo->type=='P')/*Map projected images are tougher.*/
	{
		double time,slant;
		meta_get_timeSlantDop(sar,yLine,xSample,&time,&slant,NULL);
		return slant+sar->geo->slantShift;
	}
	else /*Unknown projection type.*/
	{
		printf("Error!  Unknown projection type '%c' passed to meta_get_slant!\n",
			sar->geo->type);
		exit(1);
	}
	return 0.0;/*<- for whining compilers.*/
}

/*Converts a line, sample pair to the doppler value
at that location. Returns Hz.  Only works for SR & GR.
*/
double meta_get_dop(meta_parameters *sar,double yLine, double xSample)
{
	return sar->geo->dopRange[0]+
	       sar->geo->dopRange[1]*xSample+
	       sar->geo->dopRange[2]*xSample*xSample+
	       sar->geo->dopAz[1]*yLine+
	       sar->geo->dopAz[2]*yLine*yLine;
}


/*Return fixed-earth state vector for the given time.
Steps through state vector list; then interpolates the
right pair.*/
stateVector meta_get_stVec(meta_parameters *sar,double time)
{
	int stVecNo;
	stateVector ret;
	if (sar->stVec==NULL)
	{
		printf("Error! Requested a state vector, but"
			"no state vectors exist in the file!\n");
		exit(1);
	}
	if (sar->stVec->num<2)
	{
		printf("Error! Only %d state vector exist in file!\n",
			sar->stVec->num);
		exit(1);
	}
	stVecNo=0;
	while (stVecNo<sar->stVec->num-2
		&& sar->stVec->vecs[stVecNo+1].time<time)
		stVecNo++;
	
	interp_stVec(&sar->stVec->vecs[stVecNo].vec,
			sar->stVec->vecs[stVecNo].time,
			&sar->stVec->vecs[stVecNo+1].vec,
			sar->stVec->vecs[stVecNo+1].time,
			&ret,time);
	return ret;
}
/*Return the incidence angle: this is the angle measured
	by the target between straight up and the satellite.
	Returns radians.*/
double meta_incid(meta_parameters *sar,double y,double x)
{
	double sr=meta_get_slant(sar,y,x);
	return PI-acos((SQR(sr) + SQR(sar->ifm->er) - 
		SQR(sar->ifm->ht) ) / (2.0*sr*sar->ifm->er));
}

/*Return the look angle: this is the angle measured
	by the satellite between earth's center and the target point x.
	Returns radians*/
double meta_look(meta_parameters *sar,double y,double x)
{
	double sr=meta_get_slant(sar,y,x);
	return acos((SQR(sr) + SQR(sar->ifm->ht) - 
		SQR(sar->ifm->er) ) / (2.0*sr*sar->ifm->ht));
}

/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   These are the external entry points.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independance.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"

void meta_get_orig_img_dimensions(meta_parameters *sar, long *lines, long *samples)
{
  *lines = sar->ifm->orig_nLines;
  *samples = sar->ifm->orig_nSamples;
}

/*Interferometry calls:*/
double meta_get_sat_height(meta_parameters *meta, long line, long sample)
{
	double ht, time;
	stateVector stVec;

	time = meta_get_time(meta, line, sample);
        stVec = meta_get_stVec(meta, time);
        ht = sqrt(stVec.pos.x*stVec.pos.x+stVec.pos.y*stVec.pos.y+stVec.pos.z*stVec.pos.z);

	return ht;
}

double meta_get_earth_radius(meta_parameters *meta, long line, long sample)
{
	double re=6378144.0, rp=6356754.9, lat, ht, er, time;
	stateVector stVec;

	time = meta_get_time(meta, line, sample);
        stVec = meta_get_stVec(meta, time);
        ht = sqrt(stVec.pos.x*stVec.pos.x+stVec.pos.y*stVec.pos.y+stVec.pos.z*stVec.pos.z);
        lat = asin(stVec.pos.z/ht);
        er=(double) (re*rp)/sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));

	return er;
}

void meta_get_slants(meta_parameters *sar,double *slantFirst, double *slantPer)
{
	*slantFirst=sar->geo->slantFirst;
	*slantPer=sar->geo->xPix;
}

double meta_get_k(meta_parameters *sar)
{
	return 2*PI/sar->geo->wavelen;
}
double meta_scene_frac(meta_parameters *sar,int y)
{
	return (double)(y - sar->ifm->orig_nLines/2)/(double)sar->ifm->orig_nLines;
}
void meta_interp_baseline(meta_parameters *sar,const baseline base,int y,double *Bn_y,double *Bp_y)
{
	double frac=meta_scene_frac(sar,y);
	*Bn_y = base.Bn + base.dBn*frac;
	*Bp_y = base.Bp + base.dBp*frac;
}
double meta_flat(meta_parameters *sar,double y,double x)
{
	return meta_look(sar,y,x)-sar->ifm->lookCenter;
}
double meta_flat_phase(meta_parameters *sar,const baseline base,int y,int x)
{
	double flat=meta_flat(sar,y,x);
	double Bn_y,Bp_y;
	meta_interp_baseline(sar,base,y,&Bn_y,&Bp_y);
	return 2.0*meta_get_k(sar)*(Bp_y*cos(flat)-Bn_y*sin(flat));
}
double meta_phase_rate(meta_parameters *sar,const baseline base,int y,int x)
{
	double sr=meta_get_slant(sar,y,x);
	double flat=meta_flat(sar,y,x);
	double incid=meta_incid(sar,y,x);
	double Bn_y,Bp_y;
	meta_interp_baseline(sar,base,y,&Bn_y,&Bp_y);
/*Note: this is the slant range times sin of the incidence angle, 
	divided by the derivative of meta_flat_phase.*/
	return (sr*sin(incid))/(2.0*meta_get_k(sar)*(-Bp_y*sin(flat)-Bn_y*cos(flat)));
}

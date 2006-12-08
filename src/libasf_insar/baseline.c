#include "asf.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "asf_insar.h"
#include <assert.h>

/* function declarations */
double get_days(meta_state_vectors *s1,meta_state_vectors *s2);
void get_sep(stateVector stVec1, meta_parameters *meta2,
	     double range, double dop,double *Bn,double *Bp);

baseline find_baseline(char *file1,char *file2)
{
#define MAX_STVEC 1000
  int N_STVEC;/*Number of state vectors in scene-- must be >=2.*/
  int i;
  double range,dop;
  double Bn[MAX_STVEC],Bp[MAX_STVEC];
  stateVector stVec;
  baseline base = {0.0, 0.0, 0.0, 0.0, 1.0};
  meta_parameters *meta1 = meta_read(file1);
  meta_parameters *meta2 = meta_read(file2);
  int ncol;
  
  ncol = meta1->sar->original_sample_count;

  /*Calculate the range and doppler of beam center.*/
  range = meta_get_slant(meta1,0,ncol/2);
  dop   = meta_get_dop(meta1,0,ncol/2);
  
  if (meta1->state_vectors==NULL)
    {
      sprintf(errbuf,"   ERROR: The image file '%s' has no\n"
	      "   state vectors!\n",file1);
      printErr(errbuf);
    }
  
  N_STVEC=meta1->state_vectors->vector_count;
  
  /*Get image 1's state vectors, and advance each
    of image 2's so they line up.
    This creates an array of normal and parallel baseline values.*/
  for (i=0;i<N_STVEC;i++)
    {
      stVec=meta1->state_vectors->vecs[i].vec;
      get_sep(stVec,meta2,range,dop,&Bn[i],&Bp[i]);
    }
  
  /*Find constant baseline value:
    Just average the normal and parallel baselines.
  */
  for (i=0;i<N_STVEC;i++)
    {
      base.Bn+=Bn[i];
      base.Bp+=Bp[i];
    }
  base.Bn/=N_STVEC;
  base.Bp/=N_STVEC;
  i=N_STVEC/2;
  if (!quietflag) printf("   Orbit Curvature=%.2fm normal, %.2fm parallel\n",
			 Bn[i]-base.Bn,Bp[i]-base.Bp);
  
  /*Find change in baseline value:
    Average the baseline delta for each point.
  */
  for (i=0;i<N_STVEC;i++)
    {
      /*                double weight=0.5-((double)i)/(N_STVEC-1);*/
      
      /* change we made once for Radarsat processing; 
	 needs to be looked at some more when time permits */
      double weight=-0.5+((double)i)/(N_STVEC-1);    
      
      if (weight!=0.0)
	{
	  base.dBn+=(Bn[i]-base.Bn)/weight;
	  base.dBp+=(Bp[i]-base.Bp)/weight;
	}
    }
  base.dBn/=(N_STVEC-1);
  base.dBp/=(N_STVEC-1);
  
  base.temporal=get_days(meta1->state_vectors,meta2->state_vectors);
  
  return base;
}

/*Return number of days between beginnings of the given images.
Computes s2->time-s1->time.*/
double get_days(meta_state_vectors *s1,meta_state_vectors *s2)
{
  double day_diff=0.0;
  
  /*First off, find the image with the smaller year*/
  int currYear,endYear;
  int swap_dir=1;/*1->forward; -1->backward*/
  
  if (s1->year>s2->year)
    {endYear=s1->year;currYear=s2->year;swap_dir=-1;}
  else
    {endYear=s2->year;currYear=s1->year;swap_dir=1;}
  
  /*Next, step along years until year difference is taken care of*/
  while (currYear<endYear)
    day_diff+=date_getDaysInYear(currYear++)*swap_dir;
  
  /*Now compensate for the day and time difference*/
  day_diff+=(s2->julDay-s1->julDay);
  day_diff+=(s2->second-s1->second)/(24.0*60*60);
  
  return day_diff;
}
/*
  Get_sep:
  Calculates the separation between two satellites along
  the satellite beam, in the normal- and parallel- to look direction.
  Inputs are: a state vector from scene 1, in GEI coordinates;
  the name of the ceos for image 2; the slant range and doppler of
  the center of the beam; and pointers to the output normal and 
  parallel baselines.
  
*/
void get_sep(stateVector stVec1, meta_parameters *meta2,
	     double range, double dop,double *Bn,double *Bp)
{
  double lat,phi,earthRadius;
  vector target,up,relPos;
  vector upBeam,alongBeam,beamNormal;
  double t,timeDelta;
  stateVector stVec2;
  GEOLOCATE_REC *g;
  
  /*Target is the patch of ground at beam center.*/
  /*Initialize the transformation.*/
  g=init_geolocate_meta(&stVec1,meta2);
  getLoc(g,range,dop,
	 &lat,&phi,&earthRadius);
  free_geolocate(g);
  sph2cart(earthRadius,lat,phi,&target);
  
  /*Create beam plane unit vectors.*/
  vecSub(stVec1.pos,target,&alongBeam);	vecNormalize(&alongBeam);
  up=stVec1.pos;				vecNormalize(&up);
  vecCross(up,alongBeam,&beamNormal);	vecNormalize(&beamNormal);
  vecCross(alongBeam,beamNormal,&upBeam);	vecNormalize(&upBeam);
  
  /*Find the time when the second satellite crosses the first's beam.*/
  t=0.0;
  stVec2=meta_get_stVec(meta2,t);
  timeDelta=1.0;
  while (fabs(timeDelta)>0.00001)
    {
      vecSub(stVec1.pos,stVec2.pos,&relPos);
      timeDelta=vecDot(beamNormal,relPos)/vecDot(beamNormal,stVec2.vel);
      t+=timeDelta;
      if (!quietflag) {
	printf("   Time=%f sec, delta=%f sec\n",t,timeDelta);
	printf("   Distance from beam plane=%f m\n",vecDot(beamNormal,relPos));
      }
      stVec2=meta_get_stVec(meta2,t);
    }
  
  /*Now we have the second satellite sitting in the plane of the first's beam,
    so we just separate that position into components along-beam and across-beam,
    and return.*/
  vecSub(stVec2.pos,stVec1.pos,&relPos);
  *Bp=vecDot(alongBeam,relPos);
  *Bn=vecDot(upBeam,relPos);
}


baseline read_baseline(char *fName)
{
  baseline b;
  FILE *fbase;
  fbase = FOPEN(fName,"r");
  
  if (5!=fscanf(fbase, "%lf %lf %lf %lf %lf", &b.Bn, &b.dBn, &b.Bp, &b.dBp,
		&b.temporal))
    {
      sprintf(errbuf,"   ERROR: Couldn't read five baseline components\n"
	      "         from baseline file named '%s'!\n",fName);
      printErr(errbuf);
    }
  fclose(fbase);
  printf("   Baseline:   Normal: %f, delta: %f\n",b.Bn,b.dBn);
  printf("               Parallel: %f, delta: %f\n",b.Bp,b.dBp);
  printf("               Temporal: %f days\n\n",b.temporal);
  if (logflag) {
    sprintf(logbuf,"   Baseline:   Normal: %f, delta: %f\n",b.Bn,b.dBn);
    printLog(logbuf);
    sprintf(logbuf,"               Parallel: %f, delta: %f\n",b.Bp,b.dBp);
    printLog(logbuf);
    sprintf(logbuf,"               Temporal: %f days\n\n",b.temporal);
    printLog(logbuf);
  }
  return b;
}

void write_baseline(char *fnm, baseline b)
{
  FILE *fp=FOPEN(fnm,"w");
  
  sprintf(logbuf,
	  "\n   Baseline: Bn = %f, dBn = %f, Bp = %f, dBp = %f, Btemp = %f\n",
	  b.Bn,b.dBn,b.Bp,b.dBp,b.temporal);
  printf("%s",logbuf);
  if (logflag) { printLog(logbuf); }
  
  fprintf(fp,"%f  %f  %f  %f %f\n",b.Bn,b.dBn,b.Bp,b.dBp,b.temporal);
  FCLOSE(fp);
  return;
}

void meta_interp_baseline(meta_parameters *meta,const baseline base,int y,double *Bn_y,double *Bp_y)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double frac=meta_scene_frac(meta,y);
	*Bn_y = base.Bn + base.dBn*frac;
	*Bp_y = base.Bp + base.dBp*frac;
}

double meta_flat_phase(meta_parameters *meta,const baseline base,int y,int x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	   || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double flat=meta_flat(meta,y,x);
	double Bn_y,Bp_y;
	meta_interp_baseline(meta,base,y,&Bn_y,&Bp_y);
	return 2.0*meta_get_k(meta)*(Bp_y*cos(flat)-Bn_y*sin(flat));
}

double meta_phase_rate(meta_parameters *meta,const baseline base,int y,int x)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double sr=meta_get_slant(meta,y,x);
	double flat=meta_flat(meta,y,x);
	double incid=meta_incid(meta,y,x);
	double Bn_y,Bp_y;
	meta_interp_baseline(meta,base,y,&Bn_y,&Bp_y);
/*Note: this is the slant range times sin of the incidence angle, 
	divided by the derivative of meta_flat_phase.*/
	return (sr*sin(incid))/(2.0*meta_get_k(meta)*(-Bp_y*sin(flat)-Bn_y*cos(flat)));
}

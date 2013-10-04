/******************************************************************************
NAME: This file contains subroutines to calculate the SCH coordinates
      that are needed for ROI

SYNOPSIS:

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    9/18   T. Logan     Seasat Proof of Concept Project - ASF
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
  double mat[3][3];
  double matinv[3][3];
  double ov[3];
  double radcur;
} pegtrans;

pegtrans ptm;

typedef struct {
  double lat;
  double lon;
  double hdg;
} pegtype;

pegtype peg;

typedef struct {
  double a;
  double e2;
} ellipsoid;

ellipsoid elp;

double earthspindot = 7.29211573052e-5;
double earthgm = 3.98600448073e14;
double awgs84=6378137.0;
double e2wgs84=0.00669437999015;
double pi = 3.141592653589793;
double rtod = 57.2957795130823;
double dtor = 0.01745329251994;
int xyztollh = 2;
int llhtoxyz = 1;
int schtoxyz = 0;
int xyztosch = 1;


/* calculates the norm of a vector 
 --------------------------------*/
double norm(double *v)
{
  return(sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]));
}

/* calculate the cross product of two vectors
 -------------------------------------------*/
void cross(double *u, double *v, double *w)
{
  w[0] = u[1]*v[2] - u[2]*v[1];
  w[1] = u[2]*v[0] - u[0]*v[2];
  w[2] = u[0]*v[1] - u[1]*v[0];
}

/* multiple a matrix times a vector
 -------------------------------------------*/
void matvec(double t[3][3], double *v, double *w)
{
  w[0] = t[0][0]*v[0] + t[1][0]*v[1] + t[2][0]*v[2];
  w[1] = t[0][1]*v[0] + t[1][1]*v[1] + t[2][1]*v[2];
  w[2] = t[0][2]*v[0] + t[1][2]*v[1] + t[2][2]*v[2];
}

/* compute the transpose of a matrix
 ----------------------------------*/
void tranmat(double a[3][3],double b[3][3])
{
  int i,j;
  for (i=0; i<3; i++)
    for (j=0; j<3; j++)
      b[i][j] = a[j][i];
}

/* form the linear combination of two vectors
 -------------------------------------------*/
void lincomb(double k1, double *u, double k2, double *v, double *w)
{
  w[0] = k1*u[0]+k2*v[0];
  w[1] = k1*u[1]+k2*v[1];
  w[2] = k1*u[2]+k2*v[2];
}

/* compute the product of 2 3x3 matrices
 --------------------------------------*/
void matmat(double a[3][3], double b[3][3], double c[3][3])
{
  int i;
  for (i=0; i<3; i++)  {        
    c[0][i] = a[i][0]*b[0][0] + a[i][1]*b[1][0] + a[i][2]*b[2][0];
    c[1][i] = a[i][0]*b[0][1] + a[i][1]*b[1][1] + a[i][2]*b[2][1];
    c[2][i] = a[i][0]*b[0][2] + a[i][1]*b[1][2] + a[i][2]*b[2][2];
  }
}

/* Takes a lat and lon and returns a change of basis matrix
   from ENU to geocentric coordinates.
 ---------------------------------------------------------*/
void enubasis(double lat, double lon, double enumat[3][3])
{
  double slt, clt, clo, slo;
  
  clt = cos(lat);
  slt = sin(lat);
  clo = cos(lon);
  slo = sin(lon);
  
  /* North vector */
  enumat[1][0] = -slt*clo;
  enumat[1][1] = -slt*slo;
  enumat[1][2] = clt;
  
  /* East vector */
  enumat[0][0] = -slo;
  enumat[0][1] = clo;
  enumat[0][2] = 0.0;
  
  /* Up vector */
  enumat[2][0]= clt*clo;
  enumat[2][1]= clt*slo;
  enumat[2][2]= slt;
}

/* perform a quadratic interpolation
 --------------------------------------------*/
double polint(double *time, double *vec, double seek_time)
{
  int i, m, ns;
  double dy, den, dif, dift, ho, hp, w;
  double c[10], d[10];
  double retval;

  ns=1;
  dif = fabs(seek_time-time[0]);
  for(i=0; i<3; i++) {
    dift = fabs(seek_time-time[i]);
    if (dift<dif) { ns=i; dif=dift; }
    c[i]=vec[i];
    d[i]=vec[i];
  }
  retval=vec[ns];
  ns = ns-1;
  
  for (m=1; m<3; m++) {
    for (i=0; i<3-m; i++)  {
      ho = time[i]-seek_time;
      hp = time[i+m]-seek_time;
      w = c[i+1]-d[i];
      den = ho-hp;
      if (den==0) { printf("failure in polint!\n");}
      den = w/den;
      d[i]=hp*den;
      c[i]=ho*den;
    }
    if (2*(ns+1)<3-m){
     dy = c[ns+1];
    } else {
     dy=d[ns];ns=ns-1;
    }
    retval=retval+dy;
  }
  
  return(retval);
}

/* Inter-Motion - interpolate state vector to center of scene
   using a quadratic interpolator
 -----------------------------------------------------------*/
void inter_motion(double *times, double xyz[][3], int npts, double seek_time, 
		  double *retxyz)
{
   int ind; 
   double interp_vec[3];
   double interp_time[3];
   int use[3];
   double bck, fwd;
   int i,j;
   
   /* find the closest state vector */
   ind = 0;
   do { ind++; } while (times[ind]<seek_time);
   
   bck = fabs(seek_time - times[ind]);
   fwd = fabs(seek_time - times[ind+1]);
   
   if (bck <= fwd && ind+1 < npts && ind-1 >= 1) {
	interp_time[0] = times[ind-1];
        interp_time[1] = times[ind];
        interp_time[2] = times[ind+1];
        use[0] = ind-1;
        use[1] = ind;
        use[2] = ind+1;
  } else if (bck <= fwd && ind+2 <= npts && ind-1 <= 0) {
	interp_time[0] = times[ind];
        interp_time[1] = times[ind+1];
        interp_time[2] = times[ind+2];
        use[0] = ind;
        use[1] = ind+1;
        use[2] = ind+2;
  } else if (bck > fwd && ind+2 <= npts) {
	interp_time[0] = times[ind];
        interp_time[1] = times[ind+1];
        interp_time[2] = times[ind+2];
        use[0] = ind;
        use[1] = ind+1;
        use[2] = ind+2;
  } else if (bck > fwd && ind+1 <= npts && ind-1 >= 1) {
	interp_time[0] = times[ind-1];
        interp_time[1] = times[ind];
        interp_time[2] = times[ind+1];
        use[0] = ind-1;
        use[1] = ind;
        use[2] = ind+1;
  } else { printf("Problem with motion data...\n"); exit(1); }
  
  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) { interp_vec[j]=xyz[use[j]][i];}
    retxyz[i] = polint(interp_time,interp_vec,seek_time);
  }
}
  
/* computes the transformation matrix from xyz to a local sch frame.
 -----------------------------------------------------------------*/
void schbasis(pegtrans ptm,double *sch, double xyzschmat[3][3], double schxyzmat[3][3])
{
  double coss, cosc, sins, sinc;
  double xyzv[3], llh[3], schdg;
  double matschxyzp[3][3];
  double tempmat[3][3];
  int i,j;
   
  coss = cos(sch[0]/ptm.radcur);
  sins = sin(sch[0]/ptm.radcur);
   
  cosc = cos(sch[1]/ptm.radcur);
  sinc = sin(sch[1]/ptm.radcur);
   
  matschxyzp[0][0] = -sins;
  matschxyzp[0][1] = -sinc*coss;
  matschxyzp[0][2] = coss*cosc;
  matschxyzp[1][0] = coss;
  matschxyzp[1][1] = -sinc*sins;
  matschxyzp[1][2] = sins*cosc;
  matschxyzp[2][0] = 0.0;
  matschxyzp[2][1] = cosc;
  matschxyzp[2][2] = sinc;
	
  for(i=0;i<3;i++) for(j=0;j<3;j++) tempmat[i][j] = ptm.mat[i][j];
	
  matmat(tempmat,matschxyzp,schxyzmat);
  tranmat(schxyzmat,xyzschmat);
}

/* converts a vector to lat,lon and height above the reference ellipsoid
   or given a lat,lon and height produces a geocentric vector. 
   dir=1 lat,lon -> vec, dir=2 vec -> lat,lon
 ----------------------------------------------------------------------*/
void latlon(ellipsoid elp, double *v, double *llh, int dir)
{
  int ft;
  double re,q2,q3,b,q;
  double p,tant,theta,a,e2;
  
  a = elp.a;
  e2 = elp.e2;
  
  if (dir == 1) {  /* convert lat,lon to a vector */
    re = a/sqrt(1.0-(e2*sin(llh[0])*sin(llh[0])));
    v[0] = (re + llh[2])*cos(llh[0])*cos(llh[1]);
    v[1] = (re + llh[2])*cos(llh[0])*sin(llh[1]); 
    v[2] = (re*(1.0-e2) + llh[2])*sin(llh[0]);
  } else {
  
    q2 = 1.0/(1.0-e2);
    q = sqrt(q2);
    q3 = q2 - 1.0;
    b = a * sqrt(1.0-e2);
    
    llh[1] = atan2(v[1],v[0]);
    
    p = sqrt(v[0]*v[0]+v[1]*v[1]);
    tant = (v[2]/p)*q;
    theta = atan(tant);
    tant = (v[2] + q3*b*pow(sin(theta),3))/(p-e2*a*pow(cos(theta),3));
    llh[0] = atan(tant);
    re = a/sqrt(1.0-e2*sin(llh[0])*sin(llh[0]));
    llh[2] = p/cos(llh[0]) - re;
  }
}

/* applies the affine matrix provided to convert the sch coordinates xyz
   WGS-84 coordintes or the inverse transformation. 
   dir=0 sch->xyz, dir=1 xyz->sch
 ----------------------------------------------------------------------*/
void convert_sch_to_xyz(pegtrans ptm,double *schv,double *xyzv, int dir)
{
  int i,j,t;
  double schvt[3], llh[3];
  double tempmat[3][3];
  ellipsoid sph;
  
  sph.a = ptm.radcur;
  sph.e2 = 0.0;
  
  if (dir == 0) {
    llh[0] = schv[1]/ptm.radcur;
    llh[1] = schv[0]/ptm.radcur;
    llh[2] = schv[2];
    
    t = 1;
    
    latlon(sph,schvt,llh,t);
    
    for(i=0;i<3;i++) for(j=0;j<3;j++) tempmat[i][j] = ptm.mat[i][j];
    matvec(tempmat,schvt,xyzv);
    lincomb(1.0,xyzv,1.0,ptm.ov,xyzv);
    
  } else {
  
    lincomb(1.0,xyzv,-1.0,ptm.ov,schvt);
    matvec(ptm.mat,schvt,schv);
    t = 2;
    latlon(sph,schv,llh,t);
    schv[0] = ptm.radcur*llh[1];
    schv[1] = ptm.radcur*llh[0];
    schv[2] = llh[2];
  }
}

/* This routine does some stuff to some numbers...  *sigh*
 --------------------------------------------------------*/
double rdir(double a, double e2, double hdg, double lat)
{
  double re, rn, tmp;
  
  re = a/sqrt(1.0 - e2*pow(sin(lat),2));
  rn = (a*(1.0 - e2))/pow((1.0 - e2*pow(sin(lat),2)),1.5);
  tmp = (re*rn) / (re*pow(cos(hdg),2) + rn*pow(sin(hdg),2));
  return(tmp);
}


/* This routine computes the transformation matrix and translation 
   vector needed to get between radar (s,c,h) coordinates and 
   (x,y,z) WGS-84 coordinates.
 ----------------------------------------------------------------*/
void radar_to_xyz(ellipsoid elp, pegtype peg, pegtrans *ptm)
{
  int i,j,dir, type;
  double radcur, llh[3], p[3], slt, clt, clo, slo, up[3];
  double chg, shg;
  
  /* first determine the rotation matrix */
  clt = cos(peg.lat);
  slt = sin(peg.lat);
  clo = cos(peg.lon);
  slo = sin(peg.lon);
  chg = cos(peg.hdg);
  shg = sin(peg.hdg);
  
  ptm->mat[0][0] = clt*clo;
  ptm->mat[0][1] = -shg*slo - slt*clo*chg;
  ptm->mat[0][2] = slo*chg - slt*clo*shg;
  ptm->mat[1][0] = clt*slo;
  ptm->mat[1][1] = clo*shg - slt*slo*chg;
  ptm->mat[1][2] = -clo*chg - slt*slo*shg;
  ptm->mat[2][0] = slt;
  ptm->mat[2][1] = clt*chg;
  ptm->mat[2][2] = clt*shg;
  
  for(i=0; i<3; i++)
    for (j=0; j<3; j++)
      ptm->matinv[i][j]=ptm->mat[j][i];
  
  /* find the translation vector */
  ptm->radcur = rdir(elp.a,elp.e2,peg.hdg,peg.lat);

  type = 1;
  llh[0] = peg.lat;
  llh[1] = peg.lon;
  llh[2] = 0.0;
  latlon(elp,p,llh,type);
  
  clt = cos(peg.lat);
  slt = sin(peg.lat);
  clo = cos(peg.lon);
  slo = sin(peg.lon);
  up[0] = clt*clo;
  up[1] = clt*slo;
  up[2] = slt;
  
  for (i=0; i<3; i++) ptm->ov[i] = p[i] - ptm->radcur*up[i];
    
}

#define MAX_OBS 1000

/* This is the main routine...
   Given a start time, number of lines, and prf,
   calculate the SCH vel, SCH acc, and space craft height
 -----------------------------------------------------------------------*/
void get_peg_info(double start_time, int nl, int prf, char *vecfile,
                  double *schvel, double *schacc, double *height, double *earthrad,
                  double *height_dt, double *lat, double *lon)
{
  FILE *fpin;
  double times[MAX_OBS];
  double xyz[MAX_OBS][3];
  double vxyz[MAX_OBS][3];
  double llh[MAX_OBS][3];
  double enumat[3][3];
  double xyzenumat[3][3];
  double xyzschmat[3][3];
  double schxyzmat[3][3];
  double enuvel[3];
  double xyzpeg[3];
  double llhpeg[3];
  double vxyzpeg[3];
  double spinvec[3];
  double inertialacc[3];
  double bodyacc[3];
  double platsch[3];
  double platvel[3];
  double platacc[3];
  double tempv[3], tempvec[3], tempa[3];
  double scene_cen_time;
  double xyznorm;
  
  int i,j,k,numobs;
  
  printf("Calculating peg information at time %lf\n",start_time);

  elp.a = awgs84;
  elp.e2 = e2wgs84;
  
  fpin = fopen(vecfile,"r");
  if (fpin == NULL) {printf("Unable to open vector file\n"); exit(1);}
  i = 0;
  if (fscanf(fpin,"%lf %lf %lf %lf %lf %lf %lf\n",&times[i],
  	&xyz[i][0],&xyz[i][1],&xyz[i][2],&vxyz[i][0],&vxyz[i][1],&vxyz[i][2])!=7)
    {printf("Can't read vector file\n"); exit(1);}
  
  do {
    i++;
    j=fscanf(fpin,"%lf %lf %lf %lf %lf %lf %lf\n",&times[i],
  	&xyz[i][0],&xyz[i][1],&xyz[i][2],&vxyz[i][0],&vxyz[i][1],&vxyz[i][2]);
  } while (j==7);

  numobs = i;
  
  printf("Read %i observations from vector file\n",numobs);
  
  /* Convert the position data to lat,lon for each point 
  for (k=0; k<numobs; k++)
    {
      latlon(elp,xyz[k],llh[k],xyztollh);
      printf("Observation #: %i  ",k);
      printf("Lat, Lon & Height: %10.5f %10.5f %12.3f\n",llh[k][0]*rtod,llh[k][1]*rtod,llh[k][2]);
    }
  */

  latlon(elp,xyz[0],llh[0],xyztollh);
  printf("Lat, Lon & Height at start: %10.5f %10.5f %12.3f\n",llh[0][0]*rtod,llh[0][1]*rtod,llh[0][2]);
  
  scene_cen_time = start_time + ((float)nl/(2.0*(float)prf));
  printf("Time to first/middle scene: %12.3lf %12.3lf\n",start_time,scene_cen_time);
  
  /* interpolate the motion data to the scene center using a quadratic interpolator */
  inter_motion(times,xyz,numobs,scene_cen_time,xyzpeg);
  inter_motion(times,vxyz,numobs,scene_cen_time,vxyzpeg);
  
  printf("Pos Peg = %12.3lf %12.3lf %12.3lf\n",xyzpeg[0],xyzpeg[1],xyzpeg[2]);
  printf("Vel Peg = %12.3lf %12.3lf %12.3lf\n",vxyzpeg[0],vxyzpeg[1],vxyzpeg[2]);
  
  /* take the lat,lon as the peg point and the heading as the peg heading */
  latlon(elp,xyzpeg,llhpeg,xyztollh);
  enubasis(llhpeg[0],llhpeg[1],enumat);
  tranmat(enumat,xyzenumat);
  matvec(xyzenumat,vxyzpeg,enuvel);
  peg.hdg = atan2(enuvel[0],enuvel[1]);
  peg.lat = llhpeg[0];
  peg.lon = llhpeg[1];
  radar_to_xyz(elp,peg,&ptm);
  
  spinvec[0] = 0.0;
  spinvec[1] = 0.0;
  spinvec[2] = earthspindot;
  
  xyznorm =norm(xyzpeg);
  cross(spinvec,xyzpeg,tempv);
  
  for (k=0; k<3; k++) inertialacc[k] = - (earthgm*xyzpeg[k])/pow(xyznorm,3);
   
  cross(spinvec,vxyzpeg,tempa);
  cross(spinvec,tempv,tempvec);

  for(k=0; k<3; k++) bodyacc[k] = inertialacc[k] - 2*tempa[k] - tempvec[k];
  
  convert_sch_to_xyz(ptm,platsch,xyzpeg,xyztosch);
  schbasis(ptm,platsch,xyzschmat,schxyzmat);
  matvec(xyzschmat,bodyacc,platacc);
  matvec(xyzschmat,vxyzpeg,platvel);
  
  schvel[0] = platvel[0];
  schvel[1] = platvel[1];
  schvel[2] = platvel[2];
  schacc[0] = platacc[0];
  schacc[1] = platacc[1];
  schacc[2] = platacc[2];
  
  *height = llhpeg[2];
  *height_dt = (*height-llh[0][2])/(scene_cen_time - start_time);
  *earthrad = ptm.radcur;
  *lat = llhpeg[0];
  *lon = llhpeg[1];
}


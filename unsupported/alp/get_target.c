/*****************************************************************
  get_trg_pos - calculates a target's lat,lon based on input 
		slant range, doppler frequency, and spacecraft
		position.

	11/20/97	T. Logan	Based on code from M. Jin 

*****************************************************************/
#include "fnc_defs.h"

extern char PROCMODE;
extern double RE;
extern double RP;
extern double ECC_E;

int i;
double tmpvec[3],tmp2vec[3];
double calc_dop();
double calc_incAng();
double mag();
double dot();
double calc_swath_vel(double *, double *);
double u[3],v[3];
double acel[3];		/* relative acceleration vector     */
double dop_rate;	/* calculated doppler rate 	    */

get_trg_pos(
  double sr0, 		/* slant range to target 	    */ 
  double fd0,		/* doppler frequency 		    */
  double *sVc,       	/* sc vector - 0,1,2 pos  3,4,5 vel */
  double *incidence,	/* returned incidence angle 	    */
  double *lookangle,	/* returned look angle      	    */
  double *yawangle,	/* returned yaw angle       	    */
  double *returnlat,	/* returned (geodetic) latitude     */
  double *returnlon,	/* returned longitude 	 	    */
  double *swathvel,     /* returned swath velocity          */
  double *doprate)      /* returned doppler rate            */
{
  double pVc[3]; 		/* pointing vector from spacecraft    	*/
  double tVc[6]; 		/* target vector (pos & velocity)     	*/
  double lat, lat_d, lon; 	/* calculated positions               	*/
  double lookAng=20.0, yaw=0.0, /* initial look angle and yaw	      	*/
         sr=0.0, fd=0.0,	/* calculated slant range and doppler 	*/
	 vel, incAng;		/* vel difference & incidence angle   	*/
  int    iter = 0;		/* iteration counter		      	*/
  double tmp;
  double trackAngle;

  /* Convert values from kilometers to meters */
  sr0 *= 1000.0; sVc[XP]*=1000.0; sVc[YP]*=1000.0; sVc[ZP]*=1000.0; 

  for (i=0; i<3; i++) { acel[i] = -1.0*sVc[i+6]; }

  while ( fabs(fd-fd0)>0.01 || fabs(sr-sr0)>0.01 )
   {
    calc_pt_vec(sVc,lookAng,yaw,pVc);
    calc_tg_pos(sVc,pVc,tVc);
    conv_to_ll(tVc,&lat,&lat_d,&lon);
    calc_tg_vel(lat,lon,lat_d,tVc);
    incAng = calc_incAng(lookAng,sVc,tVc);
    fd = calc_dop(sVc,tVc);
    sr  = sqrt(SQR(sVc[XP]-tVc[XP])+SQR(sVc[YP]-tVc[YP])+SQR(sVc[ZP]-tVc[ZP]));
    vel = sqrt(SQR(sVc[XV]-tVc[XV])+SQR(sVc[YV]-tVc[YV])+SQR(sVc[ZV]-tVc[ZV]));
    lookAng += atand( (sr0-sr)/(tand(incAng)*sr) ); 
    yaw -= asind( LAMBDA*(fd0-fd)/(2.0*vel) );
    if (iter>99) { printf("Error: failed to converge on target\n"); break; }
    else iter++;
   }

  /* Store return values */
/*
  tmp = asind(LAMBDA*fd0/(2.0*vel)); 
  printf("Calculated squint angle is %lf\n",tmp); 
*/

  *incidence = incAng; *lookangle = lookAng; *yawangle  = yaw;
  *returnlat = lat_d;
  *returnlon = lon;
  *swathvel  = calc_swath_vel(sVc,tVc);
  *doprate   = dop_rate; 
  sVc[XP]/=1000.0; sVc[YP]/=1000.0; sVc[ZP]/=1000.0; 
}

/********************************************************************/
double calc_incAng(double lookAng,double *sVc, double *tVc)
 { double rs,rt,lat,lat_d;
   rs = mag(sVc); rt = mag(tVc);
   lat = atand(tVc[2]/sqrt(SQR(tVc[0])+SQR(tVc[1])));
   lat_d = atand(tand(lat)/(1-SQR(ECC_E)));
   return ( ( asind((rs/rt)*sind(lookAng)) - (lat_d-lat)));
 }
/***************************************************************
   Calculate the pointing direction for the spacecraft
***************************************************************/
calc_pt_vec(double *sVc, double lookAng, double yaw, double *pVc) {
  register double cosYaw, cosLoa, sinYaw, sinLoa;
  double u[3],v[3];          /* cross product of sc velocity & position */
  double lsVc[3], tmp[3];    /* local copy of sc_vec */
  cosYaw = cosd(yaw); cosLoa = cosd(lookAng);
  sinYaw = sind(yaw); sinLoa = sind(lookAng);

  if (PROCMODE == 'R')
   { /* LEFT LOOKING */
    for (i=0;i<3;i++) tmp[i] = -1.0 * sVc[i];
    unitcross(&sVc[3],tmp,u);
    unitcross(tmp,u,v);
   }
  else
   { /* RIGHT LOOKING */
    unitcross(&sVc[3],sVc,u);
    unitcross(sVc,u,v);
   }
  normalize(sVc,lsVc);
  for (i=0; i<3; i++) {
    pVc[i] = -1.0*lsVc[i]*cosLoa + (u[i]*cosYaw-v[i]*sinYaw)*sinLoa;
  }
  normalize(pVc,pVc);
 }
/***************************************************************************
  Determine what the spacecraft sees on earth looking down pointing vector
 **************************************************************************/
calc_tg_pos(double *sVc, double *pVc, double *tVc)
{ double a,b,c,discrmnt,rst; 
  a = ((pVc[0]*pVc[0])+(pVc[1]*pVc[1]))/SQR(RE) + (pVc[2]*pVc[2])/SQR(RP);
  b = 2.0*((pVc[0]*sVc[0]+pVc[1]*sVc[1])/SQR(RE) + pVc[2]*sVc[2]/SQR(RP));
  c = (SQR(sVc[0])+SQR(sVc[1]))/SQR(RE) + SQR(sVc[2])/SQR(RP) - 1.0;
  discrmnt = SQR(b) - 4.0 * a * c;
  if (discrmnt < 0.0) printf("error: can not see a target\n");
  else {
	 rst = (-1.0*b-sqrt(discrmnt))/(2.0*a);
         for (i=0; i<3; i++) tVc[i] = sVc[i] + pVc[i]*rst;
  }
}
/********************************************************************
  Convert earth body fixed vector to lat and lon
 ********************************************************************/
conv_to_ll(double *tVc, double *lat,double *lat_d,double *lon)
 {
   *lat = asind(tVc[2]/mag(tVc)); 
   *lat_d = atand(tand(*lat)/(1-SQR(ECC_E)));
   *lon = atan2d(tVc[1],tVc[0]);
 }
/********************************************************************/
calc_tg_vel(double lat,double lon,double lat_d,double *tVc)
 { double Rn, rx;

   Rn = RE/sqrt(1-SQR(ECC_E*sind(lat_d)));
   tVc[0] = Rn*cosd(lon)*cosd(lat_d);
   tVc[1] = Rn*sind(lon)*cosd(lat_d);
   tVc[2] = Rn*(1-SQR(ECC_E))*sind(lat_d);
   rx = OMEGA_E*sqrt(SQR(tVc[0])+SQR(tVc[1]));
   tmpvec[0]=0.0; tmpvec[1]=0.0; tmpvec[2]=1.0;
   unitcross(tmpvec,tVc,tmp2vec);
   tVc[3] = rx*tmp2vec[0]; tVc[4] = rx*tmp2vec[1]; tVc[5] = rx*tmp2vec[2];
 }
/********************************************************************/
double calc_dop(double *sVc, double *tVc)
 { double diff[6], vDOTr, relR, invrelR;
   for (i=0; i<6; i++) diff[i] = tVc[i]-sVc[i];
   vDOTr = dot(diff,&(diff[3]));
   relR = mag(&(diff[0]));
   dop_rate= (-2.0*abs(SQR(mag(&(diff[3])))-dot(diff,acel)))/(LAMBDA*relR);
   return( (-2.0*vDOTr/(LAMBDA*relR)) );
 } 

double calc_swath_vel(double *sVc, double *tVc)
 { double diff[3], ratio;
   ratio = mag(tVc)/mag(sVc);
   diff[0]=ratio*sVc[XV]-tVc[XV]; diff[1]=ratio*sVc[YV]-tVc[YV];
   diff[2]=ratio*sVc[ZV]-tVc[ZV]; return(mag(diff));
 }

/** Calculate the magnitude/length/euclidean normal (whatever) of a vec */
/** Normalize input vector to output vector **********************/
/** Calculate the dot product of two vectors *********************/
/** Calculate the normalized cross product of two vectors ********/
/** Calculate the cross product of two vectors *******************/
double mag(double *vec) {return(sqrt(SQR(vec[0])+SQR(vec[1])+SQR(vec[2])));}
double dot(double *u, double *v) {return(u[0]*v[0]+u[1]*v[1]+u[2]*v[2]); }
normalize(double *in, double *o)
 { double tmp; tmp=1.0/mag(in);o[0]=in[0]*tmp;o[1]=in[1]*tmp;o[2]=in[2]*tmp;}
unitcross(double *u, double *v, double *r)
 { r[0]=u[1]*v[2]-u[2]*v[1]; r[1]=u[2]*v[0]-u[0]*v[2]; r[2]=u[0]*v[1]-u[1]*v[0];
   normalize(r,r); }
cross(double *u, double *v, double *r)
 { r[0]=u[1]*v[2]-u[2]*v[1];r[1]=u[2]*v[0]-u[0]*v[2];r[2]=u[0]*v[1]-u[1]*v[0];}

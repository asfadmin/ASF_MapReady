/*
   CONSTANTS.H

   Constants used in SARMODEL.A
*/

#ifndef __CONSTANTS_H
#define __CONSTANTS_H

#define PIE       3.141592653589793238
#define PI4       12.566371
#define PIE2      6.283185307179586476
#define LITESPEED 2.99792457e8
#define C         2.99792457e8
#define WE        (PIE / (12.0 * 3600.0))
#define RPD       (PIE / 180.0)
#define MU        3.986012e14

/*Prototypes:*/

 void calc4norm (double *tar, double *tar1, double *tar2, double *tar3, double *tar4, double *normal); 
 double calcfd2 (double *sat, double *tar, double clos, double wavelength, double dt); 
 void calcmask (double *sat, double *tar, double *normal, double *mask); 
 void calcnorm (double *tar, double *tar1, double *tar2, double *normal); 
 
  void calcrcoefs (double strtrc, double centrc, double endrc, 
  	double strttime, double centtime, double endtime, double *rcoefs); 
 void fortran_cart2sph (double *vec, double *lat, double *longs, double *r); 
 void centh2det (double *smsm, double latc, double rc, double *latd, double *h); 
 void changellips (double *ollh, double *osmsm, double *offcent, double *nsmsm, double *nllh); 
 
 void cintensity (double *sat, double *tar, double *normal, double *bytes, double *sini); 
 double compang (double tdtime, double trka, double *normal, 
	double lg, double lt, double gi, double li, int form); 
 void det2centh (double *smsm, double latd, double h, double *latc, double *rc); 
 double dot (double *a, double *b); 
 double fd2dot (double *sat, double *tar, double clos, double wavelength, double dt); 
 void ic2efc (double *satint, double *timeref);
 double calcbee (double yr);
 double fulljd (double yr, double mo, double day); 
 void inttar (double latp, double longp, double r, double *tarint); 
 double los (double *sat, double *tar); 
 double mag (double *a); 
 
 void movesat (double *satint, double *rcoefs, double *vcoefs, 
 	double *gammacoefs, double *mcoefs, double dtime, double *sat); 

 void newvec (double *vec1, double *vec2, double angle, double vecmag, double *vec3);
 
 void movetar (double *tarint, double dtime, double *tar); 
 double packtar (double *latdt, double *longt, double *elevt, 
 	double *tarint, double *tar); 
 
 void init_sar_model (double *azcoefs, double *grcoefs, 
 	double user_val_pix, char *hdname, int frm, ...);
 
 void sar_sim (double latdt, double longt, double elevt, 
 	double latdt1, double longt1, double elevt1, 
 	double latdt2, double longt2, double elevt2, 
 	double latdt3, double longt3, double elevt3, 
 	double latdt4, double longt4, double elevt4, 
 	int viewflg, double *intensity, double *sini, double *mask, 
 	double *line, double *sample); 

 void fortran_sph2cart (double latp, double longp, double r, double *vec); 

#endif


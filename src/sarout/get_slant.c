#include "asf.h"
#include "math.h"
#include "asf_meta.h"

double getSRfromGR(stateVector scVec, double sr0, double eradcntr, double grX);
double get_rho(stateVector scVec, double sr0, double eradcntr);

double getSRfromGR(stateVector scVec, double sr0, double eradcntr, double grX)
 {
  double r_sc;		/* Radius from center or earth to spacecraft */
  double r_close; 	/* Near Slant Range */
  double r_earth;       /* Radius of the earth */
  double a,x,y;
  double rg0;		/* Ground range to first pixel */
  double rg, tmp;

  /* calculate ground range to first point */
  r_sc = vecMagnitude(scVec.pos);
  if (r_sc > 10000.0) r_sc /=1000.0;
  r_close = sr0;
  r_earth = eradcntr;
/*
  printf("r_sc = %f; r_close = %f\n",r_sc,r_close);
  printf("r_earth = %f\n",r_earth);
*/
  a = (r_sc-r_earth)/r_earth; 
  x = 1.0+a;
  y = r_close/r_earth;
  rg0 = r_earth * acos((1.0 + x*x - y*y) / (2.0*x));
 /* printf("Ground Range to first Pixel %lf\n",rg0); */
  rg = rg0 + grX;
 /* printf("Ground Range to center Pixel %lf\n",rg); */

  tmp = r_earth * sqrt(2.0 * x * (1.0-cos(rg/r_earth)) + a*a);

  /* printf("Slant Range to center pixel %lf\n",tmp);  */
  return(tmp);
 }


double get_rho(stateVector scVec, double sr0, double eradcntr)
 {
  double r_sc;		/* Radius from center or earth to spacecraft */
  double r_close; 	/* Near Slant Range */
  double r_earth;       /* Radius of the earth */
  double a,x,y;
  double rho;

  /* calculate ground range to first point */
  r_sc = vecMagnitude(scVec.pos);
  r_close = sr0;
  r_earth = eradcntr;

  a = (r_sc-r_earth)/r_earth; 
  x = 1.0+a;
  y = r_close/r_earth;

  rho = acos((1.0 + x*x - y*y) / (2.0*x));

  return(rho);
 }





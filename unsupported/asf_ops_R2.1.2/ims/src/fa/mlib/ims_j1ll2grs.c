static char *sccs = "@(#)ims_j1ll2grs.c	5.2 25 Apr 1996";


#include <math.h>

#define RAD(x) (x / 57.29577)
#define ARAD(x) (x * 57.29577)

extern double ims_fa_abs(double);

/* j1ll2grs
 * Corey Porter
 * 12/18/95
 */

#include <ims_cpfort.h>

int ims_j1ll2grs(double xincl,long icyrevs,long icydays,double rsp_0_lon,
             long n_rows, double grd_lat, double grd_lon, long *path,
             long *row)
{
  long izone, n_row;
  double a,b,gclat,theta_0,lamda_a, xpath, gamma, xi;
  int ier = 0;

	xi = 180 - xincl;

  *path = *row = -1000000;

  /* I am not responsible for the following kludge: */

  if(ims_fa_abs(grd_lon) > 180) goto eight_oo_one;
  if(ims_fa_abs(grd_lat) >= 90) goto eight_oo_two;
  if(grd_lat < -82.8)    goto eight_oo_three;
  if(grd_lat > 86.4)     goto eight_oo_four;

  /* input is ok */

  a = 6378.144;
  b = a - a/298.257;
  gclat = ARAD(atan((b/a)*(b/a) * tan(RAD(grd_lat))));
  izone = 1;
  if(grd_lat < 75.8835) izone = 2;
  if(grd_lat < 59.2945) izone = 3;
  if(grd_lat < -59.2945) izone = 4;
  if(grd_lat < -75.8835) izone = 5;

  if(izone != 1) goto and;

  *row = (((76.044 - gclat)/0.358)+170.5);
  xpath = (-155.169-grd_lon)*659.0/360.0;
  if(xpath<0.5) xpath += 659.0;
  if(xpath>659.5) xpath -= 659.0;

  n_row = 0;

  if(*row == 142) n_row = 12;
  if(*row == 143) n_row = 11;
  if(*row == 144) n_row = 10;
  if(*row == 145) n_row = 9;
  if(*row >= 146 && *row <= 147) n_row = 8;
  if(*row >= 148 && *row <= 149) n_row = 7;
  if(*row >= 150 && *row <= 153) n_row = 6;
  if(*row >= 154 && *row <= 157) n_row = 5;
  if(*row >= 158 && *row <= 164) n_row = 4;
  if(*row >= 165 && *row <= 170) n_row = 3;

  *path = ((xpath-1)/n_row+0.5) * n_row + 1;

  if(*path > 659) *path = 1;
  if(*path <= 0) *path += 659;

  goto nine_nine_nine_nine;

and:
  
  if(izone != 5) goto two_thousand;
  *row = (((-76.044 - gclat) / 0.358) + 431.5);
  xpath = (128.827 - grd_lon) * 659 / 360;

  if(xpath < 0.5) xpath += 659;
  if(xpath > 659.5) xpath -= 659;

  n_row = 0;

  if(*row >= 431 && *row <= 436) n_row = 3;
  if(*row >= 437 && *row <= 443) n_row = 4;
  if(*row >= 444 && *row <= 447) n_row = 5;
  if(*row >= 448 && *row <= 449) n_row = 6;

  *path = ((xpath - 1.0)/n_row + 0.5) * n_row + 1;

  if(*path > 659) *path = 1;
  if(*path <= 0) *path += 659;
  goto nine_nine_nine_nine;

two_thousand:
  
  gamma = 180 - ARAD(asin(sin(RAD(gclat))/sin(RAD(xi))));
  *row = (gamma * 600.0 / 360.0 + 1);

  theta_0 = ARAD(atan(cos(RAD(xi))*tan(RAD(gamma)))) + 180.0;
  lamda_a = grd_lon + theta_0 + 0.25 * gamma / 3.744318;
  xpath = (659.0 / 360.0) * (-1.153 - lamda_a);
  if(xpath < 0.5) xpath += 659;
  if(xpath > 659.5) xpath -= 659;
  if(izone == 3) *path = xpath + 0.5;
  if(izone == 2 || izone == 4)
    *path = ((int)((xpath-1)/2.0 + 0.5))*2 + 1;
  if(*path <= 0) *path += 659;
  if(*path > 659) *path = 1;
  goto nine_nine_nine_nine;

eight_oo_one:

  ier = -3;
  goto nine_nine_nine_nine;

eight_oo_two:

  ier = -2;
  goto nine_nine_nine_nine;

eight_oo_three:

  ier = -1;
  goto nine_nine_nine_nine;

eight_oo_four:

  ier = 1;
  goto nine_nine_nine_nine;

nine_nine_nine_nine:

  if (ier)
	 return(IMS_ERROR);

  return(IMS_OK);
}

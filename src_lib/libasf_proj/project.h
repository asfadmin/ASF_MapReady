#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

#include "asf_meta.h"

int project_poly(double lat, double lon,
		 double *x, double *y);
int project_utm(double lon_0, double lat, double lon,
		double *x, double *y);
int project_ps(double lat_0, double lon_0, 
	       int is_north_pole,
	       double lat, double lon,
	       double *x, double *y);
int project_ps_s(proj_ps * ps,
		 int is_north_pole,
		 double lat, double lon,
		 double *x, double *y);
int project_lamaz(double lat_0, double lon_0,
		  double lat, double lon, double *x, double *y);
int project_lamaz_s(proj_lamaz * lamaz,
		    double lat, double lon, double *x, double *y);
int project_lamcc(double lat_1, double lat_2, double lat_0, double lon_0, 
		  double lat, double lon, double *x, double *y);
int project_lamcc_s(proj_lamcc * lamcc,
		    double lat, double lon, double *x, double *y);
int project_albers(double lat_1, double lat_2, double lat_0, double lon_0,
		   double lat, double lon, double *x, double *y);
int project_albers_s(proj_albers * alb, 
		     double lat, double lon, double *x, double *y);

#endif

#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

int project_poly(double lat, double lon,
		 double *x, double *y);
int project_utm(double lon_0, double lat, double lon,
		double *x, double *y);
int project_ps(double lat_0, double lon_0, double lat, double lon,
	       double *x, double *y);

#endif

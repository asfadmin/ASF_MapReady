#include <math.h>

#define EARTH_RADIUS_MICRON 0.1
#define FLOAT_COMPARE(a, b) (fabs(a - b) < EARTH_RADIUS_MICRON ? 1 : 0)

/***********Arrays taken from asf_geolib's sphdz.c (Sept 2002)************
CODE     DATUM NAME                 EQUITORIAL RADIUS       POLAR RADIUS
 0:   Clarke 1866                      6378206.4           6356583.8
 1:   Clarke 1880                      6378249.145         6356514.86955
 2:   Bessel                           6377397.155         6356078.96284
 3:   International 1967               6378157.5           6356772.2
 4:   International 1909               6378388.0           6356911.94613
 5:   WGS 72                           6378135.0           6356750.519915
 6:   Everest                          6377276.3452        6356075.4133                          
 7:   WGS 66                           6378145.0           6356759.769356                      
 8:   GRS 1980/WGS 84                  6378137.0           6356752.31414   
 9:   Airy                             6377563.396         6356256.91
10:   Modified Everest                 6377304.063         6356103.039        
11:   Modified Airy                    6377340.189         6356034.448
12:   Walbeck                          6378137.0           6356752.314245
13:   Southeast Asia                   6378155.0           6356773.3205
14:   Australian National              6378160.0           6356774.719
15:   Krassovsky                       6378245.0           6356863.0188                          
16:   Hough                            6378270.0           6356794.343479
17:   Mercury 1960                     6378166.0           6356784.283666
18:   Modified Mercury 1968            6378150.0           6356768.337303   
19:   Sphere of Radius 6370997 meters  6370997.0           6370997.0
*************************************************************************/
int earth_radius2datum(double re_major, double re_minor)
{
	static double major[20] = /*EQUITORIAL RADIUS*/
		{6378206.4,      6378249.145,    6377397.155,    6378157.5,      6378388.0,
		 6378135.0,      6377276.3452,   6378145.0,      6378137.0,      6377563.396,
		 6377304.063,    6377340.189,    6378137.0,      6378155.0,      6378160.0,
		 6378245.0,      6378270.0,      6378166.0,      6378150.0,      6370997.0};
	static double minor[20] = /*POLAR RADIUS*/
		{6356583.8,      6356514.86955,  6356078.96284,  6356772.2,      6356911.94613,
		 6356750.519915, 6356075.4133,   6356759.769356, 6356752.31414,  6356256.91,
		 6356103.039,    6356034.448,    6356752.314245, 6356773.3205,   6356774.719,
		 6356863.0188,   6356794.343479, 6356784.283666, 6356768.337303, 6370997.0};
	int major_index = 0, minor_index = 0;

	for (major_index=0; major_index<20; major_index++) {
		if ( FLOAT_COMPARE(major[major_index],re_major) ) {
			for (minor_index=0; minor_index<20; minor_index++) {
				if ( FLOAT_COMPARE(minor[minor_index],re_minor) ) {
					if ( FLOAT_COMPARE(major_index,minor_index) ) {
					 	return major_index;
					}
				}
			}
		}
	}
	if ( FLOAT_COMPARE(major[minor_index],re_major) ) {
		return minor_index;
	}

/*	printf("\n"
	       "WARNING: Function earth_radius2datum was unable to figure\n"
	       "         supported datum... setting DDR datum code to -1\n");*/
	return (-1);
}

void datum2earth_radius(int datum, double *re_major, double *re_minor)
{
	static double major[20] = /*EQUITORIAL RADIUS*/
		{6378206.4,      6378249.145,    6377397.155,    6378157.5,      6378388.0,
		 6378135.0,      6377276.3452,   6378145.0,      6378137.0,      6377563.396,
		 6377304.063,    6377340.189,    6378137.0,      6378155.0,      6378160.0,
		 6378245.0,      6378270.0,      6378166.0,      6378150.0,      6370997.0};
	static double minor[20] = /*POLAR RADIUS*/
		{6356583.8,      6356514.86955,  6356078.96284,  6356772.2,      6356911.94613,
		 6356750.519915, 6356075.4133,   6356759.769356, 6356752.31414,  6356256.91,
		 6356103.039,    6356034.448,    6356752.314245, 6356773.3205,   6356774.719,
		 6356863.0188,   6356794.343479, 6356784.283666, 6356768.337303, 6370997.0};

	*re_major = major[datum];
	*re_minor = minor[datum];
}

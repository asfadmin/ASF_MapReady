


#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#define sind(x) sin((x)*dtr)
#define cosd(x) cos((x)*dtr)
#define tand(x) tan((x)*dtr)
#define asind(x) (asin(x)*rtd)
#define acosd(x) (acos(x)*rtd)
#define atand(x) (atan(x)*rtd)
#define atan2d(y,x) (atan2(y,x)*rtd)

#define SQR(A)  ((A)*(A))
#define ECC_E   8.1827385E-2    	/* Eccentricity of  earth    */
#define ECC2    (ECC_E*ECC_E)      	/* Eccentricity squared      */
#define RE      6378144.0		/* Equatorial Radius         */
#define RP      6356755.0		/* Polar Radius              */
#define ecc_e   8.1827385e-2		/* Eccentricity of the Earth */
#define ecc2    6.6957209e-3		/* Eccentricity Squared      */
#define cspeed  299.792458e6		/* Light Speed 		     */
#define fc      5300.0e6		/* Frequency of Carrier Wave */
#define omega_e 7.29211585e-5		/* Earth's Rotational Rate   */
#define gmass   3.9860045e14		/* Earth's Mass       */
#define dtr 	0.01745329   		/* Degrees to Radians */
#define rtd 	57.2957795   		/* Radinas to Degrees */
#define PIover4 0.78539816
#define PIover2 1.57079633
#define         WE      0.00417807442   /* Earth Rotation Rate (degrees/second\) */
#ifndef pi
#define pi 3.14159265358979323
#endif

#define deg2rad (pi/180.0)

#define MAX(a,b)	(a>b)?(a):(b);
#define MIN(a,b)	(a<b)?(a):(b);



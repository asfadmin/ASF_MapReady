/*Geolocate.h:
	This header lists the the non-CEOS dependant part 
of the geolocation library.  This library includes 
routines for computing the location
on the earth (lat,lon) of a single point on a SAR image 
(line,sample or time, slant range).

These routines live in asf_meta.a
*/
#ifndef __GEOLOCATE_H
#define __GEOLOCATE_H
typedef struct {
	double x;
	double y;
	double z;
} vector;
typedef struct {
	vector pos;
	vector vel;
} stateVector;

/**************** Specialized Utility Routines ************/
double utc2gha (int year,int julianDay, int hour,int min, double sec);
void fixed2gei(stateVector *stateVec,double gha);
void gei2fixed(stateVector *stateVec,double gha);

void interp_stVec(stateVector *st1,double time1, 
				stateVector *st2, double time2,
				stateVector *stOut,double timeOut);

void cart2sph(const vector pos,double *r,double *theta,double *phi);
void sph2cart(double r,double theta,double phi,vector *pos);
double atan2_check(double y, double x);

vector vecNew(double x,double y,double z);
void vecAdd(const vector a,const vector b, vector *c);
void vecSub(const vector a,const vector b, vector *c);
void vecScale(vector *v,double scale);
double vecMagnitude(const vector v);
void vecNormalize(vector *v);
double vecDot(const vector a,const vector b);
void vecCross(const vector a,const vector b,vector *aXb);
void vecMul(const vector *matrix,const vector src,vector *dest);

/************** Big Routines *****************
	The below routines form the bulk of the geolocation
library.  They rely on the geolocation globals (see earthloc.h)
but are otherwise independant of CEOS.  The state vectors are
in Geocentric Equatorial Inertial coordinates (like CEOS state vectors)
and are arranged:
	Inputs:
		stVec.pos.x=X position of s/c (m)
		stVec.pos.y=Y position of s/c (m)
		stVec.pos.z=Z position of s/c (m) (towards north pole)
		stVec.vel.x=X velocity of s/c (m/s)
		stVec.vel.y=Y velocity of s/c (m/s)
		stVec.vel.z=Z velocity of s/c (m/s)


init_geolocate returns a GEOLOCATE_REC, which is a handle
passed to all other routines.  init_geolocate_meta (in asf_meta.h),
copies over the look side, wavelength, etc. from a meta_parameters
record, which is much better.

GetLatLongMeta (in asf_meta.h) is the main entry-- it takes a state vector, 
slant range, doppler, elevation (estimate), greenwich hour angle, 
and returns a lat,long, and earth radius.

The low-level routines revolve around two new angles: gamma, which is pi minus the
look angle, and azimuth (az), which is the yaw-rotation of the spacecraft, 
measured in radians clockwise (when viewed from the top) from a vector 
perpendicular to the spacecraft flight path.

getLook calculates gamma given a state vector, slant range, precision (m), side ('L' or 'R'),
and azimuth.  It assumes an ellipsoidal earth described in earthRec.

getLoc iterates to find the target geocentric latitude, ground angle phi 
(latitude+GHA), and earth radius.

getLookYaw returns the look and yaw angles of the imaged target, in radians.

getDoppler calculates the target vector, relative velocity vector, doppler
frequency (fd), doppler rate (fdot), given a state vector, gamma angle, and azimuth
angle.

yaw2doppler computes an estimated doppler frequency based on the
yaw angle.  This frequency is returned in Hz.

getHeading computes the heading of a state vector-- the track angle
to true north, measured counterclockwise when viewed from above.
*/
/*Also see the geolocate initialization routines in asf_meta.h!*/

/***************** A "geolocate_rec", a private 
structure used in the geolocate library. *************/
typedef struct {
/*Satellite parameters:*/
	stateVector stVec;
	double lambda;/*Satellite wavelength*/
	char side;/*SAR look side ('L'->left; 'R'->right)*/	
	vector look_matrix[3];/*3x3 matrix to translate s/c-centered coords to earth-centered coords*/

/*Earth Parameters*/
	double re;/*Equator Radius, in meters*/
	double rp;/*Polar Radius, in meters*/
	double dayLength; /*Length of day, in seconds.*/
	double angularVelocity; /* 2*PI/dayLength, in radians per second.*/
	double gxMe; /*Gravitational Constant times mass of planet (Kg).*/
} geolocate_rec;
#define GEOLOCATE_REC geolocate_rec

GEOLOCATE_REC *init_geolocate(const stateVector *stVec); 
void free_geolocate(GEOLOCATE_REC *g);

int getLook(GEOLOCATE_REC *g,double range,double yaw, /* Inputs */
            double *look);                            /* Outputs */

int getLoc(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
           double *latitude,double *phi,double *earthRadius); /*Outputs.*/

int getLookYaw(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
               double *out_look,double *out_yaw);/* Outputs, in radians.*/

int getDoppler(GEOLOCATE_REC *g,double look,double yaw,
               double *fd, double *fdot,
               vector *targPos,vector *relVel);

double yaw2doppler(GEOLOCATE_REC *g,double slantRange,double yawAngleDeg);

double getHeading(const stateVector *stVec);

#endif


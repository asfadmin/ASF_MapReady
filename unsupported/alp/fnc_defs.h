#include "asf.h"
#include "ceos.h"
#include "ddr.h"
#include "math.h"

/*** Constants of the Universe!!! ****************************************/
#define         EROT    0.00007292115   /* Earth Rotation Rate (radians) */
#define         WE      0.00417807442   /* Earth Rotation Rate (degrees) */
#define		rtd	(180.0/M_PI)	/* Radians to Degrees conversion */
#define		dtr	(M_PI/180.0)	/* Degrees to Radians conversion */
#define         CSPEED  299.792458E6    /* Speed of Light                */


/*** Constants for the ERS-1 & ERS-2 Sattelite Platforms *****************/
#define E1_LINE_SPACING	3.9648392	/* Azimuth Line spacing (meters) */
#define E1_SAMP_SPACING 7.9061179	/* Slant Range Sample spacing (m)*/
#define E1_RNG_BNDWID	15.55		/* Range bandwidth		 */
#define E1_AZI_BNDWID	1260.0		/* Azimuth bandwidth		 */
#define         FC      5300.0E6        /* Frequency of the carrier wave */
#define         LAMBDA  CSPEED / FC	/* Wavelength of SAR             */


/*** Constants for GEM 6 Earth Model *************************************/
#define         OMEGA_E 7.29211585E-5   /* Earth rotation rate (rad/sec) */


/*** Macro Definitions ***************************************************/
#define         SQR(A)  (double)((A)*(A))  /* Square of a double 	 */
#define         ECC2    (ECC_E*ECC_E)      /* Eccentricity squared       */
#define         RE2     (RE*RE)		   /* Equatorial Radius Squared  */
#define         RP2     (RP*RP)		   /* Polar Radius Squared       */
#define DMAX(A,B)           (((A)>(B)) ? (A) : (B))
#define DMIN(A,B)           (((A)<(B)) ? (A) : (B))
#define sind(x) sin((x)*dtr)
#define asind(x) (asin(x)*rtd)
#define cosd(x) cos((x)*dtr)
#define acosd(x) (acos(x)*rtd)
#define tand(x) tan((x)*dtr)
#define atand(x) (atan(x)*rtd)
#define atan2d(x,y) (atan2((x),(y))*rtd)



/*** Definitions for readability of code ********************************/
#define		STRT	0
#define		NEAR	0
#define		CNTR	1
#define		END	2
#define		FAR	2
#define 	ST 	0
#define 	NR 	0
#define 	CN 	1
#define 	EN 	2
#define 	FR 	2
#define		XP	0
#define		YP	1
#define		ZP	2
#define		XV	3
#define		YV	4
#define		ZV	5


struct scene_params {
  double  time,
          slntRng,
          dopFreq,
          dopRate,
	  earthRad,
          lookAng,
          incAng,
	  scVec[9],
          lineNum,
          sampNum,
	  gha,
	  lat, lon,
	  swathVel,
	  heading,
	  yaw;
};


/*************************************************************************
  These #defines convert floats, ints, and strings to/from ascii.
  We can do this because the offset into the ASCII buffer doesn't depend
  on the direction you're translating.  Hence, the below routines just
  provide the mapping between the two formats. 
**************************************************************************/
#define uc (unsigned char *)
#define fltV(qFld,bfOf,bfSz) flt2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define sngV(qFld,bfOf,bfSz) sng2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define shrtV(qFld,bfOf,bfSz) shrt2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define intV(qFld,bfOf,bfSz) int2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define longV(qFld,bfOf,bfSz) long2asc(&(q->qFld),&bf[bfOf],bfSz,dir);off+=bfSz;
#define strV(qFld,bfOf,bfSz) str2asc(q->qFld,&bf[bfOf],bfSz,dir);off+=bfSz;
#define fltA(tng,bfOf,bfSz)  flt2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define shrtA(tng,bfOf,bfSz) shrt2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define intA(tng,bfOf,bfSz)  int2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define longA(tng,bfOf,bfSz) long2asc(&tng,&bf[bfOf],bfSz,dir);off+=bfSz;
#define strA(tng,bfOf,bfSz)  str2asc(tng,&bf[bfOf],bfSz,dir);off+=bfSz;


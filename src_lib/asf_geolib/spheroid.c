/*******************************************************************************
NAME			      SPHEROID

PURPOSE	     Places spheroid values into projection parameter array

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
D. Steinwand		Aug, 1988	Original Development
D. Etrheim		Jul, 1990	Standardized error message handling
					

PROJECT     LAS

ALGORITHM 
	If the first two elements of the projection parameter array
           are not zero, return
	Pack the first two fields based on the datum code entered
	return


ALGORITHM REFERENCES

1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987. 

2.  "Software Documentation for GCTP General Cartographic Transformation
    Package", U.S. Geological Survey National Mapping Division, May 1982.

3.  Clarie, Charles N, "State Plane Coordinates by Automatic Data Processing",
    U.S. Department of Commerce, Environmental Science Services Admin.,
    Coast and Geodetic Survey, Publication 62-4, 1973.
*******************************************************************************/
#include "worgen.h"

int spheroid(proj, datum, projparms)

int proj;		/* GCTP projection code */
int datum;		/* GCTP projection datum code */
double projparms[15];	/* Array of 15 GCTP projection parameters */
{
double a=0;		/* Semi-major axis of ellipsoid */
double b=0;		/* Semi-minor axis of ellipsoid */

/* Check for a valid datum code
  ----------------------------*/
if((datum < 0) || (datum > 19)) 
   {
   c_errmsg("Datum code value out of valid range","spheroid-datum",NON_FATAL);
   return(E_FAIL);
   }

/* If proj is Geographic, UTM, or State Plane Coordinate System, return
  --------------------------------------------------------------------*/
if((proj == 0) || (proj == 1) || (proj == 2)) return(E_SUCC);

/* If a spheroid or sphere definition already exists, return
  ---------------------------------------------------------*/
if(projparms[0] != 0.0) return(E_SUCC);

/* Pack spheriod or sphere values into the first two elements of the projection
   parameter array
  ---------------*/
switch (datum) {
   case 0:  a = 6378206.4;    b = 6356583.8;      break; /* Clarke 1866       */
   case 1:  a = 6378249.145;  b = 6356514.86955;  break; /* Clarke 1880       */
   case 2:  a = 6377397.155;  b = 6356078.9628;   break; /* Bessel            */
   case 3:  a = 6378157.5;    b = 6356772.2;      break; /* Internat. 1967    */
   case 4:  a = 6378388.0;    b = 6356911.94613;  break; /* Internat. 1909    */
   case 5:  a = 6378135.0;    b = 6356750.519915; break; /* WGS 72            */
   case 6:  a = 6377276.3452; b = 6356075.4133;   break; /* Everest           */
   case 7:  a = 6378145.0;    b = 6356759.769356; break; /* WGS 66            */
   case 8:  a = 6378137.0;    b = 6356752.31414;  break; /* GRS 1980          */
   case 9:  a = 6377563.396;  b = 6356256.91;     break; /* Airy              */
   case 10: a = 6377304.063;  b = 6356103.039;    break; /* Modified Everest  */
   case 11: a = 6377341.89;   b = 6356036.143;    break; /* Modified Airy     */
   case 12: a = 6376896.0;    b = 6355834.8467;   break; /* Walbeck           */
   case 13: a = 6378155.0;    b = 6356773.3205;   break; /* Southeast Asia    */
   case 14: a = 6378160.0;    b = 6356774.719;    break; /* Australian Nat.   */
   case 15: a = 6378245.0;    b = 6356863.0188;   break; /* Krassovsky        */
   case 16: a = 6378270.0;    b = 6356794.343479; break; /* Hough             */
   case 17: a = 6378166.0;    b = 6356784.283666; break; /* Mercury 1960      */
   case 18: a = 6378150.0;    b = 6356768.337303; break; /* Mod. Mercury 1968 */
   case 19: a = 6370997.0;    b = 6370997.0;      break; /* Sphere: 6370997 m */
   }

/* Calculate the eccentricity squared
  ----------------------------------*/
projparms[1] = ((a * a) - (b * b)) / (a * a);

/* Place the semi-major axis in the first parameter array element
  --------------------------------------------------------------*/
projparms[0] = a;

return(E_SUCC);
}

/****************************************************************************
NAME:				c_mapedg

PURPOSE:  Calculates the minimum and maximum projection coordinates for
          given upper left and lower right latitude, longitude coordinates.

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  1.0    1981?  J. Peterson and        Original development on pdp-11
                J. Thormodsgard        (CAMGRD FORTRAN IV+ subroutine)	
  2.0    1984?  D. Steinwand  CSB      Modified to compile on various systems
				       (CORNERS FORTRAN 77 subroutine)
  2.1    9/87   B.Ailts       CSB      To only check for min/max around the 
                                       edge of the output space instead of the
				       entire area; Convert for newLAS
				       (EDGE FORTRAN 77 subroutine)
  5.0	 2/89	D. Steinwand  CSB      LAS 5.0 clean-up; C conversion
  5.1    7/89   D. Steinwand  CSB      Fixed bug--now able to use DMS coords
  5.2	 7/90	D. Etrheim    CSB      Standardized error message handling

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:   must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Initialize variables
	Calculate min/max coordinates along east edge of output area
	Calculate min/max coordinates along west edge of output area
	Calculate min/max coordinates along south edge of output area
	Calculate min/max coordinates along north edge of output area
 	Return
*****************************************************************************/
#include "worgen.h"
#include "geompak.h"
#include "cproj.h"
#include "proj.h"

int  c_mapedg(seast, eeast, snorth, enorth, inunits, proj_code, zone, 
                  parout, units, pxmin, pxmax, pymin, pymax)

double *seast;		/* Upper left longitude coordinate */
double *eeast;		/* Lower right longitude coordinate */
double *snorth;		/* Upper left latitude coordinate */
double *enorth;		/* Lower right latitude coordinate */
double *pxmin;		/* Projection minimum in X */
double *pxmax;		/* Projection maximum in X */
double *pymin;		/* Projection minimum in Y */
double *pymax;		/* Projection maximum in Y */
double parout[15];	/* Projection parameters; output space */
int *inunits;		/* Input space units */
int *proj_code;	/* Projection code; output space */
int *zone;		/* Projection zone code; output space */
int *units;		/* Projection units code; output space */
{
double delta_east;	/* Longitude increment */
double delta_north;	/* Latitude increment */
double inx, iny;	/* Input space coordinates given to proj() */
double outx, outy;	/* Output space coordinates given to proj() */
double inparm[15];	/* Input projection parameters (constant--all zeros) */
int i;			/* Loop counter */
int geo = 0;		/* Input space projection--always geographic */
int nozone = 62;	/* Input space zone code */
int size = 75;		/* Interval of increment to check for min/max */

/* If input coordinates are given in DMS format, convert to degrees
  ----------------------------------------------------------------*/
if (*inunits == 5)
   {
   if (c_decdeg(seast, "DMS", "LON") != E_SUCC)
      {
      c_errmsg("Error returned from decdeg","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
   if (c_decdeg(eeast, "DMS", "LON") != E_SUCC)
      {
      c_errmsg("Error returned from decdeg","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
   if (c_decdeg(snorth, "DMS", "LAT") != E_SUCC)
      {
      c_errmsg("Error returned from decdeg","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
   if (c_decdeg(enorth, "DMS", "LAT") != E_SUCC)
      {
      c_errmsg("Error returned from decdeg","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
   *inunits = 4;
   }

/* Initialization of variables.
  ---------------------------*/
for (i = 0; i < 15; inparm[i++] = 0.0);
delta_east = (*eeast - *seast) / (size - 1);
delta_north = (*enorth - *snorth) / (size - 1);

*pxmin = 1000000000.0;
*pymin = 1000000000.0;
*pxmax = -1000000000.0;
*pymax = -1000000000.0;

/* Calculate the minimum and maximum coordinates along the east side of 
    the output area
  ----------------*/
for (i = 0; i < size; i++)
   {
   inx = *eeast;
   iny = *enorth - (delta_north * i);
   if (c_proj(&geo, inunits, &nozone, inparm, proj_code, units, zone, parout,
        &inx, &iny, &outx, &outy) != E_SUCC) 
      {
      c_errmsg("Error returned from proj","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }

   if (outx < *pxmin) *pxmin = outx;
   if (outx > *pxmax) *pxmax = outx;
   if (outy < *pymin) *pymin = outy;
   if (outy > *pymax) *pymax = outy;
   }

/* Calculate the minimum and maximum coordinates along the west side of 
    the output area
  ----------------*/
for (i = 0; i < size; i++)
   {
   inx = *eeast - (delta_east * (size - 1));
   iny = *enorth - (delta_north * i);
   if (c_proj(&geo, inunits, &nozone, inparm, proj_code, units, zone, parout,
        &inx, &iny, &outx, &outy) != E_SUCC) 
      {
      c_errmsg("Error returned from proj","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
     
   if (outx < *pxmin) *pxmin = outx;
   if (outx > *pxmax) *pxmax = outx;
   if (outy < *pymin) *pymin = outy;
   if (outy > *pymax) *pymax = outy;
   }


/* Calculate the minimum and maximum coordinates along the south side of 
    the output area
  ----------------*/
for (i = 0; i < size; i++)
   {
   inx = *eeast - (delta_east * i);
   iny = *enorth - (delta_north * (size - 1));
   if (c_proj(&geo, inunits, &nozone, inparm, proj_code, units, zone, parout,
        &inx, &iny, &outx, &outy) != E_SUCC) 
      {
      c_errmsg("Error returned from proj","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
     
   if (outx < *pxmin) *pxmin = outx;
   if (outx > *pxmax) *pxmax = outx;
   if (outy < *pymin) *pymin = outy;
   if (outy > *pymax) *pymax = outy;
   }


/* Calculate the minimum and maximum coordinates along the north side of 
    the output area
  ----------------*/
for (i = 0; i < size; i++)
   {
   inx = *eeast - (delta_east * i);
   iny = *enorth;
   if (c_proj(&geo, inunits, &nozone, inparm, proj_code, units, zone, parout,
        &inx, &iny, &outx, &outy) != E_SUCC)
      {
      c_errmsg("Error returned from proj","mapedg-call",NON_FATAL);
      return(E_FAIL);
      }
     
   if (outx < *pxmin) *pxmin = outx;
   if (outx > *pxmax) *pxmax = outx;
   if (outy < *pymin) *pymin = outy;
   if (outy > *pymax) *pymax = outy;
   }

return(E_SUCC);
}

/****************************************************************
FUNCTION NAME: calcSwathWidth - calculates ground range swath width

SYNTAX:
  calcSwathWidth(double scVec[3], double sr0, double eradcntr, double sr1)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    scVec	double[3]	Spacecraft position vector
    sr0		double		Slant Range to near swath
    eradcntr	double		Earth Radius at center of swath
    sr1		double 		Slant Range to far swath

DESCRIPTION:

     Given the slant range to the near and far edges of a swath,
     the spacecraft position, and the radius of the earth at the 
     center of the swath, calculate the swath width based on 
     the law of cosines.

RETURN VALUE: swath width 

SPECIAL CONSIDERATIONS: make sure that the units for the inputs
			are all the same!  (Typically in km).

PROGRAM HISTORY:
  1.0   T. Logan  4/98	 modified from sr2gr_vec.c routine 

****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "ifm.h"

double calcSwathWidth(meta_parameters *meta, int ns);
double calcSwathWidth(meta_parameters *meta, int ns)
 {
  double r_sc;		/* Range from center or earth to spacecraft */
  double r_earth;       /* Radius of the earth */
  double a,x,y;
  double sr0, sr1;
  double groundRange0;	/* Ground range from nadir to first pixel */
  double groundRange1;	/* Ground range to last pixel from nadir */
  double groundRange;	/* Ground range from first to last pixel */

  /* calculate ground range to first point */
  r_sc = vecMagnitude(meta->stVec->vecs[1].vec.pos);
  r_earth = meta->ifm->er;

  a = (r_sc-r_earth)/r_earth; 
  x = 1.0+a;

  sr0 = meta->geo->slantFirst+meta->geo->slantShift;
  y = sr0/r_earth;
  groundRange0 = r_earth * acos((1.0 + x*x - y*y) / (2.0*x));

  sr1 = sr0 + ns * meta->geo->xPix;
  y = sr1/r_earth;
  groundRange1 = r_earth * acos((1.0 + x*x - y*y) / (2.0*x));

  groundRange = groundRange1 - groundRange0;
 
  return(groundRange);
 }

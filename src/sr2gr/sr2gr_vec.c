/******************************************************************************
NAME:

SYNOPSIS:

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0     11/88  R.E.CARANDE  Fortran Routine MKSL2GR for JPL
    1.0      2/97  T. Logan	C Routine for ASF 
    1.1      3/98  T. Logan     Modified to work from AISP inputs

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
 
    This subroutine constructs the slantrange to groundrange interpolation
    vector.
 
        Input:      ht = distance from Earth center to SAR in meters  (real*4)
                    sr = slant range distance to first interpolation point.
                         This may be a point different from the slant range
                         to the first sampled point due to range migration.
                         Units = meters. (real*4)
                    re = radius of Earth at center swath (mid range) in meters.
                         (real*4)
                 srinc = slant range increment in meters (real*4). This
                         determined by the SAR's A/D sampling rate.
                 grinc = ground range increment in meters (real*4)
                         For ASF = 12.5meters
 
        Output:    sr2gr = 8k (real*4) vector that contains the interpolation
                          points for slant range to ground range conversion.
                          The first element is always 0, which means the first
                          interpolation point is at r_close.  This vector
                          is in units of slant range bins.
 
        Algorithm:  The local Earth surface (within image) is approximated
                    by a sphere of radius r_earth.  This radius may be the
                    local Earth radius as determined by a higher order model,
                    such as an ellipsoid.  (Deviations between the elliptical
                    surface and the sphere, for the purpose of this resampling,
                    are small.)  The following equations are used to determine
                    the resampling vector sr2gr and are inverses of each other:
 
    slant range (sr) as a function of ground range (rg):
 
        sr=sqrt(ht*ht+re*re-2*ht*re*cos(rg/re));
        
    ground range (rg) as a function of slant range (sr):
 
	    rg=re*acos((ht*ht+re*re-sr*sr)/(2*ht*re));
 
    First the ground range at the close slant range is determined from the first equation
    above. This ground range, plus the enumerated increment (grinc) is used
    to calculate the slant range to the interpolation points.  Finally the
    interpolation points are normalized to slant range bins by dividing by
    the slant range bin size, rsinc.
 
 
ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "sr2gr.h"

void sr2gr_vec(meta_parameters *meta,float srinc, float newSize,float *sr2gr)
{
	double rg,rg0;/*Ground range distances from nadir, along curve of earth.*/
	double ht,re,sr;/*S/C height, earth radius, slant range [m]*/
	int    ii;

	ht = meta_get_sat_height(meta, 0, 0);
	re = meta_get_earth_radius(meta, 0, 0);
	sr = meta_get_slant(meta,0,0);

	/* calculate ground range to first point */
	rg0 = re * acos((ht*ht+re*re-sr*sr) / (2*ht*re));
	
	/* begin loop */
	rg = rg0;
	for (ii = 0; ii<MAX_IMG_SIZE; ii++)
	{
		double this_slant = sqrt(ht*ht+re*re-2.0*ht*re*cos(rg/re));
		sr2gr[ii] = (this_slant - sr) / srinc;
		rg += newSize;
	}
}

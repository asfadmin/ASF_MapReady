/******************************************************************************
DESCRIPTION:
    Calculates the ground range pixel that corresponds to each slant
    range pixel in the range direction of the SAR image.  This is done
    by incrementing the slant range distance, calculating the resulting
    ground range distance, and dividing this distance by the ground
    range increment to get the ground range pixel.


HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

 This subroutine constructs the slantrange to groundrange interpolation vector.

        Input:    r_sc = distance from Earth center to SAR in meters  (real*4)
               r_close = slant range distance to first interpolation point.
                         This may be a point different from the slant range
                         to the first sampled point due to range migration.
                         Units = meters. (real*4)
               r_earth = radius of Earth at center swath (mid range) in meters.
                         (real*4)
                 srinc = slant range increment in meters (real*4). This
                         determined by the SAR's A/D sampling rate.
                 grinc = ground range increment in meters (real*4)
                         For ASF = 12.5meters

        Output:    gr2sr = 8k (real*4) vector that contains the interpolation
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
                    the resampling vector gr2sr and are inverses of each other:

    slant range as a function of ground range:

        rground(rslant) = r_earth*acos( (1+(1+a)**2 - (rslant/r_earth)**2)/
                          (2*(1+a))  )

    ground range as a function of slant range:

        rslant(rground) = r_earth*sqrt(2(1+a)*(1-cos(rground/r_earth) )+a*a)

                where a = (r_sc-r_earth)/r_earth

    First the ground range at r_close is determined from the first equation
    above.  Then, the slant range is incremented and the ground range is
    found for each point.  Using the difference in ground range divided
    by the ground range increment, for each slant range pixel, a corresponding
    ground range pixel is derived.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0     11/88  R.E.CARANDE  Fortran Routine MKSL2GR for JPL
    1.0      2/97  T. Logan     C Routine for ASF

******************************************************************************/
#include "gr2sr.h"

void gr2sr_vec(meta_parameters *meta, float *gr2sr)
{
  FILE   *fp;
  char   outfile[40];   /* Output vector file                            */
  int    i;             /* Counter                                       */
  float  r_sc;          /* radius from center of the earth for satellite */
  float  r_earth;       /* radius of the earth                           */
  float  r_close;       /* near slant range distance                     */
  float  rg0, tmp;      /* Ground range to first pixel, G.R. to cur pix  */
  float  a, x, x2, y;   /* temporaries                                   */
  float  srinc, grinc;  /* Slant and Ground range pixel spacings         */
  float  rslant;        /* slant range distance to current pixel         */
  const double speed_of_light = 299792458.0;

  /* Radius from the center of the earth to the spacecraft, slant range to
     first pixel, and radius of the earth */
	r_sc = meta_get_sat_height(meta, 0, 0);
	r_close = meta_get_slant(meta,0,0);
	r_earth = meta_get_earth_radius(meta, 0, 0);

  /* Set the ground range and slant range increments */
  grinc = meta->general->x_pixel_size;
  srinc = speed_of_light / (2.0 * meta->sar->range_sampling_rate);

  printf("r_sc = %f; r_close = %f\n",r_sc,r_close);
  printf("r_earth = %f\n",r_earth);
  printf("grinc = %f; srinc = %f\n",grinc,srinc);

  /* calculate ground range to first point */
  a = (r_sc-r_earth)/r_earth;
  x = 1.0+a;
  x2 = x * x;
  y = r_close/r_earth;
  rg0 = r_earth * acos((1.0 + x2 - y*y) / (2.0*x));

  /* begin loop */
  for(i = 0, rslant = r_close; i<MAX_IMG_SIZE; i++, rslant+=srinc) {
    y = rslant/r_earth;
    tmp = r_earth*acos((1.0+x2-y*y)/(2.0*x));
    gr2sr[i] = (tmp - rg0)/grinc;
  }
/* can likely nuke this
  for (i = 0; i < MAX_IMG_SIZE; i+=64) {
    if (i%512==0)
      printf("\n");
    printf("%8.2f ",gr2sr[i]);
  }
  printf("\n");
  printf("Last %8.2f\n",gr2sr[MAX_IMG_SIZE-1]);
*/
}

/**************************************************************************
FUNCTION NAME:	gr2ml_vec - Computes Ground Range to Slant Range vector

SYNTAX:	gr2ml_vec(char *infile, float *nlooks, float gr2ml[])

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    infile      char * 		input image triplet (.img, .ddr, .L)
    nlooks      float * 	number of looks for output slant range image
    gr2ml[]     float  		Output resampling vector

DESCRIPTION:
    Calculates the slant range pixel that corresponds to each ground
    range pixel.  This is done by simple interpolation based on the
    ratio of the slant range pixel spacing to the ground range pixel
    spacing.

RETURN VALUE:	Returns a vector of ground range to multilooked slant range
		resampling values.

SPECIAL CONSIDERATIONS:

	This program is hardcoded with the single look Azimuth pixel
	spacing of the ERS-1 satellite (3.965) and the standard fullres
	pixel spacing (12.5).

PROGRAM HISTORY:  2/97	T. Logan	Initial Implementation

**************************************************************************/
#include "gr2sr.h"

#define SQR(X) (X*X)

void gr2ml_vec(meta_parameters *meta, float *gr2ml)
{
  double srinc, grinc, sr;
  double speed_of_satellite;
  double vx, vy, vz;
  int ii;

  asfPrintStatus("Calculating the azimuth resampling vector\n");

  /* Calculate the speed of the satellite based on the center state vector */
  if (meta->state_vectors == NULL) {
    asfPrintError("Need state vectors block in metadata for calculation of azimuth resampling vector.\n");
  }
  ii = meta->state_vectors->vector_count / 2 + 1;
  vx = meta->state_vectors->vecs[ii].vec.vel.x;
  vy = meta->state_vectors->vecs[ii].vec.vel.y;
  vz = meta->state_vectors->vecs[ii].vec.vel.z;
  speed_of_satellite = sqrt ( SQR(vx) + SQR(vy) + SQR(vz) );

  /* Set the ground range and slant range increments */
  srinc = speed_of_satellite / meta->sar->prf * meta->sar->look_count;
  grinc = meta->general->y_pixel_size;

  asfPrintStatus("grinc = %f; srinc = %f\n",grinc,srinc);

  for (ii=0, sr=0; ii<MAX_IMG_SIZE; ii++, sr+=srinc) {
    gr2ml[ii] = sr / grinc;
  }
}

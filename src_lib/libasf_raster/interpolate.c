/*******************************************************************
FUNCTION NAME:   interpolate - performs the interpolation to calculate
                 the pixel value e.g. during resampling

INPUT: interpolation  - interpolation method (nearest neighbor, bilinear, sinc)
       inbuf          - input image buffer
       nLines         - number of lines (image buffer)
       nSamples       - number of samples per line (image buffer)
       xLine          - line of interest
       xSample        - sample position within line
       weighting      - weighting function to be applied (Kaiser, Hamming)
       nKernel        - kernel size for sinc function
*******************************************************************/
#include <assert.h>
#include <math.h>

#include <glib.h>
#include <gsl/gsl_sf_bessel.h>
#include "asf.h"
#include "asf_raster.h"
#include "float_image.h"

#define SQR(X) ((X)*(X))

// Some parameters and weights for bicubic interpolation
#define CUBE(X) ((X)*(X)*(X))
#define P(X) ( X > 0  ? X : 0 )

// For sinc interpolation, the goal is to weight source values using a
// sinc function.  We want to position the peak of the sinc at the
// point of interest.  The sinc filter itself will interpolate a small
// number of point, perhaps 8 or 16, for example.  In order to achieve
// the desired location of the peak, we want to introduce a shift in
// the sinc function computed.  However, we don't want to recompute
// the sinc function for every point to be interpolated!  The solution
// is to precompute an array of slightly shifted sinc functions, and
// then choose the appropriate one for the point we are interpolating.
// For example, if we have known values for 1 and 2, and we want to
// interpolate a value for 1.2, we choose the (1.2 - 1.0) * (2 - 1) *
// NUM_SINCS sinc function to evaluate the result.
#define NUM_SINCS 512 // Number of sinc functions to compute.

// For convenience using buffered image functions
#define GET_PIXEL(x, y) float_image_get_pixel (inbuf, x, y)
#define SET_PIXEL(x, y, value) float_image_set_pixel (inbuf, x, y, value)

// Fetch pixel ii, jj from inbuf, where inbuf is taken to be an image
// nSamples wide and nLines high.  If ii, jj is outside the image, a
// "reflected" value is returned.  For example, these two calls will
// return the same pixel:
//
//     get_pixel_with_reflection (inbuf, sample_count, line_count, -1, -2) 
//     get_pixel_with_reflection (inbuf, sample_count, line_count, 1, 2);
//
// If the reflected indicies are still outside the image, an exception
// is triggered.
static float
get_pixel_with_reflection (float *inbuf, int nSamples, int nLines, int sample,
			   int line)
{
  int ii = sample;		// Convenience alias.
  int jj = line;		// Convenience alias.
  // Handle reflection at image edges.
  if ( G_UNLIKELY (ii < 0) ) { 
    ii = -ii; 
  }
  else if ( G_UNLIKELY (ii >= nSamples) ) { 
    ii = ii - (ii - nSamples + 1) - 1; 
  }
  if ( G_UNLIKELY (jj < 0) ) { 
    jj = -jj; 
  }
  else if ( G_UNLIKELY (jj >= nLines) ) { 
    jj = jj - (jj - nLines + 1) - 1; 
  }
  // Now we better be in the image.
  assert (ii >= 0 && ii < nSamples);
  assert (jj >= 0 && jj < nLines);

  return inbuf[nSamples * jj + ii];
}

void samples2coefficients(FloatImage *inbuf, char dimension)
{
  int ii, kk;
  float z = sqrt(3.0) - 2.0;
  float lambda = (1.0 - z) * (1.0 - 1.0/z);
  float value;

  // Apply overall gain
  for (ii=0; ii<inbuf->size_x; ii++)
    for (kk=0; kk<inbuf->size_y; kk++) {
      value = GET_PIXEL(ii,kk) * lambda;
      SET_PIXEL(ii,kk,value);
    }

  for (kk=0; kk<inbuf->size_y; kk++) {
    // Causal initialization (first pixel of line)
    
  }

  // Causal recursion
  
  // Anticausal initialization
  
  // Anticausal recursion
  

}

float interpolate(interpolate_type_t interpolation, FloatImage *inbuf, float yLine, 
		  float xSample, weighting_type_t weighting, int sinc_points)
{
  int ix, iy, ii, kk, ll, xI[4], yI[4];
  float a00, a10, a01, a11;
  float value, dx, dy, wx[4], wy[4], w;
             
  /* Get closed pixel position to start from */

  switch ( interpolation ) 
    {
    case NEAREST:
      ix = (int) (xSample + 0.5);
      iy = (int) (yLine + 0.5);
      value = GET_PIXEL(ix,iy);
      break;

    case BILINEAR:
      assert (xSample >= 0.0);
      assert (yLine >= 0.0);
      assert (xSample <= inbuf->size_x - 1);
      assert (yLine <= inbuf->size_y - 1);
      ix = floor(xSample);
      iy = floor(yLine);
      
      a00 = GET_PIXEL(ix,iy);
      a10 = GET_PIXEL(ix+1,iy) - GET_PIXEL(ix,iy);
      a01 = GET_PIXEL(ix,iy+1) - GET_PIXEL(ix,iy);
      a11 = GET_PIXEL(ix,iy) - GET_PIXEL(ix+1,iy) - GET_PIXEL(ix,iy+1) 
	+ GET_PIXEL(ix+1,iy+1);
      value = (a00 + a10 * (xSample - ix) + a01 * (yLine - iy) 
	       + a11 * (xSample - ix) * (yLine - iy));
      break;

    case BICUBIC:
      assert (xSample >= 0.0);
      assert (yLine >= 0.0);
      assert (xSample <= inbuf->size_x - 1);
      assert (yLine <= inbuf->size_y - 1);
      ix = floor(xSample);
      iy = floor(yLine);
      dx = xSample - ix;
      dy = yLine - iy;
      
      // Calculating weights
      for (ii=-1; ii<=2; ii++) {
	wx[ii+1] = 1.0/6.0 * (CUBE(P(ii-dx+2)) - 4*CUBE(P(ii-dx+1)) 
			      + 6*CUBE(P(ii-dx)) - 4*CUBE(P(ii-dx-1)));
	wy[ii+1] = 1.0/6.0 * (CUBE(P(dy-ii+2)) - 4*CUBE(P(dy-ii+1)) 
			      + 6*CUBE(P(dy-ii)) - 4*CUBE(P(dy-ii-1)));
      }
      
      // Get the interpolated pixel value
      value = 0.0;
      for (ii=-1; ii<=2; ii++)
	for (kk=-1; kk<=2; kk++)
	  value += GET_PIXEL(ix+ii,iy+kk) * wx[ii+1] * wy[kk+1];
      
      break;
      
    case SPLINES:
      // Determine B-Spline coefficients.
      // This is a essentially a pre-filtering applied to the entire image 
      // that converts the image from a sample representation into a 
      // representation based on B-spline coefficients. 
      // Without this step the result would look blurry at the end.

      // Convert samples to coefficients in x direction
      samples2coefficients(inbuf, 'x');

      // Convert samples to coefficients in y direction
      samples2coefficients(inbuf, 'y');

      // Calculate interpolation indices
      ii = (int)floor(xSample) - 1;
      kk = (int)floor(yLine) - 1;
      for (ll=0; ll<=3; ll++) {
	xI[ll] = ii++;
	yI[ll] = kk++;
      }

      // Calculate interpolation weights
      w = xSample - (float)xI[1];
      wx[3] = (1.0/6.0)*CUBE(xSample);
      wx[0] = (1.0/6.0) + (1.0/2.0)*w*(w-1.0) - wx[3];
      wx[2] = w + wx[0] - 2.0*wx[3];
      wx[1] = 1.0 - wx[0] - wx[2] - wx[3];
      w = yLine - (float)yI[1];
      wy[3] = (1.0/6.0)*CUBE(xSample);
      wy[0] = (1.0/6.0) + (1.0/2.0)*w*(w-1.0) - wy[3];
      wy[2] = w + wy[0] - 2.0*wy[3];
      wy[1] = 1.0 - wy[0] - wy[2] - wy[3];

      // Get interpolation value
      value = 0.0;
      for (kk=0; kk<=3; kk++) {
	w = 0.0;
	for (ii=0; ii<=3; ii++)
	  w += wx[ii] * 1;
	value += wy[kk] * w;
      }
      break;
    case SINC:
      break;
    default:
      assert (FALSE);
      break;
  }

  return value;
}

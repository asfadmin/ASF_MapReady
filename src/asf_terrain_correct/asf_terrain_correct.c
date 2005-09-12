// The essential function of this program is to take a SAR image,
// complete with information about the orbital geometry from which it
// was acquired, and use it to color the pixels of a map projected
// digital elevation model (DEM) with radar backscatter values.

// Standard headers.
#include <complex.h>
#include <stdlib.h>

// Headers from external libraries.
#include <fftw3.h>
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_vector.h>

// Headers from custom ASF libraries and code in this package.
#include "ITRS_platform_path.h"
#include "ITRS_point.h"
#include <asf_meta.h>
#include "earth_constants.h"
#include <libasf_proj.h>
#include "map_projected_dem.h"
#include "orbital_state_vector.h"
#include "platform_path.h"
#include "progress_meter.h"
#ifdef BK_DEBUG
#  include <scratchplot.h>
#endif
#include "slant_range_image.h"
#include "dem_geom_info.h"
#include "lsm.h"

// Print user information to error output location and exit with a
// non-zero exit code.
static void
usage (void)
{
  g_printerr ("usage: asf_terrain_correct dem_base_name image_base_name "
	      "output_base_name\n");
  exit (EXIT_FAILURE);
}

// Convenience functions so we don't have to type long method names.
static void
SP (FloatImage *i, ssize_t xi, ssize_t yi, float value)
{
  float_image_set_pixel (i, xi, yi, value);
}
static float
GP (FloatImage *i, ssize_t xi, ssize_t yi)
{
  return float_image_get_pixel (i, xi, yi);
}

// Factors for going between degrees and radians.
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	0.0174532925199432958

// Marshalled form of arguments that need to go to a callback (see
// below).
typedef struct {
  Vector *target;
  ITRSPlatformPath *pp;
} target_distance_params;

// Range function we want to minimize.  Returns the distance in meters
// between the platform represented by ((target_distance_params *)
// params)->pp at time and target ((target_distance_params *)
// params)->arget.
static double 
target_distance (double time, void *params)
{
  Vector *target = ((target_distance_params *) params)->target;
  ITRSPlatformPath *pp = ((target_distance_params *) params)->pp;

  static Vector platform_position;
  ITRS_platform_path_position_at_time (pp, time, &platform_position);

  static Vector difference;
  vector_set (&difference, platform_position.x, platform_position.y, 
	      platform_position.z);
  vector_subtract (&difference, target);

  return vector_magnitude (&difference);
}

// Compute a score that increases as the autocorrelation of square
// image under rotation increases.  The score is nothing standard, but
// is useful for comparing to the scores of other images.  The basic
// idea here is to use the fact that since mountains are bright on one
// side, they decorrelate nicely under rotation.  This lets us find
// the nice mountainous regions in SAR images simulated from DEMs that
// we expect will coregister well with real SAR images (since radar
// backscatter is determined mostly from terrain incidence angle only
// in steeper regions, and is dominated by land cover radiometric
// characteristics elsewhere).
static long double
rotational_autocorrelation_score (FloatImage *image)
{
  g_assert (image->size_x == image->size_y);

  FloatImage *im = image;	// Convenience alias.

  size_t sz = im->size_x;	// Convenience alias.

  // Form normalized version of the image (pixels sum to 1.0).
  FloatImage *ni = float_image_new (sz, sz);
  double pixel_sum = 0;
  size_t ii, jj;
  for ( ii = 0 ; ii < sz; ii++ ) {
    for ( jj = 0 ; jj < sz ;  jj++ ) {
      pixel_sum += GP (im, jj, ii);
    }
  }
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ;  jj++ ) {
      SP (ni, jj, ii, GP (im, jj, ii) / pixel_sum);
    }
  }

  // We will sample the images every sample_stride, rather than
  // computing correlations based on every pixel in the image.
  const int sample_stride = 3;

  // Compute unrotated autocorrelation as the sum of pixel values
  // squared, for a sparse subset of pixels.
  long double correlation_0 = 0;
  for ( ii = 0 ; ii < sz ; ii += sample_stride ) {
    for ( jj = 0 ; jj < sz ; jj += sample_stride ) {
      correlation_0 += pow (GP (ni, jj, ii), 2);
    }
  }

  // Form a version of the normalized image tile rotated clockwise
  // ninety degrees.
  FloatImage *rni = float_image_new (sz, sz);
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      SP (rni, jj, ii, GP (ni, ii, sz - jj - 1));
    }
  }

  // Compute the correlation between the rotated and unrotated
  // normalized images.
  long double correlation_90 = 0;
  for ( ii = 0 ; ii < sz ; ii += sample_stride ) {
    for ( jj = 0 ; jj < sz ; jj += sample_stride ) {
      correlation_90 += GP (ni, jj, ii) * GP (rni, jj, ii);
    }
  }  

  // We are done with the intermediate images.
  float_image_free (ni);
  float_image_free (rni);

  // The score we are interested in is hopefully ordered about the
  // same as this ratio of characteristics.
  return correlation_90 / correlation_0;
}

// Type to hold some information about a tile of a SAR image, a
// corresponding tile of a simulated image, and a score indicating the
// autocorrelation of the simulated image tile with itself under
// rotation.
typedef struct {
  // Rotational autocorrelation score (see above).
  long double ras;
  FloatImage *sim_img;		// Simulated image tile.
  FloatImage *sri_img;		// Slant range SAR image tile.
  size_t txi, tyi;		// x and y indicies of tile in image.
} image_tile_record_type;

// Compare the autocorrelation scores of *a and *b, returning -1 if
// (*a)->ras < (*b)->ras, 0 if (*a)->ras == (*b)->ras, or 1 if
// (*a)->ras > (*b)->ras.
static gint
compare_rotational_autocorrelation_scores (image_tile_record_type **a,
					   image_tile_record_type **b)
{
  if ( (*a)->ras < (*b)->ras ) {
    return -1;
  }
  else if ( (*a)->ras > (*b)->ras ) {
    return 1;
  }
  else {
    return 0;
  }
}

// Create a 2D Kaiser window image of size size with smoothing
// coefficient beta.  The weights of the window are
//
//      w(x, y) 
//        = I0 (beta * sqrt (1 - pow (2 * hypot (x - (size - 1.0) / 2.0,
//						 y - (size - 1.0) / 2.0)
//                                    / hypot (size, size), 2))
//          / I0 (beta)
//
// Where I0 is the regular modied cylindrical Bessel function of
// zeroth order.
//
// This window is useful for avoiding edge effects in FFTs and the like.
//
// The returned FloatImage must be released with float_image_free when
// no longer needed.
static FloatImage *
kaiser_window (size_t size, double beta)
{
  FloatImage *result = float_image_new (size, size);
  size_t ii, jj;
  for ( ii = 0 ; ii < size ; ii++ ) {
    for ( jj = 0 ; jj < size ; jj++ ) {
      // This value must stay greater than or equal to zero.  So we
      // make sure floating point problems don't cause it to fail to
      // do so.
      double tmp 
	= 1.0 - pow (2.0 * hypot ((jj - (size - 1.0) / 2.0),
				  (ii - (size - 1.0) / 2.0))
		     / hypot (size, size),
		     2.0);
      if ( G_UNLIKELY (tmp < 0.0) ) {
	tmp = 0.0;
      }
      SP (result, jj, ii, (gsl_sf_bessel_I0 (beta * sqrt (tmp))
			   / gsl_sf_bessel_I0 (beta)));
    }
  }
  
  return result;
}

// Multiply the pixels of self by the pixels of other.  For
// convenience, return a pointer to self.
static FloatImage *
float_image_multiply (FloatImage *self, FloatImage *other)
{
  g_assert (self->size_x == other->size_x);
  g_assert (self->size_y == other->size_y);

  size_t sz_x = self->size_x, sz_y = self->size_y;

  size_t ii, jj;
  for ( ii = 0 ; ii < sz_y ; ii++ ) {
    for ( jj = 0 ; jj < sz_x ; jj++ ) {
      SP (self, jj, ii, GP (self, jj, ii) * GP (other, jj, ii));
    }
  }

  return self;
}


// Given poins (x[0], y[0]), (x[1], y[1]), and (x[2], y[2]), return
// coefficients a, b, and d of parabolic fit y = ax^2 + bx + c.  This
// routine is called 'bad_fit_parabola' because it doesn't seem to
// work very well when the three points being fit are close to one
// another and far from zero.  Probably there is some sensitivity that
// kill us when using the naive algebraic solution used here.  It may
// fail in other cases as well.  But for finding the peak of the
// parabola which intersects three evenly space points around zero it
// works fine, and that's all we need.
static void
bad_fit_parabola (double *a, double *b, double *c, double x[3], double y[3])
{
  // Try to catch stupid errors (may still fail to catch them due to
  // floating point wierdness -- a float doesn't necessarily even
  // compare equal to itself, depending on whether its stored in
  // memory or in a register).
  g_assert (x[0] != x[1]);
  g_assert (x[0] != x[2]);
  g_assert (x[1] != x[2]);

  *a = ((y[0] - y[2] - ((y[1] - y[2]) / (x[1] - x[2])) * (x[0] - x[2]))
	/ ((pow (x[0], 2) - pow (x[2], 2)) + ((-pow (x[1], 2) + pow (x[2], 2))
					      * (x[0] - x[2])
					      / (x[1] - x[2]))));
  *b = ((y[1] - y[2] + *a * (-pow (x[1], 2), pow (x[2], 2)))
	/ (x[1] - x[2]));
  *c = y[2] - *a * pow (x[2], 2) - *b * x[2];
}

// Find the offset of square image b from square image a.  The images
// must overlatp by at least 50%.  After the first call, this funcion
// keeps a some image-sized chunks of memory lying around -- you might
// want to look into it if you are trying to coregister huge images.
// Images a and b must be equal sized.  The output values x_offset and
// y_offset are the distances that the pixels of b are offset from the
// pixels of a, in the FloatImage coordinate directions (i.e. top left
// pixel is (0, 0), and x increases as we move right and y increases
// as we move down).  The coregistration is done in memory using the
// fftw library.  On success 0 is returned, otherwise nonzero is
// returned.  FIXME: at least for SAR images, this routine doesn't
// work very well (it doesn't give the same results as brute force
// coregistration).
static int
coregister (double *x_offset, double *y_offset, FloatImage *a, FloatImage *b)
{
  g_assert (a->size_x == a->size_y);
  g_assert (b->size_x == b->size_y);
  g_assert (a->size_x == b->size_x);

  size_t sz = a->size_x;	// Convenience alias.

  // Discrete FFTs have the effect of making the data look periodic
  // when we go to correlate things, which is bad.  In order to ensure
  // that we the coregistered tiles are matching wrapped version of
  // themselves, we pad them both out with an image worth of zeros all
  // around.
  size_t psz = 3 * sz;

  // We need to window the input data to avoid edge effects when we
  // zero pad the input images.  The value of beta determines how
  // quicly the window rolls off (i.e. how much of and how much the
  // edges are attenuated).
  double beta = 8;
  FloatImage *kw = kaiser_window (sz, beta);
  FloatImage *a_w = float_image_copy (a);   // Windowed version of image a.
  float_image_multiply (a_w, kw);
  FloatImage *b_w = float_image_copy (b);   // Windowed version of image b.
  float_image_multiply (b_w, kw);

  // FIXME: fftw supports real-data-specific trasforms which exploit
  // hermitian symetry, in-place transforms, better performance at
  // certain transform sizes, etc., but these are a pain to sort out
  // and I'm lazy.

  // Allocate memory and fftw transform plans, if required, or reuse
  // existing ones if the images have the same dimensions as last time
  // through.  The uses of these memory regions are described when
  // they are used, rather than here.
  static ssize_t a_sz_x = -1, a_sz_y = -1, b_sz_x = -1, b_sz_y = -1;
  static fftw_complex *pwa_in = NULL;
  static fftw_complex *pwb_in = NULL;
  static fftw_complex *pwa_out = NULL;
  static fftw_complex *pwb_out = NULL;
  static fftw_complex *pwb_out_conj = NULL;
  static fftw_complex *pwa_by_pwb_conj_freq = NULL;
  static fftw_complex *pwa_correlate_pwb = NULL;
  static fftw_plan pwa_plan, pwb_plan, reverse_plan;
  static FloatImage *pwa = NULL, *pwb = NULL, *correlation_image = NULL;
  // If the dimensions this time aren't the same as last time, we have
  // to (re)allocate the memory and plan resources.
  g_assert (a->size_x <= SSIZE_MAX && a->size_y <= SSIZE_MAX 
	    && b->size_x <= SSIZE_MAX && b->size_y <= SSIZE_MAX);
  if ( !((ssize_t) a->size_x == a_sz_x 
	 && (ssize_t) a->size_y == b_sz_y 
	 && (ssize_t) b->size_x == b_sz_x 
	 && (ssize_t) b->size_y == b_sz_y) ) {
    // If this isn't out first time through, 
    if ( pwa_in != NULL ) {
      // free all the existing memory and plan resources.
      fftw_free (pwa_in);
      fftw_free (pwb_in);
      fftw_free (pwa_out);
      fftw_free (pwb_out);
      fftw_free (pwb_out_conj);
      fftw_free (pwa_by_pwb_conj_freq);
      fftw_free (pwa_correlate_pwb);
      fftw_destroy_plan (pwa_plan);
      fftw_destroy_plan (pwb_plan);
      fftw_destroy_plan (reverse_plan);
      float_image_free (pwa);
      float_image_free (pwb);
      float_image_free (correlation_image);
    }
    size_t array_size = psz * psz * sizeof (fftw_complex);
    pwa_in = fftw_malloc (array_size);
    pwb_in = fftw_malloc (array_size);
    pwa_out = fftw_malloc (array_size);
    pwb_out = fftw_malloc (array_size);
    pwb_out_conj = fftw_malloc (array_size);
    pwa_by_pwb_conj_freq = fftw_malloc (array_size);
    pwa_correlate_pwb = fftw_malloc (array_size);
    pwa_plan = fftw_plan_dft_2d (psz, psz, pwa_in, pwa_out, FFTW_FORWARD,
			       FFTW_MEASURE);    
    pwb_plan = fftw_plan_dft_2d (psz, psz, pwb_in, pwb_out, FFTW_FORWARD, 
				 FFTW_MEASURE);
    reverse_plan = fftw_plan_dft_2d (psz, psz, pwa_by_pwb_conj_freq, 
				     pwa_correlate_pwb, FFTW_BACKWARD,
				     FFTW_MEASURE);
    pwa = float_image_new_with_value (psz, psz, 0.0);
    pwb = float_image_new_with_value (psz, psz, 0.0);
    correlation_image = float_image_new (psz, psz);      
    a_sz_x = a->size_x;
    a_sz_y = a->size_y;
    b_sz_x = b->size_x;
    b_sz_y = b->size_y;
  }

  // Form the padded versions of the windowed a and b images by
  // writing the pixels of a and b into the middle of the existing
  // field of zeros.
  size_t ii, jj;
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      SP (pwa, jj + sz, ii + sz, GP (a_w, jj, ii));
      SP (pwb, jj + sz, ii + sz, GP (b_w, jj, ii));
    }
  }

  // Done with the windowed versions of the input.
  float_image_free (a_w);
  float_image_free (b_w);

  // Write padded images into fftw-ready memory.
  for ( ii = 0 ; ii < psz ; ii++ ) {
    for ( jj = 0 ; jj < psz ; jj++ ) {
      pwa_in[psz * ii + jj] = GP (pwa, jj, ii) + I * 0.0;
      pwb_in[psz * ii + jj] = GP (pwb, jj, ii) + I * 0.0;
    }
  }

  // Transform the padded input images.
  fftw_execute (pwa_plan);
  fftw_execute (pwb_plan);

  // Conjugate b_out.
  for ( ii = 0 ; ii < psz ; ii++ ) {
    for ( jj = 0 ; jj < psz ; jj++ ) {
      size_t ci = psz * ii + jj;   // Current index.
      pwb_out_conj[ci] = creal (pwb_out[ci]) - cimag (pwb_out[ci]);
    }
  }

  // Perform frequency domain multiplication.
  for ( ii = 0 ; ii < psz ; ii++ ) {
    for ( jj = 0 ; jj < psz ; jj++ ) {
      pwa_by_pwb_conj_freq[psz * ii + jj] = (pwa_out[psz * ii + jj] 
					     * pwb_out_conj[psz * ii + jj]);
    }
  }

  // Perform reverse transformation on the result of the frequency
  // domain multiplication.
  fftw_execute (reverse_plan);

  // Form correlation image..
  for ( ii = 0 ; ii < psz ; ii++ ) {
    for ( jj = 0 ; jj < psz ; jj++ ) {
      SP (correlation_image, jj, ii, 
	  (float) creal (pwa_correlate_pwb[psz * ii + jj]));
    }
  }

  // The correlation image needs to be normalized such that when we
  // look for the peak, favor is not given to offsets which feature
  // more overlapping pixels.  We will only be searching for matches
  // with overlaps of at least 50%, so we only have to normalize half
  // of the padded image corners.
  FloatImage *nci = float_image_copy (correlation_image);
  // FIXME Just so we can take good look at the correlation image, we
  // pseudo-normalize the whold image by dividing everything by the
  // largest normalization factor, then undo this with a multiply for
  // the pixels that are actually getting normalized for the degree of
  // overlap.  This isn't needed except to keep export_as_jpeg method
  // working correctly.
  float tmin, tmax, tmean, tsdev;
  float_image_statistics (correlation_image, &tmin, &tmax, &tmean, &tsdev,
			  NAN);
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      SP (nci, jj, ii, tmean / ((sz - 1) * (sz - 1)));
    }
  }
  for ( ii = 0 ; ii < sz / 2 + 1 ; ii++ ) {
    for ( jj = 0 ; jj < sz / 2 + 1 ; jj++ ) {

      // FIXME: we stupidly used size_t for the normalization factor
      // below, so we better not overflow that type with the square of
      // the chip size on a side.
      g_assert ((double) sz * sz <= SIZE_MAX);
      // FIXME: we have here the stupid pseudo-normalization factor
      // referred to above (so we can undo it for pixels that are
      // actually getting normalized).
      double pnf = ((double) sz - 1) * (sz - 1);

      // Normalization factor for pixels in the upper left corner of
      // the correlation image.  This is the number of pixels of
      // overlap we have between the images being correlated for this
      // offset.
      size_t nf = (sz - jj) * (sz - ii);
      // Unnormalized pixel value.
      float upv = pnf * GP (correlation_image, jj, ii);
      float npv = upv / nf;	// Normalized pixel value.
      SP (nci, jj, ii, npv);	// Set normalized correlation image pixel.

      // Analogous to the above, for the upper right corner.
      nf = (sz - jj - 1) * (sz - ii);
      upv = pnf * GP (correlation_image, psz - jj - 1, ii);
      npv = upv / nf;
      SP (nci, psz - jj - 1, ii, npv);

      // Analogous to the above, for the lower right corner.
      nf = (sz - jj - 1) * (sz - ii - 1);
      upv = pnf * GP (correlation_image, psz - jj - 1, psz - ii - 1);
      npv = upv / nf;
      SP (nci, psz - jj - 1, psz - ii - 1, npv);

      // Analogous to the above, for the lower left corner.
      nf = (sz - jj) * (sz - ii - 1);
      upv = pnf * GP (correlation_image, jj, psz - ii - 1);
      npv = upv / nf;
      SP (nci, jj, psz - ii - 1, npv);
    }
  }

  float_image_export_as_jpeg (nci, "nci.jpg", psz, 0.0);

  // Find the largest pixel of the normalized part of the correlation
  // image.
  float normalized_correlation_max = -FLT_MAX;
  // Location of correlation peak, with compiler reassurance
  // initializers.
  size_t normalized_max_x = -1, normalized_max_y = -1;	
  for ( ii = 0 ; ii < sz / 2 + 1 ; ii++ ) {
    for ( jj = 0 ; jj < sz / 2 + 1 ; jj++ ) {
      // Search for a peak in this pixel in the upper left corner
      float pv = float_image_get_pixel (nci, jj, ii);
      if ( pv > normalized_correlation_max ) {
	normalized_correlation_max = pv;
	normalized_max_x = jj;
	normalized_max_y = ii;
      }

      // Search for a peak in the upper right corner.
      /* These searches are commented out because they aren't needed
      pv = float_image_get_pixel (nci, psz - jj - 1, ii);
      if ( pv > normalized_correlation_max ) {
	normalized_correlation_max = pv;
	normalized_max_x = psz - jj - 1;
	normalized_max_y = ii;
      }
      */

      // Search for a peak in the lower right corner.
      /*
      pv = float_image_get_pixel (nci, psz - jj - 1, psz - ii - 1);
      if ( pv > normalized_correlation_max ) {
	normalized_correlation_max = pv;
	normalized_max_x = psz - jj - 1;
	normalized_max_y = psz - ii - 1;
      }
      */

      // Search for a peak in the lower left corner.
      pv = float_image_get_pixel (nci, jj, psz - ii - 1);
      if ( pv > normalized_correlation_max ) {
	normalized_correlation_max = pv;
	normalized_max_x = jj;
	normalized_max_y = psz - ii - 1;
      }
    }
  }

  // Get the indicies of the pixels on the four sides of the pixel
  // with the maximum value in the correlation image.  We may wrap
  // around an image edge in doing this, which is ok.
  ssize_t ia = normalized_max_y - 1; // Index above.
  if ( ia == -1 ) { ia = psz - 1; }
  ssize_t ib = normalized_max_y + 1; // Index below.
  g_assert (psz < SSIZE_MAX);
  if ( ib == (ssize_t) psz ) { ib = 0; }
  ssize_t il = normalized_max_x - 1; // Index left.
  if ( il == -1 ) { il = psz - 1; }
  ssize_t ir = normalized_max_x + 1; // Index right.
  if ( ir == (ssize_t) psz ) { ir = 0; }

  // We want to find the peak of a parabola fitted to the maximum
  // pixel and its two neighbors, in each dimension.  The locations of
  // the peaks of the parabolas will be considered the best estimation
  // of the optimal match location.
  double *index = g_new (double, 3);
  double *value = g_new (double, 3);
  double d, e, f;		// Coefficients of parabola.
  // The pixels are evenly spaced, and our parabolic fit function
  // works best for argument values, so we solve things around zero
  // and then adjust back to the true index position later.
  index[0] = -1.0;
  index[1] = 0.0;
  index[2] = 1.0;
  value[0] = GP (nci, il, normalized_max_y);
  value[1] = normalized_correlation_max;
  value[2] = GP (nci, ir, normalized_max_y);
  bad_fit_parabola (&d, &e, &f, index, value);
  // Location of peak of parabola in x direction.
  double peak_x = -e / (2 * d) + normalized_max_x;
  value[0] = GP (nci, normalized_max_x, ia);
  value[1] = normalized_correlation_max;
  value[2] = GP (nci, normalized_max_x, ib);
  bad_fit_parabola (&d, &e, &f, index, value);  
  // Location of peak of parabola in y direction.
  double peak_y = -e / (2 * d) + normalized_max_y;
  g_free (index);
  g_free (value);

  // If the parabolic peak interpolation worked, the peak better be
  // somewhere between the neighboring indicies in both directions.
  g_assert (peak_x >= -1.0);
  g_assert (peak_x <= 1.0);
  g_assert (peak_y >= -1.0);
  g_assert (peak_y <= 1.0);

  // FIXME: for debugging purposes, we take a look at the "normalized"
  // correlation image.  Note that since the nci isn't actually
  // normalized everywhere, there is goind to be quite a bit of
  // saturation.
  float_image_export_as_jpeg (nci, "correlation.jpg", sz, 0.0);

  float_image_free (nci);

  // Fill in the discovered offsets.
  if ( peak_x <= psz / 2 ) {
    *x_offset = -peak_x;
  }
  else {
    *x_offset = psz - peak_x;
  }
  if ( peak_y <= psz / 2 ) {
    *y_offset = -peak_y;
  }
  else {
    *y_offset = psz - peak_y;
  }

  return 0;			// Return success code.
}

// Add up all term_count terms in terms_to_sum in place, in pairwise
// fashion to avoid adding big and small numbers and losing accuracy.
// Note that the terms_to_sum array is permuted continually, and in
// the end only terms_to_sum[0] will have any meaning (it will be
// equal to the return value).
static double 
sum_terms (double *terms_to_sum, size_t term_count)
{
  while ( term_count > 1 ) {
    size_t ii;
    for ( ii = 0 ; ii < term_count / 2 ; ii++ ) {
      terms_to_sum[ii] = terms_to_sum[ii * 2] + terms_to_sum[ii * 2 + 1];
    }
    if ( term_count % 2 == 1 ) {
      terms_to_sum[ii - 1] += terms_to_sum[ii * 2];
    }
    term_count /= 2;
  }

  return terms_to_sum[0];
}

// Brute force coregistration of a cropped version of image b with
// image a.  This routine is like coregister, but it used brute force
// correlation instead of FFTs, doesn't use all of image b, only
// searches for offset up to 20 pixels, doesn't use normalization, and
// doesn't bother doing parabolic peak interpolation.  It seems to
// give the same results as bf_coregister, below.
static int
bf_coregister_b_cropped (double *x_offset, double *y_offset, FloatImage *a,
			 FloatImage *b)
{
  g_assert (a->size_x <= SSIZE_MAX);
  ssize_t sz = a->size_x;
  g_assert (sz == (ssize_t) a->size_y);
  g_assert (b->size_x == a->size_x);
  g_assert (b->size_y == a->size_y);

  // Form a cropped zero padded version of image b.
  FloatImage *b_cropped = float_image_new (b->size_x, b->size_y);
  ssize_t ii, jj;
  g_assert (sz < SSIZE_MAX);
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      if ( jj < 20 || jj >= sz - 20 || ii < 20 || ii >= sz - 20 ) {
	SP (b_cropped, jj, ii, 0.0);
      }
      else {
	SP (b_cropped, jj, ii, GP (b, jj, ii));
      }
    }
  }

  FloatImage *mc = float_image_new (40, 40);

  double *terms_to_sum = g_new (double, sz * sz);
  size_t current_term = 0;

  for ( ii = -20 ; ii < 20 ; ii++ ) {
    for ( jj = -20 ; jj < 20 ; jj++ ) {
      ssize_t kk, ll;
      for ( kk = 0 ; kk < sz ; kk++ ) {
	for ( ll = 0 ; ll < sz ; ll++ ) {
	  float p1;
	  if ( jj + ll >= 0 && jj + ll < sz && ii + kk >= 0 && ii + kk < sz ) {
	    p1 = GP (a, jj + ll, ii + kk);
	  }
	  else {
	    p1 = 0.0;
	  }
	  float p2 = GP (b_cropped, ll, kk);
	  terms_to_sum[current_term] = p1 * p2;
	  current_term++;
	}
      }
      double sum = sum_terms (terms_to_sum, current_term);
      SP (mc, jj + 20, ii + 20, sum);
      current_term = 0;
    }
  }

  g_free (terms_to_sum);

  // Maximum correlation pixel value.
  float mcpv = -FLT_MAX;
  int mcpx = -1, mcpy = -1;	// Compiler reassurance.
  g_assert (mc->size_y <= INT_MAX);
  for ( ii = 0 ; ii < (int) mc->size_y ; ii++ ) {
    g_assert (mc->size_x <= INT_MAX);
    for ( jj = 0 ; jj < (int) mc->size_x ; jj++ ) {
      float cp = GP (mc, jj, ii);
      if ( cp > mcpv ) {
	mcpv = cp;
	mcpx = jj;
	mcpy = ii;
      }
    }
  }

  do {
    float_image_free (mc); mc = NULL;
  } while ( 0 );

  // Return offsets of tile b with respect to tile a.
  *x_offset = 20 - mcpx;
  *y_offset = 20 - mcpy;

  return 0;			// Return success code.
}

/*  Brute forc coregistration of image a and image b, with
    normalization.  This is like bf_coregister_b_cropped, except b
    isn't cropped, normalization is used, and a wider area is searched
    for a peak.
static int
bf_coregister (double *x_offset, double *y_offset, FloatImage *a,
	       FloatImage *b)
{
  size_t sz = a->size_x;

  g_assert (sz % 2 == 0);

  FloatImage *mc = float_image_new (sz, sz);

  size_t ii, jj;
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      int offx = jj - sz / 2;
      int offy = ii - sz / 2;
      size_t kk, ll;
      long double sum = 0;
      for ( kk = 0 ; kk < sz - abs (offy) - 1 ; kk++ ) {
	for ( ll = 0 ; ll < sz - abs (offx) - 1 ; ll++ ) {
	  float p1 = GP (a, offx < 0 ? ll : offx + ll, 
			    offy < 0 ? kk : offy + kk);
	  if ( (offx < 0 ? -offx + (int) ll : (int) ll) < 0
	       || (offx < 0 ? -offx + (int) ll : (int) ll) >= (int) sz ) {
	    g_assert_not_reached ();
	  }
	  if ( (offy < 0 ? -offy + (int) kk : (int) kk) < 0
	       || (offy < 0 ? -offy + (int) kk : (int) kk) >= (int) sz ) {
	    g_assert_not_reached ();
	  }
	  float p2 = GP (b, offx < 0 ? -offx + (int) ll : (int) ll,
			    offy < 0 ? -offy + (int) kk : (int) kk);
	  sum += p1 * p2;
	}
      }
      sum /= (sz - abs (offx)) * (sz - abs (offy));
      SP (mc, jj, ii, sum);
    }
  }

  float_image_export_as_jpeg (mc, "bf_correlation.jpg", sz, 0.0);

  // Maximum correlation pixel value.
  float mcpv = -FLT_MAX;
  size_t mcpx, mcpy;
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      float cp = GP (mc, jj, ii);
      if ( cp > mcpv ) {
	mcpv = cp;
	mcpx = jj;
	mcpy = ii;
      }
    }
  }

  *x_offset = (ssize_t) sz / 2 - mcpx;
  *y_offset = (ssize_t) sz / 2 - mcpy;

  return 0;			// Return success code.
}
*/

// Determine the extent of ground range image with metadata imd in
// projection coordinates space of projection_type having
// projection_parameters, returning results in *min_x, *min_y, *max_x,
// *max_y.
static void
get_extents_in_projection_coordinate_space 
  (meta_parameters *imd, 
   MapProjectedDEM *dem,
   double *min_x, double *max_x,
   double *min_y, double *max_y)
{
  // We are expecting a ground range SAR image.
  g_assert (imd->sar->image_type == 'G');

  // Determine the projection function to use to convert lat/longs to
  // map projection coordinates.
  int (*project_arr) (project_parameters_t *pps, double *lat, double *lon,
		      double **projected_x, double **projected_y, 
		      long length);
  switch ( dem->projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    project_arr = project_utm_arr;
    break;
  case POLAR_STEREOGRAPHIC:
    project_arr = project_ps_arr;
    break;
  case ALBERS_EQUAL_AREA:
    project_arr = project_albers_arr;
    break;
  case LAMBERT_CONFORMAL_CONIC:
    project_arr = project_lamcc_arr;
    break;
  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    project_arr = project_lamaz_arr;
    break;
  default:
    project_arr = NULL;
    g_assert_not_reached ();
    break;
  }

  *min_x = DBL_MAX;
  *max_x = -DBL_MAX;
  *min_y = DBL_MAX;
  *max_y = -DBL_MAX;

  // Input image dimensions in pixels in x and y directions.
  size_t ii_size_x = imd->general->sample_count;
  size_t ii_size_y = imd->general->line_count;

  // Index variables.
  size_t ii, jj;

  // We will need to find the lat/long of every image edge pixel at
  // both the minimum and maximum height present in the DEM, to be
  // sure we don't miss any portion of the image due to the height
  // argument required by meta_get_latLon.
  double min_height = DBL_MAX, max_height = -DBL_MAX;
  for ( ii = 0 ; ii < dem->data->size_y ; ii++ ) {
    for ( jj = 0 ; jj < dem->data->size_x ; jj++ ) {
      float ch = GP (dem->data, jj, ii);
      if ( ch < min_height ) { min_height = ch; }
      if ( ch > max_height ) { max_height = ch; }
    }
  }

  // The actual number of edge points of the ground range image the
  // projection coordinates of which we will be considereing.  We need
  // twice as many lat/lon storage positions as we have edge pixels
  // because we will be computing the lat lon for each image edge
  // pixel at both the minimum and maximum elevations found in the
  // DEM.
  size_t edge_point_count = 2 * (2 * ii_size_x + 2 * ii_size_y - 4);
  double *lats = g_new (double, edge_point_count);
  double *lons = g_new (double, edge_point_count);
  size_t current_edge_point = 0;
  ii = 0;
  jj = 0;
  for ( ; ii < ii_size_x - 1 ; ii++ ) {
    meta_get_latLon (imd, (double) jj, (double) ii, min_height, 
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double) jj, (double) ii, max_height, 
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  for ( ; jj < ii_size_y - 1 ; jj++ ) {
    meta_get_latLon (imd, (double)jj, (double)ii, min_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double)jj, (double)ii, max_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  for ( ; ii > 0 ; ii-- ) {
    meta_get_latLon (imd, (double)jj, (double)ii, min_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double)jj, (double)ii, max_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  for ( ; jj > 0 ; jj-- ) {
    meta_get_latLon (imd, (double)jj, (double)ii, min_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double)jj, (double)ii, max_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  g_assert (current_edge_point == edge_point_count);

  // Pointers to arrays of projected coordinates to be filled in.
  // The projection function will allocate this memory itself.
  double *x = NULL, *y = NULL;
  x = y = NULL;
  // Project all the edge pixels.
  int return_code = project_arr (&(dem->projection_parameters), lats, lons, &x,
				 &y, edge_point_count);
  g_assert (return_code == TRUE);
  // Find the extents of the image in projection coordinates.
  for ( ii = 0 ; ii < edge_point_count ; ii++ ) {
    if ( x[ii] < *min_x ) { *min_x = x[ii]; }
    if ( x[ii] > *max_x ) { *max_x = x[ii]; }
    if ( y[ii] < *min_y ) { *min_y = y[ii]; }
    if ( y[ii] > *max_y ) { *max_y = y[ii]; }
  }
  
  free (y);
  free (x);
  g_free (lons);
  g_free (lats);
}

// Main program.
int
main (int argc, char **argv)
{
  // This block tries to open a file containing frozen versions of
  // test images and coregister them a couple of different ways.  Some
  // later test code generated these so you don't have to wait to get
  // to the point in the routine where the coregistration happens to
  // test things out.  FIXME: remove this debug junk.
  /* 
  {
    FILE *ttmp = fopen ("frozen_test_images", "r");
    g_assert (ttmp != NULL);
    FloatImage *tia = float_image_thaw (ttmp);
    float_image_export_as_jpeg (tia, "tia.jpg", tia->size_x, NAN);
    FloatImage *tib = float_image_thaw (ttmp);
    float_image_export_as_jpeg (tib, "tib.jpg", tib->size_x, NAN);
    int rreturn_code = fclose (ttmp);
    g_assert (rreturn_code == 0);
    double x_offset, y_offset;
    // Regular coregistration.
    {
      float min_a, max_a, mean_a, sdev_a;
      float_image_statistics (tia, &min_a, &max_a, &mean_a, &sdev_a, NAN);    
      float min_b, max_b, mean_b, sdev_b;
      float_image_statistics (tib, &min_b, &max_b, &mean_b, &sdev_b, NAN);    
      size_t ii, jj;
      for ( ii = 0 ; ii < tia->size_y ; ii++ ) {
	for ( jj = 0 ; jj < tia->size_x ; jj++ ) {
	  SP (tia, jj, ii, GP (tia, jj, ii) * mean_b / mean_a);
	}
      }
    }
    rreturn_code = coregister (&x_offset, &y_offset, tia, tib);
    // Out of curiosity, see what a brute force coregister returns.
    //  rreturn_code = bf_coregister (&x_offset, &y_offset, tia, tib);
    rreturn_code = bf_coregister_b_cropped (&x_offset, &y_offset, tia, tib);
    float_image_free (tia);
    float_image_free (tib);
  }
  */

  // Three arguments are required.
  if ( argc != 4 ) {
    usage ();
  }

  // Get the reference DEM base name argument.
  GString *reference_dem = g_string_new (argv[argc - 3]);
  reference_dem = reference_dem; /* Remove this compiler reassurance.  */

  // Form the names of the input data and metadata files by adding
  // extensions to the image_base_name argument.
  GString *input_meta_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_meta_file, ".meta");
  GString *input_data_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_data_file, ".img");

  // Form the names of the output metadata and data files by adding
  // extensions to the output_base_name argument.
  GString *output_meta_file = g_string_new (argv[argc - 1]);
  g_string_append_printf (output_meta_file, ".meta");
  GString *output_ddr_file_base_name = g_string_new (argv[argc - 1]);
  GString *output_data_file = g_string_new (argv[argc - 1]);
  g_string_append_printf (output_data_file, ".img");
  GString *output_jpeg_file = g_string_new (argv[argc - 1]);
  g_string_append_printf (output_jpeg_file, ".jpg");

  // Load the reference DEM.  FIXME: at the moment we can only handle
  // LAS dems in UTM projection, as provided by Joanne Groves
  // Obviously this must change.
  g_print ("Loading reference DEM... ");
  GString *reference_dem_ddr = g_string_new (reference_dem->str);
  g_string_append (reference_dem_ddr, ".ddr");
  GString *reference_dem_img = g_string_new (reference_dem->str);
  g_string_append (reference_dem_img, ".img");
  MapProjectedDEM *dem
    = map_projected_dem_new_from_las (reference_dem->str,
				      reference_dem_img->str);
  g_print ("done.\n");

  // Take a quick look at the DEM for FIXME: debug purposes.
  //float_image_export_as_jpeg (dem->data, "dem_view.jpg", 
  //			      GSL_MAX (dem->data->size_x, dem->data->size_y), 
  //			      0.0);

  // We will need a slant range version of the image being terrain
  // corrected.  Defining BK_DEBUG will cause the program to try to
  // use a serialized version of the slant range image, to save the
  // time otherwise needed to load it.  This speeds debgging.
#ifndef BK_DEBUG
  g_print ("Loading SAR image and converting to slant range... ");
  SlantRangeImage *sri 
    = slant_range_image_new_from_ground_range_image (input_meta_file->str,
    						     input_data_file->str);
  g_print ("done.\n");
#else
  SlantRangeImage *sri;
  if ( g_file_test ("bk_debug_sri_freeze", G_FILE_TEST_EXISTS) ) {
    g_print ("Loading serialized slant range image file... ");
    sri = slant_range_image_thaw ("bk_debug_sri_freeze");
    g_print ("done.\n");
  }
  else {
    g_print ("Loading SAR image and converting to slant range... ");    
    sri = slant_range_image_new_from_ground_range_image (input_meta_file->str,
							 input_data_file->str);
    g_print ("done.\n");
    g_print ("Serializing SAR slant range image for future use... ");
    slant_range_image_freeze (sri, "bk_debug_sri_freeze");
    g_print ("done.\n");
  }
#endif

  // Take a quick look at the slant range image for FIXME: debug
  // purposes.
  float_image_export_as_jpeg (sri->data, "sri.jpg", 2000, NAN);

  // Read the image metadata.
  meta_parameters *imd = meta_read (input_meta_file->str);
  // We are expecting a ground range SAR image.
  g_assert (imd->sar->image_type == 'G');

  // We will essentially be coloring the DEM with radar backscatter
  // values.  But we won't need to worry about portions of the DEM for
  // which we don't have corresponding imagery.  So here we determine
  // the part of the DEM which is covered by the image and crop off
  // the parts of the DEM that we don't need.
  double min_x, max_x, min_y, max_y; // Projection coordinate range endpoints.
  get_extents_in_projection_coordinate_space (imd, dem, &min_x, &max_x,
					      &min_y, &max_y);
  MapProjectedDEM *tmp_dem = map_projected_dem_new_subdem (dem, min_x, max_x,
							   min_y, max_y);
  map_projected_dem_free (dem);
  dem = tmp_dem;

  // Take a look at the cropped DEM.
  float_image_export_as_jpeg (dem->data, "cropped_dem.jpg", 
			      GSL_MAX (dem->data->size_x, dem->data->size_y),
			      NAN);

  // If the DEM is significanly lower resolution than the SAR image,
  // we will need to generate a lower resolution version of the image
  // by averaging pixels together.  The trouble is sparsely sampling a
  // speckled thing like a SAR image gives really bad results.
  double dem_pixel_size = GSL_MAX (dem->projection_coordinates_per_x_pixel,
				   dem->projection_coordinates_per_y_pixel);
  double sar_pixel_size = GSL_MAX (imd->general->x_pixel_size, 
				   imd->general->y_pixel_size);
  // FIXME: look into this 1.5 rule of thumb and verify that its a
  // decent way to go.
  if ( dem_pixel_size > 1.5 * sar_pixel_size ) {
    long int scale_factor = ceil (dem_pixel_size / sar_pixel_size);
    if ( scale_factor % 2 != 1 ) {
      scale_factor++;
    }
    g_print ("SAR image resolution is significantly higher than DEM\n"
	     "resolution.  Scaling SAR image by a factor of %ld by \n"
	     "averaging blocks of image pixels together... ", 
	     scale_factor);
    SlantRangeImage *sri_reduced
      = slant_range_image_new_from_model_scaled (sri, scale_factor);
    g_print ("done.\n");
    slant_range_image_free (sri);
    sri = sri_reduced;

    // Take a quick look at the reduced resolution slant range image
    // for FIXME: debug purposes.
    float_image_export_as_jpeg (sri->data, "sri_reduced_res_view.jpg", 2000,
				NAN);
  }

  int svc = imd->state_vectors->vector_count;   // State vector count.
  g_assert (svc >= 3);

  double *observation_times = g_new (double, svc);
  OrbitalStateVector **observations = g_new (OrbitalStateVector *, svc);
  
  // Load the observation times, positions, and velocities from the
  // metadata, converting the latter into Geocentric equitorial
  // inertial coordinates.  Using the matrix method to convert things
  // is overkill for this simple case, but it corresponds closely with
  // the way these operation are described in the literature, so we do
  // it.
  int ii;
  // International terrestrial reference system (ITRS) coordinates of
  // state vector (the form they come in in the metadata).
  gsl_vector *itrs_pos = gsl_vector_alloc (3);
  gsl_vector *itrs_vel = gsl_vector_alloc (3);
  // Corresponding geocentric equitorial inertial (GEI) coordinates of
  // state vector (the form we need in order to propagate them).
  gsl_vector *gei_pos = gsl_vector_alloc (3);
  gsl_vector *gei_vel = gsl_vector_alloc (3);
  // Earth angle rotation matrix (see below for details).
  gsl_matrix *earm = gsl_matrix_alloc (3,3);
  // Temporary vector.
  gsl_vector *tmp = gsl_vector_alloc (3);
  DateTime *observation_date = NULL;
  for ( ii = 0 ; ii < svc ; ii++ ) {

    observation_times[ii] = imd->state_vectors->vecs[ii].time;

    // If the observation date isn't set yet (because this is our
    // first iteration of this loop), load the date and time from the
    // metadata.
    if ( observation_date == NULL ) {
      // Note that the 'julDay' field of imd->state_vectors is badly
      // misnamed (it is actually the day of year).
      observation_date = date_time_new (imd->state_vectors->year,
					imd->state_vectors->julDay,
					imd->state_vectors->second, UTC);
    }
    // Otherwise, just add the difference in observations times to the
    // previous date.
    else {
      date_time_add_seconds (observation_date, (observation_times[ii] 
						- observation_times[ii - 1]));
    }

    // Indicies of x, y, and z vector components in gsl_vector type.
    const size_t xi = 0, yi = 1, zi = 2;

    // Load position and velocity vectors in earth fixed form into
    // vectors which we can rotate.
    gsl_vector_set (itrs_pos, xi, imd->state_vectors->vecs[ii].vec.pos.x);
    gsl_vector_set (itrs_pos, yi, imd->state_vectors->vecs[ii].vec.pos.y);
    gsl_vector_set (itrs_pos, zi, imd->state_vectors->vecs[ii].vec.pos.z);
    gsl_vector_set (itrs_vel, xi, imd->state_vectors->vecs[ii].vec.vel.x);
    gsl_vector_set (itrs_vel, yi, imd->state_vectors->vecs[ii].vec.vel.y);
    gsl_vector_set (itrs_vel, zi, imd->state_vectors->vecs[ii].vec.vel.z);

    // Get the angle of the earth during this observation.
    double theta = date_time_earth_angle (observation_date);

    // Create sidereal time (earth angle) rotation matrix as described
    // in "Satellite Geodesy, 2nd Edition" by Gunter Seeber, section
    // 2.1.2, except with the angle reversed, since we are going from
    // earth fixed back to geocentric equitorial inertial (GEI)
    // coordinates.
    gsl_matrix_set (earm, 0, 0, cos (-theta));
    gsl_matrix_set (earm, 0, 1, sin (-theta));
    gsl_matrix_set (earm, 0, 2, 0.0);
    gsl_matrix_set (earm, 1, 0, -sin (-theta));
    gsl_matrix_set (earm, 1, 1, cos (-theta));
    gsl_matrix_set (earm, 1, 2, 0.0);
    gsl_matrix_set (earm, 2, 0, 0.0);
    gsl_matrix_set (earm, 2, 1, 0.0);
    gsl_matrix_set (earm, 2, 2, 1.0);

    // Perform rotation from earth fixed back to GEI coordinates.
    gsl_vector_set_zero (gei_pos);
    int return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, itrs_pos, 0.0, 
				      gei_pos);
    g_assert (return_code == GSL_SUCCESS);

    // The fixed earth velocity vectors are affected by the rotation
    // of the earth itself, so first we have to subtract this term
    // out.
    gsl_vector_set (tmp, xi, (gsl_vector_get (itrs_vel, xi) 
			      - (EARTH_ROTATION_RATE 
				 * gsl_vector_get (itrs_pos, yi))));
    gsl_vector_set (tmp, yi, (gsl_vector_get (itrs_vel, yi) 
			      + (EARTH_ROTATION_RATE 
				 * gsl_vector_get (itrs_pos, xi))));
    gsl_vector_set (tmp, zi, gsl_vector_get (itrs_vel, zi));

    // Now we can rotate the remaining velocity back into the GEI
    // system.  FIXME: we use a slightly different (by ~10
    // microdegrees) earth angle than the code in asf_meta, so the
    // velocity ends up being different by as much as 10 m/s in some
    // components -- generally not an issue for a 15 second frame but
    // bad practice nevertheless.  We ought to change things so the
    // correct values are used everywhere.
    return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, tmp, 0.0, 
				  gei_vel);
    g_assert (return_code == GSL_SUCCESS);

    // Store the result as an OrbitalStateVector instance.
    observations[ii] = orbital_state_vector_new (gsl_vector_get (gei_pos, 0),
						 gsl_vector_get (gei_pos, 1),
						 gsl_vector_get (gei_pos, 2),
						 gsl_vector_get (gei_vel, 0),
						 gsl_vector_get (gei_vel, 1),
						 gsl_vector_get (gei_vel, 2));
  }
  date_time_free (observation_date);
  gsl_vector_free (tmp);
  gsl_matrix_free (earm);
  gsl_vector_free (gei_vel);
  gsl_vector_free (gei_pos);
  gsl_vector_free (itrs_vel);
  gsl_vector_free (itrs_pos);

  g_print ("Creating orbital arc model... ");

  // Number of control points to use for the cubic splines that
  // approximate the satellite motion in the ITRSPlatformPath.
  const int cpc = 10000;
  // Guard time in seconds to add on either side of the first and last
  // observations.  This will save us in case the point of closest
  // approach to some pixel is actually outside the time window for
  // which we are provided state vectors. (though cleanup of some sort
  // will still have to be done).
  const double gt = 0.5;
  DateTime *base_date = date_time_new (imd->state_vectors->year,
				       imd->state_vectors->julDay,
				       imd->state_vectors->second,
				       UTC);

  // Create orbital arc model.
  ITRSPlatformPath *pp_fixed 
    = ITRS_platform_path_new (cpc, observation_times[0] - gt,
  			      observation_times[svc - 1] + gt,
  			      svc, base_date, observation_times, observations);

  g_print ("done.\n");

  // Now we are ready to actually paint the DEM with backscatter.
  // FIXME: actually we aren't really ready to do that yet, since we
  // have to do coregistration to line up image and DEM somehow first.
  // How isn't worked out yet though.  We go ahead and create the
  // painted DEM (that would be right if image and DEM lined up
  // correctly) anyway (unless a frozen version of dgi is revived),
  // just so we can take a loot at it if desired.
  g_assert (dem->size_y <= LONG_MAX);

  // Backscatter painted DEM.
  FloatImage *pd = float_image_new (dem->size_x, dem->size_y);  

  // Storage for converting one row of DEM pixels to geodetic
  // lat/longs/heights.
  double *lats = g_new (double, dem->size_x);
  double *lons = g_new (double, dem->size_x);
  double *heights = g_new (double, dem->size_x);

  // Find the closest point of approach for each DEM pixel, look up
  // the corresponding backscatter value from the slant range image,
  // and use it to paint the DEM.

  // Current pixel target point, in earth fixed cartesian coordinates.
  Vector cp_target;

  // Set up the GNU Scientific Library minimizer.
  int status;   // Status of the solver.
  //  const gsl_min_fminimizer_type *mimimizer_type = gsl_min_fminimizer_brent;
  const gsl_min_fminimizer_type *mimimizer_type 
    = gsl_min_fminimizer_brent;
  gsl_min_fminimizer *minimizer = gsl_min_fminimizer_alloc (mimimizer_type);
  gsl_function distance_function;
  distance_function.function = target_distance;
  target_distance_params tdp;
  tdp.target = &cp_target;
  tdp.pp = pp_fixed;
  distance_function.params = &tdp;
  // Convergence tolerance we will try for.  This amounts to about
  // 1/20 th of a pixel in the time direction for your average ERS-1
  // or ERS-2 image.
  double tolerance = 0.0001;
  // Pixels that don't converge at least this well are extra bad, we
  // coun them separately.
  double bad_tolerance = 10 * tolerance;
  // Count of number of pixels which fail to meet the convergence
  // tolerance requirements.
  long int failed_pixel_count = 0;
  // Pixels which don't even converge to within ten times the desired
  // tolerance.
  long int extra_bad_pixel_count = 0;
  // We will keep track of the mean tolerance of the convergence of
  // the different pixels so we can report that as well.
  long double mean_tolerance = 0;

  // We now go through the DEM and sort out a bunch of useful
  // information from the geometrical relationships of things.
  DEMGeomInfo *dgi;

  // If possible, we just restore a debugging version that we have
  // around.
  if ( g_file_test ("bk_debug_dgi_freeze", G_FILE_TEST_EXISTS) ) {
    FILE *tmp = fopen ("bk_debug_dgi_freeze", "r");
    g_assert (tmp != NULL);
    dgi = dem_geom_info_thaw (tmp);
    int return_code = fclose (tmp);
    g_assert (return_code == 0);


  } else {
    // Otherwise, we have to actually calculate a new instance.
    dgi = dem_geom_info_new(dem->size_x, dem->size_y);

  g_print ("Gathering DEM geometry information: \n");
  ProgressMeter *progress_meter 
    = progress_meter_new_with_callback (g_print, dem->size_y);

  // For each DEM row...
  for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {

    // Get the latitude and longitude of each pixel in this row.
    g_assert (ii <= SSIZE_MAX);
    map_projected_dem_get_latitudes_longitudes_heights (dem, ii, lats, lons, 
							heights);

    // The time of the point of closest approach (minimum discovered
    // by solver) for a DEM pixel.  We have this outside the loop so
    // we can use it as the starting point for solution to
    // neighboring pixels and save some iterations.
    double min = -DBL_MAX;

    size_t jj;
    for ( jj = 0 ; jj < dem->size_x ; jj++ ) {
      double cp_lat = lats[jj];   // Current pixel latitude.
      double cp_lon = lons[jj];   // Current pixel longitude.
      // Current pixel height.
      double cp_height = heights[jj];
      // Current target point in earth fixed coordinates.
      ITRSPoint *ctp
	= ITRS_point_new_from_geodetic_lat_long_height (cp_lat, cp_lon, 
							cp_height);

      // Copy earth fixed coordinate values to the current pixel
      // target vector.
      cp_target.x = ctp->x;
      cp_target.y = ctp->y;
      cp_target.z = ctp->z;

      ITRS_point_free (ctp);

      // Current iteration, maximum number of iterations to try.
      int iteration = 0, max_iterations = 200;  

      double sor;   // Start of time range in which to look for minimum.
      double eor;   // End of time range in which to look for minimum.

      // If this is the first pixel in a row, we are conservative and
      // search the whole orbital arc segment for the point of closest
      // approach,
      if ( jj == 0 ) {
	sor = observation_times[0] - gt;
	eor = observation_times[svc - 1] + gt;
	min = sor + eor / 2.0;
      }
      // otherwise, we use the results from the pixel right next to
      // the current pixel as a pretty good guess where the point of
      // closest approach will fall, being sure not to let the range
      // fall out of the interval supported by the arc model.
      else {
	const double max_pixel_seperation_in_time = 0.1;
	sor = min - max_pixel_seperation_in_time;
	if ( sor < observation_times[0] - gt ) {
	  sor = observation_times[0] - gt;
	}
	eor = min + max_pixel_seperation_in_time;
	if ( eor > observation_times[0] + gt ) {
	  eor = observation_times[svc - 1] + gt;
	}
      }

      gsl_set_error_handler_off ();

      int return_code = gsl_min_fminimizer_set (minimizer, &distance_function, 
						min, sor, eor);
      // If there is no minimum in this range, it means our orbital
      // arc model doesn't cover this part of the DEM, so just set the
      // painted dem pixel to zero, set a sentinal value in the dem
      // geometry informationi record, and go on to the next pixel.
      if ( return_code == GSL_FAILURE ) {
	float_image_set_pixel (pd, jj, ii, 0.0);
	float_image_set_pixel (dgi->slant_range_value, jj, ii, -1);
	break;
      }
	
      gsl_set_error_handler (NULL);

      do {
	iteration++;
	status = gsl_min_fminimizer_iterate (minimizer);
    
	min = gsl_min_fminimizer_x_minimum (minimizer);
	sor = gsl_min_fminimizer_x_lower (minimizer);
	eor = gsl_min_fminimizer_x_upper (minimizer);
    
	status = gsl_min_test_interval (sor, eor, tolerance, 0.0);
      }
      while (status == GSL_CONTINUE && iteration < max_iterations);

      // How close did the convergence come to perfection?
      double error = sor - eor;

      // We want to keep some statistics on how many pixels fail to
      // converge to within our desired tolerance.
      if ( status != GSL_SUCCESS ) {
	if ( fabs (sor - eor) < bad_tolerance ) {
	  failed_pixel_count++;
	}
	else {
	  extra_bad_pixel_count++;
	}
      }

      // Update our notion of the mean tolerance.
      if ( G_UNLIKELY (ii == 0 && jj == 0) ) {
	mean_tolerance = error;
      }
      else {
	// The recurence relation we use to compute the running mean
	// needs the current 1-based pixel number.
	unsigned long int pixel_number = ii * dem->size_x + jj + 1;
	mean_tolerance += (error - mean_tolerance) / pixel_number;
      }
    
      // The resulting minimum is time in the arc model of the point
      // of closest approach.  FIXME: how to verify input images are
      // zero-doppler processed?  Possible with meta->sar->deskewed.
      // Also probably need to check meta->sar->look_count and make
      // sure it's 1.
      double solved_time = min;

      // The slant range can be found from the distance between the
      // target and the platform at the point of closest approach.
      Vector poca; 
      ITRS_platform_path_position_at_time (pp_fixed, solved_time, &poca);
      Vector *poca_to_target = vector_copy (&poca);
      vector_subtract (poca_to_target, &cp_target);
      double solved_slant_range = vector_magnitude (poca_to_target);
      vector_free (poca_to_target);

      // If the DEM pixel falls in the slant range image, set the
      // discovered geometry information.
      if ( slant_range_image_contains (sri, solved_slant_range, 
				       solved_time, 1e-3) ) {
	// FIXME: is cp_height (the height above the WGS84 ellipsoid)
	// really what's wanted here?
	dem_geom_info_set (dgi, jj, ii, &cp_target, solved_time, 
			   solved_slant_range, cp_height, &poca);
      }
      else {
	// otherwise, since we aren't absolutely sure we have image
	// over this portion of the DEM, set a sentinal value in the
	// geometry information to indicate this,
	float_image_set_pixel (dgi->slant_range_value, jj, ii, -1);
      }
    }

    // Print progress update every so many lines.
    const int lines_per_progress_update = 100;
    if ( (ii + 1) % lines_per_progress_update == 0 
	 || (size_t) (ii + 1) == dem->size_y ) {
      progress_meter_advance (progress_meter, 100);
    }
  }

  progress_meter_free (progress_meter);

  FILE *tmp = fopen ("bk_debug_dgi_freeze", "w");
  g_assert (tmp != NULL);
  dem_geom_info_freeze (dgi, tmp);
  int return_code = fclose (tmp);
  g_assert (return_code == 0);
  
  // Test to see if the thaw 'ed version is is the same as the one we
  // just froze.
  tmp = fopen ("bk_debug_dgi_freeze", "r");
  g_assert (tmp != NULL);
  DEMGeomInfo *dgi_thaw_test = dem_geom_info_thaw (tmp);
  return_code = fclose (tmp);
  g_assert (return_code == 0);

  g_assert (dem_geom_info_equals (dgi, dgi_thaw_test, 0.0000001));

  } // End of dgi construction by calculation.

  // Make a quick jpeg showing the part of the DEM we believe the
  // image covers for FIXME debug purposes.
  /*
  FloatImage *dem_coverage = float_image_new (dem->size_x, dem->size_y);
  // For each DEM row except the first and last...
  for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {
    long int jj;
    // For every other DEM pixel except the first and last two...
    for ( jj = 0 ; (size_t) jj < dem->size_x ; jj++ ) {
      if ( dem_geom_info_get_slant_range_value (dgi, jj, ii) < 0 ) {
	SP (dem_coverage, jj, ii, GP (dem->data, jj, ii));
      }
      else {
	SP (dem_coverage, jj, ii, 2 * GP (dem->data, jj, ii));
      }
    }
  }
  float_image_export_as_jpeg (dem_coverage, "dem_coverage.jpg",
			      GSL_MAX (dem_coverage->size_x,
				       dem_coverage->size_y), NAN);
  */

  //  FloatImage * lsm = lsm_generate_mask(dgi);
  //  lsm = lsm;		    /* FIXME: remove compiler reassurance.  */

  // Because the geolocation of images is often quite bad, we will
  // march throught the DEM and create a simulated SAR image, then
  // measure the offset of the simulated image from the actual slant
  // range image.  We can then subject the slant range image pixel
  // lookups used to color the DEM to the discovered offset.
  g_print ("Generating simulated SAR image... ");
  SlantRangeImage *sim_img 
    = slant_range_image_new_empty (sri->upper_left_pixel_range,
				   sri->upper_left_pixel_time,
				   sri->slant_range_per_pixel,
				   sri->time_per_pixel,
				   sri->data->size_x,
				   sri->data->size_y);

  // We can hopefully get away with ignoring the backscatter
  // contributions of the very edge facets, which saves the pain of
  // special handling for them.

  // For each DEM row except the first and last...
  int neg_sim_pixels = 0;
  for ( ii = 1 ; (size_t) ii < dem->size_y  - 1; ii++ ) {
    long int jj;
    // For every other DEM pixel except the first and last two...
    //    for ( jj = 1 ; (size_t) jj < dem->size_x - 2; jj += 2 ) {
    for ( jj = 1 ; (size_t) jj < dem->size_x - 1; jj++ ) {


      // We are interested in the four triangular facets defined by
      // the non-diagonal neighbors, so to entirely cover the image,
      // we need only considerer every other pixel in each row.  We
      // need to cover even pixels in one row, odd pixels in the next,
      // etc.  So here we compute the DEM x index being considered the
      // center of the four facet for each pixel in this row of
      // pixels.
      size_t xi;
      if ( ii % 2 == 1 ) {
	xi = jj;
      }
      else {
	xi = jj + 1;
      }

      // If we are in shadow this DEM pixel contributes nothing to the
      // backscatter in the simulated image.
      /* For the moment we just assume this doesn't happen.
      if ( lsm_image_mask_value_is_shadow (lsm, ii, xi) ) {
	continue;
      }
      */

      // If this DEM pixel and all its neighbors that we will be
      // considering don't all fall in the image, it contributes
      // nothing.
      if ( dem_geom_info_get_slant_range_value (dgi, xi, ii) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii - 1) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi + 1, ii) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii + 1) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi - 1, ii) < 0 ) {
	continue;
      }

      // We want to compute the normals of the upper left, upper
      // right, lower left, and lower right triangular facets, then
      // average them together to get a normal at a given dem pixel.
      // We can do this by forming vectors from the current dem pixel
      // to its four non-diagonal neighbors, then taking cross
      // products of adjacent pairs of vectors, then adding the
      // results together and normalizing.

      // Vectors for the current DEM pixel, and the pixels above, to
      // the right, below, and to the left respectively, in ITRS
      // coordinates.
      Vector c, a, r, b, l;
      vector_set (&c, float_image_get_pixel (dgi->cp_target_x, xi,  ii),
		  float_image_get_pixel (dgi->cp_target_y, xi, ii),
		  float_image_get_pixel (dgi->cp_target_z, xi, ii));
      vector_set (&a, float_image_get_pixel (dgi->cp_target_x, xi, ii - 1),
		  float_image_get_pixel (dgi->cp_target_y, xi, ii - 1),
		  float_image_get_pixel (dgi->cp_target_z, xi, ii - 1));
      vector_set (&r, float_image_get_pixel (dgi->cp_target_x, xi + 1, ii),
		  float_image_get_pixel (dgi->cp_target_y, xi + 1, ii),
		  float_image_get_pixel (dgi->cp_target_z, xi + 1, ii));
      vector_set (&b, float_image_get_pixel (dgi->cp_target_x, xi, ii + 1),
		  float_image_get_pixel (dgi->cp_target_y, xi, ii + 1),
		  float_image_get_pixel (dgi->cp_target_z, xi, ii + 1));
      vector_set (&l, float_image_get_pixel (dgi->cp_target_x, xi - 1, ii),
		  float_image_get_pixel (dgi->cp_target_y, xi - 1, ii),
		  float_image_get_pixel (dgi->cp_target_z, xi - 1, ii));

      // We now change the vectors to the neighbors st instead of
      // pointing from the ITRS origin to the target DEM pixel, they
      // point from the target DEM pixel to the appropriate neighbor
      // DEM pixel.
      vector_subtract (&a, &c);
      vector_subtract (&r, &c);
      vector_subtract (&b, &c);
      vector_subtract (&l, &c);

      // Compute the normal vectors for each of the facets.
      Vector *uln = vector_cross (&a, &l);
      Vector *urn = vector_cross (&r, &a);
      Vector *lrn = vector_cross (&b, &r);
      Vector *lln = vector_cross (&l, &b);

      Vector *n = vector_copy (uln);
      vector_add (n, urn);
      vector_add (n, lrn);
      vector_add (n, lln);

      if ( vector_magnitude (n) < 0.00000001 ) {
	g_warning ("tiny normal vector magnitude at DEM pixel %ld, %ld\n",
		   (long int) jj, (long int) ii);
	g_print ("c: %lf %lf %lf  \n"
		 "a: %lf %lf %lf  \n"
		 "r: %lf %lf %lf  \n"
		 "b: %lf %lf %lf  \n"
		 "l: %lf %lf %lf  \n",
		 c.x, c.y, c.z,
		 a.x, a.y, a.z,
		 r.x, r.y, r.z,
		 b.x, b.y, b.z,
		 l.x, l.y, l.z);
      }
      else {
	vector_multiply (n, 1.0 / vector_magnitude (n));
      }
      /*
      vector_multiply (n, 1.0 / vector_magnitude (n));
      g_assert (vector_magnitude (n) > 0.5);
      */

      // Recall the slant range and imaging time for this DEM pixel.
      double sr = dem_geom_info_get_slant_range_value (dgi, xi, ii);
      double it = dem_geom_info_get_imaging_time (dgi, xi, ii);

      // Compute a vector from the current target to the platform at
      // the point of closest aproach.
      Vector pp;
      ITRS_platform_path_position_at_time (pp_fixed, it, &pp);
      Vector *target_to_poca = vector_copy (&pp);
      vector_subtract (target_to_poca, &c);

      // The incidence angle is the angle between the mean normal of
      // the facets and the line between the current target (DEM
      // pixel) and the point of closest approach.
      double incidence_angle = vector_angle (n, target_to_poca);

      // We will assume that the backscatter returned by the dirt in
      // this DEM pixel is proportional to the cosine of the angle
      // between the normal vector and the vector between the
      // platform and the target, i.e. the return is proportional to
      // the cosine of the local incidence angle at the point of
      // closest approach.
      
      // FIXME: for the moment we exclude backslopes this way, since
      // the shadow mask isn't finished yet.  FIXME: should be M_PI /
      // 2.0, but I don't want to change it at the moment while trying
      // to sort out coregistration issues.
      if ( !(incidence_angle > M_PI ) ) {
	if ( slant_range_image_contains (sim_img, sr, it, 1e-3) ) {
	  if ( cos (incidence_angle) < 0 ) {
	    neg_sim_pixels++;
	  }
	  slant_range_image_add_energy (sim_img, sr, it, 
					pow (cos (incidence_angle), 2.0));
	}
      }
  
      vector_free (uln);
      vector_free (urn);
      vector_free (lrn);
      vector_free (lln);
      vector_free (n);
      vector_free (target_to_poca);
    }
  }
  g_print ("done.\n");
  g_print ("Negative simulator energy addiions: %d\n", neg_sim_pixels);

  // Take a look at the simulated image for (FIXME) debug purposes.
  float_image_export_as_jpeg (sim_img->data, "sim_img.jpg",
			      GSL_MIN (sim_img->data->size_x,
				       sim_img->data->size_y), NAN);

  // Break slant range and simulated images up into tiles.
  const int tile_size = 200;	// Tile size in pixels.
  GPtrArray *image_tile_records = g_ptr_array_new ();
  for ( ii = 0 ; (size_t) ii < sim_img->data->size_y / tile_size ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; (size_t) jj < sim_img->data->size_x / tile_size ; jj++ ) {

      // Simulated image tile.
      FloatImage *cst = float_image_new_subimage (sim_img->data, 
						  jj * tile_size,
						  ii * tile_size, tile_size,
						  tile_size);
      // How many zeros are there in the simulated image tile?  If
      // there are too many this tile falls partly outside the DEM or
      // in a big hole we don't want to use it for coregistration, so
      // we go on to the next tile.  We have to specificly exclude
      // such tiles, since they are likely to have low autocorrelation
      // under rotation scores (see elsewhere).
      int zero_count = 0;
      size_t kk, ll;
      for ( kk = 0 ; kk < cst->size_y ; kk++ ) {
	for ( ll = 0 ; ll < cst->size_x ; ll++ ) {
	  if ( GP (cst, ll, kk) == 0.0 ) {
	    zero_count++;
	  }
	}
      }
      double max_zero_fraction = 0.05;
      if ( (double) zero_count / (tile_size * tile_size) 
	   > max_zero_fraction ) {
	float_image_free (cst);
	continue;
      }
	
      // Corresponding tile in slant range image.
      FloatImage *cit = float_image_new_subimage (sri->data, jj * tile_size,
						  ii * tile_size, tile_size,
						  tile_size);
      image_tile_record_type *itr = g_new (image_tile_record_type, 1);
      itr->ras = rotational_autocorrelation_score (cst);
      itr->sim_img = cst;
      itr->sri_img = cit;
      itr->txi = jj;
      itr->tyi = ii;
      g_ptr_array_add (image_tile_records, itr);
    }
  }

  g_ptr_array_sort (image_tile_records, 
		    (GCompareFunc) compare_rotational_autocorrelation_scores);

  FloatImage *translation_map_x 
    = float_image_new (sim_img->data->size_x / tile_size,
		       sim_img->data->size_y / tile_size);

  FloatImage *translation_map_y 
    = float_image_new (sim_img->data->size_x / tile_size,
		       sim_img->data->size_y / tile_size);

  for ( ii = 0 ; (size_t) ii < image_tile_records->len ; ii++ ) {
    GString *cstn = g_string_new ("image_tiles/sim_image_tile_");
    g_string_append_printf (cstn, "%d.jpg", (int) ii);
    FloatImage *csimt = ((image_tile_record_type *) 
			 g_ptr_array_index (image_tile_records, ii))->sim_img;
    float min, max, mean, sdev;
    float_image_statistics (csimt, &min, &max, &mean, &sdev, NAN);
    if ( min != max ) {
      float_image_export_as_jpeg (csimt, cstn->str, 
				  GSL_MAX (csimt->size_x, csimt->size_y), 
				  NAN);
    }
    g_string_free (cstn, TRUE);
    GString *citn = g_string_new ("image_tiles/image_tile_");
    g_string_append_printf (citn, "%d.jpg", (int) ii);
    FloatImage *cimt = ((image_tile_record_type *) 
			g_ptr_array_index (image_tile_records, ii))->sri_img;
    float_image_statistics (cimt, &min, &max, &mean, &sdev, NAN);
    if ( min != max ) {
      float_image_export_as_jpeg (cimt, citn->str, 
				  GSL_MAX (cimt->size_x, cimt->size_y), 
				  NAN);
    }
    g_string_free (citn, TRUE);
  }

  for ( ii = 0 ; (size_t) ii < image_tile_records->len ; ii++ ) {
    double x_offset, y_offset;
    image_tile_record_type *itr = g_ptr_array_index (image_tile_records, ii);
    int return_code 
      = coregister (&x_offset, &y_offset, itr->sim_img, itr->sri_img);
    g_assert (return_code == 0);

    GString *ditscs = g_string_new ("eog image_tiles/image_tile_");
    g_string_append_printf (ditscs, "%d.jpg", (int) ii);
    //    pid_t pid = fork ();
    /*
    if ( pid == 0 ) { 
      system (ditscs->str); 
      exit (EXIT_SUCCESS);
    }
    */
    g_string_free (ditscs, TRUE);

    GString *dstscs = g_string_new ("eog image_tiles/sim_image_tile_");
    g_string_append_printf (dstscs, "%d.jpg", (int) ii);
    /*
    pid = fork ();
    if ( pid == 0 ) {
      system (dstscs->str);
      exit (EXIT_SUCCESS);
    }
    */
    g_string_free (dstscs, TRUE);

    g_print ("Tile %lld offsets: %lg, %lg\n", (long long int) ii, x_offset,
	     y_offset);

    // FIXME: remove debug
    FILE *tmp = fopen ("frozen_test_images", "w");
    g_assert (tmp != NULL);
    float_image_freeze (itr->sim_img, tmp);
    float_image_freeze (itr->sri_img, tmp);
    return_code = fclose (tmp);
    g_assert (return_code == 0);

    tmp = fopen ("frozen_test_images", "r");
    g_assert (tmp != NULL);
    FloatImage *tia = float_image_thaw (tmp);
    FloatImage *tib = float_image_thaw (tmp);
    return_code = fclose (tmp);
    g_assert (return_code == 0);
    g_assert (float_image_equals (itr->sim_img, tia, 0.00001));
    g_assert (float_image_equals (itr->sri_img, tib, 0.00001));

    // Out of curiosity, see what a brute force coregister returns.
    /*
    return_code = bf_coregister (&x_offset, &y_offset, itr->sim_img,
				 itr->sri_img);
    g_assert (return_code == 0);
    g_print ("Tile %lld brute force offsets: %lg, %lg\n", (long long int) ii,
	     x_offset, y_offset);
    */

    // Brute for coregistration with cropped b image method.
    bf_coregister_b_cropped (&x_offset, &y_offset, itr->sim_img,
			     itr->sri_img);
    g_assert (return_code == 0);
    g_print ("Tile %lld cropped unnormalized brute force offsets: %lg, %lg\n",
	     (long long int) ii, x_offset, y_offset);
    g_print ("Tile %lld coordinates: %lld, %lld\n", (long long int) ii,
	     (long long int) itr->txi, (long long int) itr->tyi);
    SP (translation_map_x, itr->txi, itr->tyi, x_offset);
    SP (translation_map_y, itr->txi, itr->tyi, y_offset);
  }

  float_image_export_as_jpeg (translation_map_x, "translation_map_x.jpg",
			      GSL_MAX (translation_map_x->size_x,
				       translation_map_x->size_y), NAN);

  float_image_export_as_jpeg (translation_map_y, "translation_map_y.jpg",
			      GSL_MAX (translation_map_y->size_x,
				       translation_map_y->size_y), NAN);

  g_print("Generating layover/shadow mask... \n");
  FloatImage * lsm = lsm_generate_mask(dgi);
  lsm = lsm;
  g_print("done\n");
  
  g_print ("Painting %ld DEM pixel rows with SAR image pixel values...\n",
	   (long int) dem->size_y);

  ProgressMeter *progress_meter 
    = progress_meter_new_with_callback (g_print, dem->size_y);

  // For each DEM row...
  for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {

    g_assert (ii <= SSIZE_MAX);

    size_t jj;
    // For each pixel in the row...
    for ( jj = 0 ; jj < dem->size_x ; jj++ ) {

      // Pull out previously calculated values
      double solved_slant_range =
	dem_geom_info_get_slant_range_value(dgi, jj, ii);
      double solved_time =
	dem_geom_info_get_imaging_time(dgi, jj, ii);

      // Look up the backscatter value for the found slant range and
      // time.
      float backscatter;
      if ( slant_range_image_contains (sri, solved_slant_range, 
				       solved_time, 1e-3) ) {
	backscatter 
	  = slant_range_image_sample (sri, solved_slant_range, solved_time,
				      FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC);
      }
      else {
	// We don't have image over this part of the DEM, so set to
	// the mask value (FIXME: better mask handling needed).
	backscatter = 0.0;
      }

      // Set the pixel in the painted dem.
      float_image_set_pixel (pd, jj, ii, backscatter);
    }

    // Print progress update every so many lines.
    const int lines_per_progress_update = 100;
    if ( (ii + 1) % lines_per_progress_update == 0 
	 || (size_t) (ii + 1) == dem->size_y ) {
      progress_meter_advance (progress_meter, 100);
    }
  }

  g_print ("Done painting DEM.\n");

  g_print ("Average convergence tolerance of the time of point of closest\n"
	   "approach minimization (TOPOCAM) was %Lg seconds.\n",
	   mean_tolerance);
  g_print ("For a total of %ld pixel(s), the TOPOCAM did not converge to\n"
	   "within the desired precision of %lg seconds.\n", 
	   failed_pixel_count, 
	   tolerance);
  g_print ("For a total of %ld pixel(s) the TOPOCAM did not even converge to\n"
	   "within %lg seconds.\n", extra_bad_pixel_count, 
	   bad_tolerance);

  g_print ("Log scaling the painted DEM... ");
  // Log scale the painted DEM.
  size_t jj;
  for ( ii = 0 ; ii < (int) pd->size_x ; ii++ ) {
    for ( jj = 0 ; jj < pd->size_y ; jj++ ) {
      float cp = float_image_get_pixel (pd, ii, jj);
      float opv;   // Output pixel value.	
      if ( cp <= 0 ) {
	opv = 0.0;
      }
      else {
	float_image_set_pixel (pd, ii, jj, 10 * log10 (cp));
      }
    }
  }
  g_print ("done.\n");

  g_print ("Exporting painted DEM as a JPEG image... ");
  float_image_export_as_jpeg (pd, output_jpeg_file->str, 
			      GSL_MAX (pd->size_x, pd->size_y), 0.0);
  g_print ("done.\n");

  // Generate a LAS version of the output metadata.  The only works if
  // we have a LAS DEM given as the input.  It intended for testing
  // work with Joanne.
  struct DDR output_ddr;
  lasErr error_code = c_getddr (reference_dem->str, &output_ddr);
  g_assert (error_code == 0);
  output_ddr.dtype = 4;		/* Means floating point samples.  */
  error_code = c_putddr (output_ddr_file_base_name->str, &output_ddr);
  g_assert (error_code == 0);
  
  g_print ("Saving painted DEM as raw data... ");
  float_image_store (pd, output_data_file->str,
		     FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_print ("done.\n");

  // FIXME: this debugging schlop can safely be removed.
  /*
  FloatImage *volcano = float_image_new_subimage (pd, 2100, 3100, 300, 300);
  float_image_export_as_jpeg (volcano, "volcano.jpg",
			      GSL_MAX (volcano->size_x, volcano->size_y), 0.0);
  */

  // Free all the memory and resources we used.
  g_print ("Freeing resources... ");
  g_string_free (reference_dem, TRUE);
  g_string_free (input_meta_file, TRUE);
  g_string_free (input_data_file, TRUE);
  g_string_free (output_meta_file, TRUE);
  g_string_free (output_data_file, TRUE);
  g_string_free (output_jpeg_file, TRUE);
  g_string_free (reference_dem_ddr, TRUE);
  g_string_free (reference_dem_img, TRUE);
  map_projected_dem_free (dem);
  slant_range_image_free (sri);
  meta_free (imd);
  g_free (observation_times);
  for ( ii = 0 ; ii < svc ; ii++ ) {
    orbital_state_vector_free (observations[ii]);
  }
  g_free (observations);
  date_time_free (base_date);
  ITRS_platform_path_free (pp_fixed);
  float_image_free (pd);
  g_free (lats);
  g_free (lons);
  g_free (heights);
  gsl_min_fminimizer_free (minimizer);
  progress_meter_free (progress_meter);
  g_print ("done.\n");

  exit (EXIT_SUCCESS);
}

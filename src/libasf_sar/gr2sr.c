/******************************************************************************
NAME:   gr2sr - Remaps a ground range image to a slant range image

DESCRIPTION:
        Remaps a ground range image to a slant range image using resampling
        vectors calculated by the  gr2ml_vec (azimuth resampling vector) and
        gr2sr_vec (range resampling vector) subroutines.  The resampling
        uses bi-linear interpolation based on the vectors calculated.

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    infile.img          Input data file
    infile.meta         Input metadata about image file

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "asf_sar.h"
#include <asf.h>
#include <asf_meta.h>
#include <asf_reporting.h>
#include <assert.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_roots.h>

#define VERSION 0.1

static void gr2sr_vec(meta_parameters *meta, float srinc, float *gr2sr)
{
  int    i;             /* Counter                                       */
  float  r_sc;          /* radius from center of the earth for satellite */
  float  r_earth;       /* radius of the earth                           */
  float  r_close;       /* near slant range distance                     */
  float  rg0, rg;        /* Ground range to first pixel, G.R. to cur pix  */
  float  a, x, x2, y;   /* temporaries                                   */
  float  grinc;  /* Slant and Ground range pixel spacings         */
  float  rslant;        /* slant range distance to current pixel         */

  /* Radius from the center of the earth to the spacecraft, slant range to
     first pixel, and radius of the earth */
  r_sc = meta_get_sat_height(meta, 0, 0);
  r_close = meta_get_slant(meta,0,0);  
  r_earth = meta_get_earth_radius_pp(meta);

  /* Set the ground range and slant range increments */
  grinc = meta->general->x_pixel_size;

  /* calculate ground range to first point */
  a = (r_sc-r_earth)/r_earth;
  x = 1.0+a;
  x2 = x * x;
  y = r_close/r_earth;
  rg0 = r_earth * acos((1.0 + x2 - y*y) / (2.0*x));

  /* begin loop */
  for(i = 0; i<MAX_IMG_SIZE; i++) {
    rslant = r_close + i *srinc;
    y = rslant/r_earth;
    rg = r_earth*acos((1.0+x2-y*y)/(2.0*x));
    gr2sr[i] = (rg - rg0)/grinc;
  }
}

static char * replExt(const char *filename, const char *ext)
{
  char *ret = MALLOC(sizeof(char)*(strlen(filename)+strlen(ext)+5));
  strcpy(ret, filename);

  char *p = findExt(ret);
  if (p) *p = '\0';

  if (ext[0] != '.') strcat(ret, ".");
  strcat(ret, ext);

  return ret;
}

/* Utility struct for iterate_sr_pixel_size */
struct sr_pixel_size_params {
        meta_parameters *meta;
        double target_gr_pixel_size;
        int onp;
};

/* The function we minimize in iterate_sr_pixel_size -- returns 0 when
   we have the slant range pixel size that results in the desired gr
   pixel size */
static double getObjective(double slantPer, void *params)
{
    struct sr_pixel_size_params *p =
        (struct sr_pixel_size_params *)params;

    meta_parameters *meta = p->meta;

    int ns = p->onp; // meta->general->sample_count;
    int nl = meta->general->line_count;

    double er = meta_get_earth_radius(meta, nl/2, ns/2);
    double satHt = meta_get_sat_height(meta, nl/2, ns/2);

    double slantFirst, slantPer_ignore, slantLast;
    meta_get_slants(meta, &slantFirst, &slantPer_ignore);

//  Had to take this out... see comment for corresponding line in deskew_dem
//    slantFirst += slantPer*meta->general->start_sample+1;
    slantPer *= meta->sar->sample_increment;
    slantLast = slantFirst + (ns-1)*slantPer;

    double minPhi = acos((satHt*satHt+er*er - slantFirst*slantFirst)/
                         (2.0*satHt*er));
    double maxPhi = acos((satHt*satHt+er*er - slantLast*slantLast)/
                         (2.0*satHt*er));
    double phiMul=(ns-1)/(maxPhi-minPhi);
    double grPixelSize = er/phiMul;

    return grPixelSize - p->target_gr_pixel_size;
}

/* Uses a gsl root finder to locate the slant range pixel size that
   we need to use that will result in the user's desired ground range
   pixel size */
static double iterate_sr_pixel_size(meta_parameters *meta, double pixel_size,
                                    double start_sr_pixel_size, int output_ns)
{
    int status;
    int iter = 0, max_iter = 100;
    const gsl_root_fsolver_type *T;
    gsl_root_fsolver *s;
    gsl_function F;
    gsl_error_handler_t *prev;
    struct sr_pixel_size_params params;
    double lo, hi;
    double sr_pixel_size;

    F.function = &getObjective;
    F.params = &params;

    params.meta = meta;
    params.target_gr_pixel_size = pixel_size;
    params.onp = output_ns;

    prev = gsl_set_error_handler_off();

    lo = 0;
    hi = pixel_size;

    T = gsl_root_fsolver_brent;
    s = gsl_root_fsolver_alloc (T);
    gsl_root_fsolver_set (s, &F, lo, hi);

    do {
        ++iter;
        status = gsl_root_fsolver_iterate(s);
        sr_pixel_size = gsl_root_fsolver_root(s);
        status = gsl_root_test_residual(
            getObjective(sr_pixel_size, (void*)&params), 1.0e-7);
    } while (status == GSL_CONTINUE && iter < max_iter);

    if (status == GSL_SUCCESS) {
        printf("Converged after %d iterations.\n", iter);
        printf("SR Pixel Size: %.3f m\n",sr_pixel_size);
        printf("   (for comparison) Original Pixel Size: %.3f m\n",
               start_sr_pixel_size);
        printf("   GR with this Target: %.3f m\n",
               getObjective(sr_pixel_size, (void*)&params) + pixel_size);
    } else {
        asfPrintWarning("Failed to determine slant range pixel size!\n"
                        "iter: %d, sr_pixel_size=%.3f, res=%.5f\n"
                        "Proceeding using the starting estimate: %.3f m\n"
                        "Starting points were: lo: %.3f -> %.4f\n"
                        "                      hi: %.3f -> %.4f\n",
                        iter, sr_pixel_size, 
                        getObjective(sr_pixel_size, (void*)&params),
                        start_sr_pixel_size,
                        lo, getObjective(lo, (void*)&params),
                        hi, getObjective(hi, (void*)&params));
        sr_pixel_size = start_sr_pixel_size;
    }

    gsl_set_error_handler(prev);
    return sr_pixel_size;
}

static int 
gr2sr_pixsiz_imp(const char *infile, const char *outfile, float srPixSize,
                 float targetGrPixelSize)
{
  meta_parameters *inMeta, *outMeta;

  int   np, nl;         /* in number of pixels,lines      */
  int   onp, onl;       /* out number of pixels,lines     */
  int   ii;
  float *gr2sr;    /* GR 2 SR resampling vector for Range  */
  int   *lower;    /* floor of gr2sr vector                */
  int   *upper;    /* ceiling of gr2sr vector              */
  float *ufrac;    /* Upper fraction from gr2sr vector     */
  float *lfrac;    /* Lower fraction from gr2sr vector     */

  float *inBuf;          /* Input buffer                  */
  float *outBuf;         /* Output buffer                 */
  FILE  *fpi, *fpo;      /* File pointers                 */
  int   line;            /* Loop counter                  */
  char  *iimgfile;       /* .img input file               */
  char  *oimgfile;       /* .img output file              */
 
  gr2sr = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);
  upper = (int *) MALLOC(sizeof(int) * MAX_IMG_SIZE);
  lower = (int *) MALLOC(sizeof(int) * MAX_IMG_SIZE);
  ufrac = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);
  lfrac = (float *) MALLOC(sizeof(float) * MAX_IMG_SIZE);

  inMeta = meta_read(infile);

  nl = inMeta->general->line_count;
  np = inMeta->general->sample_count;

  /* If srPixSize < 0, calculate the proper pixel size.
     If targetGrPixelSize > 0, we'll need this value as well, to
         seed the search for the desired pixel size */
  if (srPixSize < 0 || targetGrPixelSize > 0) {
    /*
      In an e-mail from Rick:
       Slant Range Pixel Size = C (speed of light) / [SampleRate (sample 
       rate) * 2,000,000.]

       SampleRate can be extracted from the L1 metadata :
          RNG CMPLX SAMPLE RATE        18.9599991

       The meta->sar->range_sampling_rate is 10^6 times the value above,
       so we use C/(2*meta->sar->range_sampling_rate)
    */
    srPixSize = SPD_LIGHT / ((2.0 * inMeta->sar->range_sampling_rate) *
      inMeta->general->sample_count / inMeta->sar->original_sample_count);
  }

  onl=nl;
  gr2sr_vec(inMeta, srPixSize, gr2sr);
  
  /* Determine the output image size */
  onp = 0;
  for (ii=0; ii<MAX_IMG_SIZE; ii++) {
     if (gr2sr[ii]<np) onp=ii; /* gr input still in range-- keep output */
     else break; /* gr input is off end of image-- stop sr output */
  }

  /* If the user wants us to target a specified ground range pixel size,
     we do an iterative search to find the right slant range pixel size */
  if (targetGrPixelSize > 0) {
      srPixSize = (float)iterate_sr_pixel_size(inMeta, targetGrPixelSize,
                                               srPixSize, onp);
  }

  /* Split gr2sr into resampling coefficients */
  for (ii=0; ii<onp; ii++) {
     lower[ii] = (int) gr2sr[ii];
     upper[ii] = lower[ii] + 1;
     ufrac[ii] = gr2sr[ii] - (float) lower[ii];
     lfrac[ii] = 1.0 - ufrac[ii];
     if (lower[ii]>=np) lower[ii]=np-1; /* range clip */
     if (upper[ii]>=np) upper[ii]=np-1; /* range clip */
  }
  
  //outMeta = meta_copy(inMeta);
  outMeta = meta_read(infile);
  outMeta->sar->slant_shift += ((inMeta->general->start_sample)
                                * inMeta->general->x_pixel_size);
  outMeta->general->start_sample = 0.0;
  outMeta->sar->sample_increment = 1.0;
  outMeta->sar->image_type       = 'S';
  outMeta->general->x_pixel_size = srPixSize;
  outMeta->general->sample_count = onp;

  iimgfile = replExt(infile, "img");
  oimgfile = replExt(outfile, "img");

  fpi = FOPEN(iimgfile,"rb");
  fpo = FOPEN(oimgfile,"wb");
  inBuf = (float *) MALLOC (np*sizeof(float));
  outBuf = (float *) MALLOC (onp*sizeof(float));

  for (line = 0; line < onl; line++) {
    get_float_line(fpi, inMeta, line, inBuf);
    for (ii=0; ii<onp; ii++) { /* resample to slant range */
       outBuf[ii] = inBuf[lower[ii]]*lfrac[ii]+inBuf[upper[ii]]*ufrac[ii];
    }
    put_float_line(fpo,outMeta,line,outBuf);
  }

  meta_write(outMeta, outfile);
  meta_free(inMeta);
  meta_free(outMeta);

  FREE(ufrac);
  FREE(lfrac);
  FREE(gr2sr);
  FREE(upper);
  FREE(lower);

  FREE(inBuf);
  FREE(outBuf);
  FCLOSE(fpi);
  FCLOSE(fpo);
  FREE(iimgfile);
  FREE(oimgfile);

  return TRUE;
}

/**
  Converts to slant range using the specified slant range pixel size.

  If srPixSize<0, then an appropriate slant range pixel size will be
  calculated from the metadata.
*/
int 
gr2sr_sr_pixsiz(const char *infile, const char *outfile, float srPixSize)
{
    gr2sr_pixsiz_imp(infile, outfile, srPixSize, -1);
}

/**
  Converts to slant range, so that when the image is converted back to
  ground range, it will have the specified ground range pixel size.
*/
int 
gr2sr_gr_pixsiz(const char *infile, const char *outfile, float grPixSize)
{
    
    gr2sr_pixsiz_imp(infile, outfile, -1, grPixSize);
}

/**
  Converts to slant range, using an appropriate slant range pixel size
  calculated from the metadata.
*/
int gr2sr(const char *infile, const char *outfile)
{
  return gr2sr_sr_pixsiz(infile, outfile, -1);
}

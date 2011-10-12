/*******************************************************************

   Resampling library method based on the "resample" code originally
   written by Tom Logan.

   Here are the comments from that program, edited to reflect that it
   is no longer a standalone program.  It still operates on files
   instead of on in-memory images, however.

   Also modified to use separate scale factors in the  x and y
   directions, as well as taking scale factors as arguments instead
   of pixel sizes.

DESCRIPTION:
        Resamples the input file to a desired output pixel resolution
    (assumed to be in meters).  Images are downsized by a factor equal
    to the desired resolution divided by the true resolution of the
    image (as given in the image's metadata).  This is the size used
    for the kernel processing.  Resampling proceeds by selecting the
    pixel in the input image that closest represents the middle of the
    new pixel in the output image.  The output pixel is given the value
    of the average of the kernel around the choosen input pixel.  This
    program combines the programs filter.c and subsample.c from the
    original EDC Terrain Correction software (LAS Modules).
        The image used as input is single-banded byte valued
    LAS 6.0 .img file.
        The output file produced by resample is a flat file (as per
    LAS 6.0 image standard) and a LAS ddr file is also created or updated
    to provide image metadata (size, etc).

ALGORITHM DESCRIPTION:
    Establish kernel processing parameters
    copy input metadata to output metadata (with update)
    Open input and output files
    for each output line
       calculate file read position
       read next kernel from input file
       for each output pixel
   apply kernel to input data at appropriate position to get output value
       write output line to file
    Close input and output files

*******************************************************************/
#include "asf.h"
#include "asf_endian.h"
#include <asf_raster.h>

static float filter(      /****************************************/
    float *inbuf,         /* input image buffer                   */
    int    nl,            /* number of lines for inbuf            */
    int    ns,            /* number of samples per line for inbuf */
    int    x,             /* sample in desired line               */
    int    nsk,           /* number of samples in kernel          */
    int    nn_flag)       /* true if we should just use nearest   */
                          /* instead of interpolating b/w points  */
{                         /****************************************/
    float  kersum =0.0;                    /* sum of kernel       */
    int    half   =(nsk-1)/2,              /* half size kernel    */
           base   =(x-half),               /* index into inbuf    */
           total  =0,                      /* valid kernel values */
           i, j;                           /* loop counters       */
                                           /***********************/
                                           
    if (nn_flag) {

      if (base>=0 && base<nl*ns)
        return inbuf[base];
      else if (base<0)
        return inbuf[0];
      else if (base>=nl*ns)
        return inbuf[ns*nl-1];
      asfPrintError("Not reached.\n");

    }
    else {
      for (i = 0; i < nl; i++)
      {
        for (j = x-half; j <= x+half; j++)
        {
          if (base>=0 && base<nl*ns && inbuf[base] != 0 && j < ns)
          {
            kersum += inbuf[base];
            total++;
          }
          base++;
        }
        base += ns;
        base -= nsk;
      }

      if (total != 0)
        kersum /= (float) total;

      return (kersum);
    }
}

static int
resample_impl(const char *infile, const char *outfile,
              double xscalfact, double yscalfact, int update_meta,
              int nn_flag)
{
    FILE            *fpin, *fpout;  /* file pointer                   */
    float           *inbuf,         /* stripped input buffer          */
                    *outbuf;        /* stripped output buffer         */
    meta_parameters *metaIn, *metaOut;
    int      np, nl,                /* in number of pixels,lines      */
             onp, onl,              /* out number of pixels,lines     */
             xnsk,                  /* kernel size in samples (x)     */
             ynsk,                  /* kernel size in samples (y)     */
             xhalf,yhalf,           /* half of the kernel size        */
             n_lines,               /* number of lines in this kernel */
             s_line,                /* start line for input file      */
             xi = 0,                /* inbuf int x sample #           */
             yi = 0,                /* inbuf int y line #             */
             i,j,k,l;               /* loop counters                  */
    float    xpixsiz,               /* range pixel size               */
             ypixsiz,               /* azimuth pixel size             */
             xbase,ybase,           /* base sample/line               */
             xrate,yrate,           /* # input pixels/output pixel    */
             tmp;

    //asfPrintStatus("\n\n\nResample: Performing filtering and subsampling..\n\n");
    //asfPrintStatus("  Input image is %s\n",infile);
    //asfPrintStatus("  Output image is %s\n\n",outfile);

    metaIn = meta_read(infile);
    metaOut = meta_read(infile);
    nl = metaIn->general->line_count;
    np = metaIn->general->sample_count;
    xpixsiz = metaIn->general->x_pixel_size/xscalfact;
    ypixsiz = metaIn->general->y_pixel_size/yscalfact;
    xnsk = (int) (xpixsiz/metaIn->general->x_pixel_size + 0.5);
    ynsk = (int) (ypixsiz/metaIn->general->y_pixel_size + 0.5);
    if (xnsk%2 == 0) xnsk++;              /* Must be odd sized kernel */
    if (ynsk%2 == 0) ynsk++;              /* Must be odd sized kernel */

    onp = (int) (np * xscalfact);
    onl = (int) (nl * yscalfact);
    xbase = 1.0 / (2.0 * xscalfact) - 0.5;
    xrate = 1.0 / xscalfact;
    xhalf = (xnsk-1)/2;

    ybase = 1.0 / (2.0 * yscalfact) - 0.5;
    yrate = 1.0 / yscalfact;
    yhalf = (ynsk-1)/2;
    n_lines = ynsk;

    inbuf= (float *) MALLOC (ynsk*np*sizeof(float));
    outbuf = (float *) MALLOC (onp*sizeof(float));

   /*----------  Open the Input & Output Files ---------------------*/
    char *imgfile = MALLOC(sizeof(char) * (10 + strlen(outfile)));
    strcpy(imgfile, outfile);
    append_ext_if_needed(imgfile, ".img", NULL);

    // NOTE: Do NOT send just the basename to fopenImage().  IF perchance, a file
    // by that name exists and does not have a file extension, e.g. a renamed
    // workreport file without the usual ".txt" extension ...just the basename
    // with no extension, then fopenImage() will preferentially open that file.
    // It is likely that many read errors will occur if you run into this situation.
    char *infile_img = MALLOC(sizeof(char) * (10 + strlen(infile)));
    strcpy(infile_img, infile);
    append_ext_if_needed(infile_img, ".img", NULL);
    fpin=fopenImage(infile_img,"rb");
    if (fpin == NULL)
      asfPrintError("Cannot open input file for binary read:\n  %s\n", infile);

    metaOut->general->line_count = onl;
    metaOut->general->sample_count = onp;

    if (!metaOut->optical)
        metaOut->general->data_type = REAL32;

    if (update_meta)
    {
      /* Write output metadata file */
      metaOut->general->x_pixel_size = xpixsiz;
      metaOut->general->y_pixel_size = ypixsiz;
      metaOut->general->line_scaling /= yscalfact;
      metaOut->general->sample_scaling /= xscalfact;
      if (metaOut->sar) {
        metaOut->sar->range_time_per_pixel /= xscalfact;
        metaOut->sar->azimuth_time_per_pixel /= yscalfact;
        metaOut->sar->range_doppler_coefficients[1] /= xscalfact;
        metaOut->sar->range_doppler_coefficients[2] /= xscalfact / xscalfact;
        metaOut->sar->azimuth_doppler_coefficients[1] /= yscalfact;
        metaOut->sar->azimuth_doppler_coefficients[2] /= yscalfact * yscalfact;
      }
      if (metaOut->projection) {
        metaOut->projection->perX = xpixsiz;
        metaOut->projection->perY = -ypixsiz;
      }
      // This is kind of a kludge to work around differences in how
      // the slant/ground range code in meta_get_latLon() differs from
      // the transform/airsar/projected code handles the start/line
      // sample values.  (They are applied before/after the perX/Y
      // is applied.)
      else if (!metaOut->transform && !metaOut->airsar) {        
        metaOut->general->start_sample *= xscalfact;
        metaOut->general->start_line *= yscalfact;
      }
    }

    char **band_name = extract_band_names(metaIn->general->bands,
                                          metaIn->general->band_count);

    char *metafile = appendExt(outfile, ".meta");
    meta_write(metaOut, metafile);

    for (k=0; k < metaIn->general->band_count; ++k)
    {
        if (metaIn->general->band_count != 1)
            asfPrintStatus("Resampling band: %s\n", band_name[k]);

        fpout=fopenImage(imgfile, k==0 ? "wb" : "ab");
        n_lines = ynsk;
        /*--------  Process inbuf to give outbuf ------------------------*/
        for (i = 0; i < onl; i++)
        {
            /*--------- Read next set of lines for kernel ---------------*/
            yi = i * yrate + ybase + 0.5;
            s_line = yi-yhalf;
            if (s_line < 0) s_line = 0;
            if (nl < ynsk+s_line) n_lines = nl-s_line;

            s_line += k*nl;
            get_float_lines(fpin, metaIn, s_line, n_lines, inbuf);
	    if (metaIn->general->radiometry >= r_SIGMA_DB &&
		metaIn->general->radiometry <= r_GAMMA_DB) {
	      for (l=0; l<(np*n_lines); l++) {
		tmp = inbuf[l];
		inbuf[l] = pow(10.0, tmp/10.0);
	      }
	    }
	    

            /*--------- Produce the output line and write to disk -------*/
            for (j = 0; j < onp; j++)
            {
                xi = j * xrate + xbase + 0.5;
                outbuf[j] = filter(inbuf,n_lines,np,xi,xnsk,nn_flag);
		if (metaOut->general->radiometry >= r_SIGMA_DB &&
		    metaOut->general->radiometry <= r_GAMMA_DB) {
		  tmp = outbuf[j];
		  outbuf[j] = 10.0 * log10(tmp);
		}
            }

            put_float_line(fpout, metaOut, i, outbuf);
            asfLineMeter(i, onl);
        }

        FCLOSE(fpout);
    }

    for (i=0; i < metaIn->general->band_count; i++)
        FREE(band_name[i]);
    FREE(band_name);

    meta_free(metaOut);
    meta_free(metaIn);

    FCLOSE(fpin);

    FREE(inbuf);
    FREE(outbuf);

    FREE(imgfile);
    FREE(metafile);
    FREE(infile_img);

    return(0);
}

static void 
get_scalfact(const char *infile, double xpixsiz, double ypixsiz,
             double *xscalfact, double *yscalfact)
{
  meta_parameters *metaIn = meta_read(infile);
  
  *xscalfact = metaIn->general->x_pixel_size/xpixsiz;
  *yscalfact = metaIn->general->y_pixel_size/ypixsiz;
  
  meta_free(metaIn);  
}


/*********************** External methods ***********************************/

// Resample- specify scale factors (in both directions)
// This one is for backwards compatibility, use resample_ext.
int resample(const char *infile, const char *outfile,
             double xscalfact, double yscalfact)
{
  return resample_impl(infile, outfile, xscalfact, yscalfact, TRUE, FALSE);
}

// Resample- specify scale factors (in both directions), with nearest
//           neighbor allowed as an option
int resample_ext(const char *infile, const char *outfile,
                 double xscalfact, double yscalfact, int use_nn)
{
  return resample_impl(infile, outfile, xscalfact, yscalfact, TRUE, use_nn);
}

// Resample- specify a square pixel size
int resample_to_square_pixsiz(const char *infile, const char *outfile,
                              double pixsiz)
{
  return resample_to_pixsiz(infile, outfile, pixsiz, pixsiz);
}

// Resample- specify pixel size (in both directions)
int resample_to_pixsiz(const char *infile, const char *outfile,
                       double xpixsiz, double ypixsiz)
{
  double xscalfact, yscalfact;
  get_scalfact(infile, xpixsiz, ypixsiz, &xscalfact, &yscalfact);
  
  return resample_ext(infile, outfile, xscalfact, yscalfact, FALSE);
}

// Resample- specify pixel size (in both directions), use nearest neighbor
int resample_to_pixsiz_nn(const char *infile, const char *outfile,
                          double xpixsiz, double ypixsiz)
{
  double xscalfact, yscalfact;
  get_scalfact(infile, xpixsiz, ypixsiz, &xscalfact, &yscalfact);
  
  return resample_ext(infile, outfile, xscalfact, yscalfact, TRUE);
}

// Resample- specify scale factors, but don't update the metadata!
int resample_nometa(const char *infile, const char *outfile,
                    double xscalfact, double yscalfact)
{
  return resample_impl(infile, outfile, xscalfact, yscalfact, FALSE, FALSE);
}


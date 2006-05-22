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
#include "asf_reporting.h"

static float filter(      /****************************************/
    float *inbuf,         /* input image buffer                   */
    int    nl,            /* number of lines for inbuf            */
    int    ns,            /* number of samples per line for inbuf */
    int    x,             /* sample in desired line               */
    int    nsk)           /* number of samples in kernel          */
{                         /****************************************/
    float  kersum =0.0;                    /* sum of kernel       */
    int    half   =(nsk-1)/2,              /* half size kernel    */
           base   =(x-half),               /* index into inbuf    */
           total  =0,                      /* valid kernel values */ 
           i, j;                           /* loop counters       */
                                           /***********************/
    for (i = 0; i < nl; i++)
      {
       for (j = x-half; j <= x+half; j++)
         {
          if (inbuf[base] != 0 && j < ns)
           {
             kersum += inbuf[base];
             total  += 1;
           }
          base++;
         }
       base += ns;
       base -= nsk;
      }
    if (total != 0) kersum /= (float) total;
    return (kersum);
}

static int
resample_impl(char *infile, char *outfile,
	      double xscalfact, double yscalfact, int update_meta)
{
    FILE            *fpin, *fpout;  /* file pointer                   */
    float           *inbuf,         /* stripped input buffer          */
                    *outbuf;        /* stripped output buffer         */
    meta_parameters *metaIn, *metaOut;
    int      np, nl,                /* in number of pixels,lines      */
             onp, onl,              /* out number of pixels,lines     */
             xnsk,                  /* kernel size in samples (x)     */
             ynsk,                  /* kernel size in samples (y)     */
	     xhalf,yhalf,	    /* half of the kernel size        */
	     n_lines,		    /* number of lines in this kernel */
	     s_line,		    /* start line for input file      */
             xi = 0,                /* inbuf int x sample #           */
             yi = 0,                /* inbuf int y line #             */
             j;                     /* loop counters                  */
    int      i;
    float    xpixsiz,               /* range pixel size               */
             ypixsiz,               /* azimuth pixel size             */
             xbase,ybase,           /* base sample/line               */
             xrate,yrate;           /* # input pixels/output pixel    */

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

    onp = (np) * xscalfact;
    onl = (nl) * yscalfact;

    xbase = 1.0 / (2.0 * xscalfact) - 0.5;
    xrate = 1.0 / xscalfact;
    xhalf = (xnsk-1)/2;

    ybase = 1.0 / (2.0 * yscalfact) - 0.5;
    yrate = 1.0 / yscalfact;
    yhalf = (ynsk-1)/2;
    n_lines = ynsk;
 
    inbuf= (float *) MALLOC (xnsk*np*sizeof(float));
    outbuf = (float *) MALLOC (onp*sizeof(float));
 
   /*----------  Open the Input & Output Files ---------------------*/
    fpin=fopenImage(infile,"rb");
    fpout=fopenImage(outfile,"wb");

    if (update_meta)
    {
      /* Write output metadata file */ 
      metaOut->general->line_count = onl;
      metaOut->general->sample_count = onp;
      metaOut->general->data_type = REAL32;
      metaOut->general->x_pixel_size = xpixsiz;
      metaOut->general->y_pixel_size = ypixsiz;
      metaOut->sar->range_time_per_pixel /= xscalfact;
      metaOut->sar->azimuth_time_per_pixel /= yscalfact;
      metaOut->sar->range_doppler_coefficients[1] /= xscalfact;
      metaOut->sar->range_doppler_coefficients[2] /= xscalfact / xscalfact;
      metaOut->sar->azimuth_doppler_coefficients[1] /= yscalfact;
      metaOut->sar->azimuth_doppler_coefficients[2] /= yscalfact * yscalfact;
    }

    meta_write(metaOut, outfile);

  /*--------  Process inbuf to give outbuf ------------------------*/
    for (i = 0; i < onl; i++)
      {
       /*--------- Read next set of lines for kernel ---------------*/
       yi = i * yrate + ybase + 0.5;
       s_line = yi-yhalf;
       if (s_line < 0) s_line = 0;
       if (nl < ynsk+s_line) n_lines = nl-s_line;
      
       get_float_lines(fpin, metaIn, s_line, n_lines, inbuf);

       /*--------- Produce the output line and write to disk -------*/ 
       for (j = 0; j < onp; j++)
         {
          xi = j * xrate + xbase + 0.5;
          outbuf[j] = filter(inbuf,n_lines,np,xi,xnsk);
         }
       
       put_float_line(fpout, metaOut, i, outbuf);
       //asfLineMeter(i, onl);
      }
       
    //asfPrintStatus("Finished.\n");

    meta_free(metaOut);
    meta_free(metaIn);

    FCLOSE(fpin);                 
    FCLOSE(fpout);

    return(0);
}

/*********************** External methods ***********************************/

// Resample- specify a square pixel size
int resample_to_square_pixsiz(char *infile, char *outfile, double pixsiz)
{
  resample_to_pixsiz(infile, outfile, pixsiz, pixsiz);
}

// Resample- specify pixel size (in both directions)
int resample_to_pixsiz(char *infile, char *outfile,
		       double xpixsiz, double ypixsiz)
{
    meta_parameters *metaIn;
    double xscalfact, yscalfact;

    metaIn = meta_read(infile);

    xscalfact = metaIn->general->x_pixel_size/xpixsiz;
    yscalfact = metaIn->general->y_pixel_size/ypixsiz;

    meta_free(metaIn);

    resample(infile, outfile, xscalfact, yscalfact);

}

// Resample- specify scale factors, but don't update the metadata!
int resample_nometa(char *infile, char *outfile,
		    double xscalfact, double yscalfact)
{
  resample_impl(infile, outfile, xscalfact, yscalfact, FALSE);
}

// Resample- specify scale factors (in both directions)
int resample(char *infile, char *outfile, double xscalfact, double yscalfact)
{
  resample_impl(infile, outfile, xscalfact, yscalfact, TRUE);
}

